/**
 * vim: set ts=4 :
 * =============================================================================
 * SourceMod Sample Extension
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */

#include "extension.h"

/**
 * @file extension.cpp
 * @brief Implement extension code here.
 */

#include <string>
#include <cstddef>
#include <map>

#include <sm_namehashset.h>

#include <tier1/bitbuf.h>
#include <eiface.h>
#include <iserverunknown.h>
#include <iservernetworkable.h>
#include <server_class.h>
#include <dt_common.h>
#include <dt_send.h>
#include <edict.h>

#include <safetyhook.hpp>

SendVarEdit g_SendVarEdit;		/**< Global singleton for extension's main interface */

SMEXT_LINK(&g_SendVarEdit);

class EditBuffer
{
    std::vector<uint8_t> buffer;

public:
    EditBuffer() {};

    void* reserve(size_t bytes)
    {
        size_t size = buffer.size();
        buffer.resize(size + bytes);
        return &buffer[size];
    }

    template<typename T>
    const T* push(const T &object)
    {
        T* buffer = (T*)reserve(sizeof(T));
        *buffer = object;
        return buffer;
    }

    void clear() { buffer.clear(); }
};
EditBuffer g_editbuffer;

enum EditAction
{
    SET,
    REPLACE,
    OMIT,
};

struct EditInfo
{
    EditAction action;
    int priority;
    int propindex;
    const SendProp* prop;
    const DVariant* var;
};

class EditEntry
{
public:
    EditEntry(int entity, int client)
        : entity(entity), client(client)
    {
        edits.reserve(8);
        has_setter = false;
    }

    int entity;
    int client;
    std::vector<EditInfo> edits;
    bool has_setter;
};

class EditList {
private:
    int entity;
    uint8_t indices[256];
    std::vector<EditEntry> entries;

public:
    EditList(int entity) : entity(entity)
    {
        entries.reserve(64);
        std::fill(indices, indices + 256, 0xFF);
    }

    inline int GetEntity() const { return entity; }

    inline EditEntry* GetEntry(int client)
    {
        return indices[client] != 0xFF ? &entries[indices[client]] : nullptr;
    }

    EditEntry* AddOrModifyEntry(int client)
    {
        if (indices[client] != 0xFF)
            return &entries[indices[client]];

        indices[client] = entries.size();
        entries.push_back(EditEntry(entity, client));
        return &entries.back();
    }
};
std::vector<EditList> g_editlists;

int g_editindices[MAX_EDICTS] = {-1};





struct RegisterInfo { const char* name;  uint8_t offset; };
#define REG_INFO(NAME) { #NAME, (uint8_t)offsetof(safetyhook::Context, NAME) }
RegisterInfo g_registers[] {
#if SAFETYHOOK_ARCH_X86_64
    REG_INFO(xmm0),
    REG_INFO(xmm1),
    REG_INFO(xmm2),
    REG_INFO(xmm3),
    REG_INFO(xmm4),
    REG_INFO(xmm5),
    REG_INFO(xmm6),
    REG_INFO(xmm7),
    REG_INFO(xmm8),
    REG_INFO(xmm9),
    REG_INFO(xmm10),
    REG_INFO(xmm11),
    REG_INFO(xmm12),
    REG_INFO(xmm13),
    REG_INFO(xmm14),
    REG_INFO(xmm15),
    REG_INFO(rflags),
    REG_INFO(r15),
    REG_INFO(r14),
    REG_INFO(r13),
    REG_INFO(r12),
    REG_INFO(r11),
    REG_INFO(r10),
    REG_INFO(r9),
    REG_INFO(r8),
    REG_INFO(rdi),
    REG_INFO(rsi),
    REG_INFO(rdx),
    REG_INFO(rcx),
    REG_INFO(rbx),
    REG_INFO(rax),
    REG_INFO(rbp),
    REG_INFO(rsp),
    REG_INFO(trampoline_rsp),
    REG_INFO(rip),
#else
    REG_INFO(xmm0),
    REG_INFO(xmm1),
    REG_INFO(xmm2),
    REG_INFO(xmm3),
    REG_INFO(xmm4),
    REG_INFO(xmm5),
    REG_INFO(xmm6),
    REG_INFO(xmm7),
    REG_INFO(eflags),
    REG_INFO(edi),
    REG_INFO(esi),
    REG_INFO(edx),
    REG_INFO(ecx),
    REG_INFO(ebx),
    REG_INFO(eax),
    REG_INFO(ebp),
    REG_INFO(esp),
    REG_INFO(trampoline_esp),
    REG_INFO(eip),
#endif
};

const RegisterInfo* FindRegister(const char* name)
{
    auto register_propindex = std::find_if(
        std::begin(g_registers),
        std::end(g_registers),
        [name](const RegisterInfo &info) { return strcmp(info.name, name) == 0; }
    );

    return register_propindex != std::end(g_registers) ? register_propindex : nullptr;
}

struct VariableInfo
{
    bool is_register;
    bool is_read;
};

template <typename T>
struct Variable
{
    VariableInfo info;
    int32_t offset;
    int32_t read;
};

class Context
{
public:
    safetyhook::Context &registers;

public:
    Context(safetyhook::Context &context)
        : registers(context)
    {}

    template<typename T>
    static inline T& read(void* address, int32_t offset)
    {
        return *(T*)((uintptr_t)address + offset);
    }

    template<typename T>
    inline T& reg(size_t reg) { return read<T>(&registers, reg); }

    template<typename T>
    inline T& stack(int32_t offset)
    {
#if SAFETYHOOK_ARCH_X86_64
        return read<T>(registers.rbp, offset);
#else
        return read<T>(registers.ebp, offset);
#endif
    }

    inline uintptr_t bp()
    {
#if SAFETYHOOK_ARCH_X86_64
        return registers.rbp;
#else
        return registers.ebp;
#endif
    }

    inline void*& ip()
    {
#if SAFETYHOOK_ARCH_X86_64
        return *(void**)&registers.rip;
#else
        return *(void**)&registers.eip;
#endif
    }

    inline uintptr_t& flags()
    {
#if SAFETYHOOK_ARCH_X86_64
        return registers.rflags;
#else
        return registers.eflags;
#endif
    }

    template<typename T>
    inline T& var(Variable<T> &var)
    {
        void* base = var.info.is_register ? &registers : (void*)bp();
        T &value = read<T>(base, var.offset);

        if (var.info.is_read)
            value = read<T>((void*)&value, var.offset);

        return value;
    }
};

#if SAFETYHOOK_ARCH_X86_64
#if defined WIN32
#define _PLATFORM "windows64"
#elif defined _LINUX
#define _PLATFORM "linux64"
#elif defined _OSX
#define _PLATFORM "mac64"
#endif
#else
#if defined WIN32
#define _PLATFORM "windows"
#elif defined _LINUX
#define _PLATFORM "linux"
#elif defined _OSX
#define _PLATFORM "mac"
#endif
#endif

class VariableReader : public ITextListener_SMC
{
private:
    int ignore_level;

    std::pair<const std::string, Variable<void>>* current;
    int current_level;
    bool current_assigned;
    bool current_read;

private:
    std::map<std::string, Variable<void>, std::less<>> variables;

private:
    bool IsPlatform(const char* string, bool* current_platform, bool* other_platform)
    {
        *current_platform = strcmp(string, _PLATFORM) == 0;
        *other_platform = !*current_platform;
        if (
            strcmp(string, "linux") != 0 && strcmp(string, "linux64") != 0 &&
            strcmp(string, "windows") != 0 && strcmp(string, "windows64") != 0 &&
            strcmp(string, "mac") != 0 && strcmp(string, "mac64") != 0
        ) {
            *other_platform = false;
        }

        return *current_platform || *other_platform;
    }

    bool ParseInt(const char* string, int32_t* value)
    {
        char* end = nullptr;
        intptr_t parsed = std::strtoll(string, &end, 0);
        if (errno == ERANGE || !(INT32_MIN <= parsed && parsed <= INT32_MAX) || string + strlen(string) != end)
            return false;

        *value = static_cast<int32_t>(parsed);
        return true;
    }

    bool ParseRead(const char* string, Variable<void>* variable)
    {
        if (!ParseInt(string, &variable->read))
            return false;

        variable->info.is_read = true;

        return true;
    }

    bool ParseStack(const char* string, Variable<void>* variable)
    {
        if (!ParseInt(string, &variable->offset))
            return false;

        variable->info.is_register = false;

        return true;
    }

    bool ParseRegister(const char* string, Variable<void>* variable)
    {
        const RegisterInfo* reg = FindRegister(string);
        if (!reg)
            return false;

        variable->info.is_register = true;
        variable->offset = reg->offset;

        return true;
    }

public:
    virtual void ReadSMC_ParseStart() override
    {
        ignore_level = 0;

        current = nullptr;
        current_level = 0;
        current_assigned = false;
        current_read = false;
    };

    virtual void ReadSMC_ParseEnd(bool halted, bool failed) override
    {
        if ((halted || failed) && current)
            variables.erase(current->first);

        ignore_level = 0;

        current = nullptr;
        current_level = 0;
        current_assigned = false;
        current_read = false;
    }

    virtual SMCResult ReadSMC_NewSection(const SMCStates* states, const char* name) override
    {
        bool current_platform, other_platform;
        IsPlatform(name, &current_platform, &other_platform);

        // skip over other platforms
        if (ignore_level || other_platform) {
            ignore_level++;
            return SMCResult_Continue;
        }

        if (current)
            current_level++;

        // go into current platform
        if (current_platform)
            return SMCResult_Continue;

        // can't start a new variable when one is already gettig parsed
        if (current)
            return SMCResult_HaltFail;

        // start new variable
        auto assignment = variables.insert_or_assign(name, Variable<void>{});
        current = &*assignment.first;
        current_level++;

        return SMCResult_Continue;
    }

    virtual SMCResult ReadSMC_LeavingSection(const SMCStates* states) override
    {
        // inside skipped platform
        if (ignore_level) {
            ignore_level--;
            return SMCResult_Continue;
        }

        if (current)
            current_level--;

        // continue if there is no current variable or is still parsing one
        if (!current || current_level)
            return SMCResult_Continue;

        // finished parsing a variable

        // fail if variable wasn't assigned a register or stack offset
        if (!current_assigned)
            return SMCResult_HaltFail;

        current = nullptr;
        current_assigned = false;
        current_read = false;

        return SMCResult_Continue;
    }

    virtual SMCResult ReadSMC_KeyValue(const SMCStates* states, const char* key, const char* value) override
    {
        // inside skipped platform
        if (ignore_level)
            return SMCResult_Continue;

        // parse read offset
        if (strcmp(key, "read") == 0) {
            if (current_read)
                return SMCResult_HaltFail;

            if (!ParseRead(value, &current->second))
                return SMCResult_HaltFail;

            current_read = true;
            return SMCResult_Continue;
        }

        // check for a platform specifier key
        bool current_platform, other_platform;
        if (value && IsPlatform(key, &current_platform, &other_platform)) {
            if (other_platform)
                return SMCResult_Continue;

            // stack and register are parsed with key variable
            key = value;
            value = nullptr;
        }
        else {
            return SMCResult_HaltFail;
        }

        if (current_assigned)
            return SMCResult_HaltFail;

        // try to parse stack offset
        if (ParseStack(key, &current->second)) {
            current_assigned = true;
            return SMCResult_Continue;
        }

        // try to parse register name
        if (ParseRegister(key, &current->second)) {
            current_assigned = true;
            return SMCResult_Continue;
        }

        return SMCResult_HaltFail;
    }

public:
    template<typename T>
    bool GetVariable(const char* name, Variable<T>* variable)
    {
        auto search = variables.find(name);
        if (search == variables.end())
            return false;

        *variable = *(Variable<T>*)&search->second;

        return true;
    }

    std::map<std::string, Variable<void>, std::less<>>::const_iterator begin() const { return variables.begin(); }
    std::map<std::string, Variable<void>, std::less<>>::const_iterator end() const { return variables.end(); }
};
VariableReader g_variables;

struct PackedEntity;
struct CEntityWriteInfo;

struct Struct_CEntityWriteInfo
{
    int entity; // int
    int output; // bf_write*
    int client; // int
    int oldpack; // PackedEntity*
    int newpack; // PackedEntity*
};

struct Variables_SV_DetermineUpdateType
{
    Variable<CEntityWriteInfo*> entitywriteinfo;
    Variable<int> propcount;
};

struct Points_SV_DetermineUpdateType
{
    void* props_changed_call;
    void* propcount_positive_block;
};

struct Variables_SendTable_WritePropList
{
    Variable<SendTable*> table;
    Variable<bf_write*> output;
    Variable<int> propindex;
    Variable<int> output_lastpropindex;
};

struct Points_SendTable_WritePropList
{
    void* loop_continue;
};

struct PropTypeFns
{
    void (*Encode)(void* _struct, DVariant* var, SendProp* prop, bf_write* buffer, int entity);
    void* other[7];
    void (*SkipProp)(SendProp* prop, bf_read* buffer);
};

struct Struct_CSendTablePrecalc
{
    int props; // SendProp**
    int propcount; // int
};

struct GameInfo
{
    Struct_CEntityWriteInfo struct_EWI;

    Variables_SV_DetermineUpdateType vars_DUT;
    Points_SV_DetermineUpdateType points_DUT;

    Variables_SendTable_WritePropList vars_WPL;
    Points_SendTable_WritePropList points_WPL;

    PropTypeFns* proptypefns;
    Struct_CSendTablePrecalc struct_STP;
};
GameInfo g_gameinfo;





bool GetEntityInfo(IPluginContext* pContext, int entref, int* index, edict_t** edict, CBaseEntity** entity, IServerUnknown** unknown, IServerNetworkable** networkable)
{
    *index = gamehelpers->ReferenceToIndex(entref);
    *edict = gamehelpers->EdictOfIndex(*index);
    if (!(*edict))
        return pContext->ThrowNativeError("Entity %d (%d) is invalid.", *index, entref);

    *entity = gamehelpers->ReferenceToEntity(entref);

    *unknown = (IServerUnknown*)(*entity);
    *networkable = (*unknown)->GetNetworkable();
    if (!(*networkable))
        return pContext->ThrowNativeError("Edict %d (%d) is not networkable", *index, entref);

    return true;
}

// float SendProxyAngle(float angle)
cell_t smn_SendProxyAngle(IPluginContext* pContext, const cell_t* params)
{
    return sp_ftoc(anglemod(params[1]));
}

// void SendProxyQAnglesNative(const float qangles[3], float vec[3])
cell_t smn_SendProxyQAngles(IPluginContext* pContext, const cell_t* params)
{
    cell_t* sp_qangles;
    pContext->LocalToPhysAddr(params[1], &sp_qangles);

    cell_t* sp_vec;
    pContext->LocalToPhysAddr(params[2], &sp_vec);

    for (int i = 0; i < 3; i++)
        sp_vec[i] = sp_ftoc(anglemod(sp_ctof(sp_qangles[i])));

    return 0;
}

// int SendProxyEHandle(int entity)
cell_t smn_SendProxyEHandle(IPluginContext* pContext, const cell_t* params)
{
    int entref = params[1];
    if (entref == -1)
        return INVALID_NETWORKED_EHANDLE_VALUE;

    int index;
    edict_t* edict;
    CBaseEntity* entity;
    IServerUnknown* unknown;
    IServerNetworkable* networkable;
    if (!GetEntityInfo(pContext, entref, &index, &edict, &entity, &unknown, &networkable))
        return INVALID_NETWORKED_EHANDLE_VALUE;

    const CBaseHandle* handle = &unknown->GetRefEHandle();

    // doing what SendProxy_EHandleToInt is doing
    int serial = handle->GetSerialNumber() & ((1 << NUM_NETWORKED_EHANDLE_SERIAL_NUMBER_BITS) - 1);
    return handle->GetEntryIndex() | (serial << MAX_EDICT_BITS);
}

HandleType_t g_SendVarType = 0;
class SendVarTypeHandler : public IHandleTypeDispatch
{
public:
    virtual void OnHandleDestroy(HandleType_t type, void* object) override
    {
    }
};
SendVarTypeHandler g_SendVarTypeHandler;

std::vector<Handle_t> g_editdatahandles;

Handle_t CreateSendVarHandle(IPluginContext* pContext, const DVariant &var)
{
    Handle_t handle = handlesys->CreateHandle(
        g_SendVarType,
        (void*)g_editbuffer.push(var),
        pContext->GetIdentity(),
        myself->GetIdentity(),
        nullptr
    );

    g_editdatahandles.push_back(handle);

    return handle;
}

const DVariant* ReadSendVarHandle(IPluginContext* pContext, Handle_t handle)
{
    const DVariant* var = nullptr;
    HandleSecurity security(nullptr, myself->GetIdentity());
    HandleError error = handlesys->ReadHandle(handle, g_SendVarType, &security, (void**)&var);
    if (error != HandleError_None) {
        pContext->ThrowNativeError("Invalid sendvar data handle %x (error %d)", handle, error);
        return nullptr;
    }

    return var;
}

// Handle SendVarInt(any value)
cell_t smn_SendVarInt(IPluginContext* pContext, const cell_t* params)
{
    int value = params[1];

    DVariant var;
    var.m_Type = DPT_Int;
    var.m_Int = value;

    return CreateSendVarHandle(pContext, var);
}

// SendVarFloat(float value)
cell_t smn_SendVarFloat(IPluginContext* pContext, const cell_t* params)
{
    float value = params[1];

    DVariant var;
    var.m_Type = DPT_Float;
    var.m_Float = value;

    return CreateSendVarHandle(pContext, var);
}

// Handle SendVarVector(const float vec[3])
cell_t smn_SendVarVector(IPluginContext* pContext, const cell_t* params)
{
    cell_t* sp_vec;
    pContext->LocalToPhysAddr(params[1], &sp_vec);

    Vector vec;
    for (int i = 0; i < 3; i++)
        vec[i] = sp_ctof(sp_vec[i]);

    DVariant var;
    var.m_Type = DPT_Vector;
    for (int i = 0; i < 3; i++)
        var.m_Vector[i] = vec[i];

    return CreateSendVarHandle(pContext, var);
}

// Handle SendVarVectorXY(const float vec[2])
cell_t smn_SendVarVectorXY(IPluginContext* pContext, const cell_t* params)
{
    cell_t* sp_vec;
    pContext->LocalToPhysAddr(params[1], &sp_vec);

    Vector vec;
    for (int i = 0; i < 2; i++)
        vec[i] = sp_ctof(sp_vec[i]);

    DVariant var;
    var.m_Type = DPT_VectorXY;
    for (int i = 0; i < 2; i++)
        var.m_Vector[i] = vec[i];

    return CreateSendVarHandle(pContext, var);
}

// Handle SendVarString(const char[] string)
cell_t smn_SendVarString(IPluginContext* pContext, const cell_t* params)
{
    char* sp_string;
    pContext->LocalToString(params[1], &sp_string);

    size_t length = strlen(sp_string);
    char* string = (char*)g_editbuffer.reserve(length + 1);
    memcpy(string, sp_string, length + 1);

    DVariant var;
    var.m_Type = DPT_String;
    var.m_pString = string;

    return CreateSendVarHandle(pContext, var);
}





struct SendPropInfo
{
    const SendProp* prop;
    int propindex;
};

// basically DataTableInfo from HalfLife2.h
struct SendTableInfo
{
    struct SendPropPolicy
    {
        static inline bool matches(const char* name, const SendPropInfo &info)
        {
            return strcmp(name, info.prop->GetName()) == 0;
        }
        static inline uint32_t hash(const detail::CharsAndLength &key)
        {
            return key.hash();
        }
    };

    static inline bool matches(const char* name, const SendTableInfo* info)
    {
        return strcmp(name, info->serverclass->GetName()) == 0;
    }
    static inline uint32_t hash(const detail::CharsAndLength &key)
    {
        return key.hash();
    }

    SendTableInfo(ServerClass* serverclass) : serverclass(serverclass) {}

    ServerClass* serverclass;
    NameHashSet<SendPropInfo, SendPropPolicy> lookup;
};
NameHashSet<SendTableInfo*> g_classes;

bool FindClassSendPropInfo(ServerClass* serverclass, const char* propname, SendPropInfo* info)
{
    SendTableInfo* sendtableinfo;
    if (!g_classes.retrieve(serverclass->GetName(), &sendtableinfo)) {
        sendtableinfo = new SendTableInfo(serverclass);
        g_classes.insert(serverclass->GetName(), sendtableinfo);
    }

    if (!sendtableinfo->lookup.retrieve(propname, info)) {
        CSendTablePrecalc* precalc = serverclass->m_pTable->m_pPrecalc;

        int propcount = Context::read<int>(precalc, g_gameinfo.struct_STP.propcount);
        SendProp** props = Context::read<SendProp**>(precalc, g_gameinfo.struct_STP.props);

        SendProp** prop = std::find_if(props, props + propcount, [propname](SendProp* prop) { return strcmp(prop->GetName(), propname) == 0; });

        int propindex = prop - props;
        if (propindex == propcount)
            return false;

        *info = SendPropInfo { *prop, propindex };
        sendtableinfo->lookup.insert(propname, *info);
    }

    return true;
}

// void HasNetworkableProp(int entity, const char[] prop)
cell_t smn_HasNetworkableProp(IPluginContext* pContext, const cell_t* params)
{
    int entref = params[1];
    char* propname;
    pContext->LocalToString(params[2], &propname);

    int index;
    edict_t* edict;
    CBaseEntity* entity;
    IServerUnknown* unknown;
    IServerNetworkable* networkable;
    if (!GetEntityInfo(pContext, entref, &index, &edict, &entity, &unknown, &networkable))
        return false;

    SendPropInfo info;
    return FindClassSendPropInfo(networkable->GetServerClass(), propname, &info);
}

bool FindEntitySendPropInfo(IPluginContext* pContext, int entref, const char* propname, SendPropInfo* info)
{
    int index;
    edict_t* edict;
    CBaseEntity* entity;
    IServerUnknown* unknown;
    IServerNetworkable* networkable;
    if (!GetEntityInfo(pContext, entref, &index, &edict, &entity, &unknown, &networkable))
        return false;

    if (!FindClassSendPropInfo(networkable->GetServerClass(), propname, info)) {
        const char* classname = gamehelpers->GetEntityClassname(edict);
        return pContext->ThrowNativeError(
            "Networkable property \"%s\" not found (entity %d/%s)",
            propname,
            entref,
            ((classname) ? classname : "")
        );
    }

    return true;
}

void AddSendVarEdit(EditAction action, int entref, int client, int propindex, const SendProp* prop, const DVariant* var, int priority)
{
    if (client == -1) {
        int maxclients = playerhelpers->GetMaxClients();
        for (client = 1; client <= maxclients; client++) {
            IGamePlayer* player = playerhelpers->GetGamePlayer(client);
            if (!player->IsConnected())
                continue;

            AddSendVarEdit(action, entref, client, propindex, prop, var, priority);
        }

        return;
    }

    int index = g_editindices[entref];
    if (index == -1) {
        index = g_editlists.size();
        g_editlists.push_back(EditList(entref));
        g_editindices[entref] = index;
    }

    EditList &list = g_editlists[index];
    EditEntry* entry = list.AddOrModifyEntry(client);

    EditInfo info = { action, priority, propindex, prop, var };
    entry->edits.push_back(info);

    if (action == EditAction::SET)
        entry->has_setter = true;
}

bool IsValidClient(IPluginContext* pContext, int client)
{
    if ((client < 1) || (client > playerhelpers->GetMaxClients()))
        return pContext->ThrowNativeError("Client index %d is invalid.", client);

    IGamePlayer* player = playerhelpers->GetGamePlayer(client);
    if (!player->IsConnected())
        return pContext->ThrowNativeError("Client %d is not connected.", client);

    return true;
}

// void SetSendVar(int entity, int client, const char[] prop, Handle value, int priority = 0)
cell_t smn_SetSendVar(IPluginContext* pContext, const cell_t* params)
{
    int entref = params[1];
    int client = params[2];
    char* propname;
    pContext->LocalToString(params[3], &propname);
    Handle_t handle = params[4];
    int priority = params[5];

    if (client != -1 && !IsValidClient(pContext, client))
        return 0;

    SendPropInfo info;
    if (!FindEntitySendPropInfo(pContext, entref, propname, &info))
        return 0;

    const DVariant* var = ReadSendVarHandle(pContext, handle);
    if (!var)
        return 0;

    const char* typenames[] = { "int", "float", "vector", "vector XY", "string", "array", "data table" };
    if (info.prop->GetType() != var->m_Type) {
        return pContext->ThrowNativeError(
            "SendProp %s type is not %s ([%d,%d] != %d)",
            propname,
            typenames[var->m_Type],
            info.prop->GetType(),
            info.prop->m_nBits,
            var->m_Type
        );
    }

    AddSendVarEdit(EditAction::SET, entref, client, info.propindex, info.prop, var, priority);

    return 0;
}

// void ReplaceSendVar(int entity, int client, const char[] prop, Handle value, int priority = 0)
cell_t smn_ReplaceSendVar(IPluginContext* pContext, const cell_t* params)
{
    int entref = params[1];
    int client = params[2];
    char* propname;
    pContext->LocalToString(params[3], &propname);
    Handle_t handle = params[4];
    int priority = params[5];

    if (client != -1 && !IsValidClient(pContext, client))
        return 0;

    SendPropInfo info;
    if (!FindEntitySendPropInfo(pContext, entref, propname, &info))
        return 0;

    const DVariant* var = ReadSendVarHandle(pContext, handle);
    if (!var)
        return 0;

    const char* typenames[] = { "int", "float", "vector", "vector XY", "string", "array", "data table" };
    if (info.prop->GetType() != var->m_Type) {
        return pContext->ThrowNativeError(
            "SendProp %s type is not %s ([%d,%d] != %d)",
            propname,
            typenames[var->m_Type],
            info.prop->GetType(),
            info.prop->m_nBits,
            var->m_Type
        );
    }

    AddSendVarEdit(EditAction::REPLACE, entref, client, info.propindex, info.prop, var, priority);

    return 0;
}

// void OmitSendVar(int entity, int client, const char[] prop, int priority = 0)
cell_t smn_OmitSendVar(IPluginContext* pContext, const cell_t* params)
{
    int entref = params[1];
    int client = params[2];
    char* propname;
    pContext->LocalToString(params[3], &propname);
    int priority = params[4];

    if (client != -1 && !IsValidClient(pContext, client))
        return 0;

    SendPropInfo info;
    if (!FindEntitySendPropInfo(pContext, entref, propname, &info))
        return 0;

    AddSendVarEdit(EditAction::OMIT, entref, client, info.propindex, info.prop, nullptr, priority);

    return 0;
}





HandleType_t g_TransmitBitVecType = 0;
class TransmitBitVecTypeHandler : public IHandleTypeDispatch
{
public:
    virtual void OnHandleDestroy(HandleType_t type, void* object) override
    {
    }
};
TransmitBitVecTypeHandler g_TransmitBitVecTypeHandler;

// bool TransmitBitVec.Get(int entity)
cell_t smn_TransmitBitVec_Get(IPluginContext* pContext, const cell_t* params)
{
    Handle_t handle = params[1];
    CCheckTransmitInfo* pInfo = nullptr;
    HandleSecurity security(nullptr, myself->GetIdentity());
    HandleError error = handlesys->ReadHandle(handle, g_TransmitBitVecType, &security, (void**)&pInfo);
    if (error != HandleError_None)
        return pContext->ThrowNativeError("Invalid transmit bitvec handle %x (error %d)", handle, error);

    int entref = params[2];

    int index;
    edict_t* edict;
    CBaseEntity* entity;
    IServerUnknown* unknown;
    IServerNetworkable* networkable;
    if (!GetEntityInfo(pContext, entref, &index, &edict, &entity, &unknown, &networkable))
        return false;

    return pInfo->m_pTransmitEdict->Get(index);
}

// void TransmitBitVec.Set(int entity, bool transmit)
cell_t smn_TransmitBitVec_Set(IPluginContext* pContext, const cell_t* params)
{
    Handle_t handle = params[1];
    CCheckTransmitInfo* pInfo = nullptr;
    HandleSecurity security(nullptr, myself->GetIdentity());
    HandleError error = handlesys->ReadHandle(handle, g_TransmitBitVecType, &security, (void**)&pInfo);
    if (error != HandleError_None)
        return pContext->ThrowNativeError("Invalid transmit bitvec handle %x (error %d)", handle, error);

    int entref = params[2];

    int index;
    edict_t* edict;
    CBaseEntity* entity;
    IServerUnknown* unknown;
    IServerNetworkable* networkable;
    if (!GetEntityInfo(pContext, entref, &index, &edict, &entity, &unknown, &networkable))
        return false;

    bool transmit = params[3];
    pInfo->m_pTransmitEdict->Set(index, transmit);
    if (pInfo->m_pTransmitAlways)
        pInfo->m_pTransmitAlways->Set(index, transmit);

    return 0;
}





const sp_nativeinfo_t MyNatives[] =
{
    {"SendProxyAngle", smn_SendProxyAngle},
    {"SendProxyQAnglesNative", smn_SendProxyQAngles},
    {"SendProxyEHandle", smn_SendProxyEHandle},
    {"HasNetworkableProp", smn_HasNetworkableProp},
    {"SendVarInt", smn_SendVarInt},
    {"SendVarFloat", smn_SendVarFloat},
    {"SendVarVector", smn_SendVarVector},
    {"SendVarVectorXY", smn_SendVarVectorXY},
    {"SetSendVar", smn_SetSendVar},
    {"ReplaceSendVar", smn_ReplaceSendVar},
    {"OmitSendVar", smn_OmitSendVar},
    {"TransmitBitVec.Get", smn_TransmitBitVec_Get},
    {"TransmitBitVec.Set", smn_TransmitBitVec_Set},
    {NULL, NULL},
};





EditEntry* HookDiscoverEntry(CEntityWriteInfo* entitywriteinfo)
{
    int entity = Context::read<int>(entitywriteinfo, g_gameinfo.struct_EWI.entity);

    if (!( 0 < entity && (size_t)entity < sizeof(g_editindices) ) || g_editindices[entity] == -1)
        return nullptr;

    EditList* list = &g_editlists[g_editindices[entity]];

    int client = Context::read<int>(entitywriteinfo, g_gameinfo.struct_EWI.client);

    // check if edit list can be indexed with client. it should, but who knows.
    if (client < 0 || client >= 256)
        return nullptr;

    return list->GetEntry(client);
}

// store entry address at the very end of the output buffer
inline EditEntry** HookGetEntryAddress(bf_write* output)
{
    uintptr_t entry_address = (uintptr_t)output->m_pData + output->m_nDataBytes - sizeof(EditEntry*);
    return (EditEntry**)entry_address;
}

// get entry address in output buffer if a valid one was stored
inline EditEntry* HookGetEntry(bf_write* output)
{
    return output->m_nDataBits != 8*output->m_nDataBytes ? *HookGetEntryAddress(output) : nullptr;
}

// write the current entry/null to the end of the output buffer
safetyhook::MidHook g_MidHook_SV_DetermineUpdateType_Start{};
void MidHook_SV_DetermineUpdateType_Start(safetyhook::Context &registers)
{
    Context context(registers);

    CEntityWriteInfo* entitywriteinfo = context.var(g_gameinfo.vars_DUT.entitywriteinfo);

    bf_write* output = Context::read<bf_write*>(entitywriteinfo, g_gameinfo.struct_EWI.output);

    *HookGetEntryAddress(output) = HookDiscoverEntry(entitywriteinfo);
    output->m_nDataBits = 8*(output->m_nDataBytes - sizeof(EditEntry*)); // hint that end was written to
}

// skip to writing to output when there is a set edit and the entity hasn't changed to avoid edit getting ignored
safetyhook::MidHook g_MidHook_SV_DetermineUpdateType_PackCheck{};
void MidHook_SV_DetermineUpdateType_PackCheck(safetyhook::Context &registers)
{
    Context context(registers);

    CEntityWriteInfo* entitywriteinfo = context.var(g_gameinfo.vars_DUT.entitywriteinfo);

    bf_write* output = Context::read<bf_write*>(entitywriteinfo, g_gameinfo.struct_EWI.output);

    EditEntry* entry = HookGetEntry(output);
    if (!entry)
        return;

    PackedEntity* oldpack = Context::read<PackedEntity*>(entitywriteinfo, g_gameinfo.struct_EWI.oldpack);
    PackedEntity* newpack = Context::read<PackedEntity*>(entitywriteinfo, g_gameinfo.struct_EWI.newpack);

    if (oldpack == newpack && entry->has_setter) {
        context.var(g_gameinfo.vars_DUT.propcount) = 0;
        context.ip() = g_gameinfo.points_DUT.propcount_positive_block;
        return;
    }
    else {
        context.ip() = g_gameinfo.points_DUT.props_changed_call;
        return;
    }
}

// skip changed prop count check if there is a set edit
safetyhook::MidHook g_MidHook_SV_DetermineUpdateType_VCRPrintCheck{};
void MidHook_SV_DetermineUpdateType_VCRPrintCheck(safetyhook::Context &registers)
{
    Context context(registers);

    CEntityWriteInfo* entitywriteinfo = context.var(g_gameinfo.vars_DUT.entitywriteinfo);

    bf_write* output = Context::read<bf_write*>(entitywriteinfo, g_gameinfo.struct_EWI.output);

    EditEntry* entry = HookGetEntry(output);
    if (!entry || !entry->has_setter)
        return;

    context.ip() = g_gameinfo.points_DUT.propcount_positive_block;
}

safetyhook::InlineHook g_Hook_SendTable_WritePropList{};
void Hook_SendTable_WritePropList(SendTable* sendtable, void* inputdata, int inputlength, bf_write* output, int entity, int* propindexlist, int propindexlistlength)
{
    EditEntry* entry = HookGetEntry(output);

    if (!entry) {
        // normal call
        return g_Hook_SendTable_WritePropList.unsafe_call(sendtable, inputdata, inputlength, output, entity, propindexlist, propindexlistlength);
    }

    // might as well sort the edits here where it is potentially threaded
    std::vector<EditInfo> &edits = entry->edits;
    std::sort(
        edits.begin(),
        edits.end(),
        [](const EditInfo &a, const EditInfo &b) {
            return a.propindex < b.propindex || (a.propindex == b.propindex && a.priority > b.priority);
        }
    );
    edits.erase(
        std::unique(
            edits.begin(),
            edits.end(),
            [](const EditInfo &a, const EditInfo &b) { return a.propindex == b.propindex; }
        ),
        edits.end()
    );

    // modify propindexlist such that it includes/excludes props according to the edits
    int length = 0;
    int customlist[MAX_DATATABLE_PROPS];
    size_t io = 0, ic = 0;
    int no = propindexlist[io], nc = edits[ic].propindex;
    while (io < (size_t)propindexlistlength || ic < edits.size()) {
        // add propindex if one was set
        if (nc < no) {
            if (edits[ic].action == EditAction::SET)
                customlist[length++] = nc;

            nc = ++ic < edits.size() ? edits[ic].propindex : MAX_DATATABLE_PROPS;
        }
        // add propindex according to action
        else if (nc == no) {
            switch (edits[ic].action) {
                case EditAction::SET:
                case EditAction::REPLACE:
                    customlist[length++] = nc;
                    break;
                case EditAction::OMIT:
                    // skip without adding a propindex
                    break;
            }

            nc = ++ic < edits.size() ? edits[ic].propindex : MAX_DATATABLE_PROPS;
            no = ++io < (size_t)propindexlistlength ? propindexlist[io] : MAX_DATATABLE_PROPS;
        }
        // add propindex from original list
        else if (no < nc) {
            customlist[length++] = no;
            no = ++io < (size_t)propindexlistlength ? propindexlist[io] : MAX_DATATABLE_PROPS;
        }
    }

    propindexlist = customlist;
    propindexlistlength = length;

    // custom call
    return g_Hook_SendTable_WritePropList.unsafe_call(sendtable, inputdata, inputlength, output, entity, propindexlist, propindexlistlength);
}

// does the same thing as SendTable_WritePropList, but instead of copying from the input buffer it encodes the given DVariant
safetyhook::MidHook g_MidHook_SendTable_WritePropList_BreakCondition{};
void MidHook_SendTable_WritePropList_BreakCondition(safetyhook::Context &registers)
{
    Context context(registers);

    bf_write* output = context.var(g_gameinfo.vars_WPL.output);
    const EditEntry* entry = HookGetEntry(output);
    if (!entry)
        return;

    SendTable* table = context.var(g_gameinfo.vars_WPL.table);
    SendProp** props = Context::read<SendProp**>(table->m_pPrecalc, g_gameinfo.struct_STP.props);

    int propindex = context.var(g_gameinfo.vars_WPL.propindex);
    SendProp* prop = props[propindex];

    // no support for arrays and data tables (sent as multiple seperate props i think)
    if (prop->GetType() == DPT_Array || prop->GetType() == DPT_DataTable)
        return;

    // smutils->LogMessage(myself, "edit: prop = %d, %s", propindex, prop->GetName());

    // search for the current propindex in the entry edits list
    const EditInfo* info = nullptr;
    for (const EditInfo &ei : entry->edits) {
        if (ei.propindex == propindex) {
            info = &ei;
            break;
        }
    }

    if (!info)
        return;

    // const char* string = ((DVariant*)(info->var))->ToString();
    // smutils->LogMessage(myself, "edit: entry = %d, %s", info->propindex, string);

    // follow procedure of the SendTable_WritePropList loop, but encode instead of copy

    // write prop index delta and update variable keeping track of the last output propindex
    {
        int* output_lastpropindex = &context.var(g_gameinfo.vars_WPL.output_lastpropindex);

        int diff = propindex - *output_lastpropindex;
        *output_lastpropindex = propindex;

        output->WriteOneBit(1);
        output->WriteUBitVar(diff - 1);
    }

    switch (prop->GetType()) {
        case DPT_Int:
        case DPT_Float:
        case DPT_Vector:
        case DPT_VectorXY:
        case DPT_String:
            g_gameinfo.proptypefns[prop->m_Type].Encode(nullptr, (DVariant*)info->var, prop, output, entry->entity);
            break;
        // case DPT_Array:
        //     break;
        // case DPT_DataTable:
        //     break;
    }

    // continue; the loop
    context.ip() = g_gameinfo.points_WPL.loop_continue;
}

IForward* g_Forward_Post_OnCheckTransmit;

void Hook_CheckTransmit(CCheckTransmitInfo* pInfo, const unsigned short* pEdictIndices, int nEdicts)
{
    int client = gamehelpers->IndexOfEdict(pInfo->m_pClientEnt);

    Handle_t handle = handlesys->CreateHandle(
        g_TransmitBitVecType,
        pInfo,
        myself->GetIdentity(),
        myself->GetIdentity(),
        nullptr
    );

    if (g_Forward_Post_OnCheckTransmit->GetFunctionCount()) {
        g_Forward_Post_OnCheckTransmit->PushCell(client);
        g_Forward_Post_OnCheckTransmit->PushCell(handle);
        g_Forward_Post_OnCheckTransmit->Execute();
    }

    HandleSecurity security(nullptr, myself->GetIdentity());
    handlesys->FreeHandle(handle, &security);

    RETURN_META(MRES_IGNORED);
}

IForward* g_Forward_Pre_OnSendClientMessages;
IForward* g_Forward_Post_OnSendClientMessages;

safetyhook::InlineHook g_Hook_CGameServer__SendClientMessages{};
class Hook_CGameServer
{
public:
    void SendClientMessages(bool b)
    {
        if (g_Forward_Pre_OnSendClientMessages->GetFunctionCount())
            g_Forward_Pre_OnSendClientMessages->Execute();

        g_Hook_CGameServer__SendClientMessages.thiscall(this, b);

        if (g_Forward_Post_OnSendClientMessages->GetFunctionCount())
            g_Forward_Post_OnSendClientMessages->Execute();

        // free handles
        HandleSecurity security(nullptr, myself->GetIdentity());
        for (Handle_t handle : g_editdatahandles)
            handlesys->FreeHandle(handle, &security);
        g_editdatahandles.clear();

        // clear edits
        std::fill(g_editindices, g_editindices + MAX_EDICTS, -1);
        g_editlists.clear();
        g_editbuffer.clear();

        return;
    }
};

#define RETURN_ERROR(ERR)                                \
{                                                        \
    gameconfs->CloseGameConfigFile(gameconf);            \
    SDK_OnUnload();                                      \
    size_t len = ke::SafeSprintf(error, maxlength, ERR); \
    if (len >= maxlength)                                \
        error[maxlength - 1] = '\0';                     \
    return false;                                        \
}                                                        \

SH_DECL_HOOK3_void(IServerGameEnts, CheckTransmit, SH_NOATTRIB, 0, CCheckTransmitInfo*, const unsigned short*, int);

IServerGameEnts* g_gameents;
bool SendVarEdit::SDK_OnMetamodLoad(ISmmAPI* ismm, char* error, size_t maxlen, bool late)
{
    GET_V_IFACE_CURRENT(GetServerFactory, g_gameents, IServerGameEnts, INTERFACEVERSION_SERVERGAMEENTS);

    return true;
}

bool SendVarEdit::SDK_OnLoad(char* error, size_t maxlength, bool late)
{
    std::fill(g_editindices, g_editindices + MAX_EDICTS, -1);

    // get info about what registers and stack offsets function variables are stored at
    gameconfs->AddUserConfigHook("Variables", &g_variables);

    IGameConfig* gameconf;
    if (!gameconfs->LoadGameConfigFile("sendvaredit", &gameconf, error, maxlength))
        return false;

    // fill out game info struct
    {
        gameconf->GetOffset("CEntityWriteInfo entity", &g_gameinfo.struct_EWI.entity);
        gameconf->GetOffset("CEntityWriteInfo output", &g_gameinfo.struct_EWI.output);
        gameconf->GetOffset("CEntityWriteInfo client", &g_gameinfo.struct_EWI.client);
        gameconf->GetOffset("CEntityWriteInfo oldpack", &g_gameinfo.struct_EWI.oldpack);
        gameconf->GetOffset("CEntityWriteInfo newpack", &g_gameinfo.struct_EWI.newpack);

        g_variables.GetVariable("SV_DetermineUpdateType entitywriteinfo", &g_gameinfo.vars_DUT.entitywriteinfo);
        g_variables.GetVariable("SV_DetermineUpdateType propcount", &g_gameinfo.vars_DUT.propcount);

        if (!gameconf->GetAddress("SV_DetermineUpdateType props changed call", &g_gameinfo.points_DUT.props_changed_call))
            RETURN_ERROR("Failed to find SV_DetermineUpdateType props changed call.");

        if (!gameconf->GetAddress("SV_DetermineUpdateType positive propcount block", &g_gameinfo.points_DUT.propcount_positive_block))
            RETURN_ERROR("Failed to find SV_DetermineUpdateType positive propcount block.");

        g_variables.GetVariable("SendTable_WritePropList table", &g_gameinfo.vars_WPL.table);
        g_variables.GetVariable("SendTable_WritePropList output", &g_gameinfo.vars_WPL.output);
        g_variables.GetVariable("SendTable_WritePropList propindex", &g_gameinfo.vars_WPL.propindex);
        g_variables.GetVariable("SendTable_WritePropList output_lastpropindex", &g_gameinfo.vars_WPL.output_lastpropindex);

        if (!gameconf->GetAddress("SendTable_WritePropList loop continue", &g_gameinfo.points_WPL.loop_continue))
            RETURN_ERROR("Failed to find SendTable_WritePropList loop continuation point.");

        if (!gameconf->GetAddress("&g_PropTypeFns", (void**)&g_gameinfo.proptypefns))
            RETURN_ERROR("Failed to find g_PropTypeFns.");

        gameconf->GetOffset("CSendTablePrecalc props", &g_gameinfo.struct_STP.props);
        gameconf->GetOffset("CSendTablePrecalc propcount", &g_gameinfo.struct_STP.propcount);
    }

    // hook CGameServer::SendClientMessages to catch when the server is about to network stuff to clients
    {
        void* address;
        if (!gameconf->GetAddress("CGameServer::SendClientMessages", &address))
            RETURN_ERROR("Failed to find CGameServer::SendClientMessages.");

        auto function = &Hook_CGameServer::SendClientMessages;
        auto hook = safetyhook::InlineHook::create(address, (void*&)function);
        if (!hook.has_value())
            RETURN_ERROR("Failed to hook CGameServer::SendClientMessages.");

        g_Hook_CGameServer__SendClientMessages = std::move(*hook);
    }

    // hook CServerGameEnts::CheckTransmit to catch when the server decides what entities to send to a client
    {
        SH_ADD_HOOK(IServerGameEnts, CheckTransmit, g_gameents, SH_STATIC(Hook_CheckTransmit), true);
    }

    // hook SendTable_WritePropList to check for an entry
    {
        void* address;
        if (!gameconf->GetAddress("SendTable_WritePropList", &address))
            RETURN_ERROR("Failed to find SendTable_WritePropList.");

        auto hook = safetyhook::InlineHook::create(address, Hook_SendTable_WritePropList);
        if (!hook.has_value())
            RETURN_ERROR("Failed to hook SendTable_WritePropList.");

        g_Hook_SendTable_WritePropList = std::move(*hook);
    }

    // create a mid-hook at the SV_DetermineUpdateType start that finds the entry for the client and entity
    // the pointer to the entry (possibily null) will be stored at the very end of the output buffer
    {
        void* address;
        if (!gameconf->GetAddress("SV_DetermineUpdateType start", &address))
            RETURN_ERROR("Failed to find SV_DetermineUpdateType start.");

        auto midhook = safetyhook::MidHook::create(address, MidHook_SV_DetermineUpdateType_Start);
        if (!midhook.has_value())
            RETURN_ERROR("Failed to mid-hook SV_DetermineUpdateType start.");

        g_MidHook_SV_DetermineUpdateType_Start = std::move(*midhook);
    }

    // create a mid-hook at the SV_DetermineUpdateType pack equality check that skips all modification checks and
    // goes straight to writing custom edits if the packs are equal and an edit sets a prop
    // also skips over a hltv cache for stv recordings if they aren't equal
    {
        void* address;
        if (!gameconf->GetAddress("SV_DetermineUpdateType pack equality check", &address))
            RETURN_ERROR("Failed to find SV_DetermineUpdateType pack equality check.");

        auto midhook = safetyhook::MidHook::create(address, MidHook_SV_DetermineUpdateType_PackCheck);
        if (!midhook.has_value())
            RETURN_ERROR("Failed to mid-hook SV_DetermineUpdateType pack equality check.");

        g_MidHook_SV_DetermineUpdateType_PackCheck = std::move(*midhook);
    }

    // create a mid-hook at the SV_DetermineUpdateType VCR print check that always passes the positive propcount check if a set edit was made
    {
        void* address;
        if (!gameconf->GetAddress("SV_DetermineUpdateType VCR print check", &address))
            RETURN_ERROR("Failed to find SV_DetermineUpdateType VCR print check.");

        auto midhook = safetyhook::MidHook::create(address, MidHook_SV_DetermineUpdateType_VCRPrintCheck);
        if (!midhook.has_value())
            RETURN_ERROR("Failed to mid-hook SV_DetermineUpdateType VCR print check.");

        g_MidHook_SV_DetermineUpdateType_VCRPrintCheck = std::move(*midhook);
    }

    // create a mid-hook at the SendTable_WritePropList loop break condition that skips to the continuation point if an edit was made
    {
        void* address;
        if (!gameconf->GetAddress("SendTable_WritePropList loop break condition", &address))
            RETURN_ERROR("Failed to find SendTable_WritePropList loop break condition.");

        auto midhook = safetyhook::MidHook::create(address, MidHook_SendTable_WritePropList_BreakCondition);
        if (!midhook.has_value())
            RETURN_ERROR("Failed to mid-hook SendTable_WritePropList loop break condition.");

        g_MidHook_SendTable_WritePropList_BreakCondition = std::move(*midhook);
    }

    gameconfs->CloseGameConfigFile(gameconf);

    return true;
}

void SendVarEdit::SDK_OnAllLoaded()
{
    g_Forward_Pre_OnSendClientMessages = forwards->CreateForward("OnSendClientMessages", ET_Ignore, 0, nullptr);
    g_Forward_Post_OnSendClientMessages = forwards->CreateForward("OnSendClientMessagesPost", ET_Ignore, 0, nullptr);

    g_Forward_Post_OnCheckTransmit = forwards->CreateForward("OnCheckTransmit", ET_Ignore, 2, nullptr, Param_Cell, Param_Cell);

    HandleAccess rules;
    handlesys->InitAccessDefaults(nullptr, &rules);
    rules.access[HandleAccess_Delete] = HANDLE_RESTRICT_IDENTITY;

    g_SendVarType = handlesys->CreateType(
        "SendVarType",
        &g_SendVarTypeHandler,
        0,
        nullptr,
        &rules,
        myself->GetIdentity(),
        nullptr
    );

    g_TransmitBitVecType = handlesys->CreateType(
        "TransmitBitVec",
        &g_TransmitBitVecTypeHandler,
        0,
        nullptr,
        &rules,
        myself->GetIdentity(),
        nullptr
    );

    sharesys->RegisterLibrary(myself, "sendvaredit");
    sharesys->AddNatives(myself, MyNatives);
}

void SendVarEdit::SDK_OnUnload()
{
    if (g_TransmitBitVecType)
        handlesys->RemoveType(g_TransmitBitVecType, myself->GetIdentity());

    if (g_SendVarType)
        handlesys->RemoveType(g_SendVarType, myself->GetIdentity());

    for (NameHashSet<SendTableInfo*>::iterator iter = g_classes.iter(); !iter.empty(); iter.next())
        delete *iter;

    if (g_Forward_Post_OnCheckTransmit)
        forwards->ReleaseForward(g_Forward_Post_OnCheckTransmit);

    if (g_Forward_Pre_OnSendClientMessages)
        forwards->ReleaseForward(g_Forward_Pre_OnSendClientMessages);

    if (g_Forward_Post_OnSendClientMessages)
        forwards->ReleaseForward(g_Forward_Post_OnSendClientMessages);

    if (g_MidHook_SendTable_WritePropList_BreakCondition.enabled())
        g_MidHook_SendTable_WritePropList_BreakCondition = {};

    if (g_MidHook_SV_DetermineUpdateType_VCRPrintCheck.enabled())
        g_MidHook_SV_DetermineUpdateType_VCRPrintCheck = {};

    if (g_MidHook_SV_DetermineUpdateType_PackCheck.enabled())
        g_MidHook_SV_DetermineUpdateType_PackCheck = {};

    if (g_MidHook_SV_DetermineUpdateType_Start.enabled())
        g_MidHook_SV_DetermineUpdateType_Start = {};

    if (g_Hook_SendTable_WritePropList.enabled())
        g_Hook_SendTable_WritePropList = {};

    SH_REMOVE_HOOK(IServerGameEnts, CheckTransmit, g_gameents, SH_STATIC(Hook_CheckTransmit), true);

    if (g_Hook_CGameServer__SendClientMessages.enabled())
        g_Hook_CGameServer__SendClientMessages = {};

    gameconfs->RemoveUserConfigHook("Variables", &g_variables);
}
