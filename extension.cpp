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

#include <sm_namehashset.h>

#include <tier1/bitbuf.h>
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
    }

    int entity;
    int client;
    std::vector<EditInfo> edits;
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

struct FunctionSpan
{
    void* start;
    int size;
};

struct Stack_CGameServer__WriteDeltaEntities
{
    int client;
};

struct Stack_SendTable_WritePropList
{
    int table; // SendTable*
    int output; // bf_write*
    int input; // bf_read
    int output_lastpropindex; // int
    int input_lastpropindex; // int
    int prop; // SendProp*
};

struct PropIndex_SendTable_WritePropList
{
    size_t reg;
    int32_t read; // if not -1, treat register as a pointer and read at this offset
};

struct Point_SendTable_WritePropList
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
    FunctionSpan span_WDE;
    Stack_CGameServer__WriteDeltaEntities stack_WDE;

    Stack_SendTable_WritePropList stack_WPL;
    PropIndex_SendTable_WritePropList propindex_WPL;
    Point_SendTable_WritePropList point_WPL;

    PropTypeFns* proptypefns;
    Struct_CSendTablePrecalc struct_STP;
};
GameInfo g_gameinfo;

struct RegisterInfo { const char* name;  size_t offset; };
#define REG_INFO(NAME) { #NAME, offsetof(safetyhook::Context, NAME) }
RegisterInfo g_registers[] {
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





bool GetEntityInfo(IPluginContext* pContext, int entref, int* index, edict_t** edict, CBaseEntity** entity, IServerUnknown** unknown, IServerNetworkable** networkable)
{
    *index = gamehelpers->ReferenceToIndex(entref);
    *edict = gamehelpers->EdictOfIndex(*index);
    if (!(*edict))
        return pContext->ThrowNativeError("Entity %d (%d) is invalid.", *index, entref);

    *entity = gamehelpers->ReferenceToEntity(entref);

    *unknown = (IServerUnknown *)(*entity);
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
    cell_t *sp_qangles;
    pContext->LocalToPhysAddr(params[1], &sp_qangles);

    cell_t *sp_vec;
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
    IServerUnknown *unknown;
    IServerNetworkable *networkable;
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
    void OnHandleDestroy(HandleType_t type, void *object)
    {
        // const DVariant* var = (DVariant*)object;
        // switch (var->m_Type) {
        //     // special type cleanup
        //     default:
        //         break;
        // }
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
    cell_t *sp_vec;
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
    cell_t *sp_vec;
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

        int propcount = *(int*)((uint8_t*)precalc + g_gameinfo.struct_STP.propcount);
        SendProp** props = *(SendProp***)((uint8_t*)precalc + g_gameinfo.struct_STP.props);

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
    IServerUnknown *unknown;
    IServerNetworkable *networkable;
    if (!GetEntityInfo(pContext, entref, &index, &edict, &entity, &unknown, &networkable))
        return false;

    SendPropInfo info;
    return FindClassSendPropInfo(networkable->GetServerClass(), propname, &info);
}

bool FindEntitySendPropInfo(IPluginContext *pContext, int entref, const char* propname, SendPropInfo* info)
{
    int index;
    edict_t* edict;
    CBaseEntity* entity;
    IServerUnknown *unknown;
    IServerNetworkable *networkable;
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
            IGamePlayer *player = playerhelpers->GetGamePlayer(client);
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
}

bool IsValidClient(IPluginContext *pContext, int client)
{
    if ((client < 1) || (client > playerhelpers->GetMaxClients()))
        return pContext->ThrowNativeError("Client index %d is invalid.", client);

    IGamePlayer *player = playerhelpers->GetGamePlayer(client);
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
    {NULL, NULL},
};





EditEntry* DiscoverEntry(int entity)
{
    if (entity < 0 || g_editindices[entity] == -1)
        return nullptr;

    EditList* list = &g_editlists[g_editindices[entity]];

    // compiling with "-fno-omit-frame-pointer" (gcc) or "/Oy-" (msvc)
    void* ebp_cur;
#ifdef _MSC_VER
    __asm mov ebp_cur, ebp
#else
    asm ("mov %%ebp, %0" : "=r"(ebp_cur));
#endif

    // walk up the frame stack to get the CBaseClient* parameter of CGameServer::WriteDeltaEntities
    int counter = 0;
    void** ebp_walk = (void**)ebp_cur;
    while (true) {
        // might also come from CGameServer::WriteTempEntities
        if (counter++ > 8)
            return nullptr;

        void* eip = ebp_walk[1];
        ebp_walk = (void**)ebp_walk[0];

        if (g_gameinfo.span_WDE.start <= eip && eip < (uint8_t*)g_gameinfo.span_WDE.start + g_gameinfo.span_WDE.size)
            break;
    }

    // extract client index from CBaseClient
    void* baseclient = *(void**)((uint8_t*)ebp_walk + g_gameinfo.stack_WDE.client);
    int client = *(int*)((uint8_t*)baseclient + 16);

    // check if edit list can be indexed with client. it should, but who knows.
    if (client < 0 || client >= 256)
        return nullptr;

    return list->GetEntry(client);
}

// store entry address at the very end of the output buffer
EditEntry** GetHookEntryAddress(bf_write* output)
{
    uintptr_t address = (uintptr_t)output->m_pData + output->m_nDataBytes - sizeof(EditEntry*);
    return (EditEntry**)address;
}

safetyhook::InlineHook g_Hook_SendTable_WritePropList{};
void Hook_SendTable_WritePropList(SendTable* sendtable, void* inputdata, int inputlength, bf_write* output, int entity, int* propindexlist, int propindexlistlength)
{
    EditEntry* entry = DiscoverEntry(entity);

    *GetHookEntryAddress(output) = entry;

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

class ContextUtil
{
public:
    safetyhook::Context &registers;

public:
    ContextUtil(safetyhook::Context &context)
        : registers(context)
    {}

    inline uintptr_t& reg(size_t reg) { return *(uintptr_t*)((uint8_t*)&registers + reg); }
    inline safetyhook::Xmm& xmm(size_t reg) { return *(safetyhook::Xmm*)((uint8_t*)&registers + reg); }

    inline void*& ip()
    {
        return *(void**)&registers.eip;
    }

    template<typename T>
    inline T& read(size_t reg, int32_t offset=0)
    {
        return *(T*)(this->reg(reg) + offset);
    }

    template<typename T>
    inline T& stack(int32_t offset)
    {
        return *(T*)(registers.ebp + offset);
    }
};

// does the same thing as SendTable_WritePropList, but instead of copying from the input buffer it encodes the given DVariant
safetyhook::MidHook g_MidHook_SendTable_WritePropList_BreakCondition{};
void MidHook_SendTable_WritePropList_BreakCondition(safetyhook::Context &registers)
{
    ContextUtil context(registers);

    bf_write* output = context.stack<bf_write*>(g_gameinfo.stack_WPL.output);
    const EditEntry* entry = *GetHookEntryAddress(output);
    if (!entry)
        return;

    SendTable* table = context.stack<SendTable*>(g_gameinfo.stack_WPL.table);
    SendProp** props = *(SendProp***)((uint8_t*)table->m_pPrecalc + g_gameinfo.struct_STP.props);

    int propindex = g_gameinfo.propindex_WPL.read == -1 ? context.reg(g_gameinfo.propindex_WPL.reg) : context.read<int>(g_gameinfo.propindex_WPL.reg, g_gameinfo.propindex_WPL.read);
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

    bf_read* input = &context.stack<bf_read>(g_gameinfo.stack_WPL.input);

    // follow procedure of the SendTable_WritePropList loop, but encode instead of copy

    // write prop index delta and update variable keeping track of the last output propindex
    {
        int* output_lastpropindex = &context.stack<int>(g_gameinfo.stack_WPL.output_lastpropindex);

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
    context.ip() = g_gameinfo.point_WPL.loop_continue;
}

IForward *g_Forward_Pre_OnSendClientMessages;
IForward *g_Forward_Post_OnSendClientMessages;

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

bool SendVarEdit::SDK_OnLoad(char *error, size_t maxlength, bool late)
{
    std::fill(g_editindices, g_editindices + MAX_EDICTS, -1);

    IGameConfig* gameconf;
    if (!gameconfs->LoadGameConfigFile("sendvaredit", &gameconf, error, maxlength))
        return false;

    // fill out game info struct
    {
        if (!gameconf->GetAddress("CBaseServer::WriteDeltaEntities", &g_gameinfo.span_WDE.start))
            RETURN_ERROR("Failed to find CBaseServer::WriteDeltaEntities.");
        gameconf->GetOffset("CBaseServer::WriteDeltaEntities size", &g_gameinfo.span_WDE.size);

        gameconf->GetOffset("CBaseServer::WriteDeltaEntities ebp client", &g_gameinfo.stack_WDE.client);

        gameconf->GetOffset("SendTable_WritePropList ebp table", &g_gameinfo.stack_WPL.table);
        gameconf->GetOffset("SendTable_WritePropList ebp output", &g_gameinfo.stack_WPL.output);
        gameconf->GetOffset("SendTable_WritePropList ebp input", &g_gameinfo.stack_WPL.input);
        gameconf->GetOffset("SendTable_WritePropList ebp output_lastpropindex", &g_gameinfo.stack_WPL.output_lastpropindex);
        gameconf->GetOffset("SendTable_WritePropList ebp input_lastpropindex", &g_gameinfo.stack_WPL.input_lastpropindex);
        gameconf->GetOffset("SendTable_WritePropList ebp prop", &g_gameinfo.stack_WPL.prop);

        const char* register_propindex_string = gameconf->GetKeyValue("SendTable_WritePropList register propindex");
        if (!register_propindex_string)
            RETURN_ERROR("Failed to get SendTable_WritePropList propindex register name.");

        const RegisterInfo* register_propindex = FindRegister(register_propindex_string);
        if (!register_propindex)
            RETURN_ERROR("Failed to match SendTable_WritePropList propindex register name.");

        g_gameinfo.propindex_WPL.reg = register_propindex->offset;

        int32_t read_propindex = -1;
        gameconf->GetOffset("SendTable_WritePropList read propindex", &read_propindex);
        g_gameinfo.propindex_WPL.read = read_propindex;

        if (!gameconf->GetAddress("SendTable_WritePropList loop continue", &g_gameinfo.point_WPL.loop_continue))
            RETURN_ERROR("Failed to find SendTable_WritePropList loop continuation point.");

        if (!gameconf->GetAddress("&g_PropTypeFns", (void**)&g_gameinfo.proptypefns))
            RETURN_ERROR("Failed to find g_PropTypeFns.");

        gameconf->GetOffset("CSendTablePrecalc props", &g_gameinfo.struct_STP.props);
        gameconf->GetOffset("CSendTablePrecalc propcount", &g_gameinfo.struct_STP.propcount);
    }

    // hook CGameServer::SendClientMessages to catch when the server is about to network stuff to clients
    {
        void* address;
        if (!gameconf->GetMemSig("CGameServer::SendClientMessages", &address))
            RETURN_ERROR("Failed to find CGameServer::SendClientMessages.");

        auto function = &Hook_CGameServer::SendClientMessages;
        auto hook = safetyhook::InlineHook::create(address, (void*&)function);
        if (!hook.has_value())
            RETURN_ERROR("Failed to hook CGameServer::SendClientMessages.");

        g_Hook_CGameServer__SendClientMessages = std::move(*hook);
    }

    // hook SendTable_WritePropList to check for an entry
    {
        void* address;
        if (!gameconf->GetMemSig("SendTable_WritePropList", &address))
            RETURN_ERROR("Failed to find SendTable_WritePropList.");

        auto hook = safetyhook::InlineHook::create(address, Hook_SendTable_WritePropList);
        if (!hook.has_value())
            RETURN_ERROR("Failed to hook SendTable_WritePropList.");

        g_Hook_SendTable_WritePropList = std::move(*hook);
    }

    // create a mid-hook at the SendTable_WritePropList loop break condition that skips to the continuation point if an edit was made
    {
        void* address;
        if (!gameconf->GetAddress("SendTable_WritePropList loop break condition", &address))
            RETURN_ERROR("Failed to find SendTable_WritePropList loop break condition.");

        auto midhook = safetyhook::MidHook::create(address, MidHook_SendTable_WritePropList_BreakCondition);
        if (!midhook.has_value())
            RETURN_ERROR("Failed to mid-hook CGameServer::SendClientMessages.");

        g_MidHook_SendTable_WritePropList_BreakCondition = std::move(*midhook);
    }

    gameconfs->CloseGameConfigFile(gameconf);

    return true;
}

void SendVarEdit::SDK_OnAllLoaded()
{
    g_Forward_Pre_OnSendClientMessages = forwards->CreateForward("OnSendClientMessages", ET_Ignore, 0, nullptr);
    g_Forward_Post_OnSendClientMessages = forwards->CreateForward("OnSendClientMessagesPost", ET_Ignore, 0, nullptr);

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

    sharesys->RegisterLibrary(myself, "sendvaredit");
    sharesys->AddNatives(myself, MyNatives);
}

void SendVarEdit::SDK_OnUnload()
{
    if (g_SendVarType)
        handlesys->RemoveType(g_SendVarType, myself->GetIdentity());

    for (NameHashSet<SendTableInfo*>::iterator iter = g_classes.iter(); !iter.empty(); iter.next())
        delete *iter;

    if (g_Forward_Pre_OnSendClientMessages)
        forwards->ReleaseForward(g_Forward_Pre_OnSendClientMessages);

    if (g_Forward_Post_OnSendClientMessages)
        forwards->ReleaseForward(g_Forward_Post_OnSendClientMessages);

    if (g_MidHook_SendTable_WritePropList_BreakCondition.enabled())
        g_MidHook_SendTable_WritePropList_BreakCondition = {};

    if (g_Hook_SendTable_WritePropList.enabled())
        g_Hook_SendTable_WritePropList = {};


    if (g_Hook_CGameServer__SendClientMessages.enabled())
        g_Hook_CGameServer__SendClientMessages = {};
}
