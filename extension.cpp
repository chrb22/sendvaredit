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

#include "patch.h"

#include <string>

#include <sm_namehashset.h>

#include <tier1/bitbuf.h>
#include <iserverunknown.h>
#include <iservernetworkable.h>
#include <server_class.h>
#include <dt_common.h>
#include <dt_send.h>

#define JMP_SIZE 5
#include <asm/asm.h>
#include <macro-assembler-x86.h>
#include <jit/jit_helpers.h>
#include <CDetour/detourhelpers.h>
#include <CDetour/detours.h>

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
    int entity; // int
    int input; // bf_read
    int output_lastpropindex; // int
    int input_lastpropindex; // int
    int prop; // SendProp*
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

    PropTypeFns* proptypefns;
    Struct_CSendTablePrecalc struct_STP;
};
GameInfo g_gameinfo;





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

// SendVarFloat(float value, bool is_angle = false)
cell_t smn_SendVarFloat(IPluginContext* pContext, const cell_t* params)
{
    float value = params[1];

    bool is_angle = params[2];
    if (is_angle)
        value = anglemod(value);

    DVariant var;
    var.m_Type = DPT_Float;
    var.m_Float = value;

    return CreateSendVarHandle(pContext, var);
}

// Handle SendVarVector(const float vec[3], bool is_qangles = false)
cell_t smn_SendVarVector(IPluginContext* pContext, const cell_t* params)
{
    cell_t *sp_vec;
    pContext->LocalToPhysAddr(params[1], &sp_vec);

    Vector vec;
    for (int i = 0; i < 3; i++)
        vec[i] = sp_ctof(sp_vec[i]);

    bool is_qangles = params[2];
    if (is_qangles)
        for (int i = 0; i < 3; i++)
            vec[i] = anglemod(vec[i]);

    DVariant var;
    var.m_Type = DPT_Vector;
    for (int i = 0; i < 3; i++)
        var.m_Vector[i] = vec[i];

    return CreateSendVarHandle(pContext, var);
}

// Handle SendVarVectorXY(const float vec[2], bool is_qangles = false)
cell_t smn_SendVarVectorXY(IPluginContext* pContext, const cell_t* params)
{
    cell_t *sp_vec;
    pContext->LocalToPhysAddr(params[1], &sp_vec);

    Vector vec;
    for (int i = 0; i < 2; i++)
        vec[i] = sp_ctof(sp_vec[i]);

    bool is_qangles = params[2];
    if (is_qangles)
        for (int i = 0; i < 2; i++)
            vec[i] = anglemod(vec[i]);

    DVariant var;
    var.m_Type = DPT_VectorXY;
    for (int i = 0; i < 2; i++)
        var.m_Vector[i] = vec[i];

    return CreateSendVarHandle(pContext, var);
}

// Handle SendVarString(const const char[] string)
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

        int propcount = *(int*)((char*)precalc + g_gameinfo.struct_STP.propcount);
        SendProp** props = *(SendProp***)((char*)precalc + g_gameinfo.struct_STP.props);

        SendProp** prop = std::find_if(props, props + propcount, [propname](SendProp* prop) { return strcmp(prop->GetName(), propname) == 0; });

        int propindex = prop - props;
        if (propindex == propcount)
            return false;

        *info = SendPropInfo { *prop, propindex };
        sendtableinfo->lookup.insert(propname, *info);
    }

    return true;
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

        if (g_gameinfo.span_WDE.start <= eip && eip < (char*)g_gameinfo.span_WDE.start + g_gameinfo.span_WDE.size)
            break;
    }

    // extract client index from CBaseClient
    void* baseclient = *(void**)((char*)ebp_walk + g_gameinfo.stack_WDE.client);
    int client = *(int*)((char*)baseclient + 16);

    // check if edit list can be indexed with client. it should, but who knows.
    if (client < 0 || client >= 256)
        return nullptr;

    return list->GetEntry(client);
}

DETOUR_DECL_STATIC7(SendTable_WritePropList, void, SendTable*, sendtable, void*, inputdata, int, inputlength, bf_write*, output, int, entity, int*, propindexlist, int, propindexlistlength)
{
    EditEntry* entry = DiscoverEntry(entity);

    // this argument isn't used by SendTable_WritePropList other than in the debug section enabled with `g_CV_DTWatchEnt` or `g_CV_DTWatchClass`,
    // so it can be used to store information on the stack
    entity = (int)entry;

    if (!entry) {
        // normal call
        DETOUR_STATIC_CALL(SendTable_WritePropList)(sendtable, inputdata, inputlength, output, entity, propindexlist, propindexlistlength);
        return;
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
    DETOUR_STATIC_CALL(SendTable_WritePropList)(sendtable, inputdata, inputlength, output, entity, propindexlist, propindexlistlength);
}
CDetour *g_Detour_SendTable_WritePropList = nullptr;

// does the same thing as SendTable_WritePropList, but instead of copying from the input buffer it encodes the given DVariant
bool __cdecl SendVarEdit_Edit(void* ebp, int propindex)
{
    // get saved entry pointer (see comment in SendTable_WritePropList detour)
    const EditEntry* entry = *(const EditEntry**)((char*)ebp + g_gameinfo.stack_WPL.entity);

    SendTable* table = *(SendTable**)((char*)ebp + g_gameinfo.stack_WPL.table);
    SendProp** props = *(SendProp***)((char*)table->m_pPrecalc + g_gameinfo.struct_STP.props);
    SendProp* prop = props[propindex];

    // no support for arrays and data tables for now
    if (prop->GetType() == DPT_Array || prop->GetType() == DPT_DataTable)
        return false;

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
        return false;

    // const char* string = ((DVariant*)(info->var))->ToString();
    // smutils->LogMessage(myself, "edit: entry = %d, %s", info->propindex, string);

    bf_read* input = (bf_read*)((char*)ebp + g_gameinfo.stack_WPL.input);
    bf_write* output = *(bf_write**)((char*)ebp + g_gameinfo.stack_WPL.output);

    // follow procedure of the SendTable_WritePropList loop, but encode instead of copy

    // write prop index delta and update variable keeping track of the last output propindex
    {
        int* output_lastpropindex = (int*)((char*)ebp + g_gameinfo.stack_WPL.output_lastpropindex);

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

    return true;
}

Patcher g_Patcher_loop;
void* g_Detour_loop;

IForward *g_Forward_Pre_OnSendClientMessages;
IForward *g_Forward_Post_OnSendClientMessages;

DETOUR_DECL_MEMBER1(CGameServer__SendClientMessages, void, bool, b)
{
    if (g_Forward_Pre_OnSendClientMessages->GetFunctionCount())
        g_Forward_Pre_OnSendClientMessages->Execute();

    DETOUR_MEMBER_CALL(CGameServer__SendClientMessages)(b);

    if (g_Forward_Post_OnSendClientMessages->GetFunctionCount())
        g_Forward_Post_OnSendClientMessages->Execute();

    // free handles
    for (Handle_t handle : g_editdatahandles)
        handlesys->FreeHandle(handle, nullptr);
    g_editdatahandles.clear();

    // clear edits
    std::fill(g_editindices, g_editindices + MAX_EDICTS, -1);
    g_editlists.clear();
    g_editbuffer.clear();
}
CDetour *g_Detour_CGameServer__SendClientMessages = nullptr;

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

    CDetourManager::Init(smutils->GetScriptingEngine(), gameconf);

    // detour CGameServer::SendClientMessages to catch when the server is about to network stuff to clients
    {
        g_Detour_CGameServer__SendClientMessages = DETOUR_CREATE_MEMBER(CGameServer__SendClientMessages, "CGameServer::SendClientMessages");
        if(!g_Detour_CGameServer__SendClientMessages)
            RETURN_ERROR("Failed to detour CGameServer::SendClientMessages.");

        g_Detour_CGameServer__SendClientMessages->EnableDetour();
    }

    // detour SendTable_WritePropList to check for an entry
    {
        CDetourManager::Init(smutils->GetScriptingEngine(), gameconf);

        g_Detour_SendTable_WritePropList = DETOUR_CREATE_STATIC(SendTable_WritePropList, "SendTable_WritePropList");
        if(!g_Detour_SendTable_WritePropList)
            RETURN_ERROR("Failed to detour SendTable_WritePropList.");

        g_Detour_SendTable_WritePropList->EnableDetour();
    }

    // fill out game info struct
    {
        if (!gameconf->GetAddress("CBaseServer::WriteDeltaEntities", &g_gameinfo.span_WDE.start))
            RETURN_ERROR("Failed to find CBaseServer::WriteDeltaEntities.");
        gameconf->GetOffset("CBaseServer::WriteDeltaEntities size", &g_gameinfo.span_WDE.size);

        gameconf->GetOffset("CBaseServer::WriteDeltaEntities ebp client", &g_gameinfo.stack_WDE.client);

        gameconf->GetOffset("SendTable_WritePropList ebp table", &g_gameinfo.stack_WPL.table);
        gameconf->GetOffset("SendTable_WritePropList ebp output", &g_gameinfo.stack_WPL.output);
        gameconf->GetOffset("SendTable_WritePropList ebp entity", &g_gameinfo.stack_WPL.entity);
        gameconf->GetOffset("SendTable_WritePropList ebp input", &g_gameinfo.stack_WPL.input);
        gameconf->GetOffset("SendTable_WritePropList ebp output_lastpropindex", &g_gameinfo.stack_WPL.output_lastpropindex);
        gameconf->GetOffset("SendTable_WritePropList ebp input_lastpropindex", &g_gameinfo.stack_WPL.input_lastpropindex);
        gameconf->GetOffset("SendTable_WritePropList ebp prop", &g_gameinfo.stack_WPL.prop);

        if (!gameconf->GetAddress("&g_PropTypeFns", (void**)&g_gameinfo.proptypefns))
            RETURN_ERROR("Failed to find g_PropTypeFns.");

        gameconf->GetOffset("CSendTablePrecalc props", &g_gameinfo.struct_STP.props);
        gameconf->GetOffset("CSendTablePrecalc propcount", &g_gameinfo.struct_STP.propcount);
    }

    // write a detour that calls SendVarEdit_Edit and skips to the continuation point if it returns true
    {
        const char* register_propindex_string = gameconf->GetKeyValue("SendTable_WritePropList register propindex");

        sp::Register register_propindex;
        bool register_match = false;
        for (int regnum = 0; regnum < 8; regnum++) {
            register_propindex = sp::Register { regnum };
            if (strcmp(register_propindex_string, register_propindex.name()) == 0) {
                register_match = true;
                break;
            }
        }

        if (!register_match)
            RETURN_ERROR("Failed to match SendTable_WritePropList propindex register name.");

        void* loop_continue_addr;
        if (!gameconf->GetAddress("SendTable_WritePropList loop continue", &loop_continue_addr))
            RETURN_ERROR("Failed to find SendTable_WritePropList loop continuation point.");

        void* loop_break_addr;
        if (!gameconf->GetAddress("SendTable_WritePropList loop break", &loop_break_addr))
            RETURN_ERROR("Failed to find SendTable_WritePropList loop break point.");

        void* base;
        if (!gameconf->GetAddress("SendTable_WritePropList loop break condition", &base))
            RETURN_ERROR("Failed to find SendTable_WritePropList loop break condition.");

        const char* mask_string = gameconf->GetKeyValue("SendTable_WritePropList loop break condition mask");
        PatchMask mask((std::string(mask_string)));
        if (!mask.Valid())
            RETURN_ERROR("Failed to parse SendTable_WritePropList loop break condition mask.");

        sp::MacroAssembler masm_edit;
        sp::Label label_resume;

        // if there isn't an edit entry, then jump back to let code run normally (see comment in SendTable_WritePropList detour)
        masm_edit.cmpl(sp::Operand(sp::ebp, g_gameinfo.stack_WPL.entity), (int32_t)nullptr);
        masm_edit.j(sp::equal, &label_resume);

        // store volatile registers
        masm_edit.push(sp::eax); // 1
        masm_edit.push(sp::ecx); // 2
        masm_edit.push(sp::edx); // 3

        // stack pointer should be a multiple of 16 on function call
        masm_edit.subl(sp::esp, 3*0x4); // 6

        // push propindex as 2nd parameter
        int offset_propindex;
        if (gameconf->GetOffset("SendTable_WritePropList read propindex", &offset_propindex))
            masm_edit.push(sp::Operand(register_propindex, offset_propindex)); // 7
        else
            masm_edit.push(register_propindex); // 7

        // push ebp as 1st parameter
        masm_edit.push(sp::ebp); // 8

        // call SendVarEdit_Edit
        masm_edit.call(ExternalAddress((void*)&SendVarEdit_Edit));

        // remove parameters from stack
        masm_edit.addl(sp::esp, 2*0x4); // 2

        // undo stack alignment
        masm_edit.addl(sp::esp, 3*0x4); // 5

        // compare return value
        masm_edit.cmpb(sp::r8_al, (int8_t)false);

        // restore volatile registers
        masm_edit.pop(sp::edx); // 6
        masm_edit.pop(sp::ecx); // 7
        masm_edit.pop(sp::eax); // 8

        // if the return value was false then exit the detour
        masm_edit.j(sp::ConditionCode::equal, &label_resume);

        // jump to loop continuation point if return value was true
        masm_edit.jmp(ExternalAddress(loop_continue_addr));

        masm_edit.bind(&label_resume); // resume

        // reimplement the part the detour overwrote (if (input_lastpropindex >= MAX_DATATABLE_PROPS) break;)
        sp::Label label_break;
        masm_edit.cmpl(sp::Operand(sp::ebp, g_gameinfo.stack_WPL.input_lastpropindex), MAX_DATATABLE_PROPS);
        masm_edit.j(sp::above_equal, &label_break);

        // jump back to where the detour was made
        masm_edit.jmp(ExternalAddress((char*)base + mask.WritableSize()));

        // break loop
        masm_edit.bind(&label_break); // break
        masm_edit.jmp(ExternalAddress(loop_break_addr));

        // create buffer for custom assembly
        g_Detour_loop = smutils->GetScriptingEngine()->AllocatePageMemory(masm_edit.length());

        // write custom assembly
        masm_edit.emitToExecutableMemory(g_Detour_loop);

        // create jump patch to detour
        sp::MacroAssembler masm_jump;
        masm_jump.jmp(ExternalAddress(g_Detour_loop));

        new(&g_Patcher_loop) Patcher(base, mask);
        if (!g_Patcher_loop.Patch(masm_jump))
            RETURN_ERROR("Failed to create overwrite patch.");
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

    if (g_Detour_loop)
        smutils->GetScriptingEngine()->FreePageMemory(g_Detour_loop);

    g_Patcher_loop.UnPatch();

    if (g_Detour_SendTable_WritePropList && g_Detour_SendTable_WritePropList->IsEnabled())
        g_Detour_SendTable_WritePropList->Destroy();

    if (g_Detour_CGameServer__SendClientMessages && g_Detour_CGameServer__SendClientMessages->IsEnabled())
        g_Detour_CGameServer__SendClientMessages->Destroy();
}
