#if defined _sendvaredit_included
 #endinput
#endif
#define _sendvaredit_included

/**
 * Wrapper for transmit BitVec that decides what entities get sent to a client.
 * Be careful with entities that parent other entities.
 */
methodmap TransmitBitVec < Handle
{
    /**
     * Get if entity should be sent to the client.
     *
     * @param entity            Entity/edict index.
     */
    public native bool Get(int entity);

    /**
     * Set if entity should be sent to the client.
     *
     * @param entity            Entity/edict index.
     * @param transmit          Transmit state.
     */
    public native bool Set(int entity, bool transmit);
}

/**
 * Called when the server is about to process updates to clients.
 */
forward void OnSendClientMessages();

/**
 * Called when the server is done processing updates to clients.
 */
forward void OnSendClientMessagesPost();

/**
 * Called after the server has decided what entities to send to the client.
 *
 * @param client            Player's index.
 * @param transmitted       BitVec of entities to send to client.
 */
forward void OnCheckTransmit(int client, TransmitBitVec transmitted);

/**
 * Checks if an entity property is networkable. This is different to `HasEntProp` that does not check if it is networkable.
 *
 * @param entity            Entity/edict index.
 * @param prop              Property name.
 */
native Handle HasNetworkableProp(int entity, const char[] prop);

/**
 * Converts an angle to a networkable float (simply ensures the angle will be between 0 and 360).
 *
 * @param angle             Angle to convert.
 */
native float SendProxyAngle(float angle);

native void SendProxyQAnglesNative(const float qangles[3], float vec[3]);
/**
 * Converts a QAngles to a networkable vector (simply ensures the angles will be between 0 and 360).
 *
 * @param qangles           QAngles to convert.
 */
stock float[] SendProxyQAngles(const float qangles[3])
{
    float vec[3];
    SendProxyQAnglesNative(qangles, vec);
    return vec;
}

/**
 * Converts the entity index of an EHandle to its networkable integer value. Use -1 to get a value of a non-existent entity.
 *
 * @param entity            Entity/edict index to convert.
 */
native int SendProxyEHandle(int entity);

/**
 * Stores an integer value to be networked. The handle is freed when client updates are completed.
 *
 * @param value             Value to set.
 */
native Handle SendVarInt(any value);

/**
 * Stores a float value to be networked. The handle is freed when client updates are completed.
 *
 * @param value             Value to set.
 */
native Handle SendVarFloat(float value);

/**
 * Stores a vector to be networked. The handle is freed when client updates are completed.
 *
 * @param vec               Vector to set.
 */
native Handle SendVarVector(const float vec[3]);

/**
 * Stores a 2D vector to be networked. The handle is freed when client updates are completed.
 *
 * @param vec               Vector to set.
 */
native Handle SendVarVectorXY(const float vec[2]);

/**
 * Stores a string to be networked. The handle is freed when client updates are completed.
 *
 * @param string            String to set.
 */
native Handle SendVarString(const char[] string);

/**
 * Sets the networked value of an entity's property and ensures it is included in client updates for the current frame.
 * Does not guarantee that the client(s) will receive an update on the current frame and does not remember the call for later frames.
 * Use `OnCheckTransmit` to know when a client is getting sent an update and use the function inside there to ensure the client receives the value.
 *
 * @param entity            Entity/edict index.
 * @param client            Player's index. Use -1 to target everyone.
 * @param prop              Property name.
 * @param value             Value to set (get handle from SendVar[Type] functions).
 * @param priority          Priority for this edit.
 */
native void SetSendVar(int entity, int client, const char[] prop, Handle value, int priority = 0)

/**
 * Replace the networked value of an entity's property if it's going to be sent for the current frame.
 *
 * @param entity            Entity/edict index.
 * @param client            Player's index. Use -1 to target everyone.
 * @param prop              Property name.
 * @param value             Value to set (get handle from SendVar[Type] functions).
 * @param priority          Priority for this edit.
 */
native void ReplaceSendVar(int entity, int client, const char[] prop, Handle value, int priority = 0)

/**
 * Omits an entity's property from being sent for the current frame.
 *
 * @param entity            Entity/edict index.
 * @param client            Player's index. Use -1 to target everyone.
 * @param prop              Property name.
 * @param priority          Priority for this edit.
 */
native void OmitSendVar(int entity, int client, const char[] prop, int priority = 0)

public Extension __ext_sendvaredit = 
{
    name = "SendVarEdit",
    file = "sendvaredit.ext",
#if defined AUTOLOAD_EXTENSIONS
    autoload = 1,
#else
    autoload = 0,
#endif
#if defined REQUIRE_EXTENSIONS
    required = 1,
#else
    required = 0,
#endif
};

#if !defined REQUIRE_EXTENSIONS
public __ext_sendvaredit_SetNTVOptional()
{
    MarkNativeAsOptional("TransmitBitVec.Get");
    MarkNativeAsOptional("TransmitBitVec.Set");
    MarkNativeAsOptional("HasNetworkableProp");
    MarkNativeAsOptional("SendProxyAngle");
    MarkNativeAsOptional("SendProxyQAnglesNative");
    MarkNativeAsOptional("SendProxyEHandle");
    MarkNativeAsOptional("SendVarInt");
    MarkNativeAsOptional("SendVarFloat");
    MarkNativeAsOptional("SendVarVector");
    MarkNativeAsOptional("SendVarVectorXY");
    MarkNativeAsOptional("SendVarString");
    MarkNativeAsOptional("SetSendVar");
    MarkNativeAsOptional("ReplaceSendVar");
    MarkNativeAsOptional("OmitSendVar");
}
#endif
