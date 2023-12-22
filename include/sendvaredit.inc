#if defined _sendvaredit_included
 #endinput
#endif
#define _sendvaredit_included

/**
 * Called when the server is about to process updates to clients.
 */
forward void OnSendClientMessages();

/**
 * Called when the server is done processing updates to clients.
 */
forward void OnSendClientMessagesPost();

/**
 * Checks if an entity property is networkable. This is different to `HasEntProp` that does not check if it is networkable.
 *
 * @param entity            Entity/edict index.
 * @param prop              Property name.
 */
native Handle HasNetworkableProp(int entity, const char[] prop);

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
 * @param is_angle          Ensures the angle will be between 0 and 360.
 */
native Handle SendVarFloat(float value, bool is_angle = false);

/**
 * Stores a vector to be networked. The handle is freed when client updates are completed.
 *
 * @param vec               Vector to set.
 * @param is_qangles        Ensures the angles will be between 0 and 360.
 */
native Handle SendVarVector(const float vec[3], bool is_qangles = false);

/**
 * Stores a 2D vector to be networked. The handle is freed when client updates are completed.
 *
 * @param vec               Vector to set.
 * @param is_qangles        Ensures the angles will be between 0 and 360.
 */
native Handle SendVarVectorXY(const float vec[2], bool is_qangles = false);

/**
 * Stores a string to be networked. The handle is freed when client updates are completed.
 *
 * @param string            String to set.
 */
native Handle SendVarString(const char[] string);

/**
 * Sets the networked value of an entity's property for the current frame.
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
    MarkNativeAsOptional("HasNetworkableProp");
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