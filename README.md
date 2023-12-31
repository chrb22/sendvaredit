# SendVarEdit
A SourceMod extension that allows you to manipulate the sent values of networkable entity properties for individual clients even while `sv_parallel_sendsnapshot` is enabled. The extension works by detouring `SendTable_WritePropList` where packed entity data is normally selectively copied to the client's buffer. Here it can write custom values into the buffer or prevent props from getting copied. Incompatible with the debug ConVars `dtwatchent` and `dtwatchclass`.

## Installing
Extract/copy the files into the game folder.

## Usage
The extension works with entity properties, the same that you're used to with the `SetEntProp[Type]` functions, except it doesn't set the entity property itself, but instead modifies the sent value. Since not all properties are networkable the extension allows you to verify if an entity property is networkable through `HasNetworkableProp`. After you know a property is networkable, you can set its sent value with `SetSendVar`, replace it only if its already going to be sent with `ReplaceSendVar`, or prevent it from being sent with `OmitSendVar`. `SetSendVar` and `ReplaceSendVar` take a custom value that are stored in a handle. These handles can be created with the `SendVar[Type]` functions, where `Type` is the type of the property. All handles are automatically freed after everything has been sent out, so you can use `SendVar[Type]` directly in the argument of `[Set/Replace]SendVar` without worry. The extension also includes the forwards `OnSendClientMessages` and `OnSendClientMessagesPost` to let you catch whenever the server is going to send out updates to clients and when it is done doing so.

## Compiling
1. Install AMBuild.
    - `git clone https://github.com/alliedmodders/ambuild`
    - `pip install ./ambuild`
2. Fetch dependencies.
    - `git clone https://github.com/alliedmodders/metamod-source mmsource -b 1.11-dev`
    - `git clone --recursive https://github.com/alliedmodders/sourcemod sourcemod -b 1.11-dev`
    - `git clone https://github.com/alliedmodders/hl2sdk hl2sdk-<SDK> -b <SDK>` (replace `<SDK>` with the game(s) you want to compile for, e.g. `tf2`)
3. Fetch SendVarEdit.
    - `git clone https://github.com/chrb22/sendvaredit`
4. Set up build environment.
    - `cd sendvaredit`
    - `mkdir build`
    - `cd build`
    - `python ../configure.py --mms-path="../../mmsource" --sm-path="../../sourcemod" --hl2sdk-root="../.." --sdks=present --enable-optimize`
5. Compile extension.
    - `ambuild`
    - The build will be in `sendvaredit/build/package`