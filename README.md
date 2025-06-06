# SendVarEdit
A SourceMod extension that allows you to manipulate the sent values of networkable entity properties for individual clients even while `sv_parallel_sendsnapshot` is enabled. The extension works by detouring `SendTable_WritePropList` where packed entity data is normally selectively copied to the client's buffer. Here it can write custom values into the buffer or prevent props from getting copied.

## Installing
Extract/copy the files into the game folder.

Currently the extension only supports 32-bit TF2, but it can in idea work with other games by expanding the gamedata.

## Usage
The extension works with entity properties, the same that you're used to with the `SetEntProp[Type]` functions, except it doesn't set the entity property itself, but instead modifies the sent value. Since not all properties are networkable, the extension allows you to verify if an entity property is networkable through `HasNetworkableProp`. After you know a property is networkable, you can set its sent value with `SetSendVar`, replace it only if its already going to be sent with `ReplaceSendVar`, or prevent it from being sent with `OmitSendVar`. If a property has multiple edits applied at the same time, then the extension will prioritize edits that use most important action present (`Set` > `Replace` > `Omit`, individual > everyone) and choose the latest amongst those.

`SetSendVar` and `ReplaceSendVar` take a custom value that are stored in a handle. These handles can be created with the `SendVar[Type]` functions, where `Type` is the sent type of the property. All handles are automatically freed at the end of the frame, so you can use `SendVar[Type]` directly in the argument of `[Set/Replace]SendVar` without worry. Some properties go through a conversion function called a proxy that might even change its sent type, so make sure to use the proxy output instead if it exists. The extension provides a few such proxies named `SendProxy[Type]` for convenience.

It is not guaranteed that a client will receive an update every frame (`cl_updaterate` < tickrate), so the the extension will remember every edit until then to make sure no action ends up missing its mark. It is possible to get notified when such a client update will happen by subscribing to the forward `OnCheckTransmit`. The forward can also be used to control what entities get sent to the client in cases where hiding or transmitting an entity through walls is desirable. The extension additionally includes the forwards `OnSendClientMessages` and `OnSendClientMessagesPost` to let you know whenever the server is preparing to send out updates to clients and when it is done doing so.

## Compiling
1. Install AMBuild.
    - `git clone https://github.com/alliedmodders/ambuild`
    - `pip install ./ambuild`
2. Fetch dependencies.
    - `git clone https://github.com/alliedmodders/metamod-source mmsource -b 1.12-dev`
    - `git clone --recursive https://github.com/alliedmodders/sourcemod sourcemod -b 1.12-dev`
    - `git clone https://github.com/alliedmodders/hl2sdk hl2sdk-<SDK> -b <SDK>` (replace `<SDK>` with the game(s) you want to compile for, e.g. `tf2`)
3. Fetch SendVarEdit.
    - `git clone --recursive https://github.com/chrb22/sendvaredit`
4. Set up build environment.
    - `cd sendvaredit`
    - `mkdir build`
    - `cd build`
    - `CC=gcc CXX=g++ python ../configure.py --mms-path="../../mmsource" --sm-path="../../sourcemod" --hl2sdk-manifest-path="../../sourcemod/hl2sdk-manifests" --hl2sdk-root="../.." --sdks=present --targets=x86 --enable-optimize`
5. Compile extension.
    - `ambuild`
    - The build will be in `sendvaredit/build/package`
