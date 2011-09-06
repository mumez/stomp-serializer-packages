*** StOMP for Dolphin Smalltalk ***

Copyright (C) 2011 Masashi Umezawa
http://stomp.smalltalk-users.jp/

StOMP is a multi-dialect object serializer built on MessagePack.
The aim is to provide a portable, fast, compact serializer for major Smalltalk dialects. 
StOMP is optimized for small/medium sized data. It is especially suitable for KVS or RPC.

- Prerequisites:
MessagePack for Dolphin Smalltalk
http://code.google.com/p/messagepack-st/

- How to install:
0. Please make sure that MessagePack for Dolphin Smalltalk has been installed.
1. Extract the zip file to Dolphin home directory.
2. From the Package Browser, select "File"->"Install Package...", then load the package "Stomp Install.pac".

*** Notes for Professional Version ***

- Deployment:

1) In the ToGo deployment wizard, you need to ensure that the image stripper will "Retain Instance Variable Names" (Step 5) - it's off by default but StOMP needs this information.

