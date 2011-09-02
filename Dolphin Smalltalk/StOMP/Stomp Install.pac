| package |
package := Package name: 'StOMP Install'.
package paxVersion: 1;
	basicComment: '** StOMP - Smalltalk Objects on MessagePack**
Copyright (C) 2011 Masashi Umezawa

Yet another multi-dialect object serializer built on MessagePack.
The aim is to provide portable, fast, compact serializer for major Smalltalk dialects. 
StOMP is optimized for small/medium sized data. It is especially suitable for KVS or RPC.

Prerequisites: MessagePack for Dolphin Smalltalk

More Info:
http://stomp.smalltalk-users.jp/

'.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\MessagePack\MessagePack Install';
	add: 'Stomp-Core';
	add: 'Stomp-Dolphin-Core';
	add: 'StompTest-Core';
	add: 'StompTest-Dolphin-Core';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

