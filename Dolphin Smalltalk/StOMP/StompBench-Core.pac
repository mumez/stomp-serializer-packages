| package |
package := Package name: 'StompBench-Core'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.001'.


package classNames
	add: #StompBench;
	add: #StompBossBench;
	add: #StompFuelBench;
	add: #StompSmartRefStreamBench;
	add: #StompSrpBench;
	add: #StompXtreamsBench;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: 'StompBench-Model';
	add: 'Stomp-Core';
	yourself).

package!

"Class Definitions"!

Object subclass: #StompBench
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompBench subclass: #StompBossBench
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompBench subclass: #StompFuelBench
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompBench subclass: #StompSmartRefStreamBench
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompBench subclass: #StompSrpBench
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompBench subclass: #StompXtreamsBench
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StompBench guid: (GUID fromString: '{C082F1A8-C5B8-4BFA-9AC9-B7A38359947D}')!
StompBench comment: ''!
!StompBench categoriesForClass!StompBench-Core! !
!StompBench class methodsFor!

benchAll

	(((MethodCategory name: 'actions') contents select: [:each | each methodClass = StompBench class]) collect: [:e | e selector])
		do: [:each | self perform: each].!

blogPost01

blogPost02

checkSerializerExists

deserialize: bytes

doBench: selector repeating: repeats

order01

person01

protoBlogComment

protoSearchBody

protoSearchDoc

protoSearchHeader

protoSearchHighlight

random

randomPhraseSized: size

randomRanged: anInteger

randomWord

searchResult01

searchResult02

serialize: anObject

session01

writeReadBlogPost01

writeReadBlogPost02

writeReadOrder01

writeReadPerson01

writeReadSearchResult01

writeReadSearchResult02

writeReadSession01
!StompBench class categoriesFor: #benchAll!public!utilities! !
!StompBench class categoriesFor: #blogPost01!factories!public! !
!StompBench class categoriesFor: #blogPost02!factories!public! !
!StompBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompBench class categoriesFor: #deserialize:!hooks!public! !
!StompBench class categoriesFor: #doBench:repeating:!public!template! !
!StompBench class categoriesFor: #order01!factories!public! !
!StompBench class categoriesFor: #person01!factories!public! !
!StompBench class categoriesFor: #protoBlogComment!factories helper!public! !
!StompBench class categoriesFor: #protoSearchBody!factories helper!public! !
!StompBench class categoriesFor: #protoSearchDoc!factories helper!public! !
!StompBench class categoriesFor: #protoSearchHeader!factories helper!public! !
!StompBench class categoriesFor: #protoSearchHighlight!factories helper!public! !
!StompBench class categoriesFor: #random!public!utilities! !
!StompBench class categoriesFor: #randomPhraseSized:!public!utilities! !
!StompBench class categoriesFor: #randomRanged:!public!utilities! !
!StompBench class categoriesFor: #randomWord!public!utilities! !
!StompBench class categoriesFor: #searchResult01!factories!public! !
!StompBench class categoriesFor: #searchResult02!factories!public! !
!StompBench class categoriesFor: #serialize:!hooks!public! !
!StompBench class categoriesFor: #session01!factories!public! !
!StompBench class categoriesFor: #writeReadBlogPost01!actions!public! !
!StompBench class categoriesFor: #writeReadBlogPost02!actions!public! !
!StompBench class categoriesFor: #writeReadOrder01!actions!public! !
!StompBench class categoriesFor: #writeReadPerson01!actions!public! !
!StompBench class categoriesFor: #writeReadSearchResult01!actions!public! !
!StompBench class categoriesFor: #writeReadSearchResult02!actions!public! !
!StompBench class categoriesFor: #writeReadSession01!actions!public! !

StompBossBench guid: (GUID fromString: '{A241949A-71F0-4013-A334-F0DCEDABAAE6}')!
StompBossBench comment: ''!
!StompBossBench categoriesForClass!StompBench-Core! !
!StompBossBench class methodsFor!

checkSerializerExists

deserialize: bytes

serialize: anObject
!StompBossBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompBossBench class categoriesFor: #deserialize:!hooks!public! !
!StompBossBench class categoriesFor: #serialize:!hooks!public! !

StompFuelBench guid: (GUID fromString: '{D78F92A3-EA77-4614-8602-12F780DE8AEC}')!
StompFuelBench comment: ''!
!StompFuelBench categoriesForClass!StompBench-Core! !
!StompFuelBench class methodsFor!

checkSerializerExists

deserialize: bytes

serialize: anObject
!StompFuelBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompFuelBench class categoriesFor: #deserialize:!hooks!public! !
!StompFuelBench class categoriesFor: #serialize:!hooks!public! !

StompSmartRefStreamBench guid: (GUID fromString: '{CACD64C9-641C-441E-B762-DDC4F835AAB1}')!
StompSmartRefStreamBench comment: ''!
!StompSmartRefStreamBench categoriesForClass!StompBench-Core! !
!StompSmartRefStreamBench class methodsFor!

checkSerializerExists

deserialize: bytes

serialize: anObject
!StompSmartRefStreamBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompSmartRefStreamBench class categoriesFor: #deserialize:!hooks!public! !
!StompSmartRefStreamBench class categoriesFor: #serialize:!hooks!public! !

StompSrpBench guid: (GUID fromString: '{AFCF010A-7D07-4D6B-B095-8C414AACB2AE}')!
StompSrpBench comment: ''!
!StompSrpBench categoriesForClass!StompBench-Core! !
!StompSrpBench class methodsFor!

checkSerializerExists

deserialize: bytes

serialize: anObject
!StompSrpBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompSrpBench class categoriesFor: #deserialize:!hooks!public! !
!StompSrpBench class categoriesFor: #serialize:!hooks!public! !

StompXtreamsBench guid: (GUID fromString: '{F0712AF3-EFCF-4F12-B6DF-2EF75BB11B82}')!
StompXtreamsBench comment: ''!
!StompXtreamsBench categoriesForClass!StompBench-Core! !
!StompXtreamsBench class methodsFor!

checkSerializerExists

deserialize: bytes

serialize: anObject
!StompXtreamsBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompXtreamsBench class categoriesFor: #deserialize:!hooks!public! !
!StompXtreamsBench class categoriesFor: #serialize:!hooks!public! !

"Binary Globals"!
