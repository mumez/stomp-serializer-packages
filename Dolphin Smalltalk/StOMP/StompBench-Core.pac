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

benchAll	"self benchAll"	Transcript cr; show: '**', self name, '**'.	"(StompBench class organization listAtCategoryNamed: #actions) do: [:each | self perform: each]."

	(((MethodCategory name: 'actions') contents select: [:each | each methodClass = StompBench class]) collect: [:e | e selector])
		do: [:each | self perform: each].!

blogPost01	"self blogPost01"	| blogPost |	blogPost := StompMockBlogPost new.	blogPost id: 1;	 title: 'My Smalltalk';	 status: 0; 	content: (self randomPhraseSized: 5000); 	creationDate: (StompPortableUtil default timestampFromNanoseconds: 3465089041421000000);	modifiedDate: (StompPortableUtil default timestampFromNanoseconds: 3465089112345000000); 	author: (StompMockAuthor authors at: #ume); 	categories: (Array with: (StompMockCategory getCategories at: #life)); 	tags: (Array with: (StompMockTag tags at: #visualworks) with: (StompMockTag tags at: #trans)).	^ blogPost !

blogPost02	"self blogPost02"	| posts post |	posts := OrderedCollection new.	1 to: 20 do: [:idx |		posts add: (self blogPost01 id: idx printString)	].	1 to: 5 do: [:idx |		post := posts at: idx.		post attachments: (Array with: StompMockAttachment attachment1 with: StompMockAttachment attachment2).		post modifiedDate: (StompPortableUtil default timestampFromNanoseconds: 3465089051421000000).		post comments: (Array with: self protoBlogComment with: self protoBlogComment).	].	^posts	!

checkSerializerExists	"override"	^true!

deserialize: bytes	"override"	^Object fromStomp: bytes!

doBench: selector repeating: repeats	| target bytes readTime times writeTime |	target := self perform: selector.	times := repeats.		self checkSerializerExists ifFalse: [self error: 'Serializer does not exist!!'].	writeTime := Time microsecondsToRun: [		times timesRepeat: [bytes := self serialize: target]	].	readTime := Time microsecondsToRun: [		times timesRepeat: [self deserialize: bytes]	].	Transcript cr; show: selector; space;		show: 'writeTime: ', writeTime printString; space;		show: 'readTime: ', readTime printString; space;		show: 'total: ', (writeTime + readTime) printString; space;		show: 'size: ', (bytes size printString)!

order01	| items order |	items := Dictionary new.	items at: (StompMockOrderItem new id: 123 ; title: 'foofoofoo'; price: 2980) put: 2.	items at: (StompMockOrderItem new id: 456 ; title: 'barbarbar'; price: 1980) put: 1.	items at: (StompMockOrderItem new id: 789 ; title: 'bazbazbaz'; price: 5980) put: 1.		order := StompMockOrder new.	order items: 12345678901234567890.	order items: items.		^order!

person01	| dict phoneNumbers |	phoneNumbers := OrderedCollection new.	phoneNumbers add: (Array with: 0 with: '000-1111-2222').	phoneNumbers add: (Array with: 1 with: '222-3333-4444').	phoneNumbers add: (Array with: 2 with: '555-6666-7777').		dict := Dictionary new.	dict at: 'id' put: self random.	dict at: 'name' put: (self randomPhraseSized: 20).	dict at: 'phoneNumbers' put: phoneNumbers.		^dict!

protoBlogComment	| comment |	comment := StompMockComment new id: self random.	comment content: (self randomPhraseSized: 100).	comment accepted: false.	comment count: self random.	comment author: (StompMockAuthor authors at: #yama).	comment creationDate: (StompPortableUtil default timestampFromNanoseconds: 3485089041421000000).	^ comment!

protoSearchBody	| body |	body := StompMockSearchBody new.	body numFounds: 3.	body start: 0.	^ body!

protoSearchDoc	| cats1 doc1 features1 |	doc1 := StompMockSearchDocument new.	doc1 id: 'MA147LL/A'.	doc1 name: '60 GB iPod with Video Playback Black'.	doc1 price: 399.0.	doc1 popularity: self random.	doc1 inStock: true.	doc1 timestamp: (StompPortableUtil default timestampFromNanoseconds: 3485089041421000000).	cats1 := Array with: 'electoronics' with: 'music'.	doc1 categories: cats1.	features1 := OrderedCollection new.	features1 add: (self randomPhraseSized: 30).	features1 add: (self randomPhraseSized: 70).	features1 add: (self randomPhraseSized: 50).	doc1 features: features1.	^ doc1!

protoSearchHeader	| header params |	header := StompMockSearchHeader new.	header status: 0.	header time: 10.	params := Dictionary new.	params at: 'wt' put: 'json'.	params at: 'indent' put: 'on'.	params at: 'hl' put: true.	header params: params.	^ header!

protoSearchHighlight	| highlight1 |	highlight1 := StompMockSearchHighlight new.	highlight1 name: (Array with: (self randomPhraseSized: 50)).	highlight1 features: (Array with: (self randomPhraseSized: 70)).	^ highlight1!

random	^ self randomRanged: 50!

randomPhraseSized: size	"self randomPhraseSized: 30"	| str |	str := ''.	[str := str, ' ', self randomWord.	str size < size] whileTrue.	^str copyFrom: 1 to: size !

randomRanged: anInteger	^ (Random new next * anInteger) ceiling!

randomWord	^(Object selectors asArray at: self random) asString!

searchResult01	"self searchResult01"	| resp header body docs highlights |	resp := StompMockSearchResponse new.	header := self protoSearchHeader.	body := self protoSearchBody.	docs := OrderedCollection new.	docs add: (self protoSearchDoc).	docs add: (self protoSearchDoc).	docs add: (self protoSearchDoc).	body docs: docs.	highlights := Dictionary new.	highlights at: 'MA147LL/A' put: self protoSearchHighlight.	highlights at: 'MA147LL/B' put: self protoSearchHighlight.	highlights at: 'MA147LL/C' put: self protoSearchHighlight.	resp header: header; body: body; highlightings: highlights.	^resp!

searchResult02	"self searchResult02"	| resp header body docs ids highlights |	resp := StompMockSearchResponse new.	header := self protoSearchHeader.	body := self protoSearchBody.	docs := OrderedCollection new.	ids := 1 to: 50.	ids do: [:eachId | docs add: (self protoSearchDoc id: eachId printString)].	body docs: docs.	highlights := Dictionary new.	ids do: [:eachId |		highlights at: eachId printString put: self protoSearchHighlight.	].	resp header: header; body: body; highlightings: highlights.	^resp!

serialize: anObject	"override"	^anObject toStomp!

session01	| session properties |	session := StompMockSession new id: 'd21d3f1a-0a21-054a-b106-93fa445b337c'.	"session expires: 60 seconds."	properties := Dictionary new.	properties at: 'url' put: 'http://somehost.com:9090/someService/some/'.	properties at: 'token' put: 'someToken'.	properties at: '_k' put: 'c20036b7-12a0-b446-a1ac-280a4dc102bc'.	session properties: properties.	^session!

writeReadBlogPost01	"self writeReadBlogPost01"	self doBench: #blogPost01 repeating: 200!

writeReadBlogPost02	"self writeReadBlogPost02"	self doBench: #blogPost02 repeating: 2!

writeReadOrder01	"self writeReadOrder01"	self doBench: #order01 repeating: 1000!

writeReadPerson01	"self writeReadPerson01"	self doBench: #person01 repeating: 1000!

writeReadSearchResult01	"self writeReadSearchResult01"	self doBench: #searchResult01 repeating: 200!

writeReadSearchResult02	"self writeReadSearchResult02"	self doBench: #searchResult02 repeating: 50!

writeReadSession01	"self writeReadSession01"	self doBench: #session01 repeating: 1000! !
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

checkSerializerExists	"override"	^(Smalltalk at: #BinaryObjectStorage ifAbsent:[]) notNil!

deserialize: bytes	| stream bos data |	stream := ReadStream on: bytes.	bos := BinaryObjectStorage onOld: stream.	data := bos next.	bos close.	^data!

serialize: anObject	| stream bos |	stream := WriteStream on: ByteArray new.	bos := BinaryObjectStorage onNew: stream.	bos nextPut: anObject.	bos close.	^stream contents! !
!StompBossBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompBossBench class categoriesFor: #deserialize:!hooks!public! !
!StompBossBench class categoriesFor: #serialize:!hooks!public! !

StompFuelBench guid: (GUID fromString: '{D78F92A3-EA77-4614-8602-12F780DE8AEC}')!
StompFuelBench comment: ''!
!StompFuelBench categoriesForClass!StompBench-Core! !
!StompFuelBench class methodsFor!

checkSerializerExists	"override"	^Smalltalk includesKey: #FLSerializer!

deserialize: bytes	| des |	des := (FLMaterializer on: bytes readStream) .	des materialize.	^ des root.!

serialize: anObject	| ser |	ser := (FLSerializer on: ByteArray new writeStream) .	ser serialize: anObject.	^ ser stream contents.! !
!StompFuelBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompFuelBench class categoriesFor: #deserialize:!hooks!public! !
!StompFuelBench class categoriesFor: #serialize:!hooks!public! !

StompSmartRefStreamBench guid: (GUID fromString: '{CACD64C9-641C-441E-B762-DDC4F835AAB1}')!
StompSmartRefStreamBench comment: ''!
!StompSmartRefStreamBench categoriesForClass!StompBench-Core! !
!StompSmartRefStreamBench class methodsFor!

checkSerializerExists	"override"	^Smalltalk includesKey: #SmartRefStream!

deserialize: bytes	^SmartRefStream objectFromStreamedRepresentation: bytes!

serialize: anObject	^SmartRefStream streamedRepresentationOf: anObject! !
!StompSmartRefStreamBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompSmartRefStreamBench class categoriesFor: #deserialize:!hooks!public! !
!StompSmartRefStreamBench class categoriesFor: #serialize:!hooks!public! !

StompSrpBench guid: (GUID fromString: '{AFCF010A-7D07-4D6B-B095-8C414AACB2AE}')!
StompSrpBench comment: ''!
!StompSrpBench categoriesForClass!StompBench-Core! !
!StompSrpBench class methodsFor!

checkSerializerExists	"override"	^(Smalltalk at: #SrpConfiguration ifAbsent: [Smalltalk at: #SRP ifAbsent:[]]) notNil!

deserialize: bytes	^SrpConfiguration default loadObjectFrom: bytes!

serialize: anObject	^SrpConfiguration default saveObject: anObject! !
!StompSrpBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompSrpBench class categoriesFor: #deserialize:!hooks!public! !
!StompSrpBench class categoriesFor: #serialize:!hooks!public! !

StompXtreamsBench guid: (GUID fromString: '{F0712AF3-EFCF-4F12-B6DF-2EF75BB11B82}')!
StompXtreamsBench comment: ''!
!StompXtreamsBench categoriesForClass!StompBench-Core! !
!StompXtreamsBench class methodsFor!

checkSerializerExists	"override"	^(Smalltalk at: #ObjectWriteStream ifAbsent: [Smalltalk at: #Xtreams ifAbsent:[]]) notNil!

deserialize: bytes	^ bytes reading marshaling get!

serialize: anObject	^ByteArray new writing marshaling put: anObject; conclusion.! !
!StompXtreamsBench class categoriesFor: #checkSerializerExists!hooks!public! !
!StompXtreamsBench class categoriesFor: #deserialize:!hooks!public! !
!StompXtreamsBench class categoriesFor: #serialize:!hooks!public! !

"Binary Globals"!

