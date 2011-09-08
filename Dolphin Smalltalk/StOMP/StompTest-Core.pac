| package |
package := Package name: 'StompTest-Core'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '5'.


package classNames
	add: #StompCustomSerializationTestCase;
	add: #StompMockByteArray;
	add: #StompMockCustomWriteObject;
	add: #StompMockFixedAlternativeClass;
	add: #StompMockFixedClass;
	add: #StompMockFixedClass2;
	add: #StompMockFixedNewFailedClass;
	add: #StompMockMementoArray;
	add: #StompMockMixedClass;
	add: #StompMockObjectInitializingOnRead;
	add: #StompMockObjectPreparingOnWrite;
	add: #StompMockObjectWithCache;
	add: #StompMockPerson;
	add: #StompMockPersonShapeChanger;
	add: #StompMockRequest;
	add: #StompMockShapeChangedObject;
	add: #StompMockShapeChanger;
	add: #StompMockShapeChangerForRenamedComplexRead;
	add: #StompMockVariableAlternativeClass;
	add: #StompMockVariableClass;
	add: #StompMockVariableNewFailedClass;
	add: #StompNewFailedTestCase;
	add: #StompPortableFixtures;
	add: #StompReaderTestCase;
	add: #StompReadWriteTestCase;
	add: #StompShapeChangerTestCase;
	add: #StompTestCase;
	add: #StompWriterTestCase;
	yourself.

package methodNames
	add: #StompPortableUtil -> #testFixturesClass;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\MessagePack\MessagePack-Core';
	add: 'Stomp-Core';
	add: '..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

Object subclass: #StompMockCustomWriteObject
	instanceVariableNames: 'name email'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockFixedClass
	instanceVariableNames: 'instVar1 instVar2 instVar3 instVar4 instVar5'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockFixedClass2
	instanceVariableNames: 'instVar1 instVar2 instVar3 instVar4 instVar5'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockFixedNewFailedClass
	instanceVariableNames: 'att1 att2 att3Block'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockPerson
	instanceVariableNames: 'name requests partners'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockRequest
	instanceVariableNames: 'id owner'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockShapeChangedObject
	instanceVariableNames: 'renamedAtt1 addedAtt1 originalAtt1'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompPortableFixtures
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Array variableSubclass: #StompMockMementoArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Array variableSubclass: #StompMockMixedClass
	instanceVariableNames: 'instVar1 instVar2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Array variableSubclass: #StompMockVariableClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Array variableSubclass: #StompMockVariableNewFailedClass
	instanceVariableNames: 'att1 att2 att3Block'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompMockVariableNewFailedClass variableSubclass: #StompMockVariableAlternativeClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ByteArray variableByteSubclass: #StompMockByteArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompMockFixedClass subclass: #StompMockObjectInitializingOnRead
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompMockFixedClass subclass: #StompMockObjectPreparingOnWrite
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompMockFixedClass subclass: #StompMockObjectWithCache
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'att1 att2 cache1'!
StompMockFixedNewFailedClass subclass: #StompMockFixedAlternativeClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompShapeChanger subclass: #StompMockPersonShapeChanger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompShapeChanger subclass: #StompMockShapeChanger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompShapeChanger subclass: #StompMockShapeChangerForRenamedComplexRead
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #StompCustomSerializationTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #StompNewFailedTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #StompTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompTestCase subclass: #StompReaderTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompTestCase subclass: #StompReadWriteTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompTestCase subclass: #StompShapeChangerTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompTestCase subclass: #StompWriterTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!StompPortableUtil methodsFor!

testFixturesClass	self subclassResponsibility ! !
!StompPortableUtil categoriesFor: #testFixturesClass!*StompTest/Core/factory!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StompMockCustomWriteObject guid: (GUID fromString: '{5AAA9083-7D2A-4DE5-A81A-38CA6933148A}')!
StompMockCustomWriteObject comment: ''!
!StompMockCustomWriteObject categoriesForClass!StompTest-Core! !
!StompMockCustomWriteObject methodsFor!

email	"Answer the value of email"	^ email!

email: anObject	"Set the value of email"	email := anObject!

initialize	name := 'Test Name'.	email := 'test-name@example.com'!

name	^name!

name: anObject	"Set the value of name"	name := anObject!

stompWriteValue	"Write myself as an array"	^StompMockMementoArray with: self name with: self email! !
!StompMockCustomWriteObject categoriesFor: #email!accessing!public! !
!StompMockCustomWriteObject categoriesFor: #email:!accessing!public! !
!StompMockCustomWriteObject categoriesFor: #initialize!initialize/release!public! !
!StompMockCustomWriteObject categoriesFor: #name!accessing!public! !
!StompMockCustomWriteObject categoriesFor: #name:!accessing!public! !
!StompMockCustomWriteObject categoriesFor: #stompWriteValue!public!stomp/objectstream/writing! !

StompMockFixedClass guid: (GUID fromString: '{A5E8D9B7-2AAE-4EBC-AAF0-BF96D5187723}')!
StompMockFixedClass comment: ''!
!StompMockFixedClass categoriesForClass!StompTest-Core! !
!StompMockFixedClass methodsFor!

equals: other	self class = other class ifFalse: [^false].	instVar1 = other instVar1 ifFalse: [^false].	instVar2 = other instVar2 ifFalse: [^false].	instVar3 = other instVar3 ifFalse: [^false].	instVar4 = other instVar4 ifFalse: [^false].	instVar5 = other instVar5 ifFalse: [^false].	^true!

instVar1	"Answer the value of instVar1"	^ instVar1!

instVar1: anObject	"Set the value of instVar1"	instVar1 := anObject!

instVar2	"Answer the value of instVar2"	^ instVar2!

instVar2: anObject	"Set the value of instVar2"	instVar2 := anObject!

instVar3	"Answer the value of instVar3"	^ instVar3!

instVar3: anObject	"Set the value of instVar3"	instVar3 := anObject!

instVar4	"Answer the value of instVar4"	^ instVar4!

instVar4: anObject	"Set the value of instVar4"	instVar4 := anObject!

instVar5	"Answer the value of instVar5"	^ instVar5!

instVar5: anObject	"Set the value of instVar5"	instVar5 := anObject! !
!StompMockFixedClass categoriesFor: #equals:!comparing!public! !
!StompMockFixedClass categoriesFor: #instVar1!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar1:!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar2!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar2:!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar3!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar3:!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar4!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar4:!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar5!accessing!public! !
!StompMockFixedClass categoriesFor: #instVar5:!accessing!public! !

StompMockFixedClass2 guid: (GUID fromString: '{4210FD04-41CA-4644-B831-EB3A774F9C22}')!
StompMockFixedClass2 comment: ''!
!StompMockFixedClass2 categoriesForClass!StompTest-Core! !
!StompMockFixedClass2 methodsFor!

equals: other	self class = other class ifFalse: [^false].	instVar1 = other instVar1 ifFalse: [^false].	instVar2 = other instVar2 ifFalse: [^false].	instVar3 = other instVar3 ifFalse: [^false].	instVar4 = other instVar4 ifFalse: [^false].	instVar5 = other instVar5 ifFalse: [^false].	^true!

instVar1	"Answer the value of instVar1"	^ instVar1!

instVar1: anObject	"Set the value of instVar1"	instVar1 := anObject!

instVar2	"Answer the value of instVar2"	^ instVar2!

instVar2: anObject	"Set the value of instVar2"	instVar2 := anObject!

instVar3	"Answer the value of instVar3"	^ instVar3!

instVar3: anObject	"Set the value of instVar3"	instVar3 := anObject!

instVar4	"Answer the value of instVar4"	^ instVar4!

instVar4: anObject	"Set the value of instVar4"	instVar4 := anObject!

instVar5	"Answer the value of instVar5"	^ instVar5!

instVar5: anObject	"Set the value of instVar5"	instVar5 := anObject! !
!StompMockFixedClass2 categoriesFor: #equals:!comparing!public! !
!StompMockFixedClass2 categoriesFor: #instVar1!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar1:!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar2!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar2:!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar3!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar3:!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar4!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar4:!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar5!accessing!public! !
!StompMockFixedClass2 categoriesFor: #instVar5:!accessing!public! !

StompMockFixedNewFailedClass guid: (GUID fromString: '{51464AFC-BD6E-4E0B-9DF0-8A79055C20BB}')!
StompMockFixedNewFailedClass comment: ''!
!StompMockFixedNewFailedClass categoriesForClass!StompTest-Core! !
!StompMockFixedNewFailedClass methodsFor!

att1	^att1!

att1: aValue	att1 := aValue!

att2	^att2!

att2: aValue	att2 := aValue!

att3Block	^att3Block!

att3Block: aValue	att3Block := aValue!

stompInitialize	att3Block := [:a :b | a < b].! !
!StompMockFixedNewFailedClass categoriesFor: #att1!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att1:!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att2!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att2:!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att3Block!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att3Block:!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #stompInitialize!initializing/stomp!public! !

!StompMockFixedNewFailedClass class methodsFor!

new	^Error new signal: '#new should not be used!!'!

prototype1	"self prototype1"	| inst |	inst := self basicNew.	inst att1: 1.	inst att2: 'TWO'.	inst att3Block: [:this | 'is block'].	^inst! !
!StompMockFixedNewFailedClass class categoriesFor: #new!instance creation!public! !
!StompMockFixedNewFailedClass class categoriesFor: #prototype1!for tests!public! !

StompMockPerson guid: (GUID fromString: '{CD8B48F0-E39A-4527-96E8-489695C68F53}')!
StompMockPerson comment: ''!
!StompMockPerson categoriesForClass!StompTest-Core! !
!StompMockPerson methodsFor!

name	^name!

name: aString	name := aString!

partners	partners isNil ifTrue: [partners := Array new].	^ partners!

partners: anObject	partners := anObject!

requests	requests isNil ifTrue: [requests := Array new].	^ requests!

requests: anObject	requests := anObject! !
!StompMockPerson categoriesFor: #name!accessing!public! !
!StompMockPerson categoriesFor: #name:!accessing!public! !
!StompMockPerson categoriesFor: #partners!accessing!public! !
!StompMockPerson categoriesFor: #partners:!accessing!public! !
!StompMockPerson categoriesFor: #requests!accessing!public! !
!StompMockPerson categoriesFor: #requests:!accessing!public! !

!StompMockPerson class methodsFor!

example1	"self example1"	| req1 req2 person1 person2 |	req1 := StompMockRequest new.	req1 id: 1.	req2 := StompMockRequest new.		req2 id: 2.	person1 := StompMockPerson new name: 'A'.	person1 requests: (Array with: req1 with: req2).	person2 := StompMockPerson new name: 'B'.	person2 partners: (Array with: person1).	req1 owner: person1.	req2 owner: person2.	^person1	! !
!StompMockPerson class categoriesFor: #example1!examples!public! !

StompMockRequest guid: (GUID fromString: '{8BF96652-95EF-484D-B24F-775F8C0E9AD8}')!
StompMockRequest comment: ''!
!StompMockRequest categoriesForClass!StompTest-Core! !
!StompMockRequest methodsFor!

id	^ id!

id: anObject	id := anObject!

owner	^ owner!

owner: anObject	owner := anObject! !
!StompMockRequest categoriesFor: #id!accessing!public! !
!StompMockRequest categoriesFor: #id:!accessing!public! !
!StompMockRequest categoriesFor: #owner!accessing!public! !
!StompMockRequest categoriesFor: #owner:!accessing!public! !

StompMockShapeChangedObject guid: (GUID fromString: '{D7D4633A-D905-4384-B0FE-5365ECFABD68}')!
StompMockShapeChangedObject comment: ''!
!StompMockShapeChangedObject categoriesForClass!StompTest-Core! !
!StompMockShapeChangedObject methodsFor!

addedAtt1	"Answer the value of addedAtt1"	^ addedAtt1!

addedAtt1: anObject	"Set the value of addedAtt1"	addedAtt1 := anObject!

originalAtt1	"Answer the value of originalAtt1"	^ originalAtt1!

originalAtt1: anObject	"Set the value of originalAtt1"	originalAtt1 := anObject!

renamedAtt1	"Answer the value of renamedAtt1"	^ renamedAtt1!

renamedAtt1: anObject	"Set the value of renamedAtt1"	renamedAtt1 := anObject!

stompInitialize	addedAtt1 := 2!

stompInstVarAt: instVarIndex named: varName put: value 	varName = 'oldNamedVar1' ifTrue: [^self renamedAtt1: value].	super stompInstVarAt: instVarIndex named: varName put: value ! !
!StompMockShapeChangedObject categoriesFor: #addedAtt1!accessing!public! !
!StompMockShapeChangedObject categoriesFor: #addedAtt1:!accessing!public! !
!StompMockShapeChangedObject categoriesFor: #originalAtt1!accessing!public! !
!StompMockShapeChangedObject categoriesFor: #originalAtt1:!accessing!public! !
!StompMockShapeChangedObject categoriesFor: #renamedAtt1!accessing!public! !
!StompMockShapeChangedObject categoriesFor: #renamedAtt1:!accessing!public! !
!StompMockShapeChangedObject categoriesFor: #stompInitialize!public!reading/stomp! !
!StompMockShapeChangedObject categoriesFor: #stompInstVarAt:named:put:!public!reading/stomp! !

StompPortableFixtures guid: (GUID fromString: '{066C17D7-DD36-4A13-AB71-B681FB6F7BE8}')!
StompPortableFixtures comment: ''!
!StompPortableFixtures categoriesForClass!StompTest-Core! !
!StompPortableFixtures class methodsFor!

blueColor	self subclassResponsibility !

double1234567890dot123456789	"^ 1234567890.123456789d"	self subclassResponsibility!

double3dot3	"^ 3.3d"	self subclassResponsibility!

float1dot2bytes	^ #[202 63 153 153 154]!

timestamp1	self subclassResponsibility!

yellowColor	self subclassResponsibility ! !
!StompPortableFixtures class categoriesFor: #blueColor!fixtures!public! !
!StompPortableFixtures class categoriesFor: #double1234567890dot123456789!fixtures!public! !
!StompPortableFixtures class categoriesFor: #double3dot3!fixtures!public! !
!StompPortableFixtures class categoriesFor: #float1dot2bytes!fixtures!public! !
!StompPortableFixtures class categoriesFor: #timestamp1!fixtures!public! !
!StompPortableFixtures class categoriesFor: #yellowColor!fixtures!public! !

StompMockMementoArray guid: (GUID fromString: '{38914F3E-2919-4BEE-AB75-BBDD72F32925}')!
StompMockMementoArray comment: ''!
!StompMockMementoArray categoriesForClass!StompTest-Core! !
!StompMockMementoArray methodsFor!

stompReadValue	| inst |	inst := StompMockCustomWriteObject new.	inst name: (self at: 1).	inst email: (self at: 2).	^inst! !
!StompMockMementoArray categoriesFor: #stompReadValue!public!stomp/objectstream/reading! !

StompMockMixedClass guid: (GUID fromString: '{8A9FF58D-BCC3-43DC-94F5-9E8D0E7329FD}')!
StompMockMixedClass comment: ''!
!StompMockMixedClass categoriesForClass!StompTest-Core! !
!StompMockMixedClass methodsFor!

equals: other	self class = other class ifFalse: [^false].	instVar1 = other instVar1 ifFalse: [^false].	instVar2 = other instVar2 ifFalse: [^false].	^super = other!

instVar1	"Answer the value of instVar1"	^ instVar1!

instVar1: anObject	"Set the value of instVar1"	instVar1 := anObject!

instVar2	"Answer the value of instVar2"	^ instVar2!

instVar2: anObject	"Set the value of instVar2"	instVar2 := anObject!

stompShouldWriteInstanceVariables	^true	! !
!StompMockMixedClass categoriesFor: #equals:!comparing!public! !
!StompMockMixedClass categoriesFor: #instVar1!accessing!public! !
!StompMockMixedClass categoriesFor: #instVar1:!accessing!public! !
!StompMockMixedClass categoriesFor: #instVar2!accessing!public! !
!StompMockMixedClass categoriesFor: #instVar2:!accessing!public! !
!StompMockMixedClass categoriesFor: #stompShouldWriteInstanceVariables!public!testing! !

!StompMockMixedClass class methodsFor!

new	^super new! !
!StompMockMixedClass class categoriesFor: #new!instance creation!public! !

StompMockVariableClass guid: (GUID fromString: '{C4AA1C71-1AA8-44FE-95E4-B47F4BAF1E20}')!
StompMockVariableClass comment: ''!
!StompMockVariableClass categoriesForClass!StompTest-Core! !
StompMockVariableNewFailedClass guid: (GUID fromString: '{F433ADCE-55A2-465C-9D09-4AA8D610165E}')!
StompMockVariableNewFailedClass comment: ''!
!StompMockVariableNewFailedClass categoriesForClass!StompTest-Core! !
!StompMockVariableNewFailedClass methodsFor!

att1	^att1!

att1: aValue	att1 := aValue!

att2	^att2!

att2: aValue	att2 := aValue!

att3Block	^att3Block!

att3Block: aValue	att3Block := aValue!

stompInitialize	att3Block := [:a :b | a < b].!

stompShouldWriteInstanceVariables 	^ true! !
!StompMockVariableNewFailedClass categoriesFor: #att1!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att1:!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att2!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att2:!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att3Block!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att3Block:!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #stompInitialize!initializing/stomp!public! !
!StompMockVariableNewFailedClass categoriesFor: #stompShouldWriteInstanceVariables!public!testing/stomp! !

!StompMockVariableNewFailedClass class methodsFor!

new: size	Error new signal: '#new: should not be called'!

prototype1	"self prototype1"	| inst |	inst := self basicNew: 3.	1 to: 3 do: [:idx | inst at: idx put: idx printString].	inst att1: 1.	inst att2: 'TWO'.	inst att3Block: [:this | 'is block'].	^inst! !
!StompMockVariableNewFailedClass class categoriesFor: #new:!instance creation!public! !
!StompMockVariableNewFailedClass class categoriesFor: #prototype1!for tests!public! !

StompMockVariableAlternativeClass guid: (GUID fromString: '{65A2DE07-6685-4AEC-ACD2-33C893EBBAC7}')!
StompMockVariableAlternativeClass comment: ''!
!StompMockVariableAlternativeClass categoriesForClass!StompTest-Core! !
!StompMockVariableAlternativeClass class methodsFor!

new: size	"new: is OK for me"	^self basicNew: size! !
!StompMockVariableAlternativeClass class categoriesFor: #new:!instance creation!public! !

StompMockByteArray guid: (GUID fromString: '{BC3E8687-FB4B-4E8D-A8D9-F90D11E265EF}')!
StompMockByteArray comment: ''!
!StompMockByteArray categoriesForClass!StompTest-Core! !
StompMockObjectInitializingOnRead guid: (GUID fromString: '{5E5297D7-E330-484F-8798-BD021A25011D}')!
StompMockObjectInitializingOnRead comment: ''!
!StompMockObjectInitializingOnRead categoriesForClass!StompTest-Core! !
!StompMockObjectInitializingOnRead methodsFor!

stompInitialize	instVar1 := 'stomp initialized 1'.	instVar2 := 'stomp initialized 2'! !
!StompMockObjectInitializingOnRead categoriesFor: #stompInitialize!initialize/release!public! !

StompMockObjectPreparingOnWrite guid: (GUID fromString: '{DDFD35DA-9614-4996-A86D-5827A8D6114F}')!
StompMockObjectPreparingOnWrite comment: ''!
!StompMockObjectPreparingOnWrite categoriesForClass!StompTest-Core! !
!StompMockObjectPreparingOnWrite methodsFor!

stompPrepareWrite 	self instVar1: self instVar1 printString.	self instVar2: self instVar2 * 2.! !
!StompMockObjectPreparingOnWrite categoriesFor: #stompPrepareWrite!public!stomp/objectstream/writing! !

StompMockObjectWithCache guid: (GUID fromString: '{6ABF0FEB-11F5-4302-A8AC-3C58A94485FA}')!
StompMockObjectWithCache comment: ''!
!StompMockObjectWithCache categoriesForClass!StompTest-Core! !
!StompMockObjectWithCache methodsFor!

stompTransientInstVarNames	^#(instVar1 instVar2 instVar3)! !
!StompMockObjectWithCache categoriesFor: #stompTransientInstVarNames!public!stomp/objectstream/writing! !

!StompMockObjectWithCache class methodsFor!

att1	"Answer the value of att1"	^ att1!

att1: anObject	"Set the value of att1"	att1 := anObject!

att2	"Answer the value of att2"	^ att2!

att2: anObject	"Set the value of att2"	att2 := anObject!

cache1	"Answer the value of cache1"	^ cache1!

cache1: anObject	"Set the value of cache1"	cache1 := anObject!

stompTransientInstVarNames	^super stompTransientInstVarNames, #(cache1)! !
!StompMockObjectWithCache class categoriesFor: #att1!accessing!public! !
!StompMockObjectWithCache class categoriesFor: #att1:!accessing!public! !
!StompMockObjectWithCache class categoriesFor: #att2!accessing!public! !
!StompMockObjectWithCache class categoriesFor: #att2:!accessing!public! !
!StompMockObjectWithCache class categoriesFor: #cache1!accessing!public! !
!StompMockObjectWithCache class categoriesFor: #cache1:!accessing!public! !
!StompMockObjectWithCache class categoriesFor: #stompTransientInstVarNames!public!stomp/objectstream/writing! !

StompMockFixedAlternativeClass guid: (GUID fromString: '{1CE0CCC7-E84B-48B1-ABDF-900B465239AE}')!
StompMockFixedAlternativeClass comment: ''!
!StompMockFixedAlternativeClass categoriesForClass!StompTest-Core! !
!StompMockFixedAlternativeClass class methodsFor!

new	"new is OK for me"	^self basicNew! !
!StompMockFixedAlternativeClass class categoriesFor: #new!instance creation!public! !

StompMockPersonShapeChanger guid: (GUID fromString: '{254B3A3D-B672-46FA-A4C6-06FAD72FACEA}')!
StompMockPersonShapeChanger comment: ''!
!StompMockPersonShapeChanger categoriesForClass!StompTest-Core! !
!StompMockPersonShapeChanger methodsFor!

loadInstVarAt: varIndex named: varName put: varValue 	varName = 'fullName' ifTrue: [^ self targetInstance name: varValue ].			^ super loadInstVarAt: varIndex named: varName put: varValue ! !
!StompMockPersonShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !

StompMockShapeChanger guid: (GUID fromString: '{AEEF7297-2E01-4C8B-9A15-4C31E3F7AD24}')!
StompMockShapeChanger comment: ''!
!StompMockShapeChanger categoriesForClass!StompTest-Core! !
!StompMockShapeChanger methodsFor!

loadAdditions	self targetInstance instVar5: 'FIVE'.!

loadInstVarAt: varIndex named: varName put: varValue 	"renamed"	varName = 'instVar2OLD' ifTrue: [^ self targetInstance instVar2: varValue ].		"removed"	"varName = 'instVar6' ifTrue: [^ self]."		^ super loadInstVarAt: varIndex named: varName put: varValue ! !
!StompMockShapeChanger categoriesFor: #loadAdditions!actions!public! !
!StompMockShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !

StompMockShapeChangerForRenamedComplexRead guid: (GUID fromString: '{A5EA3549-C7CE-4F08-925C-14CD69912248}')!
StompMockShapeChangerForRenamedComplexRead comment: ''!
!StompMockShapeChangerForRenamedComplexRead categoriesForClass!StompTest-Core! !
!StompMockShapeChangerForRenamedComplexRead methodsFor!

loadAdditions	self targetInstance addedAtt1: 22222!

loadInstVarAt: varIndex named: varName put: varValue 	"renamed"	varName = 'oldNamedVar1' ifTrue: [^ self targetInstance renamedAtt1: varValue ].			^ super loadInstVarAt: varIndex named: varName put: varValue ! !
!StompMockShapeChangerForRenamedComplexRead categoriesFor: #loadAdditions!actions!public! !
!StompMockShapeChangerForRenamedComplexRead categoriesFor: #loadInstVarAt:named:put:!actions!public! !

StompCustomSerializationTestCase guid: (GUID fromString: '{A7E83BF7-3D36-43A5-B52B-C909F4B047AF}')!
StompCustomSerializationTestCase comment: ''!
!StompCustomSerializationTestCase categoriesForClass!StompTest-Core! !
!StompCustomSerializationTestCase methodsFor!

testReadStompWriteValueAndStompReadValue	"self debug: #testReadStompWriteValueAndStompReadValue"	| object1 object2 stomp1 docodedTuple className array stomp2 readArray inArray readObject1 readObject2 |	object1 := StompMockCustomWriteObject new.	object1 name: 'Masashi Umezawa'.	object1 email: 'mu@example.com'.		object2 := StompMockCustomWriteObject new.	object2 name: 'Shiho Umezawa'.	object2 email: 'su@example.com'.		stomp1 := object1 toStomp.	docodedTuple := MpDecoder decode: stomp1.	className := ((docodedTuple at: 2) at: StompConstants klassName) asString asSymbol.	self should: [className = #StompMockMementoArray].	array := Array with: object1 with: object2 with: (Array with: object1 with: object2 with: 3).	stomp2 := array toStomp.		readArray := Object fromStomp: stomp2.	inArray := readArray at: 3.	self should: [(readArray at: 1) = (inArray at: 1)].	self should: [(readArray at: 2) = (inArray at: 2)].	self shouldnt: [(readArray at: 3) = (Array with: object1 with: object2 with: 3)].		readObject1 := readArray at: 1.	self should: [readObject1 name = 'Masashi Umezawa'].		self should: [readObject1 email = 'mu@example.com'].	readObject2 := readArray at: 2.	self should: [readObject2 name = 'Shiho Umezawa'].		self should: [readObject2 email = 'su@example.com']!

testSameStompWriteValueAndStompReadValue	"self debug: #testSameStompWriteValueAndStompReadValue"	| obj1 array array2 |	obj1 := StompMockCustomWriteObject new name: 'AA'; email: 'aa@example.com'.	array := Array with: obj1 with: obj1.	array2 := Object fromStomp: array toStomp.	self should: [(array2 at: 1) = (array2 at: 2)].!

testStompInitialize	"self debug: #testStompInitialize"	| obj readObject|	obj := StompMockObjectInitializingOnRead new.	obj instVar1: 'cache1'.	obj instVar2: 'cache2'.	obj instVar3: 3.		obj instVar4: 4.	obj instVar5: 5.	readObject := StompReader decode: (StompWriter encode: obj).	self should: [readObject instVar1 = 'stomp initialized 1'].		self should: [readObject instVar2 = 'stomp initialized 2'].		self should: [readObject instVar3 = 3].		self should: [readObject instVar4 = 4].	self should: [readObject instVar5 = 5].		self should: [obj instVar1 = 'cache1'].		self should: [obj instVar2 = 'cache2'].		self should: [obj instVar3 = 3].		self should: [obj instVar4 = 4].	self should: [obj instVar5 = 5].!

testStompPrepareWrite	"self debug: #testStompPrepareWrite"	| obj readObject|	obj := StompMockObjectPreparingOnWrite new.	obj instVar1: 1.	obj instVar2: 2.	obj instVar3: 3.		obj instVar4: 4.	obj instVar5: 5.	readObject := StompReader decode: (StompWriter encode: obj).	self should: [readObject instVar1 = '1'].		self should: [readObject instVar2 = 4].		self should: [readObject instVar3 = 3].		self should: [readObject instVar4 = 4].	self should: [readObject instVar5 = 5].		self should: [obj instVar1 = '1'].		self should: [obj instVar2 = 4].		self should: [obj instVar3 = 3].		self should: [obj instVar4 = 4].	self should: [obj instVar5 = 5].	!

testStompTransientClassInstVarNames	"self debug: #testStompTransientClassInstVarNames"	| obj encodedBytes readObject|	obj := StompMockObjectWithCache.	obj att1: 1.	obj att2: 2.	obj cache1: 'cache1'.	encodedBytes := StompWriter encode: obj.		obj att1: nil.	obj att2: nil.	obj cache1: nil.	readObject := StompReader decode: encodedBytes.	self should: [readObject att1 = 1].		self should: [readObject att2 = 2].		self should: [readObject cache1 = nil].				!

testStompTransientInstVarNames	"self debug: #testStompTransientInstVarNames"	| obj readObject|	obj := StompMockObjectWithCache new.	obj instVar1: '1'.	obj instVar2: '2'.	obj instVar3: 3.		obj instVar4: 4.	obj instVar5: 5.	readObject := StompReader decode: (StompWriter encode: obj).	self should: [readObject instVar1 = nil].		self should: [readObject instVar2 = nil].		self should: [readObject instVar3 = nil].		self should: [readObject instVar4 = 4].	self should: [readObject instVar5 = 5].		self should: [obj instVar1 = '1'].		self should: [obj instVar2 = '2'].		self should: [obj instVar3 = 3].		self should: [obj instVar4 = 4].	self should: [obj instVar5 = 5].! !
!StompCustomSerializationTestCase categoriesFor: #testReadStompWriteValueAndStompReadValue!public!testing! !
!StompCustomSerializationTestCase categoriesFor: #testSameStompWriteValueAndStompReadValue!public!testing! !
!StompCustomSerializationTestCase categoriesFor: #testStompInitialize!public!testing! !
!StompCustomSerializationTestCase categoriesFor: #testStompPrepareWrite!public!testing! !
!StompCustomSerializationTestCase categoriesFor: #testStompTransientClassInstVarNames!public!testing! !
!StompCustomSerializationTestCase categoriesFor: #testStompTransientInstVarNames!public!testing! !

StompNewFailedTestCase guid: (GUID fromString: '{D9C02168-60EE-4DAF-A651-E9CB3538819A}')!
StompNewFailedTestCase comment: ''!
!StompNewFailedTestCase categoriesForClass!StompTest-Core! !
!StompNewFailedTestCase methodsFor!

testWriteReadNewFailedClass	"self debug: #testWriteReadNewFailedClass"	| original stomp read |	original := StompMockFixedNewFailedClass prototype1.	self should: [original att1 = 1].	self should: [original att2 =  'TWO']. 	self should: [original toStomp] raise: StompInvalidSerialization.	stomp := original toStomp.		self should: [Object fromStomp: stomp] raise: StompNewFailed.	read := Object fromStomp: stomp.	self should: [read att1 = 1].	self should: [read att2 =  'TWO'].	self should: [read att3Block notNil].				!

testWriteReadNewFailedClassReceiverAlternative	"self debug: #testWriteReadNewFailedClassReceiverAlternative"	| original stomp read |	original := StompMockFixedNewFailedClass prototype1.	self should: [original att1 = 1].	self should: [original att2 =  'TWO']. 	self should: [original toStomp] raise: StompInvalidSerialization.	stomp := original toStomp.		self should: [Object fromStomp: stomp] raise: StompNewFailed.	read := [Object fromStomp: stomp] on: StompNewFailed do: [:ex | ex resume: StompMockFixedAlternativeClass].	self should: [read class = StompMockFixedAlternativeClass].	self should: [read att1 = 1].	self should: [read att2 =  'TWO'].	self should: [read att3Block notNil].				!

testWriteReadNewVariableFailedClass	"self debug: #testWriteReadNewVariableFailedClass"	| original stomp read |	original := StompMockVariableNewFailedClass prototype1.	self should: [original att1 = 1].	self should: [original att2 =  'TWO']. 	self should: [original toStomp] raise: StompInvalidSerialization.	stomp := original toStomp.		self should: [Object fromStomp: stomp] raise: StompNewFailed.	read := Object fromStomp: stomp.	self should: [read att1 = 1].	self should: [read att2 =  'TWO'].	self should: [read att3Block notNil].				!

testWriteReadNewVariableFailedClassReciverAlternative	"self debug: #testWriteReadNewVariableFailedClassReciverAlternative"	| original stomp read |	original := StompMockVariableNewFailedClass prototype1.	self should: [original att1 = 1].	self should: [original att2 =  'TWO']. 	self should: [original toStomp] raise: StompInvalidSerialization.	stomp := original toStomp.		self should: [Object fromStomp: stomp] raise: StompNewFailed.	read := [Object fromStomp: stomp] on: StompNewFailed do: [:ex | ex resume: StompMockVariableAlternativeClass].	self should: [read class = StompMockVariableAlternativeClass].	self should: [read att1 = 1].	self should: [read att2 =  'TWO'].	self should: [read att3Block notNil].				! !
!StompNewFailedTestCase categoriesFor: #testWriteReadNewFailedClass!public!tests! !
!StompNewFailedTestCase categoriesFor: #testWriteReadNewFailedClassReceiverAlternative!public!tests! !
!StompNewFailedTestCase categoriesFor: #testWriteReadNewVariableFailedClass!public!tests! !
!StompNewFailedTestCase categoriesFor: #testWriteReadNewVariableFailedClassReciverAlternative!public!tests! !

StompTestCase guid: (GUID fromString: '{66AFF64B-6A90-487B-91C1-7EF35435DE66}')!
StompTestCase comment: ''!
!StompTestCase categoriesForClass!StompTest-Core! !
!StompTestCase methodsFor!

bagAnswerBytes	^#[147 161 2 255 148 30 30 20 10]!

basicArrayAnswerBytes	^#[148 1 2 3 147 146 161 4 165 72 101 108 108 111 2 3]!

basicBitsClassAnswerBytes	^#[147 161 2 129 17 218 0 18 83 116 111 109 112 77 111 99 107 66 121 116 101 65 114 114 97 121 163 0 127 255]!

basicFixedClassAnswerBytes	^#[147 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 129 149 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 168 105 110 115 116 86 97 114 51 168 105 110 115 116 86 97 114 52 168 105 110 115 116 86 97 114 53 149 1 146 161 5 163 116 119 111 203 64 10 102 102 102 102 102 102 194 146 161 4 164 70 73 86 69]!

basicMixedClassAnswerBytes	^#[149 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 77 105 120 101 100 67 108 97 115 115 3 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 146 195 194 146 192 144 147 1 146 161 4 163 116 119 111 203 64 10 102 102 102 102 102 102]!

basicVariableClassAnswerBytes	^#[147 161 2 129 17 218 0 22 83 116 111 109 112 77 111 99 107 86 97 114 105 97 98 108 101 67 108 97 115 115 147 1 146 161 4 163 116 119 111 203 64 10 102 102 102 102 102 102]!

circularRefrencesAnswerBytes	^#[148 147 161 2 129 17 218 0 22 83 116 111 109 112 77 111 99 107 86 97 114 105 97 98 108 101 67 108 97 115 115 147 1 146 161 4 163 116 119 111 146 161 3 1 149 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 77 105 120 101 100 67 108 97 115 115 3 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 146 161 3 1 146 161 3 2 147 1 146 161 4 163 116 119 111 203 64 10 102 102 102 102 102 102 146 161 3 1 146 161 3 2]!

classAnswerBytes	^#[147 161 2 129 17 218 0 25 83 116 111 109 112 77 111 99 107 79 98 106 101 99 116 87 105 116 104 67 97 99 104 101 36 129 146 164 97 116 116 49 164 97 116 116 50 146 11 22]!

classNameAndIdsAnswerBytes	^#[148 147 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 129 149 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 168 105 110 115 116 86 97 114 51 168 105 110 115 116 86 97 114 52 168 105 110 115 116 86 97 114 53 149 1 146 161 5 163 111 110 101 192 192 192 147 161 2 0 150 0 2 146 161 5 163 116 119 111 192 192 192 149 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 77 105 120 101 100 67 108 97 115 115 0 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 3 146 161 5 165 116 104 114 101 101 144 149 161 2 1 0 147 1 4 146 161 5 164 102 111 117 114 144]!

classNameAndIdsSuppressNilWritesAnswerBytes	^#[148 147 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 1 146 161 5 163 111 110 101 147 161 2 0 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 2 146 161 5 163 116 119 111 149 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 77 105 120 101 100 67 108 97 115 115 0 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 3 146 161 5 165 116 104 114 101 101 144 149 161 2 1 0 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 4 146 161 5 164 102 111 117 114 144]!

colorAnswerBytes1	^#[147 161 2 252 147 203 0 0 0 0 0 0 0 0 203 0 0 0 0 0 0 0 0 203 63 240 0 0 0 0 0 0]!

colorAnswerBytes2	^#[147 161 2 252 147 203 63 240 0 0 0 0 0 0 203 63 240 0 0 0 0 0 0 203 0 0 0 0 0 0 0 0]!

compositeDictionaryAnswerBytes	^#[132 146 161 5 167 112 97 114 101 110 116 52 129 146 161 5 166 99 104 105 108 100 49 146 161 4 165 97 98 99 100 101 146 161 5 167 112 97 114 101 110 116 50 100 146 161 5 167 112 97 114 101 110 116 51 147 10 20 30 146 161 5 167 112 97 114 101 110 116 49 146 161 4 174 115 97 109 112 108 101 32 115 116 114 105 110 103 46]!

dateAnswerBytes	^#[147 161 2 250 206 190 169 158 128]!

dictionaryAnswerBytes	^#[131 146 161 5 164 107 101 121 51 146 161 4 166 118 97 108 117 101 51 146 161 5 164 107 101 121 49 146 161 4 166 118 97 108 117 101 49 146 161 5 164 107 101 121 50 146 161 4 166 118 97 108 117 101 50]!

durationAnswerBytes	^#[147 161 2 239 207 0 0 0 28 163 95 14 0]!

fixedClassWithNilValuesAnswerBytes	^#[147 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 129 147 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 51 168 105 110 115 116 86 97 114 53 147 1 203 64 10 102 102 102 102 102 102 146 161 4 164 70 73 86 69]!

floatingPointAnswerBytes1	^ StompPortableUtil default testFixturesClass float1dot2bytes!

floatingPointAnswerBytes2	"double"	^#[203 65 210 101 128 180 135 230 183]!

fractionAnswerBytes	^#[147 161 2 249 146 3 4]!

intervalAnswerBytes	^#[147 161 2 246 147 1 10 3]!

mixedClassNoVariableDataAnswerBytes	^#[149 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 77 105 120 101 100 67 108 97 115 115 0 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 146 195 194 146 192 144 144]!

ordredCollectionAnswerBytes	^#[147 161 2 245 147 10 20 30]!

rectangleAnswerBytes	^#[147 161 2 243 148 1 1 3 4]!

recursiveArrayAnswerBytes	^#[147 10 20 146 161 3 0]!

runArrayAnswerBytes	^#[147 161 2 236 146 147 1 2 1 147 1 2 3]!

setAnswerBytes	^ #[147 161 2 242 147 10 20 30]!

sharedReferenceAnswerBytes	^#[148 147 161 2 129 17 218 0 22 83 116 111 109 112 77 111 99 107 86 97 114 105 97 98 108 101 67 108 97 115 115 147 1 146 161 4 163 116 119 111 203 64 10 102 102 102 102 102 102 149 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 77 105 120 101 100 67 108 97 115 115 3 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 146 195 194 146 192 144 147 1 146 161 4 163 116 119 111 203 64 10 102 102 102 102 102 102 146 161 3 1 146 161 3 2]!

stringAnswerBytes	^#[146 161 4 218 0 21 72 105 44 32 73 39 109 32 83 116 114 105 110 103 45 111 98 106 101 99 116]!

symbolAnswerBytes	^#[146 161 5 167 97 98 99 100 101 102 103]!

timeAnswerBytes	^#[147 161 2 238 206 0 1 72 104]!

timestampAnswerBytes	^#[147 161 2 237 207 46 57 36 122 10 159 20 0]! !
!StompTestCase categoriesFor: #bagAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #basicArrayAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #basicBitsClassAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #basicFixedClassAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #basicMixedClassAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #basicVariableClassAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #circularRefrencesAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #classAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #classNameAndIdsAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #classNameAndIdsSuppressNilWritesAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #colorAnswerBytes1!fixtures!public! !
!StompTestCase categoriesFor: #colorAnswerBytes2!fixtures!public! !
!StompTestCase categoriesFor: #compositeDictionaryAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #dateAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #dictionaryAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #durationAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #fixedClassWithNilValuesAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #floatingPointAnswerBytes1!fixtures!public! !
!StompTestCase categoriesFor: #floatingPointAnswerBytes2!fixtures!public! !
!StompTestCase categoriesFor: #fractionAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #intervalAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #mixedClassNoVariableDataAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #ordredCollectionAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #rectangleAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #recursiveArrayAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #runArrayAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #setAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #sharedReferenceAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #stringAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #symbolAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #timeAnswerBytes!fixtures!public! !
!StompTestCase categoriesFor: #timestampAnswerBytes!fixtures!public! !

StompReaderTestCase guid: (GUID fromString: '{2AAC787A-CC1D-452E-B612-01B2D84EAFFD}')!
StompReaderTestCase comment: ''!
!StompReaderTestCase categoriesForClass!StompTest-Core! !
!StompReaderTestCase methodsFor!

collectionEquals: aCollection with: otherCollection	^MpPortableUtil default collectionEquals: aCollection with: otherCollection!

testReadBag	"self debug: #testReadBag"		| bytes inst |	bytes := self bagAnswerBytes.	inst := Object fromStomp: bytes.		self should: [inst class = Bag].	self		should: [inst size = 4].	self		should: [inst includes: 10].	self		should: [inst includes: 20].	self		should: [inst includes: 30].	self		should: [(inst occurrencesOf: 30) = 2]!

testReadBasicArray	"self debug: #testReadBasicArray"	| bytes obj |	bytes := self basicArrayAnswerBytes.	obj := (StompReader onBytes: bytes) next.		self should: [obj class = Array].	self should: [obj = #(1 2 3 #('Hello' 2 3))]!

testReadBasicBitsClass	"self debug: #testReadBasicBitsClass"	| bytes obj expected |	bytes := self basicBitsClassAnswerBytes.	obj := (StompReader onBytes: bytes) next.		expected := StompMockByteArray new: 3.	expected at: 1 put: 0.	expected at: 2 put: 127.	expected at: 3 put: 255.	self should: [obj = expected].			!

testReadBasicFixedClass	"self debug: #testReadBasicFixedClass"	| bytes obj expected |	bytes := self basicFixedClassAnswerBytes.	obj := (StompReader onBytes: bytes) next.		expected := StompMockFixedClass new.	expected instVar1: 1.	expected instVar2: #two.	expected instVar3: (StompPortableUtil default testFixturesClass double3dot3).	expected instVar4: false.	expected instVar5: 'FIVE'.		self should: [obj equals: expected].				!

testReadBasicMixedClass	"self debug: #testReadBasicMixedClass"	| bytes obj expected |	bytes := self basicMixedClassAnswerBytes.	obj := (StompReader onBytes: bytes) next.		expected := StompMockMixedClass new: 3.	expected at: 1 put: 1.	expected at: 2 put: 'two'.	expected at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).	expected instVar1: #(true false).	expected instVar2: #(nil #()).		self should: [obj equals: expected]				!

testReadBasicVariableClass	"self debug: #testReadBasicVariableClass"	| bytes obj expected |	bytes := self basicVariableClassAnswerBytes.	obj := (StompReader onBytes: bytes) next.		expected := StompMockVariableClass new: 3.	expected at: 1 put: 1.	expected at: 2 put: 'two'.	expected at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).		self should: [obj = expected]				!

testReadBoolean	"self debug: #testReadBoolean"		self should: [(Object fromStomp: (#[195])) = true].	self should: [(Object fromStomp: (#[194])) = false].!

testReadByteArray	"self debug: #testReadByteArray"		| byteArray |	byteArray := (ByteArray with: 10 with: 20 with: 30).		self should: [(Object fromStomp: (#[163 10 20 30])) = byteArray].	!

testReadCharacter	"self debug: #testReadCharacter"		| char1 char2 |	char1 := $A.		self should: [(Object fromStomp:(#[147 161 2 253 65])) = char1].

	(#DolphinSpecifix = #DolphinSpecifix) ifTrue: [^self]."Dolphin Specific - Hope unicoce support"
	char2 := (StompPortableUtil default characterFromUnicode: 12354). "Japanese Hiragana 'A' "		self should: [(Object fromStomp:(#[147 161 2 253 205 48 66])) = char2].		!

testReadCircularRefrences1	"self debug: #testReadCircularRefrences1"	| bytes reader obj inst1 inst2 expected |	bytes := self circularRefrencesAnswerBytes.	reader := StompReader onBytes: bytes.	obj := reader next.		inst1 := StompMockVariableClass new: 3.	inst1 at: 1 put: 1.	inst1 at: 2 put: 'two'.	inst1 at: 3 put: inst1.		inst2 := StompMockMixedClass new: 3.	inst2 at: 1 put: 1.	inst2 at: 2 put: 'two'.	inst2 at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).	inst2 instVar1: inst1.	inst2 instVar2: inst2.		expected := Array with: inst1 with: inst2 with: inst1 with: inst2.		self should: [obj class = expected class].	self should: [obj size = expected size]. 		self should: [(obj at: 1) class = StompMockVariableClass].	self should: [(obj at: 1) class = (expected at: 1) class].	self should: [((obj at: 1) at: 1) = 1].	self should: [((obj at: 1) at: 1) = ((expected at: 1) at: 1)].	self should: [((obj at: 1) at: 2) = 'two'].	self should: [((obj at: 1) at: 2) = ((expected at: 1) at: 2)].	self should: [((obj at: 1) at: 3) class = StompMockVariableClass].	self should: [((obj at: 1) at: 3) class = ((expected at: 1) at: 3) class].	self should: [(obj at: 1) identityHash = ((obj at: 1) at: 3) identityHash].		self should: [(obj at: 2) class = StompMockMixedClass].	self should: [(obj at: 2) class = (expected at: 2) class].	self should: [((obj at: 2) at: 1) = 1].	self should: [((obj at: 2) at: 1) = ((expected at: 2) at: 1)].	self should: [((obj at: 2) at: 2) = 'two'].	self should: [((obj at: 2) at: 2) = ((expected at: 2) at: 2)].	self should: [((obj at: 2) at: 3) = (StompPortableUtil default testFixturesClass double3dot3)].	self should: [((obj at: 2) at: 3) = ((expected at: 2) at: 3)].	self should: [((obj at: 2) instVar1) class = StompMockVariableClass].	self should: [((obj at: 2) instVar1) class = ((expected at: 2) instVar1) class].	self should: [(obj at: 2) instVar1 identityHash = (obj at: 1) identityHash].	self should: [((obj at: 2) instVar2) class = StompMockMixedClass].	self should: [((obj at: 2) instVar2) class = ((expected at: 2) instVar2) class].	self should: [(obj at: 2) instVar2 identityHash = (obj at: 2) identityHash].		self should: [(obj at: 3) class = StompMockVariableClass].	self should: [(obj at: 3) class = (expected at: 3) class].	self should: [((obj at: 3) at: 1) = 1].	self should: [((obj at: 3) at: 1) = ((expected at: 3) at: 1)].	self should: [((obj at: 3) at: 2) = 'two'].	self should: [((obj at: 3) at: 2) = ((expected at: 3) at: 2)].	self should: [((obj at: 3) at: 3) class = StompMockVariableClass].	self should: [((obj at: 3) at: 3) class = ((expected at: 3) at: 3) class].	self should: [(obj at: 3) identityHash = ((obj at: 3) at: 3) identityHash].		self should: [(obj at: 4) class = StompMockMixedClass].	self should: [(obj at: 4) class = (expected at: 4) class].	self should: [((obj at: 4) at: 1) = 1].	self should: [((obj at: 4) at: 1) = ((expected at: 4) at: 1)].	self should: [((obj at: 4) at: 2) = 'two'].	self should: [((obj at: 4) at: 2) = ((expected at: 4) at: 2)].	self should: [((obj at: 4) at: 3) = (StompPortableUtil default testFixturesClass double3dot3)].	self should: [((obj at: 4) at: 3) = ((expected at: 4) at: 3)].	self should: [((obj at: 4) instVar1) class = StompMockVariableClass].	self should: [((obj at: 4) instVar1) class = ((expected at: 4) instVar1) class].	self should: [(obj at: 4) instVar1 identityHash = (obj at: 1) identityHash].	self should: [((obj at: 4) instVar2) class = StompMockMixedClass].	self should: [((obj at: 4) instVar2) class = ((expected at: 4) instVar2) class].	self should: [(obj at: 4) instVar2 identityHash = (obj at: 2) identityHash].				!

testReadClass	"self debug: #testReadClass"	| cls |	cls := StompMockObjectWithCache.	cls att1: 11.	cls att2: 22.	self should: [(Object fromStomp: (self classAnswerBytes)) = cls]		!

testReadClassNameAndIds	"self debug: #testReadClassNameAndIds"		| bytes obj data1 data2 data3 data4 expected |	bytes := self classNameAndIdsAnswerBytes.	obj := (StompReader onBytes: bytes) next.		data1 := StompMockFixedClass new.	data1 instVar1: 1.	data1 instVar2: #one.		data2 := StompMockFixedClass new.	data2 instVar1: 2.	data2 instVar2: #two.		data3 := StompMockMixedClass new.	data3 instVar1: 3.	data3 instVar2: #three.		data4 := StompMockMixedClass new.	data4 instVar1: 4.	data4 instVar2: #four.		expected := Array with: data1 with: data2 with: data3 with: data4.		self should: [(obj at: 1) equals: (expected at: 1)].	self should: [(obj at: 2) equals: (expected at: 2)].	self should: [(obj at: 3) equals: (expected at: 3)].	self should: [(obj at: 4) equals: (expected at: 4)]				!

testReadClassNameAndIdsSuppressNilWrites	"self debug: #testReadClassNameAndIdsSuppressNilWrites"		| bytes obj data1 data2 data3 data4 expected |	bytes := self classNameAndIdsSuppressNilWritesAnswerBytes.	obj := (StompReader onBytes: bytes) next.		data1 := StompMockFixedClass new.	data1 instVar1: 1.	data1 instVar2: #one.		data2 := StompMockFixedClass new.	data2 instVar1: 2.	data2 instVar2: #two.		data3 := StompMockMixedClass new.	data3 instVar1: 3.	data3 instVar2: #three.		data4 := StompMockMixedClass new.	data4 instVar1: 4.	data4 instVar2: #four.		expected := Array with: data1 with: data2 with: data3 with: data4.		self should: [(obj at: 1) equals: (expected at: 1)].	self should: [(obj at: 2) equals: (expected at: 2)].	self should: [(obj at: 3) equals: (expected at: 3)].	self should: [(obj at: 4) equals: (expected at: 4)]				!

testReadColor	"self debug: #testReadColor"	| color1 color2 |	color1 := StompPortableUtil default testFixturesClass blueColor.	self should: [(Object fromStomp:(self colorAnswerBytes1)) = color1].		color2 := StompPortableUtil default testFixturesClass yellowColor.	self should: [(Object fromStomp:(self colorAnswerBytes2)) = color2]		!

testReadCompositeDictionary	"self debug: #testReadCompositeDictionary"	| childDic dic readDic |	childDic := Dictionary new.	childDic at: #child1 put: 'abcde'.	dic := Dictionary new.	dic at: #parent1 put: 'sample string.'.	dic at: #parent2 put: 100.	dic at: #parent3 put: #(10 20 30 ).	dic at: #parent4 put: childDic.		readDic := Object fromStomp: (self compositeDictionaryAnswerBytes).		self should: [readDic size = dic size].	self should: [(readDic at: #parent1) = (dic at: #parent1)].	self should: [(readDic at: #parent2) = (dic at: #parent2)].	self should: [self collectionEquals: (readDic at: #parent3) with: (dic at: #parent3)].	self should: [self collectionEquals: (readDic at: #parent4) associations with: (dic at: #parent4) associations].!

testReadDate	"self debug: #testReadDate"		| bytes inst |	bytes := self dateAnswerBytes.	inst := Object fromStomp: bytes.		self should: [inst class = Date].	self should: [inst = (Date fromDays: 37023)].	!

testReadDictionary	"self debug: #testReadDictionary"	| dic |	dic := Dictionary new.	dic at: #key1 put: 'value1'.	dic at: #key2 put: 'value2'.	dic at: #key3 put: 'value3'.		self should: [self collectionEquals: (Object fromStomp: (self dictionaryAnswerBytes)) with: dic]!

testReadDuration	"self debug: #testReadDuration"		| duration inst |
	(StompPortableUtil default classNamed: #Duration) ifNil: [^self].
	duration := self durationAnswerBytes.	inst := Object fromStomp: duration.		self should: [inst class = (StompPortableUtil default classNamed: #Duration)].	self should: [inst = 123 seconds].	!

testReadFixedClassWithNilValues	"self debug: #testReadFixedClassWithNilValues"	| data |	data := StompMockFixedClass new.	data instVar1: 1.	data instVar2: nil.	data instVar3: (StompPortableUtil default testFixturesClass double3dot3).	data instVar4: nil.	data instVar5: 'FIVE'.	self		should: [data				equals: (Object						fromStomp: self fixedClassWithNilValuesAnswerBytes						setting: [:ctx | ctx settings suppressNilWrite: true])]!

testReadFraction	"self debug: #testReadFraction"	| fra |	fra := 3 / 4.		self should: [(Object fromStomp: (self fractionAnswerBytes)) = fra]!

testReadInterval	"self debug: #testReadInterval"	| interval |	interval := 1 to: 10 by: 3.		self should: [(Object fromStomp: (self intervalAnswerBytes)) = interval]	!

testReadMixedClassNoVariableData	"self debug: #testReadMixedClassNoVariableData"	| data |	data := StompMockMixedClass new.	data instVar1: #(true false).	data instVar2: #(nil #()).	self should: [data equals: (Object fromStomp:(self mixedClassNoVariableDataAnswerBytes))]	!

testReadNumber	"self debug: #testReadNumber"	self should: [(Object fromStomp:(#[1])) = 1].	self should: [(Object fromStomp:(self floatingPointAnswerBytes1)) = 1.2].	self should: [(Object fromStomp:(self floatingPointAnswerBytes2))  = StompPortableUtil default testFixturesClass double1234567890dot123456789]!

testReadOrdredCollection	"self debug: #testReadOrdredCollection"	| ord |	ord := OrderedCollection new.	ord add: 10.	ord add: 20.	ord add: 30.		self should: [(Object fromStomp:(self ordredCollectionAnswerBytes)) = ord]	!

testReadRectangle	"self debug: #testReadRectangle"	| rect |	rect := (1@1 corner: 3@4).		self should: [(Object fromStomp:(self rectangleAnswerBytes)) = rect]		!

testReadRecursiveArray	"self debug: #testReadRecursiveArray"	| arr readArray |	arr := Array new: 3.	arr at: 1 put: 10.	arr at: 2 put: 20.	arr at: 3 put: arr.		readArray := (Object fromStomp:(self recursiveArrayAnswerBytes)).		self should: [readArray size = arr size].	self should: [(readArray at: 1) = (arr at: 1)].	self should: [(readArray at: 2) = (arr at: 2)].	self should: [(readArray at: 3) identityHash= readArray identityHash].		!

testReadRunArray	"self debug: #testReadRunArray"	| arr |	arr := RunArray runs: #(1 2 1) values: #(1 2 3).	self should: [(Object fromStomp:(self runArrayAnswerBytes)) = arr]	!

testReadSet	"self debug: #testReadSet"	| set |	set := Set new.	set add: 10.	set add: 20.	set add: 30.	set add: 30.	
	self should: [self collectionEquals: (Object fromStomp:(self setAnswerBytes)) with: set]	!

testReadSharedRefrences1	"self debug: #testReadSharedRefrences1"	| bytes reader obj inst1 inst2 expected  |	bytes := self sharedReferenceAnswerBytes.	reader := StompReader onBytes: bytes.	obj := reader next.		inst1 := StompMockVariableClass new: 3.	inst1 at: 1 put: 1.	inst1 at: 2 put: 'two'.	inst1 at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).		inst2 := StompMockMixedClass new: 3.	inst2 at: 1 put: 1.	inst2 at: 2 put: 'two'.	inst2 at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).	inst2 instVar1: #(true false).	inst2 instVar2: #(nil #()).		expected := Array with: inst1 with: inst2 with: inst1 with: inst2.		self should: [obj = expected].			!

testReadString	"self debug: #testReadString"	| str1 |	str1 := 'Hi, I''m String-object'.	self should: [(Object fromStomp:(self stringAnswerBytes)) = str1].		!

testReadTime	"self debug: #testReadTime"		| bytes inst |	bytes := self timeAnswerBytes.	inst := Object fromStomp: bytes.		self should: [inst class = Time].	self should: [inst = (Time fromSeconds: 84072)].	!

testReadTimestamp	"self debug: #testReadTimestamp"		| timestamp |	timestamp := StompPortableUtil default testFixturesClass timestamp1.	self should: [(Object fromStomp: (self timestampAnswerBytes)) = timestamp]	!

testReadUndefinedObject	"self debug: #testReadUndefinedObject"	self should: [(Object fromStomp:(#[192])) = nil].		! !
!StompReaderTestCase categoriesFor: #collectionEquals:with:!private! !
!StompReaderTestCase categoriesFor: #testReadBag!public!tests! !
!StompReaderTestCase categoriesFor: #testReadBasicArray!public!tests! !
!StompReaderTestCase categoriesFor: #testReadBasicBitsClass!public!tests! !
!StompReaderTestCase categoriesFor: #testReadBasicFixedClass!public!tests! !
!StompReaderTestCase categoriesFor: #testReadBasicMixedClass!public!tests! !
!StompReaderTestCase categoriesFor: #testReadBasicVariableClass!public!tests! !
!StompReaderTestCase categoriesFor: #testReadBoolean!public!tests! !
!StompReaderTestCase categoriesFor: #testReadByteArray!public!tests! !
!StompReaderTestCase categoriesFor: #testReadCharacter!public!tests! !
!StompReaderTestCase categoriesFor: #testReadCircularRefrences1!public!tests! !
!StompReaderTestCase categoriesFor: #testReadClass!public!tests! !
!StompReaderTestCase categoriesFor: #testReadClassNameAndIds!public!tests! !
!StompReaderTestCase categoriesFor: #testReadClassNameAndIdsSuppressNilWrites!public!tests! !
!StompReaderTestCase categoriesFor: #testReadColor!public!tests! !
!StompReaderTestCase categoriesFor: #testReadCompositeDictionary!public!tests! !
!StompReaderTestCase categoriesFor: #testReadDate!public!tests! !
!StompReaderTestCase categoriesFor: #testReadDictionary!public!tests! !
!StompReaderTestCase categoriesFor: #testReadDuration!public!tests! !
!StompReaderTestCase categoriesFor: #testReadFixedClassWithNilValues!public!tests! !
!StompReaderTestCase categoriesFor: #testReadFraction!public!tests! !
!StompReaderTestCase categoriesFor: #testReadInterval!public!tests! !
!StompReaderTestCase categoriesFor: #testReadMixedClassNoVariableData!public!tests! !
!StompReaderTestCase categoriesFor: #testReadNumber!public!tests! !
!StompReaderTestCase categoriesFor: #testReadOrdredCollection!public!tests! !
!StompReaderTestCase categoriesFor: #testReadRectangle!public!tests! !
!StompReaderTestCase categoriesFor: #testReadRecursiveArray!public!tests! !
!StompReaderTestCase categoriesFor: #testReadRunArray!public!tests! !
!StompReaderTestCase categoriesFor: #testReadSet!public!tests! !
!StompReaderTestCase categoriesFor: #testReadSharedRefrences1!public!tests! !
!StompReaderTestCase categoriesFor: #testReadString!public!tests! !
!StompReaderTestCase categoriesFor: #testReadTime!public!tests! !
!StompReaderTestCase categoriesFor: #testReadTimestamp!public!tests! !
!StompReaderTestCase categoriesFor: #testReadUndefinedObject!public!tests! !

StompReadWriteTestCase guid: (GUID fromString: '{C8891D0C-14E8-4FA6-82CD-FFF6509F6E84}')!
StompReadWriteTestCase comment: ''!
!StompReadWriteTestCase categoriesForClass!StompTest-Core! !
!StompReadWriteTestCase methodsFor!

testReadWriteText	"self debug: #testReadWriteText"	| bytes orig text |	(StompPortableUtil default classNamed: #Text) ifNil: [^self].	orig := 'aaa' asText allBold.	bytes := orig toStomp.	text := Object fromStomp: bytes.		self should: [orig = text]! !
!StompReadWriteTestCase categoriesFor: #testReadWriteText!public!tests! !

StompShapeChangerTestCase guid: (GUID fromString: '{ED6BD3DC-8D0E-4184-BD81-F9432BD7BD44}')!
StompShapeChangerTestCase comment: ''!
!StompShapeChangerTestCase categoriesForClass!StompTest-Core! !
!StompShapeChangerTestCase methodsFor!

fixedClassOLDAnswerArray1	^  #[147 161 2 129 17 218 0 22 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 79 76 68 129 149 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 168 105 110 115 116 86 97 114 51 168 105 110 115 116 86 97 114 52 168 105 110 115 116 86 97 114 53 149 1 146 161 5 163 116 119 111 203 64 10 102 102 102 102 102 102 194 146 161 4 164 70 73 86 69]!

fixedClassOLDAnswerArray2	 ^ #[147 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 129 149 168 105 110 115 116 86 97 114 49 171 105 110 115 116 86 97 114 50 79 76 68 168 105 110 115 116 86 97 114 51 168 105 110 115 116 86 97 114 52 168 105 110 115 116 86 97 114 54 149 1 146 161 5 170 114 101 110 97 109 101 100 84 119 111 203 64 10 102 102 102 102 102 102 194 146 161 4 167 65 68 68 69 68 45 54]!

mockPersonAnswerArray1	 ^  #[147 161 2 129 17 218 0 18 83 116 111 109 112 77 111 99 107 80 101 114 115 111 110 79 76 68 129 147 168 102 117 108 108 78 97 109 101 168 114 101 113 117 101 115 116 115 168 112 97 114 116 110 101 114 115 147 146 161 4 161 65 146 147 161 2 129 17 218 0 16 83 116 111 109 112 77 111 99 107 82 101 113 117 101 115 116 129 146 162 105 100 165 111 119 110 101 114 146 1 146 161 3 0 147 161 2 1 147 1 2 147 161 2 0 148 0 146 161 4 161 66 192 145 146 161 3 0 192]!

renamedComplexReadAnswerArray1	 ^  #[147 147 161 2 129 17 218 0 30 83 116 111 109 112 77 111 99 107 83 104 97 112 101 67 104 97 110 103 101 100 79 98 106 101 99 116 79 76 68 129 146 172 111 108 100 78 97 109 101 100 86 97 114 49 172 111 114 105 103 105 110 97 108 65 116 116 49 146 1 3 147 161 2 0 147 0 11 33 146 161 3 1]!

renamedComplexReadAnswerArray2	 ^  #[148 147 161 2 129 17 218 0 30 83 116 111 109 112 77 111 99 107 83 104 97 112 101 67 104 97 110 103 101 100 79 98 106 101 99 116 79 76 68 129 146 172 111 108 100 78 97 109 101 100 86 97 114 49 172 111 114 105 103 105 110 97 108 65 116 116 49 146 1 3 147 161 2 0 147 0 11 33 146 161 3 1 146 161 4 164 104 101 114 101]!

renamedComplexReadAnswerArray3	 ^ #[147 147 161 2 129 17 218 0 30 83 116 111 109 112 77 111 99 107 83 104 97 112 101 67 104 97 110 103 101 100 79 98 106 101 99 116 79 76 68 129 146 172 111 108 100 78 97 109 101 100 86 97 114 49 172 111 114 105 103 105 110 97 108 65 116 116 49 146 1 3 147 161 2 0 147 0 11 33 146 161 3 1]!

renamedComplexReadAnswerArray4	 ^  #[148 147 161 2 129 17 218 0 30 83 116 111 109 112 77 111 99 107 83 104 97 112 101 67 104 97 110 103 101 100 79 98 106 101 99 116 79 76 68 129 147 172 111 108 100 78 97 109 101 100 86 97 114 49 172 111 114 105 103 105 110 97 108 65 116 116 49 167 111 108 100 86 97 114 50 147 1 3 192 147 161 2 0 148 0 11 33 44 146 161 3 1 146 161 4 164 104 101 114 101]!

renamedNewFailedAnswerArray1	 ^  #[147 147 161 2 129 17 218 0 30 83 116 111 109 112 77 111 99 107 70 105 120 101 100 65 108 116 101 114 110 97 116 105 118 101 67 108 97 115 115 129 147 164 97 116 116 49 164 97 116 116 50 169 97 116 116 51 66 108 111 99 107 147 1 2 192 147 161 2 0 148 0 3 4 192 146 161 3 1]!

renamedReadFailedAnswerArray1	 ^ #[147 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 129 149 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 168 105 110 115 116 86 97 114 51 168 105 110 115 116 86 97 114 52 168 105 110 115 116 86 97 114 53 149 146 161 4 165 116 101 115 116 49 147 161 2 129 17 218 0 23 83 116 111 109 112 77 111 99 107 70 105 120 101 100 67 108 97 115 115 50 79 76 68 129 149 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 168 105 110 115 116 86 97 114 51 168 105 110 115 116 86 97 114 52 168 105 110 115 116 86 97 114 53 149 146 161 4 165 116 101 115 116 50 192 192 192 192 146 161 4 165 116 101 115 116 51 149 161 2 129 17 218 0 19 83 116 111 109 112 77 111 99 107 77 105 120 101 100 67 108 97 115 115 2 129 146 168 105 110 115 116 86 97 114 49 168 105 110 115 116 86 97 114 50 146 192 192 146 146 161 4 165 116 101 115 116 52 147 161 2 1 150 1 146 161 4 165 116 101 115 116 53 192 192 192 192 192]!

testBlockRenamedComplexShapeChangerRead	"self debug: #testBlockRenamedComplexShapeChangerRead"	| bytes arr obj1 obj2 obj3 |	bytes := self renamedComplexReadAnswerArray3.	arr := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockShapeChangedObjectOLD for: StompMockShapeChangedObject.		ctx registerShapeChangerRenameBy: [:target :oldName :value |			oldName = 'oldNamedVar1' ifTrue: [target renamedAtt1: value].		] initializeBy: [:target |			target addedAtt1: 22222.		] for: StompMockShapeChangedObject.	].		self should: [arr class = Array].	self should: [arr size = 3].	obj1 := arr at: 1.	self should: [obj1 renamedAtt1 = 1].	self should: [obj1 addedAtt1 = 22222].	self should: [obj1 originalAtt1 = 3].	obj2 := arr at: 2.	self should: [obj2 renamedAtt1 = 11].	self should: [obj2 addedAtt1 = 22222].	self should: [obj2 originalAtt1 = 33].	obj3 := arr at: 3.	self should: [obj3 = obj1].!

testBlockShapeChangedRead	"self debug: #testBlockShapeChangedRead"	| bytes inst expected |	bytes := self fixedClassOLDAnswerArray2.	inst := Object fromStomp: bytes setting: [:ctx |		ctx registerShapeChangerRenameBy: [:target :oldName :value | 			oldName = 'instVar2OLD' ifTrue: [target instVar2: value].		] initializeBy: [:target | 			target instVar5: 'FIVE'		] for: StompMockFixedClass.	].		expected := StompMockFixedClass new.	expected instVar1: 1.	expected instVar2: #renamedTwo.	expected instVar3: (StompPortableUtil default testFixturesClass double3dot3).	expected instVar4: false.	expected instVar5: 'FIVE'.		self should: [inst equals: expected].!

testNewFailedRead	"self debug: #testNewFailedRead"	| bytes arr obj1 obj2 obj3 |	bytes := self renamedNewFailedAnswerArray1.	arr := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockFixedNewFailedClass for: StompMockFixedAlternativeClass.	].	self should: [arr class = Array].	self should: [arr size = 3].	obj1 := arr at: 1.	self should: [obj1 class = StompMockFixedAlternativeClass].	self should: [obj1 att1 = 1].	self should: [obj1 att2 = 2].	self should: [obj1 att3Block notNil].	obj2 := arr at: 2.	self should: [obj2 class = StompMockFixedAlternativeClass].	self should: [obj2 att1 = 3].	self should: [obj2 att2 = 4].	self should: [obj2 att3Block notNil].	obj3 := arr at: 3.	self should: [obj3 = obj1].!

testRenamedComplexRead	"self debug: #testRenamedComplexRead"	| bytes arr obj1 obj2 obj3 |	bytes := self renamedComplexReadAnswerArray1.	arr := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockShapeChangedObjectOLD for: StompMockShapeChangedObject.	].		self should: [arr class = Array].	self should: [arr size = 3].	obj1 := arr at: 1.	self should: [obj1 renamedAtt1 = 1].	self should: [obj1 addedAtt1 = 2].	self should: [obj1 originalAtt1 = 3].	obj2 := arr at: 2.	self should: [obj2 renamedAtt1 = 11].	self should: [obj2 addedAtt1 = 2].	self should: [obj2 originalAtt1 = 33].	obj3 := arr at: 3.	self should: [obj3 = obj1].!

testRenamedComplexReadFailed	"self debug: #testRenamedComplexReadFailed"	| bytes arr |	bytes := self renamedComplexReadAnswerArray2.	arr := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockShapeChangedObjectFAILED for: StompMockShapeChangedObject.	].		self should: [arr class = Array].	self should: [arr size = 4].	self should: [arr = #(nil nil nil 'here')].!

testRenamedComplexShapeChangerRead	"self debug: #testRenamedComplexShapeChangerRead"	| bytes arr obj1 obj2 obj3 |	bytes := self renamedComplexReadAnswerArray3.	arr := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockShapeChangedObjectOLD shapeChanger: StompMockShapeChangerForRenamedComplexRead for: StompMockShapeChangedObject.	].		self should: [arr class = Array].	self should: [arr size = 3].	obj1 := arr at: 1.	self should: [obj1 renamedAtt1 = 1].	self should: [obj1 addedAtt1 = 22222].	self should: [obj1 originalAtt1 = 3].	obj2 := arr at: 2.	self should: [obj2 renamedAtt1 = 11].	self should: [obj2 addedAtt1 = 22222].	self should: [obj2 originalAtt1 = 33].	obj3 := arr at: 3.	self should: [obj3 = obj1].!

testRenamedComplexShapeChangerReadFailed	"self debug: #testRenamedComplexShapeChangerReadFailed"	| bytes arr |	bytes := self renamedComplexReadAnswerArray4.	arr := Object fromStomp: bytes setting: [:ctx |		ctx registerShapeChanger: StompMockShapeChangerForRenamedComplexRead for: StompMockFixedClass.	].		self should: [arr class = Array].	self should: [arr size = 4].	self should: [arr = #(nil nil nil 'here')].!

testRenamedRead	"self debug: #testRenamedRead"	| bytes inst expected |	bytes := self fixedClassOLDAnswerArray1.	inst := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockFixedClassOLD for: StompMockFixedClass.	].		expected := StompMockFixedClass new.	expected instVar1: 1.	expected instVar2: #two.	expected instVar3: (StompPortableUtil default testFixturesClass double3dot3).	expected instVar4: false.	expected instVar5: 'FIVE'.		self should: [inst equals: expected].!

testRenamedReadFailed	"self debug: #testRenamedReadFailed"	| bytes inst |	bytes := self renamedReadFailedAnswerArray1.	"By default, unresolved class's instance will be nil"	inst := Object fromStomp: bytes.	self should: [inst class = StompMockFixedClass].	self should: [inst instVar1 = 'test1'].	self should: [inst instVar2 isNil].	self should: [inst instVar3 = 'test3'].	self should: [inst instVar4 class = StompMockMixedClass].	self should: [(inst instVar4 at: 1) = 'test4'].	self should: [(inst instVar4 at: 2) isNil].	self should: [inst instVar5 isNil].		"Override the behavior by context"	inst := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockFixedClass2OLD for: StompMockFixedClass2.	].	self should: [inst class = StompMockFixedClass].	self should: [inst instVar1 = 'test1'].	self should: [inst instVar2 class = StompMockFixedClass2].	self should: [inst instVar2 instVar1= 'test2'].	self should: [inst instVar3 = 'test3'].	self should: [inst instVar4 class = StompMockMixedClass].	self should: [(inst instVar4 at: 1) = 'test4'].	self should: [(inst instVar4 at: 2) class = StompMockFixedClass2].	self should: [(inst instVar4 at: 2) instVar1 = 'test5'].	self should: [inst instVar5 isNil].		"Or you can use exception handling"	inst := [Object fromStomp: bytes ] on: StompClassNotFound do: [:ex |			ex resume: (ex className == #StompMockFixedClass2OLD ifTrue: [ StompMockFixedClass2] ifFalse: [ex unresolvedClass])].	self should: [inst class = StompMockFixedClass].	self should: [inst instVar1 = 'test1'].	self should: [inst instVar2 class = StompMockFixedClass2].	self should: [inst instVar2 instVar1= 'test2'].	self should: [inst instVar3 = 'test3'].	self should: [inst instVar4 class = StompMockMixedClass].	self should: [(inst instVar4 at: 1) = 'test4'].	self should: [(inst instVar4 at: 2) class = StompMockFixedClass2].	self should: [(inst instVar4 at: 2) instVar1 = 'test5'].	self should: [inst instVar5 isNil].!

testRenamedShapeChangedRead	"self debug: #testRenamedShapeChangedRead"	| bytes inst expected |	bytes := self fixedClassOLDAnswerArray2.	inst := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockFixedClassOLD shapeChanger: StompMockShapeChanger for: StompMockFixedClass.	].		expected := StompMockFixedClass new.	expected instVar1: 1.	expected instVar2: #renamedTwo.	expected instVar3: (StompPortableUtil default testFixturesClass double3dot3).	expected instVar4: false.	expected instVar5: 'FIVE'.		self should: [inst equals: expected].!

testShapeChangedRead	"self debug: #testShapeChangedRead"	| bytes inst expected |	bytes := self fixedClassOLDAnswerArray2.	inst := Object fromStomp: bytes setting: [:ctx |		ctx registerShapeChanger: StompMockShapeChanger for: StompMockFixedClass.	].		expected := StompMockFixedClass new.	expected instVar1: 1.	expected instVar2: #renamedTwo.	expected instVar3: (StompPortableUtil default testFixturesClass double3dot3).	expected instVar4: false.	expected instVar5: 'FIVE'.		self should: [inst equals: expected].!

testShapeChangedReadCircularReference	"self debug: #testShapeChangedReadCircularReference"	| bytes obj |	bytes := self mockPersonAnswerArray1.	obj := Object fromStomp: bytes setting: [:ctx |		ctx registerClassOldName: #StompMockPersonOLD shapeChanger: StompMockPersonShapeChanger for: StompMockPerson.	]. 	self should: [obj class = StompMockPerson].	self should: [obj name = 'A'].	self should: [obj requests size = 2].	self should: [(obj requests detect: [:each | each id = 1]) owner = obj].	self should: [(obj requests detect: [:each | each id = 2]) owner name = 'B'].	self should: [(obj requests detect: [:each | each id = 2]) owner partners size = 1].	self should: [((obj requests detect: [:each | each id = 2]) owner partners at: 1) = obj].! !
!StompShapeChangerTestCase categoriesFor: #fixedClassOLDAnswerArray1!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #fixedClassOLDAnswerArray2!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #mockPersonAnswerArray1!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #renamedComplexReadAnswerArray1!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #renamedComplexReadAnswerArray2!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #renamedComplexReadAnswerArray3!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #renamedComplexReadAnswerArray4!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #renamedNewFailedAnswerArray1!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #renamedReadFailedAnswerArray1!fixtures!public! !
!StompShapeChangerTestCase categoriesFor: #testBlockRenamedComplexShapeChangerRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testBlockShapeChangedRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testNewFailedRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testRenamedComplexRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testRenamedComplexReadFailed!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testRenamedComplexShapeChangerRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testRenamedComplexShapeChangerReadFailed!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testRenamedRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testRenamedReadFailed!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testRenamedShapeChangedRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testShapeChangedRead!public!testing! !
!StompShapeChangerTestCase categoriesFor: #testShapeChangedReadCircularReference!public!testing! !

StompWriterTestCase guid: (GUID fromString: '{9700CD22-4C3E-4BF2-A698-09AB511C6779}')!
StompWriterTestCase comment: ''!
!StompWriterTestCase categoriesForClass!StompTest-Core! !
!StompWriterTestCase methodsFor!

testWriteBag	"self debug: #testWriteBag"	| bag actual expected |	bag := Bag new.	bag add: 10.	bag add: 20.	bag add: 30.	bag add: 30.	expected := self bagAnswerBytes.		actual := bag toStomp.		self should: [actual size = 9].	self should: [(actual copyFrom: 1 to: 5) = (expected copyFrom: 1 to: 5)].	self should: [(actual copyFrom: 6 to: 9) asSortedCollection = (expected copyFrom: 6 to: 9) asSortedCollection]!

testWriteBasicArray	"self debug: #testWriteBasicArray"	| data wstr bytes |	data := #(1 2 3 #('Hello' 2 3)).	wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.		self should: [bytes = self basicArrayAnswerBytes]				!

testWriteBasicBitsClass	"self debug: #testWriteBasicBitsClass"	| data wstr bytes |	data := StompMockByteArray new: 3.	data at: 1 put: 0.	data at: 2 put: 127.	data at: 3 put: 255.	wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.		self should: [bytes = self basicBitsClassAnswerBytes]				!

testWriteBasicFixedClass	"self debug: #testWriteBasicFixedClass"	| data wstr bytes |	data := StompMockFixedClass new.	data instVar1: 1.	data instVar2: #two.	data instVar3: 3.3.	data instVar4: false.	data instVar5: 'FIVE'.	wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.		self should: [bytes = self basicFixedClassAnswerBytes]				!

testWriteBasicMixedClass	"self debug: #testWriteBasicMixedClass"	| data wstr bytes |	data := StompMockMixedClass new: 3.	data at: 1 put: 1.	data at: 2 put: 'two'.	data at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).	data instVar1: #(true false).	data instVar2: #(nil #()).	wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.		self should: [bytes = self basicMixedClassAnswerBytes]				!

testWriteBasicVariableClass	"self debug: #testWriteBasicVariableClass"	| data wstr bytes |	data := StompMockVariableClass new: 3.	data at: 1 put: 1.	data at: 2 put: 'two'.	data at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).		wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.		self should: [bytes = self basicVariableClassAnswerBytes]				!

testWriteBoolean	"self debug: #testWriteBoolean"	self should: [true toStomp =  #[195]].	self should: [false toStomp =  #[194]]				!

testWriteByteArray	"self debug: #testWriteByteArray"	| byteArray |	byteArray := (ByteArray with: 10 with: 20 with: 30).	self should: [byteArray toStomp = #[163 10 20 30]].			!

testWriteCharacter	"self debug: #testWriteCharacter"	| char1 char2 |	char1 := $A.		self should: [char1 toStomp =  #[147 161 2 253 65]].

	(#DolphinSpecifix = #DolphinSpecifix) ifTrue: [^self]."Dolphin Specific - Hope unicoce support"
	char2 := (StompPortableUtil default characterFromUnicode: 12354). "Japanese Hiragana 'A' "		self should: [char2 toStomp =  #[147 161 2 253 205 48 66]].			!

testWriteCircularRefrences1	"self debug: #testWriteCircularRefrences1"	| inst1 inst2 data wstr bytes |	inst1 := StompMockVariableClass new: 3.	inst1 at: 1 put: 1.	inst1 at: 2 put: 'two'.	inst1 at: 3 put: inst1.		inst2 := StompMockMixedClass new: 3.	inst2 at: 1 put: 1.	inst2 at: 2 put: 'two'.	inst2 at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).	inst2 instVar1: inst1.	inst2 instVar2: inst2.		data := Array with: inst1 with: inst2 with: inst1 with: inst2.		wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.	self should: [bytes = self circularRefrencesAnswerBytes]				!

testWriteClass	"self debug: #testWriteClass"	| cls bytes |	cls := StompMockObjectWithCache.	cls att1: 11.	cls att2: 22.	bytes := cls toStomp.	self should: [bytes = self classAnswerBytes]				!

testWriteClassNameAndIds	"self debug: #testWriteClassNameAndIds"	| data1 data2 data3 data4 wstr bytes  data |	data1 := StompMockFixedClass new.	data1 instVar1: 1.	data1 instVar2: #one.		data2 := StompMockFixedClass new.	data2 instVar1: 2.	data2 instVar2: #two.		data3 := StompMockMixedClass new.	data3 instVar1: 3.	data3 instVar2: #three.		data4 := StompMockMixedClass new.	data4 instVar1: 4.	data4 instVar2: #four.		data := Array with: data1 with: data2 with: data3 with: data4.		wstr := StompWriter onBytes: ByteArray new.	wstr settings suppressNilWrite: false.	wstr nextPut: data.	bytes := wstr contents. 			self should: [bytes = self classNameAndIdsAnswerBytes]				!

testWriteClassNameAndIdsSuppressNilWrites	"self debug: #testWriteClassNameAndIdsSuppressNilWrites"	| data1 data2 data3 data4 wstr bytes  data |	data1 := StompMockFixedClass new.	data1 instVar1: 1.	data1 instVar2: #one.		data2 := StompMockFixedClass new.	data2 instVar1: 2.	data2 instVar2: #two.		data3 := StompMockMixedClass new.	data3 instVar1: 3.	data3 instVar2: #three.		data4 := StompMockMixedClass new.	data4 instVar1: 4.	data4 instVar2: #four.		data := Array with: data1 with: data2 with: data3 with: data4.		wstr := StompWriter onBytes: ByteArray new.	wstr settings suppressNilWrite: true.	wstr nextPut: data.	bytes := wstr contents. 			self should: [bytes = self classNameAndIdsSuppressNilWritesAnswerBytes]				!

testWriteColor	"self debug: #testWriteColor"	| color1 bytes1 color2 bytes2 |	color1 := StompPortableUtil default testFixturesClass blueColor.	bytes1 := color1 toStomp.	self should: [bytes1 = self colorAnswerBytes1].		color2 := StompPortableUtil default testFixturesClass yellowColor.	bytes2 := color2 toStomp.	self should: [bytes2 = self colorAnswerBytes2]				!

testWriteCompositeDictionary	"self debug: #testWriteCompositeDictionary"	| childDic dic bytes rawDic rawChildDic |	childDic := Dictionary new.	childDic at: #child1 put: 'abcde'.	dic := Dictionary new.	dic at: #parent1 put: 'sample string.'.	dic at: #parent2 put: 100.	dic at: #parent3 put: #(10 20 30 ).	dic at: #parent4 put: childDic.	bytes := dic toStomp.	rawDic := Object fromMessagePack: bytes.	self should: [rawDic size = 4].	self should: [(rawDic at: #(#[5] #[112 97 114 101 110 116 49])) = #(#[4] #[115 97 109 112 108 101 32 115 116 114 105 110 103 46])].	self should: [(rawDic at: #(#[5] #[112 97 114 101 110 116 50])) = 100].	self should: [(rawDic at: #(#[5] #[112 97 114 101 110 116 51])) = #(10 20 30)].	rawChildDic := rawDic at: #(#[5] #[112 97 114 101 110 116 52]).	self should: [(rawChildDic at: #(#[5] #[99 104 105 108 100 49])) = #(#[4] #[97 98 99 100 101])].	!

testWriteDate	"self debug: #testWriteDate"	| date bytes |	date := Date fromDays: 37023.	bytes := date toStomp.	self should: [bytes = self dateAnswerBytes]				!

testWriteDictionary	"self debug: #testWriteDictionary"	| dic bytes rawDic |	dic := Dictionary new.	dic at: #key1 put: 'value1'.	dic at: #key2 put: 'value2'.	dic at: #key3 put: 'value3'.		bytes := dic toStomp.	rawDic := Object fromMessagePack: bytes.	self should: [rawDic size = 3].	self should: [(rawDic at: #(#[5] #[107 101 121 49])) = #(#[4] #[118 97 108 117 101 49])].	self should: [(rawDic at: #(#[5] #[107 101 121 50])) = #(#[4] #[118 97 108 117 101 50])].	self should: [(rawDic at: #(#[5] #[107 101 121 51])) = #(#[4] #[118 97 108 117 101 51])].	!

testWriteDuration	"self debug: #testWriteDuration"	| duration bytes |
	(StompPortableUtil default classNamed: #Duration) ifNil: [^self].	duration := 123 seconds.	bytes := duration toStomp.	self should: [bytes = self durationAnswerBytes]				!

testWriteFixedClassWithNilValues	"self debug: #testWriteFixedClassWithNilValues"	| data wstr bytes |	data := StompMockFixedClass new.	data instVar1: 1.	data instVar2: nil.	data instVar3: (StompPortableUtil default testFixturesClass double3dot3).	data instVar4: nil.	data instVar5: 'FIVE'.	wstr := StompWriter onBytes: ByteArray new.	wstr settings suppressNilWrite: true.	wstr nextPut: data.	bytes := wstr contents.		self should: [bytes = self fixedClassWithNilValuesAnswerBytes]				!

testWriteFraction	"self debug: #testWriteFraction"	| fra bytes |	fra := 3 / 4.		bytes := fra toStomp.	self should: [bytes = self fractionAnswerBytes]	!

testWriteInterval	"self debug: #testWriteInterval"	| interval bytes |	interval := 1 to: 10 by: 3.		bytes := interval toStomp.	self should: [bytes = self intervalAnswerBytes]	!

testWriteMixedClassNoVariableData	"self debug: #testWriteMixedClassNoVariableData"	| data wstr bytes |	data := StompMockMixedClass new.	data instVar1: #(true false).	data instVar2: #(nil #()).	wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.		self should: [bytes = self mixedClassNoVariableDataAnswerBytes]				!

testWriteNumber	"self debug: #testWriteNumber"	self should: [1 toStomp = #[1]].	self should: [1.2 toStomp = self floatingPointAnswerBytes1].	self should: [StompPortableUtil default testFixturesClass double1234567890dot123456789 toStomp = self floatingPointAnswerBytes2]!

testWriteOrdredCollection	"self debug: #testWriteOrdredCollection"	| ord bytes |	ord := OrderedCollection new.	ord add: 10.	ord add: 20.	ord add: 30.	bytes := ord toStomp.	self should: [bytes =  self ordredCollectionAnswerBytes]				!

testWriteRectangle	"self debug: #testWriteRectangle"	| rect bytes |	rect := (1@1 corner: 3@4).		bytes := rect toStomp.	self should: [bytes = self rectangleAnswerBytes]	!

testWriteRecursiveArray	"self debug: #testWriteRecursiveArray"	| arr bytes |	arr := Array new: 3.	arr at: 1 put: 10.	arr at: 2 put: 20.	arr at: 3 put: arr.	bytes := arr toStomp.	self should: [bytes = self recursiveArrayAnswerBytes]				!

testWriteRunArray	"self debug: #testWriteRunArray"	| arr bytes |	arr := RunArray runs: #(1 2 1) values: #(1 2 3).	bytes := arr toStomp.	self should: [bytes = self runArrayAnswerBytes]				!

testWriteSet	"self debug: #testWriteSet"	| set expected actual |	set := Set new.	set add: 10.	set add: 20.	set add: 30.	set add: 30.	expected := self setAnswerBytes.		actual := set toStomp.		self should: [actual size = 8].	self should: [(actual copyFrom: 1 to: 5) = (expected copyFrom: 1 to: 5)].	self should: [(actual copyFrom: 6 to: 8) asSortedCollection = (expected copyFrom: 6 to: 8) asSortedCollection]		!

testWriteSharedRefrences1	"self debug: #testWriteSharedRefrences1"	| inst1 inst2 data wstr bytes |	inst1 := StompMockVariableClass new: 3.	inst1 at: 1 put: 1.	inst1 at: 2 put: 'two'.	inst1 at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).		inst2 := StompMockMixedClass new: 3.	inst2 at: 1 put: 1.	inst2 at: 2 put: 'two'.	inst2 at: 3 put: (StompPortableUtil default testFixturesClass double3dot3).	inst2 instVar1: #(true false).	inst2 instVar2: #(nil #()).		data := Array with: inst1 with: inst2 with: inst1 with: inst2.		wstr := StompWriter onBytes: ByteArray new.	wstr nextPut: data.	bytes := wstr contents.	self should: [bytes = self sharedReferenceAnswerBytes]				!

testWriteString	"self debug: #testWriteString"	| str1 |	str1 := 'Hi, I''m String-object'.	self should: [str1 toStomp = self stringAnswerBytes].				!

testWriteSymbol	"self debug: #testWriteSymbol"	| symbol1 |	symbol1 := #abcdefg.	self should: [symbol1 toStomp = self symbolAnswerBytes].				!

testWriteTime	"self debug: #testWriteTime"	| inst bytes |	inst := Time fromSeconds: 84072.	bytes := inst toStomp.	self should: [bytes = self timeAnswerBytes]				!

testWriteTimestamp	"self debug: #testWriteTimestamp"	| bytes timestamp |	timestamp := StompPortableUtil default testFixturesClass timestamp1.	bytes := timestamp toStomp.	self should: [bytes = self timestampAnswerBytes]	!

testWriteUndefinedObject	"self debug: #testWriteUndefinedObject"	self should: [nil toStomp = #[192]].		! !
!StompWriterTestCase categoriesFor: #testWriteBag!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteBasicArray!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteBasicBitsClass!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteBasicFixedClass!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteBasicMixedClass!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteBasicVariableClass!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteBoolean!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteByteArray!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteCharacter!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteCircularRefrences1!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteClass!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteClassNameAndIds!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteClassNameAndIdsSuppressNilWrites!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteColor!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteCompositeDictionary!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteDate!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteDictionary!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteDuration!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteFixedClassWithNilValues!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteFraction!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteInterval!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteMixedClassNoVariableData!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteNumber!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteOrdredCollection!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteRectangle!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteRecursiveArray!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteRunArray!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteSet!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteSharedRefrences1!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteString!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteSymbol!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteTime!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteTimestamp!public!tests! !
!StompWriterTestCase categoriesFor: #testWriteUndefinedObject!public!tests! !

"Binary Globals"!

