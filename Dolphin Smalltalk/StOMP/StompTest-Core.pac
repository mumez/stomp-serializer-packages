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

testFixturesClass
!StompPortableUtil categoriesFor: #testFixturesClass!*StompTest/Core/factory!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StompMockCustomWriteObject guid: (GUID fromString: '{5AAA9083-7D2A-4DE5-A81A-38CA6933148A}')!
StompMockCustomWriteObject comment: ''!
!StompMockCustomWriteObject categoriesForClass!StompTest-Core! !
!StompMockCustomWriteObject methodsFor!

email

email: anObject

initialize

name

name: anObject

stompWriteValue
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

equals: other

instVar1

instVar1: anObject

instVar2

instVar2: anObject

instVar3

instVar3: anObject

instVar4

instVar4: anObject

instVar5

instVar5: anObject
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

equals: other

instVar1

instVar1: anObject

instVar2

instVar2: anObject

instVar3

instVar3: anObject

instVar4

instVar4: anObject

instVar5

instVar5: anObject
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

att1

att1: aValue

att2

att2: aValue

att3Block

att3Block: aValue

stompInitialize
!StompMockFixedNewFailedClass categoriesFor: #att1!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att1:!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att2!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att2:!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att3Block!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #att3Block:!accessing!public! !
!StompMockFixedNewFailedClass categoriesFor: #stompInitialize!initializing/stomp!public! !

!StompMockFixedNewFailedClass class methodsFor!

new

prototype1
!StompMockFixedNewFailedClass class categoriesFor: #new!instance creation!public! !
!StompMockFixedNewFailedClass class categoriesFor: #prototype1!for tests!public! !

StompMockPerson guid: (GUID fromString: '{CD8B48F0-E39A-4527-96E8-489695C68F53}')!
StompMockPerson comment: ''!
!StompMockPerson categoriesForClass!StompTest-Core! !
!StompMockPerson methodsFor!

name

name: aString

partners

partners: anObject

requests

requests: anObject
!StompMockPerson categoriesFor: #name!accessing!public! !
!StompMockPerson categoriesFor: #name:!accessing!public! !
!StompMockPerson categoriesFor: #partners!accessing!public! !
!StompMockPerson categoriesFor: #partners:!accessing!public! !
!StompMockPerson categoriesFor: #requests!accessing!public! !
!StompMockPerson categoriesFor: #requests:!accessing!public! !

!StompMockPerson class methodsFor!

example1
!StompMockPerson class categoriesFor: #example1!examples!public! !

StompMockRequest guid: (GUID fromString: '{8BF96652-95EF-484D-B24F-775F8C0E9AD8}')!
StompMockRequest comment: ''!
!StompMockRequest categoriesForClass!StompTest-Core! !
!StompMockRequest methodsFor!

id

id: anObject

owner

owner: anObject
!StompMockRequest categoriesFor: #id!accessing!public! !
!StompMockRequest categoriesFor: #id:!accessing!public! !
!StompMockRequest categoriesFor: #owner!accessing!public! !
!StompMockRequest categoriesFor: #owner:!accessing!public! !

StompMockShapeChangedObject guid: (GUID fromString: '{D7D4633A-D905-4384-B0FE-5365ECFABD68}')!
StompMockShapeChangedObject comment: ''!
!StompMockShapeChangedObject categoriesForClass!StompTest-Core! !
!StompMockShapeChangedObject methodsFor!

addedAtt1

addedAtt1: anObject

originalAtt1

originalAtt1: anObject

renamedAtt1

renamedAtt1: anObject

stompInitialize

stompInstVarAt: instVarIndex named: varName put: value 
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

blueColor

double1234567890dot123456789

double3dot3

float1dot2bytes

timestamp1

yellowColor
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

stompReadValue
!StompMockMementoArray categoriesFor: #stompReadValue!public!stomp/objectstream/reading! !

StompMockMixedClass guid: (GUID fromString: '{8A9FF58D-BCC3-43DC-94F5-9E8D0E7329FD}')!
StompMockMixedClass comment: ''!
!StompMockMixedClass categoriesForClass!StompTest-Core! !
!StompMockMixedClass methodsFor!

equals: other

instVar1

instVar1: anObject

instVar2

instVar2: anObject

stompShouldWriteInstanceVariables
!StompMockMixedClass categoriesFor: #equals:!comparing!public! !
!StompMockMixedClass categoriesFor: #instVar1!accessing!public! !
!StompMockMixedClass categoriesFor: #instVar1:!accessing!public! !
!StompMockMixedClass categoriesFor: #instVar2!accessing!public! !
!StompMockMixedClass categoriesFor: #instVar2:!accessing!public! !
!StompMockMixedClass categoriesFor: #stompShouldWriteInstanceVariables!public!testing! !

!StompMockMixedClass class methodsFor!

new
!StompMockMixedClass class categoriesFor: #new!instance creation!public! !

StompMockVariableClass guid: (GUID fromString: '{C4AA1C71-1AA8-44FE-95E4-B47F4BAF1E20}')!
StompMockVariableClass comment: ''!
!StompMockVariableClass categoriesForClass!StompTest-Core! !
StompMockVariableNewFailedClass guid: (GUID fromString: '{F433ADCE-55A2-465C-9D09-4AA8D610165E}')!
StompMockVariableNewFailedClass comment: ''!
!StompMockVariableNewFailedClass categoriesForClass!StompTest-Core! !
!StompMockVariableNewFailedClass methodsFor!

att1

att1: aValue

att2

att2: aValue

att3Block

att3Block: aValue

stompInitialize

stompShouldWriteInstanceVariables 
!StompMockVariableNewFailedClass categoriesFor: #att1!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att1:!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att2!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att2:!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att3Block!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #att3Block:!accessing!public! !
!StompMockVariableNewFailedClass categoriesFor: #stompInitialize!initializing/stomp!public! !
!StompMockVariableNewFailedClass categoriesFor: #stompShouldWriteInstanceVariables!public!testing/stomp! !

!StompMockVariableNewFailedClass class methodsFor!

new: size

prototype1
!StompMockVariableNewFailedClass class categoriesFor: #new:!instance creation!public! !
!StompMockVariableNewFailedClass class categoriesFor: #prototype1!for tests!public! !

StompMockVariableAlternativeClass guid: (GUID fromString: '{65A2DE07-6685-4AEC-ACD2-33C893EBBAC7}')!
StompMockVariableAlternativeClass comment: ''!
!StompMockVariableAlternativeClass categoriesForClass!StompTest-Core! !
!StompMockVariableAlternativeClass class methodsFor!

new: size
!StompMockVariableAlternativeClass class categoriesFor: #new:!instance creation!public! !

StompMockByteArray guid: (GUID fromString: '{BC3E8687-FB4B-4E8D-A8D9-F90D11E265EF}')!
StompMockByteArray comment: ''!
!StompMockByteArray categoriesForClass!StompTest-Core! !
StompMockObjectInitializingOnRead guid: (GUID fromString: '{5E5297D7-E330-484F-8798-BD021A25011D}')!
StompMockObjectInitializingOnRead comment: ''!
!StompMockObjectInitializingOnRead categoriesForClass!StompTest-Core! !
!StompMockObjectInitializingOnRead methodsFor!

stompInitialize
!StompMockObjectInitializingOnRead categoriesFor: #stompInitialize!initialize/release!public! !

StompMockObjectPreparingOnWrite guid: (GUID fromString: '{DDFD35DA-9614-4996-A86D-5827A8D6114F}')!
StompMockObjectPreparingOnWrite comment: ''!
!StompMockObjectPreparingOnWrite categoriesForClass!StompTest-Core! !
!StompMockObjectPreparingOnWrite methodsFor!

stompPrepareWrite 
!StompMockObjectPreparingOnWrite categoriesFor: #stompPrepareWrite!public!stomp/objectstream/writing! !

StompMockObjectWithCache guid: (GUID fromString: '{6ABF0FEB-11F5-4302-A8AC-3C58A94485FA}')!
StompMockObjectWithCache comment: ''!
!StompMockObjectWithCache categoriesForClass!StompTest-Core! !
!StompMockObjectWithCache methodsFor!

stompTransientInstVarNames
!StompMockObjectWithCache categoriesFor: #stompTransientInstVarNames!public!stomp/objectstream/writing! !

!StompMockObjectWithCache class methodsFor!

att1

att1: anObject

att2

att2: anObject

cache1

cache1: anObject

stompTransientInstVarNames
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

new
!StompMockFixedAlternativeClass class categoriesFor: #new!instance creation!public! !

StompMockPersonShapeChanger guid: (GUID fromString: '{254B3A3D-B672-46FA-A4C6-06FAD72FACEA}')!
StompMockPersonShapeChanger comment: ''!
!StompMockPersonShapeChanger categoriesForClass!StompTest-Core! !
!StompMockPersonShapeChanger methodsFor!

loadInstVarAt: varIndex named: varName put: varValue 
!StompMockPersonShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !

StompMockShapeChanger guid: (GUID fromString: '{AEEF7297-2E01-4C8B-9A15-4C31E3F7AD24}')!
StompMockShapeChanger comment: ''!
!StompMockShapeChanger categoriesForClass!StompTest-Core! !
!StompMockShapeChanger methodsFor!

loadAdditions

loadInstVarAt: varIndex named: varName put: varValue 
!StompMockShapeChanger categoriesFor: #loadAdditions!actions!public! !
!StompMockShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !

StompMockShapeChangerForRenamedComplexRead guid: (GUID fromString: '{A5EA3549-C7CE-4F08-925C-14CD69912248}')!
StompMockShapeChangerForRenamedComplexRead comment: ''!
!StompMockShapeChangerForRenamedComplexRead categoriesForClass!StompTest-Core! !
!StompMockShapeChangerForRenamedComplexRead methodsFor!

loadAdditions

loadInstVarAt: varIndex named: varName put: varValue 
!StompMockShapeChangerForRenamedComplexRead categoriesFor: #loadAdditions!actions!public! !
!StompMockShapeChangerForRenamedComplexRead categoriesFor: #loadInstVarAt:named:put:!actions!public! !

StompCustomSerializationTestCase guid: (GUID fromString: '{A7E83BF7-3D36-43A5-B52B-C909F4B047AF}')!
StompCustomSerializationTestCase comment: ''!
!StompCustomSerializationTestCase categoriesForClass!StompTest-Core! !
!StompCustomSerializationTestCase methodsFor!

testReadStompWriteValueAndStompReadValue

testSameStompWriteValueAndStompReadValue

testStompInitialize

testStompPrepareWrite

testStompTransientClassInstVarNames

testStompTransientInstVarNames
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

testWriteReadNewFailedClass

testWriteReadNewFailedClassReceiverAlternative

testWriteReadNewVariableFailedClass

testWriteReadNewVariableFailedClassReciverAlternative
!StompNewFailedTestCase categoriesFor: #testWriteReadNewFailedClass!public!tests! !
!StompNewFailedTestCase categoriesFor: #testWriteReadNewFailedClassReceiverAlternative!public!tests! !
!StompNewFailedTestCase categoriesFor: #testWriteReadNewVariableFailedClass!public!tests! !
!StompNewFailedTestCase categoriesFor: #testWriteReadNewVariableFailedClassReciverAlternative!public!tests! !

StompTestCase guid: (GUID fromString: '{66AFF64B-6A90-487B-91C1-7EF35435DE66}')!
StompTestCase comment: ''!
!StompTestCase categoriesForClass!StompTest-Core! !
!StompTestCase methodsFor!

bagAnswerBytes

basicArrayAnswerBytes

basicBitsClassAnswerBytes

basicFixedClassAnswerBytes

basicMixedClassAnswerBytes

basicVariableClassAnswerBytes

circularRefrencesAnswerBytes

classAnswerBytes

classNameAndIdsAnswerBytes

classNameAndIdsSuppressNilWritesAnswerBytes

colorAnswerBytes1

colorAnswerBytes2

compositeDictionaryAnswerBytes

dateAnswerBytes

dictionaryAnswerBytes

durationAnswerBytes

fixedClassWithNilValuesAnswerBytes

floatingPointAnswerBytes1

floatingPointAnswerBytes2

fractionAnswerBytes

intervalAnswerBytes

mixedClassNoVariableDataAnswerBytes

ordredCollectionAnswerBytes

rectangleAnswerBytes

recursiveArrayAnswerBytes

runArrayAnswerBytes

setAnswerBytes

sharedReferenceAnswerBytes

stringAnswerBytes

symbolAnswerBytes

timeAnswerBytes

timestampAnswerBytes
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

collectionEquals: aCollection with: otherCollection

testReadBag

testReadBasicArray

testReadBasicBitsClass

testReadBasicFixedClass

testReadBasicMixedClass

testReadBasicVariableClass

testReadBoolean

testReadByteArray

testReadCharacter

	(#DolphinSpecifix = #DolphinSpecifix) ifTrue: [^self]."Dolphin Specific - Hope unicoce support"


testReadCircularRefrences1

testReadClass

testReadClassNameAndIds

testReadClassNameAndIdsSuppressNilWrites

testReadColor

testReadCompositeDictionary

testReadDate

testReadDictionary

testReadDuration
	(StompPortableUtil default classNamed: #Duration) ifNil: [^self].


testReadFixedClassWithNilValues

testReadFraction

testReadInterval

testReadMixedClassNoVariableData

testReadNumber

testReadOrdredCollection

testReadRectangle

testReadRecursiveArray

testReadRunArray

testReadSet
	self should: [self collectionEquals: (Object fromStomp:(self setAnswerBytes)) with: set]

testReadSharedRefrences1

testReadString

testReadTime

testReadTimestamp

testReadUndefinedObject
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

testReadWriteText
!StompReadWriteTestCase categoriesFor: #testReadWriteText!public!tests! !

StompShapeChangerTestCase guid: (GUID fromString: '{ED6BD3DC-8D0E-4184-BD81-F9432BD7BD44}')!
StompShapeChangerTestCase comment: ''!
!StompShapeChangerTestCase categoriesForClass!StompTest-Core! !
!StompShapeChangerTestCase methodsFor!

fixedClassOLDAnswerArray1

fixedClassOLDAnswerArray2

mockPersonAnswerArray1

renamedComplexReadAnswerArray1

renamedComplexReadAnswerArray2

renamedComplexReadAnswerArray3

renamedComplexReadAnswerArray4

renamedNewFailedAnswerArray1

renamedReadFailedAnswerArray1

testBlockRenamedComplexShapeChangerRead

testBlockShapeChangedRead

testNewFailedRead

testRenamedComplexRead

testRenamedComplexReadFailed

testRenamedComplexShapeChangerRead

testRenamedComplexShapeChangerReadFailed

testRenamedRead

testRenamedReadFailed

testRenamedShapeChangedRead

testShapeChangedRead

testShapeChangedReadCircularReference
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

testWriteBag

testWriteBasicArray

testWriteBasicBitsClass

testWriteBasicFixedClass

testWriteBasicMixedClass

testWriteBasicVariableClass

testWriteBoolean

testWriteByteArray

testWriteCharacter

	(#DolphinSpecifix = #DolphinSpecifix) ifTrue: [^self]."Dolphin Specific - Hope unicoce support"


testWriteCircularRefrences1

testWriteClass

testWriteClassNameAndIds

testWriteClassNameAndIdsSuppressNilWrites

testWriteColor

testWriteCompositeDictionary

testWriteDate

testWriteDictionary

testWriteDuration
	(StompPortableUtil default classNamed: #Duration) ifNil: [^self].

testWriteFixedClassWithNilValues

testWriteFraction

testWriteInterval

testWriteMixedClassNoVariableData

testWriteNumber

testWriteOrdredCollection

testWriteRectangle

testWriteRecursiveArray

testWriteRunArray

testWriteSet

testWriteSharedRefrences1

testWriteString

testWriteSymbol

testWriteTime

testWriteTimestamp

testWriteUndefinedObject
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
