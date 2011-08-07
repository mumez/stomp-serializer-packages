| package |
package := Package name: 'Stomp-Core'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '3'.


package classNames
	add: #StompBlockShapeChanger;
	add: #StompClassNotFound;
	add: #StompConstants;
	add: #StompContext;
	add: #StompError;
	add: #StompFieldsInfo;
	add: #StompInvalidDeserialization;
	add: #StompInvalidSerialization;
	add: #StompNewFailed;
	add: #StompPopularClassMap;
	add: #StompPortableUtil;
	add: #StompReadContext;
	add: #StompReader;
	add: #StompSettings;
	add: #StompShapeChanger;
	add: #StompWarning;
	add: #StompWriteContext;
	add: #StompWriter;
	yourself.

package methodNames
	add: #Behavior -> #fromStomp:;
	add: #Behavior -> #fromStomp:setting:;
	add: #Behavior -> #stompCreateBitsInstanceFrom:;
	add: #Behavior -> #stompCreateFixedInstanceFrom:;
	add: #Behavior -> #stompCreateInstance;
	add: #Behavior -> #stompCreateInstance:;
	add: #Behavior -> #stompCreateInstanceFrom:;
	add: #Behavior -> #stompCreateMixedInstanceFrom:;
	add: #Behavior -> #stompFromBytes:;
	add: #BlockClosure -> #stompWriteContentTo:;
	add: #Boolean -> #stompShouldWriteInstanceVariables;
	add: #Boolean -> #stompSupportsReference:;
	add: #Character -> #stompWriteContentTo:;
	add: #ClassDescription -> #stompLoadContentsOnCreation;
	add: #ClassDescription -> #stompTransientInstVarNames;
	add: #Collection -> #stompAdd:at:;
	add: #Collection -> #stompDo:;
	add: #Collection -> #stompReadContentFrom:;
	add: #Collection -> #stompShouldWriteInstanceVariables;
	add: #Collection -> #stompWriteContentTo:;
	add: #Date -> #stompShouldWriteInstanceVariables;
	add: #Date -> #stompSupportsReference:;
	add: #Date -> #stompWriteContentTo:;
	add: #Fraction -> #stompWriteContentTo:;
	add: #IdentityDictionary -> #stompWriteContentTo:;
	add: #Interval -> #stompWriteContentTo:;
	add: #Metaclass -> #stompCreateInstance;
	add: #MpPortableUtil -> #stompUtil;
	add: #Number -> #stompShouldWriteInstanceVariables;
	add: #Number -> #stompSupportsReference:;
	add: #Object -> #stompAfterWrite;
	add: #Object -> #stompAt:put:;
	add: #Object -> #stompBasicReadContentFrom:;
	add: #Object -> #stompBytes;
	add: #Object -> #stompDo:;
	add: #Object -> #stompInitialize;
	add: #Object -> #stompInstVarAt:named:put:;
	add: #Object -> #stompInstVarNamed:writtenAs:;
	add: #Object -> #stompPrepareWrite;
	add: #Object -> #stompReadContentFrom:;
	add: #Object -> #stompReadValue;
	add: #Object -> #stompShouldWriteInstanceVariables;
	add: #Object -> #stompSupportsReference:;
	add: #Object -> #stompTransientInstVarNames;
	add: #Object -> #stompValueContentSize;
	add: #Object -> #stompWriteContentTo:;
	add: #Object -> #stompWriteValue;
	add: #Object -> #toStomp;
	add: #Point -> #stompWriteContentTo:;
	add: #Rectangle -> #stompWriteContentTo:;
	add: #RunArray -> #stompWriteContentTo:;
	add: #String -> #stompSupportsReference:;
	add: #Symbol -> #stompSupportsReference:;
	add: #Time -> #stompShouldWriteInstanceVariables;
	add: #Time -> #stompSupportsReference:;
	add: #Time -> #stompWriteContentTo:;
	add: #UndefinedObject -> #stompShouldWriteInstanceVariables;
	add: #UndefinedObject -> #stompSupportsReference:;
	add: 'BlockClosure class' -> #stompCreateInstanceFrom:;
	add: 'Character class' -> #stompCreateInstanceFrom:;
	add: 'Collection class' -> #stompCreateInstanceFrom:;
	add: 'Date class' -> #stompCreateInstanceFrom:;
	add: 'Fraction class' -> #stompCreateInstanceFrom:;
	add: 'IdentityDictionary class' -> #stompCreateInstanceFrom:;
	add: 'Interval class' -> #stompCreateInstanceFrom:;
	add: 'Point class' -> #stompCreateInstanceFrom:;
	add: 'Rectangle class' -> #stompCreateInstanceFrom:;
	add: 'RunArray class' -> #stompCreateInstanceFrom:;
	add: 'Symbol class' -> #stompCreateInstanceFrom:;
	add: 'Time class' -> #stompCreateInstanceFrom:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\MessagePack\MessagePack-Core';
	yourself).

package!

"Class Definitions"!

Object subclass: #StompConstants
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'tupleTags'!
Object subclass: #StompContext
	instanceVariableNames: 'requestor objectsDictionary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompFieldsInfo
	instanceVariableNames: 'type indexFieldSize'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompPopularClassMap
	instanceVariableNames: 'classToCode codeToClass'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
Object subclass: #StompPortableUtil
	instanceVariableNames: ''
	classVariableNames: 'Default DialectSpecificClass'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompShapeChanger
	instanceVariableNames: 'targetInstance'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #StompError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Warning subclass: #StompWarning
	instanceVariableNames: 'className element context'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'suppressTranscriptLogging suppressSignaling'!
StompWarning subclass: #StompClassNotFound
	instanceVariableNames: 'environmentName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompWarning subclass: #StompInvalidDeserialization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompWarning subclass: #StompInvalidSerialization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompWarning subclass: #StompNewFailed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MpDecoder subclass: #StompReader
	instanceVariableNames: 'context version'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MpEncoder subclass: #StompWriter
	instanceVariableNames: 'context'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MpSettings subclass: #StompSettings
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompContext subclass: #StompReadContext
	instanceVariableNames: 'classIdsDictionary environmentNamesDictionary instVarNamesWithIndicesDictionary classAliasesDictionary shapeChangersDictionary fieldsInfo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompContext subclass: #StompWriteContext
	instanceVariableNames: 'classesDictionary environmentsDictionary instVarNamesWithIndicesDictionary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompShapeChanger subclass: #StompBlockShapeChanger
	instanceVariableNames: 'loadInstVarsBlock loadAdditionsBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Behavior methodsFor!

fromStomp: bytes

fromStomp: bytes setting: aBlock

stompCreateBitsInstanceFrom: stompReader

stompCreateFixedInstanceFrom: stompReader

stompCreateInstance

stompCreateInstance: size

stompCreateInstanceFrom: stompReader

stompCreateMixedInstanceFrom: stompReader

stompFromBytes: rawBytes
!Behavior categoriesFor: #fromStomp:!*Stomp/Core/reading!public! !
!Behavior categoriesFor: #fromStomp:setting:!*Stomp/Core/reading!public! !
!Behavior categoriesFor: #stompCreateBitsInstanceFrom:!*Stomp/Core/instance creation/helper!public! !
!Behavior categoriesFor: #stompCreateFixedInstanceFrom:!*Stomp/Core/instance creation/helper!public! !
!Behavior categoriesFor: #stompCreateInstance!*Stomp/Core/instance creation!public! !
!Behavior categoriesFor: #stompCreateInstance:!*Stomp/Core/instance creation!public! !
!Behavior categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !
!Behavior categoriesFor: #stompCreateMixedInstanceFrom:!*Stomp/Core/instance creation/helper!public! !
!Behavior categoriesFor: #stompFromBytes:!*Stomp/Core/instance creation!public! !

!BlockClosure methodsFor!

stompWriteContentTo: stompWriter
!BlockClosure categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!BlockClosure class methodsFor!

stompCreateInstanceFrom: stompReader
!BlockClosure class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Boolean methodsFor!

stompShouldWriteInstanceVariables

stompSupportsReference: stompContext
!Boolean categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Boolean categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !

!Character methodsFor!

stompWriteContentTo: stompWriter
!Character categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Character class methodsFor!

stompCreateInstanceFrom: stompReader
!Character class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!ClassDescription methodsFor!

stompLoadContentsOnCreation

stompTransientInstVarNames
!ClassDescription categoriesFor: #stompLoadContentsOnCreation!*Stomp/Core/testing!public! !
!ClassDescription categoriesFor: #stompTransientInstVarNames!*Stomp/Core/writing!public! !

!Collection methodsFor!

stompAdd: elem at: idx

stompDo: aBlock

stompReadContentFrom: stompReader 

stompShouldWriteInstanceVariables

stompWriteContentTo: stompWriter 
!Collection categoriesFor: #stompAdd:at:!*Stomp/Core/writing!public! !
!Collection categoriesFor: #stompDo:!*Stomp/Core/writing!public! !
!Collection categoriesFor: #stompReadContentFrom:!*Stomp/Core/reading!public! !
!Collection categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Collection categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Collection class methodsFor!

stompCreateInstanceFrom: stompReader
!Collection class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Date methodsFor!

stompShouldWriteInstanceVariables

stompSupportsReference: stompContext

stompWriteContentTo: stompWriter
!Date categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Date categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !
!Date categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Date class methodsFor!

stompCreateInstanceFrom: stompReader
!Date class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Fraction methodsFor!

stompWriteContentTo: stompWriter
!Fraction categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Fraction class methodsFor!

stompCreateInstanceFrom: stompReader
!Fraction class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!IdentityDictionary methodsFor!

stompWriteContentTo: stompWriter
!IdentityDictionary categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!IdentityDictionary class methodsFor!

stompCreateInstanceFrom: stompReader
!IdentityDictionary class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Interval methodsFor!

stompWriteContentTo: stompWriter
!Interval categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Interval class methodsFor!

stompCreateInstanceFrom: stompReader
!Interval class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Metaclass methodsFor!

stompCreateInstance
!Metaclass categoriesFor: #stompCreateInstance!*Stomp/Core/instance creation!public! !

!MpPortableUtil methodsFor!

stompUtil
!MpPortableUtil categoriesFor: #stompUtil!*Stomp/core/accessing!public! !

!Number methodsFor!

stompShouldWriteInstanceVariables

stompSupportsReference: stompContext
!Number categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Number categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !

!Object methodsFor!

stompAfterWrite!

stompAt: index put: aValue 

stompBasicReadContentFrom: stompReader 

stompBytes

stompDo: aBlock

stompInitialize

stompInstVarAt: instVarIndex named: varName put: aValue 

stompInstVarNamed: varName writtenAs: writtenValue

stompPrepareWrite!

stompReadContentFrom: stompReader 

stompReadValue

stompShouldWriteInstanceVariables

stompSupportsReference: stompContext

stompTransientInstVarNames

stompValueContentSize

stompWriteContentTo: stompWriter 

stompWriteValue

toStomp
!Object categoriesFor: #stompAfterWrite!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompAt:put:!*Stomp/Core/reading!public! !
!Object categoriesFor: #stompBasicReadContentFrom:!*Stomp/Core/reading!public! !
!Object categoriesFor: #stompBytes!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompDo:!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompInitialize!*Stomp/Core/reading!public! !
!Object categoriesFor: #stompInstVarAt:named:put:!*Stomp/Core/reading!public! !
!Object categoriesFor: #stompInstVarNamed:writtenAs:!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompPrepareWrite!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompReadContentFrom:!*Stomp/Core/reading!public! !
!Object categoriesFor: #stompReadValue!*Stomp/Core/reading!public! !
!Object categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Object categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !
!Object categoriesFor: #stompTransientInstVarNames!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompValueContentSize!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !
!Object categoriesFor: #stompWriteValue!*Stomp/Core/writing!public! !
!Object categoriesFor: #toStomp!*Stomp/Core/writing!public! !

!Point methodsFor!

stompWriteContentTo: stompWriter
!Point categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Point class methodsFor!

stompCreateInstanceFrom: stompReader
!Point class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Rectangle methodsFor!

stompWriteContentTo: stompWriter
!Rectangle categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Rectangle class methodsFor!

stompCreateInstanceFrom: stompReader
!Rectangle class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!RunArray methodsFor!

stompWriteContentTo: stompWriter
!RunArray categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!RunArray class methodsFor!

stompCreateInstanceFrom: stompReader
!RunArray class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!String methodsFor!

stompSupportsReference: stompContext
!String categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !

!Symbol methodsFor!

stompSupportsReference: stompContext
!Symbol categoriesFor: #stompSupportsReference:!*Stomp/Core!public! !

!Symbol class methodsFor!

stompCreateInstanceFrom: stompReader
!Symbol class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Time methodsFor!

stompShouldWriteInstanceVariables

stompSupportsReference: stompContext

stompWriteContentTo: stompWriter
!Time categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Time categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !
!Time categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Time class methodsFor!

stompCreateInstanceFrom: stompReader
!Time class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!UndefinedObject methodsFor!

stompShouldWriteInstanceVariables

stompSupportsReference: stompContext
!UndefinedObject categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core!public! !
!UndefinedObject categoriesFor: #stompSupportsReference:!*Stomp/Core!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StompConstants guid: (GUID fromString: '{4CF03C6E-8917-493C-8A81-EBE29A02BEA3}')!
StompConstants comment: ''!
!StompConstants categoriesForClass!Stomp-Core! !
!StompConstants class methodsFor!

byteString

byteSymbol

classCode

classId

environmentId

environmentName

header

initialize

initTupleTags

isTupleTag: tag

klassName

reference

tupleTags

value

wideString

wideSymbol
!StompConstants class categoriesFor: #byteString!public!tuple tags! !
!StompConstants class categoriesFor: #byteSymbol!public!tuple tags! !
!StompConstants class categoriesFor: #classCode!attribute/tags!public! !
!StompConstants class categoriesFor: #classId!attribute/tags!public! !
!StompConstants class categoriesFor: #environmentId!attribute/tags!public! !
!StompConstants class categoriesFor: #environmentName!attribute/tags!public! !
!StompConstants class categoriesFor: #header!accessing!public! !
!StompConstants class categoriesFor: #initialize!class initialization!public! !
!StompConstants class categoriesFor: #initTupleTags!class initialization!public! !
!StompConstants class categoriesFor: #isTupleTag:!actions!public! !
!StompConstants class categoriesFor: #klassName!attribute/tags!public! !
!StompConstants class categoriesFor: #reference!public!tuple tags! !
!StompConstants class categoriesFor: #tupleTags!accessing!public! !
!StompConstants class categoriesFor: #value!public!tuple tags! !
!StompConstants class categoriesFor: #wideString!public!tuple tags! !
!StompConstants class categoriesFor: #wideSymbol!public!tuple tags! !

StompContext guid: (GUID fromString: '{67D4CCE1-FFD1-4FD1-85C4-9072678269D1}')!
StompContext comment: ''!
!StompContext categoriesForClass!Stomp-Core! !
!StompContext methodsFor!

at: key

at: key ifAbsent: block

at: key ifAbsentPut: value

at: key put: value

includesKey: key 

keys

keysAndValuesDo: block

objectDictionaryClass

objectsDictionary

objectsDictionary: anObject

printOn: aStream

removeKey: key 

removeKey: key ifAbsent: block

requestor

requestor: anObject

settings

size

values
!StompContext categoriesFor: #at:!actions!public! !
!StompContext categoriesFor: #at:ifAbsent:!actions!public! !
!StompContext categoriesFor: #at:ifAbsentPut:!actions!public! !
!StompContext categoriesFor: #at:put:!actions!public! !
!StompContext categoriesFor: #includesKey:!actions!public! !
!StompContext categoriesFor: #keys!actions!public! !
!StompContext categoriesFor: #keysAndValuesDo:!actions!public! !
!StompContext categoriesFor: #objectDictionaryClass!defaults!public! !
!StompContext categoriesFor: #objectsDictionary!accessing!public! !
!StompContext categoriesFor: #objectsDictionary:!accessing!public! !
!StompContext categoriesFor: #printOn:!printing!public! !
!StompContext categoriesFor: #removeKey:!actions!public! !
!StompContext categoriesFor: #removeKey:ifAbsent:!actions!public! !
!StompContext categoriesFor: #requestor!accessing!public! !
!StompContext categoriesFor: #requestor:!accessing!public! !
!StompContext categoriesFor: #settings!accessing!public! !
!StompContext categoriesFor: #size!actions!public! !
!StompContext categoriesFor: #values!actions!public! !

!StompContext class methodsFor!

on: requestor 
!StompContext class categoriesFor: #on:!instance creation!public! !

StompFieldsInfo guid: (GUID fromString: '{AB84D163-49B2-4D8D-BD35-AD1945949EB3}')!
StompFieldsInfo comment: ''!
!StompFieldsInfo categoriesForClass!Stomp-Core! !
!StompFieldsInfo methodsFor!

indexFieldSize

indexFieldSize: anObject

isMixedFields

isPureIndexFields

type

type: anObject
!StompFieldsInfo categoriesFor: #indexFieldSize!accessing!public! !
!StompFieldsInfo categoriesFor: #indexFieldSize:!accessing!public! !
!StompFieldsInfo categoriesFor: #isMixedFields!public!testing! !
!StompFieldsInfo categoriesFor: #isPureIndexFields!public!testing! !
!StompFieldsInfo categoriesFor: #type!accessing!public! !
!StompFieldsInfo categoriesFor: #type:!accessing!public! !

!StompFieldsInfo class methodsFor!

mixedFields

pureIndexFields

pureIndexFieldsSized: numOfFields

type: typeSymbol
!StompFieldsInfo class categoriesFor: #mixedFields!instance creation!public! !
!StompFieldsInfo class categoriesFor: #pureIndexFields!instance creation!public! !
!StompFieldsInfo class categoriesFor: #pureIndexFieldsSized:!instance creation!public! !
!StompFieldsInfo class categoriesFor: #type:!instance creation!public! !

StompPopularClassMap guid: (GUID fromString: '{0ADD9D6A-8BCA-45AA-A73B-4BED8110C3EB}')!
StompPopularClassMap comment: ''!
!StompPopularClassMap categoriesForClass!Stomp-Core! !
!StompPopularClassMap methodsFor!

associationClass

bagClass

byteStringClass

byteSymbolClass

characterClass

classAt: code 

classNamed: localClassName

classToCode

classToCode: anObject

codeAt: aClass ifPresent: aBlock

codeToClass

codeToClass: anObject

colorClass

compactDictionaryClass

compiledMethodClass

dateAndTimeClass

dateClass

durationClass

fixedPointClass

fractionClass

identityDictionaryClass

identitySetClass

initialize

intervalClass

matrixClass

orderedCollectionClass

orderedSetClass

pointClass

popularClassSelectors

prepareClassToCodeMap: classToCodeMap

prepareCodeToClassMap: codeToClassMap

prepareMaps

rectangleClass

runArrayClass

setClass

sortedCollectionClass

timeClass

timestampClass

uint16ArrayClass

uint32ArrayClass

uuidClass
!StompPopularClassMap categoriesFor: #associationClass!factory!public! !
!StompPopularClassMap categoriesFor: #bagClass!factory!public! !
!StompPopularClassMap categoriesFor: #byteStringClass!factory!public! !
!StompPopularClassMap categoriesFor: #byteSymbolClass!factory!public! !
!StompPopularClassMap categoriesFor: #characterClass!factory!public! !
!StompPopularClassMap categoriesFor: #classAt:!actions!public! !
!StompPopularClassMap categoriesFor: #classNamed:!private! !
!StompPopularClassMap categoriesFor: #classToCode!accessing!public! !
!StompPopularClassMap categoriesFor: #classToCode:!accessing!public! !
!StompPopularClassMap categoriesFor: #codeAt:ifPresent:!actions!public! !
!StompPopularClassMap categoriesFor: #codeToClass!accessing!public! !
!StompPopularClassMap categoriesFor: #codeToClass:!accessing!public! !
!StompPopularClassMap categoriesFor: #colorClass!factory!public! !
!StompPopularClassMap categoriesFor: #compactDictionaryClass!factory!public! !
!StompPopularClassMap categoriesFor: #compiledMethodClass!factory!public! !
!StompPopularClassMap categoriesFor: #dateAndTimeClass!factory!public! !
!StompPopularClassMap categoriesFor: #dateClass!factory!public! !
!StompPopularClassMap categoriesFor: #durationClass!factory!public! !
!StompPopularClassMap categoriesFor: #fixedPointClass!factory!public! !
!StompPopularClassMap categoriesFor: #fractionClass!factory!public! !
!StompPopularClassMap categoriesFor: #identityDictionaryClass!factory!public! !
!StompPopularClassMap categoriesFor: #identitySetClass!factory!public! !
!StompPopularClassMap categoriesFor: #initialize!initialize/release!public! !
!StompPopularClassMap categoriesFor: #intervalClass!factory!public! !
!StompPopularClassMap categoriesFor: #matrixClass!factory!public! !
!StompPopularClassMap categoriesFor: #orderedCollectionClass!factory!public! !
!StompPopularClassMap categoriesFor: #orderedSetClass!factory!public! !
!StompPopularClassMap categoriesFor: #pointClass!factory!public! !
!StompPopularClassMap categoriesFor: #popularClassSelectors!constants!public! !
!StompPopularClassMap categoriesFor: #prepareClassToCodeMap:!preparing!public! !
!StompPopularClassMap categoriesFor: #prepareCodeToClassMap:!preparing!public! !
!StompPopularClassMap categoriesFor: #prepareMaps!initialize/release!public! !
!StompPopularClassMap categoriesFor: #rectangleClass!factory!public! !
!StompPopularClassMap categoriesFor: #runArrayClass!factory!public! !
!StompPopularClassMap categoriesFor: #setClass!factory!public! !
!StompPopularClassMap categoriesFor: #sortedCollectionClass!factory!public! !
!StompPopularClassMap categoriesFor: #timeClass!factory!public! !
!StompPopularClassMap categoriesFor: #timestampClass!factory!public! !
!StompPopularClassMap categoriesFor: #uint16ArrayClass!factory!public! !
!StompPopularClassMap categoriesFor: #uint32ArrayClass!factory!public! !
!StompPopularClassMap categoriesFor: #uuidClass!factory!public! !

!StompPopularClassMap class methodsFor!

default

initialize

initializeAll
!StompPopularClassMap class categoriesFor: #default!accessing!public! !
!StompPopularClassMap class categoriesFor: #initialize!class initialization!public! !
!StompPopularClassMap class categoriesFor: #initializeAll!class initialization!public! !

StompPortableUtil guid: (GUID fromString: '{97BABC13-A00D-4D58-A252-406C35C6CBEB}')!
StompPortableUtil comment: ''!
!StompPortableUtil categoriesForClass!Stomp-Core! !
!StompPortableUtil methodsFor!

bytes: rawBytes intoOf: bitsClass

bytesFrom: bitsObject

bytesFromString: aString

characterFromUnicode: anInteger

classNamed: localClassName

classNamed: localClassName in: environmentQualifier 

colorFromRgbArray: rgbArray

dateAndTimeFromNanoseconds: nanoseconds

dateFromSeconds: seconds

durationFromNanoseconds: nanoseconds

encodeTypeMapperClass

environmentNameOf: anObject

instVarIndexOf: aClass for: varName 

instVarIndicesOf: aClass from: instVarNames

instVarNamed: varName put: value in: anObject

isMeta: aBehavior

isWideString: aString

isWideSymbol: aSymbol

nanosecondsFromDateAndTime: timestamp

nanosecondsFromDuration: duration

nextAvailable: size from: stream

popularClassMap

shouldWriteEnvironmentNameOf: anObject

soleInstanceOf: aMetaclass

stringFromBytes: aByteArray

timestampFromNanoseconds: nanoseconds

unicodeFromCharacter: aCharacter

useEnvironmentByDefault
!StompPortableUtil categoriesFor: #bytes:intoOf:!actions!public! !
!StompPortableUtil categoriesFor: #bytesFrom:!actions!public! !
!StompPortableUtil categoriesFor: #bytesFromString:!converting!public! !
!StompPortableUtil categoriesFor: #characterFromUnicode:!converting!public! !
!StompPortableUtil categoriesFor: #classNamed:!actions!public! !
!StompPortableUtil categoriesFor: #classNamed:in:!actions!public! !
!StompPortableUtil categoriesFor: #colorFromRgbArray:!converting!public! !
!StompPortableUtil categoriesFor: #dateAndTimeFromNanoseconds:!converting!public! !
!StompPortableUtil categoriesFor: #dateFromSeconds:!converting!public! !
!StompPortableUtil categoriesFor: #durationFromNanoseconds:!converting!public! !
!StompPortableUtil categoriesFor: #encodeTypeMapperClass!factory!public! !
!StompPortableUtil categoriesFor: #environmentNameOf:!actions!public! !
!StompPortableUtil categoriesFor: #instVarIndexOf:for:!actions!public! !
!StompPortableUtil categoriesFor: #instVarIndicesOf:from:!actions!public! !
!StompPortableUtil categoriesFor: #instVarNamed:put:in:!actions!public! !
!StompPortableUtil categoriesFor: #isMeta:!public!testing! !
!StompPortableUtil categoriesFor: #isWideString:!public!testing! !
!StompPortableUtil categoriesFor: #isWideSymbol:!public!testing! !
!StompPortableUtil categoriesFor: #nanosecondsFromDateAndTime:!converting!public! !
!StompPortableUtil categoriesFor: #nanosecondsFromDuration:!converting!public! !
!StompPortableUtil categoriesFor: #nextAvailable:from:!actions!public! !
!StompPortableUtil categoriesFor: #popularClassMap!factory!public! !
!StompPortableUtil categoriesFor: #shouldWriteEnvironmentNameOf:!actions!public! !
!StompPortableUtil categoriesFor: #soleInstanceOf:!factory!public! !
!StompPortableUtil categoriesFor: #stringFromBytes:!converting!public! !
!StompPortableUtil categoriesFor: #timestampFromNanoseconds:!converting!public! !
!StompPortableUtil categoriesFor: #unicodeFromCharacter:!converting!public! !
!StompPortableUtil categoriesFor: #useEnvironmentByDefault!actions!public! !

!StompPortableUtil class methodsFor!

default

dialectSpecificClass

dialectSpecificClass: aClass

initialize
!StompPortableUtil class categoriesFor: #default!instance creation!public! !
!StompPortableUtil class categoriesFor: #dialectSpecificClass!factory!public! !
!StompPortableUtil class categoriesFor: #dialectSpecificClass:!factory!public! !
!StompPortableUtil class categoriesFor: #initialize!class initialization!public! !

StompShapeChanger guid: (GUID fromString: '{E4A148DA-BB56-4435-950F-7753F8DBA67C}')!
StompShapeChanger comment: ''!
!StompShapeChanger categoriesForClass!Stomp-Core! !
!StompShapeChanger methodsFor!

loadAdditions

loadInstVarAt: varIndex named: varName put: varValue 

on: anInstance 

targetInstance

targetInstance: anObject
!StompShapeChanger categoriesFor: #loadAdditions!actions!public! !
!StompShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !
!StompShapeChanger categoriesFor: #on:!initialize/release!public! !
!StompShapeChanger categoriesFor: #targetInstance!accessing!public! !
!StompShapeChanger categoriesFor: #targetInstance:!accessing!public! !

!StompShapeChanger class methodsFor!

on: anInstance 
!StompShapeChanger class categoriesFor: #on:!instance creation!public! !

StompError guid: (GUID fromString: '{5FC312C9-9E25-4E60-B41E-7145921302BE}')!
StompError comment: ''!
!StompError categoriesForClass!Stomp-Core! !
!StompError class methodsFor!

signal: aString
!StompError class categoriesFor: #signal:!instance creation!public! !

StompWarning guid: (GUID fromString: '{A13DFEA3-EFB8-476D-974E-B0A9873526EC}')!
StompWarning comment: ''!
!StompWarning categoriesForClass!Stomp-Core! !
!StompWarning methodsFor!

className

className: aValue

context

context: aValue

defaultAction

	"self resume"

	^ nil!

element

element: aValue 

signal: aClassName context: dictionary 
!StompWarning categoriesFor: #className!accessing!public! !
!StompWarning categoriesFor: #className:!accessing!public! !
!StompWarning categoriesFor: #context!accessing!public! !
!StompWarning categoriesFor: #context:!accessing!public! !
!StompWarning categoriesFor: #defaultAction!exceptionDescription!public! !
!StompWarning categoriesFor: #element!accessing!public! !
!StompWarning categoriesFor: #element:!accessing!public! !
!StompWarning categoriesFor: #signal:context:!public!signaling! !

!StompWarning class methodsFor!

initialize

signal: className

signal: className context: dictionary 

suppressSignaling

suppressSignaling: anObject

suppressTranscriptLogging

suppressTranscriptLogging: anObject
!StompWarning class categoriesFor: #initialize!class initialization!public! !
!StompWarning class categoriesFor: #signal:!instance creation!public! !
!StompWarning class categoriesFor: #signal:context:!instance creation!public! !
!StompWarning class categoriesFor: #suppressSignaling!accessing!public! !
!StompWarning class categoriesFor: #suppressSignaling:!accessing!public! !
!StompWarning class categoriesFor: #suppressTranscriptLogging!accessing!public! !
!StompWarning class categoriesFor: #suppressTranscriptLogging:!accessing!public! !

StompClassNotFound guid: (GUID fromString: '{A65C5D03-123D-4809-BFE1-C3007F8C20C1}')!
StompClassNotFound comment: ''!
!StompClassNotFound categoriesForClass!Stomp-Core! !
!StompClassNotFound methodsFor!

berstReadValue 

defaultAction

description

environmentName

environmentName: anObject

stompReadValue 

unresolvedClass
!StompClassNotFound categoriesFor: #berstReadValue!factory!public! !
!StompClassNotFound categoriesFor: #defaultAction!exceptionDescription!public! !
!StompClassNotFound categoriesFor: #description!exceptionDescription!public! !
!StompClassNotFound categoriesFor: #environmentName!accessing!public! !
!StompClassNotFound categoriesFor: #environmentName:!accessing!public! !
!StompClassNotFound categoriesFor: #stompReadValue!factory!public! !
!StompClassNotFound categoriesFor: #unresolvedClass!factory!public! !

!StompClassNotFound class methodsFor!

signal: className environment: envName context: dictionary 
!StompClassNotFound class categoriesFor: #signal:environment:context:!instance creation!public! !

StompInvalidDeserialization guid: (GUID fromString: '{F9EB4C2F-F1DE-4CD2-8C12-2C1929F5BEB1}')!
StompInvalidDeserialization comment: ''!
!StompInvalidDeserialization categoriesForClass!Stomp-Core! !
!StompInvalidDeserialization methodsFor!

description
!StompInvalidDeserialization categoriesFor: #description!exceptionDescription!public! !

StompInvalidSerialization guid: (GUID fromString: '{C037F141-E2A4-449F-9A5E-409A5B12C4FD}')!
StompInvalidSerialization comment: ''!
!StompInvalidSerialization categoriesForClass!Stomp-Core! !
!StompInvalidSerialization methodsFor!

description
!StompInvalidSerialization categoriesFor: #description!exceptionDescription!public! !

StompNewFailed guid: (GUID fromString: '{0125A207-5A26-4069-9788-6BD7D6B733C4}')!
StompNewFailed comment: ''!
!StompNewFailed categoriesForClass!Stomp-Core! !
StompReader guid: (GUID fromString: '{99D49255-24E6-4E86-803E-FFA90CAFD526}')!
StompReader comment: ''!
!StompReader categoriesForClass!Stomp-Core! !
!StompReader methodsFor!

basicReadObject

context

context: anObject

createDictionary: size

decodeFrom: aStream

identifierStringFromBytes: byteArray 

initializeInstance: instance 

next

portableUtil

readArraySized: size 

readArraySized: size atFirst: firstElem

readArraySized: size atFirstTyped: firstType

readBitsOf: aClass

readByteString

readByteSymbol

readCollectionFieldsInto: anInstance

readCollectionFieldsInto: anInstance sized: size

readFixArray: firstByte

readFixRaw: type

readIdentifierString

readIndexFieldsInto: anInstance sized: size

readInstanceContentAt: classId

readInstanceContentClassCoded: clsCode

readInstanceContentClassNamed: clsName in: envName

readInstanceContentOf: aClass

readInstVarsInto: anInstance

readInstVarsInto: anInstance fromInstVarReferenceArraySized: arraySize

readInstVarsInto: anInstance namesWithIndices: varNamesWithIndices values: varValues

readObjectOf: type ifNotApplied: aBlock

readPrimitiveValues

readRaw16

readRaw32

readReference

readSizeOfArray

readSizeOfArrayIfMatched: type

readSmallArraySized: size 

readTupleBy: tag 

readUIntIfMatched: type

readValue

readValueWithAttributes: attribDict

readWideString

readWideSymbol

remember: anObject

remember: anObject at: key

settingsClass

supportsReferenceFor: anObject

version

version: anObject
!StompReader categoriesFor: #basicReadObject!private! !
!StompReader categoriesFor: #context!accessing!public! !
!StompReader categoriesFor: #context:!accessing!public! !
!StompReader categoriesFor: #createDictionary:!factory!public! !
!StompReader categoriesFor: #decodeFrom:!decoding!public! !
!StompReader categoriesFor: #identifierStringFromBytes:!private! !
!StompReader categoriesFor: #initializeInstance:!private! !
!StompReader categoriesFor: #next!public!stream/like! !
!StompReader categoriesFor: #portableUtil!accessing!public! !
!StompReader categoriesFor: #readArraySized:!dispatching!public! !
!StompReader categoriesFor: #readArraySized:atFirst:!dispatching!public! !
!StompReader categoriesFor: #readArraySized:atFirstTyped:!dispatching!public! !
!StompReader categoriesFor: #readBitsOf:!public!reading/helper! !
!StompReader categoriesFor: #readByteString!public!reading/dispatched! !
!StompReader categoriesFor: #readByteSymbol!public!reading/dispatched! !
!StompReader categoriesFor: #readCollectionFieldsInto:!public!reading/helper! !
!StompReader categoriesFor: #readCollectionFieldsInto:sized:!public!reading/helper! !
!StompReader categoriesFor: #readFixArray:!public!reading/primitives! !
!StompReader categoriesFor: #readFixRaw:!public!reading/primitives! !
!StompReader categoriesFor: #readIdentifierString!private! !
!StompReader categoriesFor: #readIndexFieldsInto:sized:!public!reading/helper! !
!StompReader categoriesFor: #readInstanceContentAt:!public!reading/helper! !
!StompReader categoriesFor: #readInstanceContentClassCoded:!public!reading/helper! !
!StompReader categoriesFor: #readInstanceContentClassNamed:in:!public!reading/helper! !
!StompReader categoriesFor: #readInstanceContentOf:!public!reading/helper! !
!StompReader categoriesFor: #readInstVarsInto:!public!reading/helper! !
!StompReader categoriesFor: #readInstVarsInto:fromInstVarReferenceArraySized:!public!reading/instance variables! !
!StompReader categoriesFor: #readInstVarsInto:namesWithIndices:values:!public!reading/instance variables! !
!StompReader categoriesFor: #readObjectOf:ifNotApplied:!dispatching!public! !
!StompReader categoriesFor: #readPrimitiveValues!public!reading/custom! !
!StompReader categoriesFor: #readRaw16!public!reading/primitives! !
!StompReader categoriesFor: #readRaw32!public!reading/primitives! !
!StompReader categoriesFor: #readReference!public!reading/dispatched! !
!StompReader categoriesFor: #readSizeOfArray!public!reading/helper! !
!StompReader categoriesFor: #readSizeOfArrayIfMatched:!public!reading/helper! !
!StompReader categoriesFor: #readSmallArraySized:!dispatching!public! !
!StompReader categoriesFor: #readTupleBy:!dispatching!public! !
!StompReader categoriesFor: #readUIntIfMatched:!public!reading/helper! !
!StompReader categoriesFor: #readValue!public!reading/dispatched! !
!StompReader categoriesFor: #readValueWithAttributes:!public!reading/attributes! !
!StompReader categoriesFor: #readWideString!public!reading/dispatched! !
!StompReader categoriesFor: #readWideSymbol!public!reading/dispatched! !
!StompReader categoriesFor: #remember:!public!remembering! !
!StompReader categoriesFor: #remember:at:!public!remembering! !
!StompReader categoriesFor: #settingsClass!factory!public! !
!StompReader categoriesFor: #supportsReferenceFor:!public!testing! !
!StompReader categoriesFor: #version!accessing!public! !
!StompReader categoriesFor: #version:!accessing!public! !

StompWriter guid: (GUID fromString: '{58CD4585-15C1-44E0-B0A0-E460D4B99D32}')!
StompWriter comment: ''!
!StompWriter categoriesForClass!Stomp-Core! !
!StompWriter methodsFor!

addClassNameAttributeTo: attributes for: writeObject

addClassSpecifierAttributesTo: attributes for: writeObject 

addEnvironmentIdAttributeTo: attributes id: envId

addEnvironmentNameAttributeTo: attributes for: writeObject

basicWriteInstVarsOf: writeObject 

bytesFromIdentifierString: aString 

classNameOf: anObject

context

context: anObject

debugEncode: bytes

defaultVersion

nextPut: anObject

nextPutAll: aCollectionOfObject

portableUtil

putReferenceOf: anObject ifExists: aBlock

rememberReferenceOf: originalObject

settingsClass

supportsReferenceFor: anObject

writeAll: collObject

writeBitsOf: writeObject

writeByteString: aString

writeByteSymbol: aSymbol

writeContent: writeObject

writeContent: object tag: tag attributes: attribs

writeEmbeddedValue: writeObject ifNotApplied: aBlock

writeIndexFieldsOf: writeObject

writeInstVarsOf: writeObject 

writeInstVarsOf: writeObject using: cachedNamesWithIndices

writeMixedFieldsOf: writeObject 

writeNotNilInstVarsOf: writeObject 

writeObject: anObject

writePrimitiveValues: anArray

writeReference: refId

writeStandardValue: writeObject 

writeStompFrom: anObject 

writeString: aString

writeSymbol: aSymbol

writeTag: tagInt

writeValue: writeObject 

writeVersion

writeWideString: aString

writeWideSymbol: aSymbol

writtenInstVarNamesWithIndicesOf: writeObject 
!StompWriter categoriesFor: #addClassNameAttributeTo:for:!public!writing/attributes! !
!StompWriter categoriesFor: #addClassSpecifierAttributesTo:for:!public!writing/attributes! !
!StompWriter categoriesFor: #addEnvironmentIdAttributeTo:id:!public!writing/attributes! !
!StompWriter categoriesFor: #addEnvironmentNameAttributeTo:for:!public!writing/attributes! !
!StompWriter categoriesFor: #basicWriteInstVarsOf:!public!writing/helper! !
!StompWriter categoriesFor: #bytesFromIdentifierString:!private! !
!StompWriter categoriesFor: #classNameOf:!public!writing/attributes! !
!StompWriter categoriesFor: #context!accessing!public! !
!StompWriter categoriesFor: #context:!accessing!public! !
!StompWriter categoriesFor: #debugEncode:!debugging!public! !
!StompWriter categoriesFor: #defaultVersion!constants!public! !
!StompWriter categoriesFor: #nextPut:!public!stream/like! !
!StompWriter categoriesFor: #nextPutAll:!public!stream/like! !
!StompWriter categoriesFor: #portableUtil!accessing!public! !
!StompWriter categoriesFor: #putReferenceOf:ifExists:!public!testing! !
!StompWriter categoriesFor: #rememberReferenceOf:!private! !
!StompWriter categoriesFor: #settingsClass!factory!public! !
!StompWriter categoriesFor: #supportsReferenceFor:!public!testing! !
!StompWriter categoriesFor: #writeAll:!public!writing/helper! !
!StompWriter categoriesFor: #writeBitsOf:!public!writing/helper! !
!StompWriter categoriesFor: #writeByteString:!public!writing! !
!StompWriter categoriesFor: #writeByteSymbol:!public!writing! !
!StompWriter categoriesFor: #writeContent:!public!writing/helper! !
!StompWriter categoriesFor: #writeContent:tag:attributes:!public!writing/helper! !
!StompWriter categoriesFor: #writeEmbeddedValue:ifNotApplied:!public!writing! !
!StompWriter categoriesFor: #writeIndexFieldsOf:!public!writing/helper! !
!StompWriter categoriesFor: #writeInstVarsOf:!public!writing/helper! !
!StompWriter categoriesFor: #writeInstVarsOf:using:!public!writing/helper! !
!StompWriter categoriesFor: #writeMixedFieldsOf:!public!writing/helper! !
!StompWriter categoriesFor: #writeNotNilInstVarsOf:!public!writing/helper! !
!StompWriter categoriesFor: #writeObject:!dispatching!public! !
!StompWriter categoriesFor: #writePrimitiveValues:!public!writing/custom! !
!StompWriter categoriesFor: #writeReference:!public!writing! !
!StompWriter categoriesFor: #writeStandardValue:!public!writing! !
!StompWriter categoriesFor: #writeStompFrom:!dispatching!public! !
!StompWriter categoriesFor: #writeString:!public!writing! !
!StompWriter categoriesFor: #writeSymbol:!public!writing! !
!StompWriter categoriesFor: #writeTag:!public!writing/helper! !
!StompWriter categoriesFor: #writeValue:!public!writing! !
!StompWriter categoriesFor: #writeVersion!public!writing/helper! !
!StompWriter categoriesFor: #writeWideString:!public!writing! !
!StompWriter categoriesFor: #writeWideSymbol:!public!writing! !
!StompWriter categoriesFor: #writtenInstVarNamesWithIndicesOf:!public!writing/helper! !

StompSettings guid: (GUID fromString: '{726AA33C-FF79-4B21-BB39-809F5EE05DE1}')!
StompSettings comment: ''!
!StompSettings categoriesForClass!Stomp-Core! !
!StompSettings methodsFor!

supportsMultibyteIdentifiers

supportsMultibyteIdentifiers: aBoolean

suppressNilWrite

suppressNilWrite: aBoolean	

useEnvironment

useEnvironment: aBoolean

withHeader

withHeader: aBoolean

writeVariableDefinitionsAsReference

writeVariableDefinitionsAsReference: aBoolean
!StompSettings categoriesFor: #supportsMultibyteIdentifiers!accessing!public! !
!StompSettings categoriesFor: #supportsMultibyteIdentifiers:!accessing!public! !
!StompSettings categoriesFor: #suppressNilWrite!accessing!public! !
!StompSettings categoriesFor: #suppressNilWrite:!accessing!public! !
!StompSettings categoriesFor: #useEnvironment!accessing!public! !
!StompSettings categoriesFor: #useEnvironment:!accessing!public! !
!StompSettings categoriesFor: #withHeader!accessing!public! !
!StompSettings categoriesFor: #withHeader:!accessing!public! !
!StompSettings categoriesFor: #writeVariableDefinitionsAsReference!accessing!public! !
!StompSettings categoriesFor: #writeVariableDefinitionsAsReference:!accessing!public! !

StompReadContext guid: (GUID fromString: '{ABD60D1E-C3E4-46B7-8654-2297ADD2EBD0}')!
StompReadContext comment: ''!
!StompReadContext categoriesForClass!Stomp-Core! !
!StompReadContext methodsFor!

classAliasesDictionary

classAliasesDictionary: anObject

classAt: classId

classIdsDictionary

classIdsDictionary: anObject

classNamed: localClassName in: environmentQualifier 

clearFieldsInfo

environmentNameAt: envId

environmentNamesDictionary

environmentNamesDictionary: anObject

fieldsInfo

fieldsInfo: anObject

instVarNamesWithIndicesAt: classId

instVarNamesWithIndicesDictionary

instVarNamesWithIndicesDictionary: anObject

mappedClassAt: classNameSymbol in: environmentQualifier 

mappedClassNameAt: classNameSymbol in: environmentQualifier 

objectDictionaryClass

registerClassOldName: oldClassNameSymbol for: existingClass

registerClassOldName: oldClassNameSymbol in: oldEnvironmentQualifier for: existingClass

registerClassOldName: oldClassNameSymbol in: oldEnvironmentQualifier shapeChanger: shapeChangerClass for: existingClass

registerClassOldName: oldClassNameSymbol shapeChanger: shapeChangerClass for: existingClass

registerShapeChanger: shapeChangerObject for: existingClass

registerShapeChangerRenameBy: loadInstVarsBlock for: existingClass

registerShapeChangerRenameBy: loadInstVarsBlock initializeBy: loadAdditionsBlock for: existingClass

rememberClass: aClass

rememberEnvironmentName: aString

rememberInstVarNames: instVarNames indices: instVarIndices of: aClass

shapeChangerFor: existingClass

shapeChangersDictionary

shapeChangersDictionary: anObject
!StompReadContext categoriesFor: #classAliasesDictionary!accessing!public! !
!StompReadContext categoriesFor: #classAliasesDictionary:!accessing!public! !
!StompReadContext categoriesFor: #classAt:!actions!public! !
!StompReadContext categoriesFor: #classIdsDictionary!accessing!public! !
!StompReadContext categoriesFor: #classIdsDictionary:!accessing!public! !
!StompReadContext categoriesFor: #classNamed:in:!actions!public! !
!StompReadContext categoriesFor: #clearFieldsInfo!initialize/release!public! !
!StompReadContext categoriesFor: #environmentNameAt:!actions!public! !
!StompReadContext categoriesFor: #environmentNamesDictionary!accessing!public! !
!StompReadContext categoriesFor: #environmentNamesDictionary:!accessing!public! !
!StompReadContext categoriesFor: #fieldsInfo!accessing!public! !
!StompReadContext categoriesFor: #fieldsInfo:!accessing!public! !
!StompReadContext categoriesFor: #instVarNamesWithIndicesAt:!actions!public! !
!StompReadContext categoriesFor: #instVarNamesWithIndicesDictionary!accessing!public! !
!StompReadContext categoriesFor: #instVarNamesWithIndicesDictionary:!accessing!public! !
!StompReadContext categoriesFor: #mappedClassAt:in:!private! !
!StompReadContext categoriesFor: #mappedClassNameAt:in:!private! !
!StompReadContext categoriesFor: #objectDictionaryClass!defaults!public! !
!StompReadContext categoriesFor: #registerClassOldName:for:!public!renaming! !
!StompReadContext categoriesFor: #registerClassOldName:in:for:!public!renaming! !
!StompReadContext categoriesFor: #registerClassOldName:in:shapeChanger:for:!public!renaming! !
!StompReadContext categoriesFor: #registerClassOldName:shapeChanger:for:!public!renaming! !
!StompReadContext categoriesFor: #registerShapeChanger:for:!public!shape changing! !
!StompReadContext categoriesFor: #registerShapeChangerRenameBy:for:!public!shape changing! !
!StompReadContext categoriesFor: #registerShapeChangerRenameBy:initializeBy:for:!public!shape changing! !
!StompReadContext categoriesFor: #rememberClass:!actions!public! !
!StompReadContext categoriesFor: #rememberEnvironmentName:!actions!public! !
!StompReadContext categoriesFor: #rememberInstVarNames:indices:of:!actions!public! !
!StompReadContext categoriesFor: #shapeChangerFor:!public!shape changing! !
!StompReadContext categoriesFor: #shapeChangersDictionary!accessing!public! !
!StompReadContext categoriesFor: #shapeChangersDictionary:!accessing!public! !

StompWriteContext guid: (GUID fromString: '{CFF8B7E0-19AA-4B93-A358-952BD40ED57B}')!
StompWriteContext comment: ''!
!StompWriteContext categoriesForClass!Stomp-Core! !
!StompWriteContext methodsFor!

classesDictionary

classesDictionary: anObject

classIdOf: aClass

environmentIdOf: aClass

environmentsDictionary

environmentsDictionary: anObject

includesClass: aClass

includesEnvironment: anEnvironment

instVarNamesWithIndicesDictionary

instVarNamesWithIndicesDictionary: anObject

instVarNamesWithIndicesOf: aClass

rememberClass: aClass

rememberEnvironment: anEnvironment

rememberInstVarNames: instVarNames indices: instVarIndices of: aClass

rememberReferenceOf: originalObject
!StompWriteContext categoriesFor: #classesDictionary!accessing!public! !
!StompWriteContext categoriesFor: #classesDictionary:!accessing!public! !
!StompWriteContext categoriesFor: #classIdOf:!actions!public! !
!StompWriteContext categoriesFor: #environmentIdOf:!actions!public! !
!StompWriteContext categoriesFor: #environmentsDictionary!accessing!public! !
!StompWriteContext categoriesFor: #environmentsDictionary:!accessing!public! !
!StompWriteContext categoriesFor: #includesClass:!actions!public! !
!StompWriteContext categoriesFor: #includesEnvironment:!actions!public! !
!StompWriteContext categoriesFor: #instVarNamesWithIndicesDictionary!accessing!public! !
!StompWriteContext categoriesFor: #instVarNamesWithIndicesDictionary:!accessing!public! !
!StompWriteContext categoriesFor: #instVarNamesWithIndicesOf:!actions!public! !
!StompWriteContext categoriesFor: #rememberClass:!actions!public! !
!StompWriteContext categoriesFor: #rememberEnvironment:!actions!public! !
!StompWriteContext categoriesFor: #rememberInstVarNames:indices:of:!actions!public! !
!StompWriteContext categoriesFor: #rememberReferenceOf:!actions!public! !

StompBlockShapeChanger guid: (GUID fromString: '{53499F9F-DC9C-4493-8371-8A4BC227378B}')!
StompBlockShapeChanger comment: ''!
!StompBlockShapeChanger categoriesForClass!Stomp-Core! !
!StompBlockShapeChanger methodsFor!

loadAdditions

loadAdditionsBlock

loadAdditionsBlock: anObject

loadInstVarAt: varIndex named: varName put: varValue 

loadInstVarsBlock

loadInstVarsBlock: anObject
!StompBlockShapeChanger categoriesFor: #loadAdditions!actions!public! !
!StompBlockShapeChanger categoriesFor: #loadAdditionsBlock!accessing!public! !
!StompBlockShapeChanger categoriesFor: #loadAdditionsBlock:!accessing!public! !
!StompBlockShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !
!StompBlockShapeChanger categoriesFor: #loadInstVarsBlock!accessing!public! !
!StompBlockShapeChanger categoriesFor: #loadInstVarsBlock:!accessing!public! !

!StompBlockShapeChanger class methodsFor!

loadInstVarsBlock: loadInstVarsBlock loadAdditionsBlock: loadAdditionsBlock
!StompBlockShapeChanger class categoriesFor: #loadInstVarsBlock:loadAdditionsBlock:!instance creation!public! !

"Binary Globals"!
