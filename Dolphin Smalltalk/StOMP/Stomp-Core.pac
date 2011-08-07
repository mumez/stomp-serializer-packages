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

fromStomp: bytes	^ StompReader decode: bytes!

fromStomp: bytes setting: aBlock	| context reader |	reader := StompReader new.	context := reader context.	aBlock value: context.	^reader decode: bytes.!

stompCreateBitsInstanceFrom: stompReader	^stompReader readBitsOf: self!

stompCreateFixedInstanceFrom: stompReader	^self stompCreateInstance!

stompCreateInstance	^[self new]		on: Error		do: [:ex | 			| alterClass | 			alterClass := StompNewFailed signal: self name.			alterClass isNil				ifTrue: [self basicNew]				ifFalse: [alterClass basicNew]]!

stompCreateInstance: size	^ [self new: size]		on: Error		do: [:ex | 			| alterClass | 			alterClass := StompNewFailed signal: self name.			alterClass isNil				ifTrue: [[self basicNew: size] on: Error do: [self stompCreateInstance]]				ifFalse: [alterClass basicNew: size]]!

stompCreateInstanceFrom: stompReader	| cls |	stompReader context clearFieldsInfo.	cls := self.	cls isBits ifTrue: [^ self stompCreateBitsInstanceFrom: stompReader].	cls isVariable		ifTrue: [^ self stompCreateMixedInstanceFrom: stompReader].			^ self stompCreateFixedInstanceFrom: stompReader!

stompCreateMixedInstanceFrom: stompReader	| type size indexFieldSize  |	type := stompReader readType. 	size := stompReader readSizeOfArrayIfMatched: type.	size = 0 ifTrue: [^ self stompCreateInstance].	size >= 1 ifTrue: [		stompReader context fieldsInfo: (StompFieldsInfo pureIndexFieldsSized: size).		^ self stompCreateInstance: size. 	].	"Otherwise, mixed fields"	indexFieldSize := stompReader readUIntIfMatched: type.	stompReader context fieldsInfo: (StompFieldsInfo mixedFields).	^ self stompCreateInstance: indexFieldSize!

stompFromBytes: rawBytes	"For bits object"	"override - if nessesary"	^StompPortableUtil default bytes: rawBytes intoOf: self. 	! !
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

stompWriteContentTo: stompWriter	StompInvalidSerialization signal: self class name.	stompWriter writeObject: self printString! !
!BlockClosure categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!BlockClosure class methodsFor!

stompCreateInstanceFrom: stompReader	StompInvalidDeserialization signal: stompReader readObject.	^nil! !
!BlockClosure class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Boolean methodsFor!

stompShouldWriteInstanceVariables	^false!

stompSupportsReference: stompContext	^false	! !
!Boolean categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Boolean categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !

!Character methodsFor!

stompWriteContentTo: stompWriter	stompWriter writeObject: (StompPortableUtil default unicodeFromCharacter: self)! !
!Character categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Character class methodsFor!

stompCreateInstanceFrom: stompReader	^ StompPortableUtil default characterFromUnicode: stompReader readObject! !
!Character class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!ClassDescription methodsFor!

stompLoadContentsOnCreation	"override"	self isBits ifTrue: [^true]. 	^ self class includesSelector: #stompCreateInstanceFrom: "Typically"	!

stompTransientInstVarNames	^Object class allInstVarNames! !
!ClassDescription categoriesFor: #stompLoadContentsOnCreation!*Stomp/Core/testing!public! !
!ClassDescription categoriesFor: #stompTransientInstVarNames!*Stomp/Core/writing!public! !

!Collection methodsFor!

stompAdd: elem at: idx	"override"	self add: elem!

stompDo: aBlock	"override"	self do: aBlock!

stompReadContentFrom: stompReader 	| fieldsInfo |	fieldsInfo := stompReader context fieldsInfo.	fieldsInfo isNil ifTrue: [^self].	fieldsInfo isPureIndexFields ifTrue: [ ^ stompReader readCollectionFieldsInto: self sized: fieldsInfo indexFieldSize].	fieldsInfo isMixedFields ifTrue: [		self stompShouldWriteInstanceVariables ifTrue: [stompReader readInstVarsInto: self].		stompReader readCollectionFieldsInto: self.	].		^self!

stompShouldWriteInstanceVariables	"override"	"Usually Collection's inst vars are not needed for serialization"	^false	!

stompWriteContentTo: stompWriter 	"Override as you wish"	"Provides basic dispatch to stompWriter"	| cls |	cls := self class.	cls isBits ifTrue: [^ stompWriter writeBitsOf: self].	^ stompWriter writeMixedFieldsOf: self! !
!Collection categoriesFor: #stompAdd:at:!*Stomp/Core/writing!public! !
!Collection categoriesFor: #stompDo:!*Stomp/Core/writing!public! !
!Collection categoriesFor: #stompReadContentFrom:!*Stomp/Core/reading!public! !
!Collection categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Collection categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Collection class methodsFor!

stompCreateInstanceFrom: stompReader	| cls |	stompReader context clearFieldsInfo.	cls := self.	cls isBits ifTrue: [^ self stompCreateBitsInstanceFrom: stompReader].	^ self stompCreateCollectionInstanceFrom: stompReader				! !
!Collection class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Date methodsFor!

stompShouldWriteInstanceVariables	^false!

stompSupportsReference: stompContext	^false!

stompWriteContentTo: stompWriter	stompWriter writeObject: self asSeconds! !
!Date categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Date categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !
!Date categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Date class methodsFor!

stompCreateInstanceFrom: stompReader	^ StompPortableUtil default dateFromSeconds: stompReader readObject! !
!Date class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Fraction methodsFor!

stompWriteContentTo: stompWriter	stompWriter writePrimitiveValues: (Array with: self numerator with: self denominator)! !
!Fraction categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Fraction class methodsFor!

stompCreateInstanceFrom: stompReader	| fraArray |	fraArray := stompReader readPrimitiveValues.	^self numerator: (fraArray at: 1) denominator: (fraArray at: 2)! !
!Fraction class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!IdentityDictionary methodsFor!

stompWriteContentTo: stompWriter	stompWriter writeMap: self.		! !
!IdentityDictionary categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!IdentityDictionary class methodsFor!

stompCreateInstanceFrom: stompReader	| dic inst |	dic := stompReader readObject.	inst := self new.	dic keysAndValuesDo: [:k :v | inst at: k put: v].		^ inst! !
!IdentityDictionary class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Interval methodsFor!

stompWriteContentTo: stompWriter	stompWriter writePrimitiveValues: (Array with: start with: stop with: step)! !
!Interval categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Interval class methodsFor!

stompCreateInstanceFrom: stompReader	| intervalArray |	intervalArray := stompReader readPrimitiveValues.	^self from: (intervalArray at: 1) to: (intervalArray at: 2) by: (intervalArray at: 3)! !
!Interval class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Metaclass methodsFor!

stompCreateInstance	^ StompPortableUtil default soleInstanceOf: self! !
!Metaclass categoriesFor: #stompCreateInstance!*Stomp/Core/instance creation!public! !

!MpPortableUtil methodsFor!

stompUtil	^ StompPortableUtil default! !
!MpPortableUtil categoriesFor: #stompUtil!*Stomp/core/accessing!public! !

!Number methodsFor!

stompShouldWriteInstanceVariables	^false	!

stompSupportsReference: stompContext	^false	! !
!Number categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Number categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !

!Object methodsFor!

stompAfterWrite!

stompAt: index put: aValue 	"override if you like"	self at: index put: aValue !

stompBasicReadContentFrom: stompReader 	| fieldsInfo |	self class isFixed ifTrue: [^ stompReader readInstVarsInto: self].		fieldsInfo := stompReader context fieldsInfo.	fieldsInfo isPureIndexFields ifTrue: [ ^ stompReader readIndexFieldsInto: self sized: fieldsInfo indexFieldSize].	fieldsInfo isMixedFields ifTrue: [		self stompShouldWriteInstanceVariables ifTrue: [stompReader readInstVarsInto: self].		stompReader readIndexFieldsInto: self sized: stompReader readSizeOfArray.	].		^self!

stompBytes	"For bytes, words object"	"override"	^StompPortableUtil default bytesFrom: self !

stompDo: aBlock	self class isFixed ifTrue:[		"I'm not an Collection - so just write myself"		^ aBlock value: self	].	self size = 0 ifTrue: [^ aBlock value: self].	1 to: self size do: [:idx |		aBlock value: (self at: idx)	].!

stompInitialize	"override if you like"	!

stompInstVarAt: instVarIndex named: varName put: aValue 	instVarIndex = 0 ifTrue: [		^ StompPortableUtil default instVarNamed: varName put: aValue in: self	].	self instVarAt: instVarIndex put: aValue!

stompInstVarNamed: varName writtenAs: writtenValue	"override"	^ writtenValue!

stompPrepareWrite!

stompReadContentFrom: stompReader 	"override"	^self stompBasicReadContentFrom: stompReader 	!

stompReadValue	"override if you like"	^self!

stompShouldWriteInstanceVariables	^true	!

stompSupportsReference: stompContext	"override"	^true	!

stompTransientInstVarNames	"override"	^#()!

stompValueContentSize	"For Mixed class which would like to write instance variables"	((self stompShouldWriteInstanceVariables			and: [self class isVariable])			and: [self class instSize > 0])		ifTrue: [^ 3].	^ 1!

stompWriteContentTo: stompWriter 	"Override as you wish"	"Provides basic dispatch to stompWriter"	| cls |	cls := self class.	cls isBits ifTrue: [^ stompWriter writeBitsOf: self].	cls isVariable ifTrue: [^ stompWriter writeMixedFieldsOf: self].	^ stompWriter writeInstVarsOf: self!

stompWriteValue	^self!

toStomp	^ StompWriter encode: self! !
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

stompWriteContentTo: stompWriter	| rectArray |	rectArray := Array with: self x with: self y.	stompWriter writePrimitiveValues: rectArray! !
!Point categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Point class methodsFor!

stompCreateInstanceFrom: stompReader	| rectArray |	rectArray := stompReader readPrimitiveValues.	^ (rectArray at: 1) @ (rectArray at: 2)! !
!Point class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Rectangle methodsFor!

stompWriteContentTo: stompWriter	| rectArray |	rectArray := Array with: self origin x with: self origin y with: self corner x with: self corner y.	stompWriter writePrimitiveValues: rectArray! !
!Rectangle categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Rectangle class methodsFor!

stompCreateInstanceFrom: stompReader	| rectArray |	rectArray := stompReader readPrimitiveValues.	^ (rectArray at: 1) @ (rectArray at: 2) corner: (rectArray at: 3) @ (rectArray at: 4)! !
!Rectangle class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!RunArray methodsFor!

stompWriteContentTo: stompWriter	stompWriter writePrimitiveValues: (Array with: runs with: values)! !
!RunArray categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!RunArray class methodsFor!

stompCreateInstanceFrom: stompReader	| fraArray |	fraArray := stompReader readPrimitiveValues.	^self runs: (fraArray at: 1) values: (fraArray at: 2)! !
!RunArray class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!String methodsFor!

stompSupportsReference: stompContext	^false! !
!String categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !

!Symbol methodsFor!

stompSupportsReference: stompContext	^false! !
!Symbol categoriesFor: #stompSupportsReference:!*Stomp/Core!public! !

!Symbol class methodsFor!

stompCreateInstanceFrom: stompReader	^ (super stompCreateInstanceFrom: stompReader) asSymbol! !
!Symbol class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!Time methodsFor!

stompShouldWriteInstanceVariables	^false!

stompSupportsReference: stompContext	^false!

stompWriteContentTo: stompWriter	stompWriter writeObject: self asSeconds! !
!Time categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core/testing!public! !
!Time categoriesFor: #stompSupportsReference:!*Stomp/Core/testing!public! !
!Time categoriesFor: #stompWriteContentTo:!*Stomp/Core/writing!public! !

!Time class methodsFor!

stompCreateInstanceFrom: stompReader	^self fromSeconds: stompReader readObject! !
!Time class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Core/instance creation!public! !

!UndefinedObject methodsFor!

stompShouldWriteInstanceVariables	^false!

stompSupportsReference: stompContext	^false	! !
!UndefinedObject categoriesFor: #stompShouldWriteInstanceVariables!*Stomp/Core!public! !
!UndefinedObject categoriesFor: #stompSupportsReference:!*Stomp/Core!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StompConstants guid: (GUID fromString: '{4CF03C6E-8917-493C-8A81-EBE29A02BEA3}')!
StompConstants comment: ''!
!StompConstants categoriesForClass!Stomp-Core! !
!StompConstants class methodsFor!

byteString	^16r04!

byteSymbol	^16r05!

classCode	^16r15!

classId	^16r12!

environmentId	^16r14!

environmentName	^16r13!

header	^'SP' asByteArray!

initialize	"self initialize"	"self initTupleTags"	!

initTupleTags	"self initTupleTags"	tupleTags := Set new.	tupleTags add: self value.	tupleTags add: self reference.	tupleTags add: self byteString.	tupleTags add: self byteSymbol.	tupleTags add: self wideString.	tupleTags add: self wideSymbol.!

isTupleTag: tag	^tag isInteger and: [self tupleTags includes: tag]!

klassName	^16r11!

reference	^16r03!

tupleTags	^tupleTags!

value	^16r02!

wideString	^16r06!

wideSymbol	^16r07! !
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

at: key	^self objectsDictionary at: key!

at: key ifAbsent: block	^self objectsDictionary at: key ifAbsent: block!

at: key ifAbsentPut: value	^self objectsDictionary at: key ifAbsentPut: value!

at: key put: value	^self objectsDictionary at: key put: value!

includesKey: key 	^self objectsDictionary includesKey: key!

keys	^self objectsDictionary keys!

keysAndValuesDo: block	^self objectsDictionary keysAndValuesDo: block!

objectDictionaryClass	^IdentityDictionary!

objectsDictionary	"Answer the value of objectsDictionary"	^ objectsDictionary ifNil: [objectsDictionary := self objectDictionaryClass new]!

objectsDictionary: anObject	"Set the value of objectsDictionary"	objectsDictionary := anObject!

printOn: aStream	aStream nextPutAll: self class name.	aStream nextPutAll: '('.	aStream cr.	self objectsDictionary associationsDo: [:assoc |		assoc printOn: aStream.		aStream cr.	].	aStream nextPutAll: ')'.!

removeKey: key 	^self objectsDictionary removeKey: key!

removeKey: key ifAbsent: block	^self objectsDictionary removeKey: key ifAbsent: block!

requestor	^ requestor!

requestor: anObject	"Set the value of requestor"	requestor := anObject!

settings	^self requestor settings!

size	^self objectsDictionary size!

values	^self objectsDictionary values! !
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

on: requestor 	^ self new requestor: requestor;		 yourself! !
!StompContext class categoriesFor: #on:!instance creation!public! !

StompFieldsInfo guid: (GUID fromString: '{AB84D163-49B2-4D8D-BD35-AD1945949EB3}')!
StompFieldsInfo comment: ''!
!StompFieldsInfo categoriesForClass!Stomp-Core! !
!StompFieldsInfo methodsFor!

indexFieldSize	"Answer the value of indexFieldSize"	^ indexFieldSize!

indexFieldSize: anObject	"Set the value of indexFieldSize"	indexFieldSize := anObject!

isMixedFields	^ self type == #mixedFields!

isPureIndexFields	^ self type == #pureIndexFields!

type	"Answer the value of type"	^ type!

type: anObject	"Set the value of type"	type := anObject! !
!StompFieldsInfo categoriesFor: #indexFieldSize!accessing!public! !
!StompFieldsInfo categoriesFor: #indexFieldSize:!accessing!public! !
!StompFieldsInfo categoriesFor: #isMixedFields!public!testing! !
!StompFieldsInfo categoriesFor: #isPureIndexFields!public!testing! !
!StompFieldsInfo categoriesFor: #type!accessing!public! !
!StompFieldsInfo categoriesFor: #type:!accessing!public! !

!StompFieldsInfo class methodsFor!

mixedFields	^ self type: #mixedFields!

pureIndexFields	^ self type: #pureIndexFields!

pureIndexFieldsSized: numOfFields	^ self pureIndexFields indexFieldSize: numOfFields!

type: typeSymbol	^ self new type: typeSymbol; yourself! !
!StompFieldsInfo class categoriesFor: #mixedFields!instance creation!public! !
!StompFieldsInfo class categoriesFor: #pureIndexFields!instance creation!public! !
!StompFieldsInfo class categoriesFor: #pureIndexFieldsSized:!instance creation!public! !
!StompFieldsInfo class categoriesFor: #type:!instance creation!public! !

StompPopularClassMap guid: (GUID fromString: '{0ADD9D6A-8BCA-45AA-A73B-4BED8110C3EB}')!
StompPopularClassMap comment: ''!
!StompPopularClassMap categoriesForClass!Stomp-Core! !
!StompPopularClassMap methodsFor!

associationClass	^ self classNamed: #Association!

bagClass	^ self classNamed: #Bag!

byteStringClass	^ self classNamed: #ByteString!

byteSymbolClass	^ self classNamed: #ByteSymbol!

characterClass	^ self classNamed: #Character!

classAt: code 	^self codeToClass at: code ifAbsent: [].	!

classNamed: localClassName	^Smalltalk at: localClassName ifAbsent: []!

classToCode	"Answer the value of classToCode"	^ classToCode!

classToCode: anObject	"Set the value of classToCode"	classToCode := anObject!

codeAt: aClass ifPresent: aBlock	| code |	code := self classToCode at: aClass ifAbsent: [].	code ifNotNil: [aBlock value: code]!

codeToClass	"Answer the value of codeToClass"	^ codeToClass!

codeToClass: anObject	"Set the value of codeToClass"	codeToClass := anObject!

colorClass	^ self classNamed: #Color!

compactDictionaryClass	^ self classNamed: #CompactDictionary!

compiledMethodClass	^ self classNamed: #CompiledMethod!

dateAndTimeClass	^ self classNamed: #DateAndTime!

dateClass	^ self classNamed: #Date!

durationClass	^ self classNamed: #Duration!

fixedPointClass	^ self classNamed: #FixedPoint!

fractionClass	^ self classNamed: #Fraction!

identityDictionaryClass	^ self classNamed: #IdentityDictionary!

identitySetClass	^ self classNamed: #IdentitySet!

initialize	classToCode := IdentityDictionary new.	codeToClass := IdentityDictionary new.	self prepareMaps!

intervalClass	^ self classNamed: #Interval!

matrixClass	^ self classNamed: #Matrix!

orderedCollectionClass	^ self classNamed: #OrderedCollection!

orderedSetClass	^ self classNamed: #OrderedSet!

pointClass	^ self classNamed: #Point!

popularClassSelectors	"Reserved popular classes - if you extend class map, the array should only be appended."	"^(self class organization listAtCategoryNamed: #factory) asSortedCollection."		^ #(#bagClass #compactDictionaryClass #characterClass #colorClass #dateAndTimeClass #dateClass #fractionClass #identityDictionaryClass #identitySetClass #intervalClass #orderedCollectionClass #pointClass #rectangleClass #setClass #sortedCollectionClass #matrixClass #durationClass #timeClass #timestampClass #runArrayClass #orderedSetClass #uuidClass #fixedPointClass #associationClass #compiledMethodClass #uint16ArrayClass #uint32ArrayClass).!

prepareClassToCodeMap: classToCodeMap	"By default, just create counter-map"	self codeToClass keysAndValuesDo: [:key :value |		classToCodeMap at: value put: key	]!

prepareCodeToClassMap: codeToClassMap	| selectors |	selectors := self popularClassSelectors.	1 to: selectors size do: [:idx | | kls |		kls := self perform: (selectors at: idx).		kls ifNotNil: [codeToClassMap at: idx put: kls].		]!

prepareMaps		self prepareCodeToClassMap: self codeToClass.	self prepareClassToCodeMap: self classToCode.!

rectangleClass	^ self classNamed: #Rectangle!

runArrayClass	^ self classNamed: #RunArray!

setClass	^ self classNamed: #Set!

sortedCollectionClass	^ self classNamed: #SortedCollection!

timeClass	^ self classNamed: #Time!

timestampClass	^ self classNamed: #Timestamp!

uint16ArrayClass	^ self classNamed: #WordArray!

uint32ArrayClass	^ self classNamed: #DwordArray!

uuidClass	^ self classNamed: #UUID! !
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

default	"Answer the value of default"	^ default ifNil: [default := super new initialize]!

initialize	"self initialize"	self class = StompPopularClassMap ifTrue: [^self initializeAll].	default := nil	!

initializeAll	"self initializeAll"	self allSubclasses do: [:each | each initialize]! !
!StompPopularClassMap class categoriesFor: #default!accessing!public! !
!StompPopularClassMap class categoriesFor: #initialize!class initialization!public! !
!StompPopularClassMap class categoriesFor: #initializeAll!class initialization!public! !

StompPortableUtil guid: (GUID fromString: '{97BABC13-A00D-4D58-A252-406C35C6CBEB}')!
StompPortableUtil comment: ''!
!StompPortableUtil categoriesForClass!Stomp-Core! !
!StompPortableUtil methodsFor!

bytes: rawBytes intoOf: bitsClass	"override"	^bitsClass new: rawBytes size!

bytesFrom: bitsObject	"override"	^bitsObject	!

bytesFromString: aString	^aString asByteArray!

characterFromUnicode: anInteger	^Character value: anInteger!

classNamed: localClassName	"override"	^ Smalltalk		at: localClassName		ifAbsent: []!

classNamed: localClassName in: environmentQualifier 	"override"	| env |	environmentQualifier ifNil: [^ self classNamed: localClassName].					"Suppose namespace is not supported, so just use Smalltalk"	env :=  Smalltalk.	^ env		at: localClassName		ifAbsent: []!

colorFromRgbArray: rgbArray	^nil!

dateAndTimeFromNanoseconds: nanoseconds	^ self timestampFromNanoseconds: nanoseconds!

dateFromSeconds: seconds	^ Date fromSeconds: seconds!

durationFromNanoseconds: nanoseconds	"^Duration fromNanoseconds: nanoseconds"	self subclassResponsibility !

encodeTypeMapperClass	^MpEncodeTypeMapper!

environmentNameOf: anObject	^#Smalltalk!

instVarIndexOf: aClass for: varName 	"override"	self subclassResponsibility.	^0!

instVarIndicesOf: aClass from: instVarNames	^ instVarNames collect: [:each |		self instVarIndexOf: aClass for: each	] !

instVarNamed: varName put: value in: anObject	"Note that when varName is invalid, just silently ignore"	| index |	index := self instVarIndexOf: anObject class for: varName.	index = 0 ifTrue: [^self].	anObject instVarAt: index put: value			!

isMeta: aBehavior	^aBehavior isMeta!

isWideString: aString	"override"	^false!

isWideSymbol: aSymbol	"override"	^false!

nanosecondsFromDateAndTime: timestamp	"Answer the number of nanoseconds since January 1, 1901."	"^timestamp asNanoseconds"	self subclassResponsibility !

nanosecondsFromDuration: duration	self subclassResponsibility !

nextAvailable: size from: stream	"even reached at end, just return contents as-is"	^stream nextAvailable: size!

popularClassMap	"override"	^StompPopularClassMap default!

shouldWriteEnvironmentNameOf: anObject	^((self environmentNameOf: anObject) ~~ #Smalltalk)!

soleInstanceOf: aMetaclass	^ aMetaclass soleInstance!

stringFromBytes: aByteArray	^aByteArray asString !

timestampFromNanoseconds: nanoseconds	"^ Timestamp fromNanoseconds: nanoseconds"	self subclassResponsibility !

unicodeFromCharacter: aCharacter	^aCharacter asciiValue!

useEnvironmentByDefault	^true! !
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

default	^Default ifNil: [Default := self dialectSpecificClass new]!

dialectSpecificClass	^DialectSpecificClass ifNil: [DialectSpecificClass := self subclasses at: 1]!

dialectSpecificClass: aClass	DialectSpecificClass := aClass!

initialize	Default := nil.	DialectSpecificClass := nil! !
!StompPortableUtil class categoriesFor: #default!instance creation!public! !
!StompPortableUtil class categoriesFor: #dialectSpecificClass!factory!public! !
!StompPortableUtil class categoriesFor: #dialectSpecificClass:!factory!public! !
!StompPortableUtil class categoriesFor: #initialize!class initialization!public! !

StompShapeChanger guid: (GUID fromString: '{E4A148DA-BB56-4435-950F-7753F8DBA67C}')!
StompShapeChanger comment: ''!
!StompShapeChanger categoriesForClass!Stomp-Core! !
!StompShapeChanger methodsFor!

loadAdditions	"override"!

loadInstVarAt: varIndex named: varName put: varValue 	"override"	 self targetInstance stompInstVarAt: varIndex named: varName put: varValue !

on: anInstance 	self targetInstance: anInstance!

targetInstance	"Answer the value of targetInstance"	^ targetInstance!

targetInstance: anObject	"Set the value of targetInstance"	targetInstance := anObject! !
!StompShapeChanger categoriesFor: #loadAdditions!actions!public! !
!StompShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !
!StompShapeChanger categoriesFor: #on:!initialize/release!public! !
!StompShapeChanger categoriesFor: #targetInstance!accessing!public! !
!StompShapeChanger categoriesFor: #targetInstance:!accessing!public! !

!StompShapeChanger class methodsFor!

on: anInstance 	^ self new on: anInstance;		 yourself! !
!StompShapeChanger class categoriesFor: #on:!instance creation!public! !

StompError guid: (GUID fromString: '{5FC312C9-9E25-4E60-B41E-7145921302BE}')!
StompError comment: ''!
!StompError categoriesForClass!Stomp-Core! !
!StompError class methodsFor!

signal: aString	| inst |	inst := self new.	inst messageText: aString.	^MpPortableUtil default signalException: inst! !
!StompError class categoriesFor: #signal:!instance creation!public! !

StompWarning guid: (GUID fromString: '{A13DFEA3-EFB8-476D-974E-B0A9873526EC}')!
StompWarning comment: ''!
!StompWarning categoriesForClass!Stomp-Core! !
!StompWarning methodsFor!

className	className ifNil: [^ className].	^className asSymbol!

className: aValue	className := aValue!

context	^context!

context: aValue	context := aValue!

defaultAction	self class suppressTranscriptLogging ifFalse: [Transcript cr; show: '#warning# ' , self description].

	"self resume"

	^ nil!

element	^ element!

element: aValue 	element := aValue!

signal: aClassName context: dictionary 	self class suppressSignaling ifTrue: [^self].	self className: aClassName.	self context: dictionary.	^MpPortableUtil default signalException: self! !
!StompWarning categoriesFor: #className!accessing!public! !
!StompWarning categoriesFor: #className:!accessing!public! !
!StompWarning categoriesFor: #context!accessing!public! !
!StompWarning categoriesFor: #context:!accessing!public! !
!StompWarning categoriesFor: #defaultAction!exceptionDescription!public! !
!StompWarning categoriesFor: #element!accessing!public! !
!StompWarning categoriesFor: #element:!accessing!public! !
!StompWarning categoriesFor: #signal:context:!public!signaling! !

!StompWarning class methodsFor!

initialize	"StompWarning initialize"	suppressSignaling := nil.	suppressTranscriptLogging := nil!

signal: className	^self signal: className context: nil !

signal: className context: dictionary 	^ self new signal: className context: dictionary !

suppressSignaling	^suppressSignaling ifNil: [suppressSignaling := false].!

suppressSignaling: anObject	"Set the value of suppressSignaling"	suppressSignaling := anObject!

suppressTranscriptLogging	^suppressTranscriptLogging ifNil: [suppressTranscriptLogging := false]!

suppressTranscriptLogging: anObject	"Set the value of suppressTranscriptLogging"	suppressTranscriptLogging := anObject! !
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

berstReadValue 	^ nil!

defaultAction	Transcript cr; show: '#warning# ' , self description.	^ self unresolvedClass!

description	^ super description , ': ' , self className printString!

environmentName	"Answer the value of environmentName"	environmentName ifNil: [^ environmentName].	^ environmentName!

environmentName: anObject	"Set the value of environmentName"	environmentName := anObject!

stompReadValue 	^ nil!

unresolvedClass	"By default my class act as an unresolved class, which instantiate nil"	^self class! !
!StompClassNotFound categoriesFor: #berstReadValue!factory!public! !
!StompClassNotFound categoriesFor: #defaultAction!exceptionDescription!public! !
!StompClassNotFound categoriesFor: #description!exceptionDescription!public! !
!StompClassNotFound categoriesFor: #environmentName!accessing!public! !
!StompClassNotFound categoriesFor: #environmentName:!accessing!public! !
!StompClassNotFound categoriesFor: #stompReadValue!factory!public! !
!StompClassNotFound categoriesFor: #unresolvedClass!factory!public! !

!StompClassNotFound class methodsFor!

signal: className environment: envName context: dictionary 	| inst |	inst := self new.	inst environmentName: envName.	^ inst signal: className context: dictionary ! !
!StompClassNotFound class categoriesFor: #signal:environment:context:!instance creation!public! !

StompInvalidDeserialization guid: (GUID fromString: '{F9EB4C2F-F1DE-4CD2-8C12-2C1929F5BEB1}')!
StompInvalidDeserialization comment: ''!
!StompInvalidDeserialization categoriesForClass!Stomp-Core! !
!StompInvalidDeserialization methodsFor!

description	^ super description , ': ' , self className printString! !
!StompInvalidDeserialization categoriesFor: #description!exceptionDescription!public! !

StompInvalidSerialization guid: (GUID fromString: '{C037F141-E2A4-449F-9A5E-409A5B12C4FD}')!
StompInvalidSerialization comment: ''!
!StompInvalidSerialization categoriesForClass!Stomp-Core! !
!StompInvalidSerialization methodsFor!

description	^ super description , ': ' , self className printString! !
!StompInvalidSerialization categoriesFor: #description!exceptionDescription!public! !

StompNewFailed guid: (GUID fromString: '{0125A207-5A26-4069-9788-6BD7D6B733C4}')!
StompNewFailed comment: ''!
!StompNewFailed categoriesForClass!Stomp-Core! !
StompReader guid: (GUID fromString: '{99D49255-24E6-4E86-803E-FFA90CAFD526}')!
StompReader comment: ''!
!StompReader categoriesForClass!Stomp-Core! !
!StompReader methodsFor!

basicReadObject	"Never remembering"	^(MpDecoder on: self readStream) readObject!

context	"Answer the value of context"	^ context ifNil: [context := StompReadContext on: self]!

context: anObject	"Set the value of context"	context := anObject!

createDictionary: size	| dic |	dic := super createDictionary: size.	self remember: dic.	^dic!

decodeFrom: aStream	| pos |	self readStream: aStream "binary".	pos := aStream position.	((self portableUtil nextAvailable: 2 from: aStream) = StompConstants header) ifTrue: [		self version: aStream next asInteger.	]. 	aStream position: pos. 	^self decode!

identifierStringFromBytes: byteArray 	byteArray ifNil: [^nil].	^ self settings supportsMultibyteIdentifiers		ifTrue: [self portableUtil stringFromBytes: byteArray]			ifFalse: [byteArray asString]!

initializeInstance: instance 	| shapeChangerClass |	instance stompInitialize.	shapeChangerClass := self context shapeChangerFor: instance class.	shapeChangerClass		ifNotNil: [(shapeChangerClass on: instance) loadAdditions]!

next	^self readObject!

portableUtil	^MpPortableUtil stomp!

readArraySized: size 	size = 0 ifTrue: [| arr | self remember: (arr := self createArray: 0). ^arr].	^self readArraySized: size atFirstTyped: self readType!

readArraySized: size atFirst: firstElem	| array |	array := self createArray: size.	self remember: array.	array at: 1 put: firstElem.	2 to: size do: [:idx | array at: idx put: self readObject].	^array!

readArraySized: size atFirstTyped: firstType	| array firstElem |	array := self createArray: size.	self remember: array.	firstElem := self readObjectOf: firstType.	array at: 1 put: firstElem.	2 to: size do: [:idx | array at: idx put: self readObject].	^array!

readBitsOf: aClass	^aClass stompFromBytes: self basicReadObject.!

readByteString	^self basicReadObject asString!

readByteSymbol	^self readByteString asSymbol!

readCollectionFieldsInto: anInstance	| size |	size := self readSizeOfArray.	^ self readCollectionFieldsInto: anInstance sized: size!

readCollectionFieldsInto: anInstance sized: size		anInstance class isVariable ifTrue: [^ self readIndexFieldsInto: anInstance sized: size].	1 to: size do:  [:idx | 		anInstance stompAdd: self readObject at: idx	].	^ anInstance!

readFixArray: firstByte	| size |	size := firstByte bitAnd: 16rF.	^ self readSmallArraySized: size!

readFixRaw: type	| bytes |	bytes := super readFixRaw: type.	self remember: bytes.	^bytes!

readIdentifierString	| type sz |	type := self readType.	(sz := type bitAnd: 2r01011111 <= 31) ifTrue: [		^ self identifierStringFromBytes: (self readStream next: sz)	].	type = MpConstants raw16 ifTrue: [^ self identifierStringFromBytes: super readRaw16].	type = MpConstants raw32 ifTrue: [^ self identifierStringFromBytes: super readRaw32].	!

readIndexFieldsInto: anInstance sized: size	1 to: size do:  [:idx | 		anInstance stompAt: idx put: self readObject	].	^ anInstance!

readInstanceContentAt: classId	| actualClass |	actualClass := self context classAt: classId.	^self readInstanceContentOf: actualClass!

readInstanceContentClassCoded: clsCode	| actualClass |	actualClass := self portableUtil popularClassMap classAt: clsCode.	actualClass ifNil: [actualClass :=  StompClassNotFound							signal: clsCode printString context: self context].	^self readInstanceContentOf: actualClass!

readInstanceContentClassNamed: clsName in: envName	| actualClass |	actualClass := self context classNamed: clsName in: envName.	actualClass ifNil: [actualClass :=  StompClassNotFound							signal: clsName environment: envName context: self context].	self context rememberClass: actualClass. 	^self readInstanceContentOf: actualClass!

readInstanceContentOf: aClass	| inst newInst newKey |	inst := aClass stompCreateInstanceFrom: self.	newKey := self remember: inst.	aClass stompLoadContentsOnCreation ifFalse: [inst stompReadContentFrom: self].	self initializeInstance: inst.	newInst := inst stompReadValue.	newKey ifNotNil: [self remember: newInst at: newKey].	^ newInst!

readInstVarsInto: anInstance	| type arraySize varNames varIndices valueSize namesWithIndices values |	type := self readType. 	arraySize := ((type bitShift: -4) = 2r1001)		ifTrue: [type bitAnd: 16rF]		ifFalse: [type = 16rDC ifTrue: [MpPortableUtil default readUint16From: self readStream]].	arraySize ifNotNil: [^ self readInstVarsInto: anInstance fromInstVarReferenceArraySized: arraySize].		varNames := self basicReadObject collect: [:each | self identifierStringFromBytes: each].	varIndices := self portableUtil instVarIndicesOf: anInstance class from: varNames.	namesWithIndices := self context rememberInstVarNames: varNames indices: varIndices of: anInstance class.		valueSize := self readSizeOfArray.	values := OrderedCollection new: valueSize.	valueSize timesRepeat: [values add: (self readObject)].	^ self readInstVarsInto: anInstance namesWithIndices: namesWithIndices values: values!

readInstVarsInto: anInstance fromInstVarReferenceArraySized: arraySize	| id  namesWithIndices values |	id :=  self basicReadObject.	namesWithIndices := self context instVarNamesWithIndicesAt: id.	values := Array new: namesWithIndices size.	1 to: values size do: [:idx | values at: idx put: self readObject].	self readInstVarsInto: anInstance namesWithIndices: namesWithIndices values: values!

readInstVarsInto: anInstance namesWithIndices: varNamesWithIndices values: varValues	| shapeChangerClass |	shapeChangerClass := self context shapeChangerFor: anInstance class.	shapeChangerClass ifNil: [ 		varNamesWithIndices with: varValues do: [:nameWithIndex :value |			anInstance stompInstVarAt: (nameWithIndex at: 2) named: (nameWithIndex at: 1) put: value].	] ifNotNil: [ | shapeChanger |		shapeChanger := shapeChangerClass on: anInstance.		varNamesWithIndices with: varValues do: [:nameWithIndex :value | 			shapeChanger loadInstVarAt: (nameWithIndex at: 2) named: (nameWithIndex at: 1) put: value].	].			^anInstance!

readObjectOf: type ifNotApplied: aBlock	(type between: 16r90 and: 16r9F) ifTrue: [^ self readFixArray: type].	^super readObjectOf: type ifNotApplied: aBlock!

readPrimitiveValues	"Assuming <= 15 fields"	| inst size |	size := self readStream next bitAnd: 16rF.	inst := self createArray: size.	size = 0 ifTrue: [^ inst].	^ super readArraySized: size!

readRaw16	| bytes |	bytes := super readRaw16.	self remember: bytes.	^bytes!

readRaw32	| bytes |	bytes := super readRaw32.	self remember: bytes.	^bytes!

readReference	| refId |	refId := self readObject.	^(self context at: refId) yourself!

readSizeOfArray	^ self readSizeOfArrayIfMatched: self readType!

readSizeOfArrayIfMatched: type	((type bitShift: -4) = 2r1001) ifTrue: [^ type bitAnd: 16rF].	type = MpConstants array16		ifTrue: [^ MpPortableUtil default readUint16From: self readStream].	type = MpConstants array32		ifTrue: [^ MpPortableUtil default readUint32From: self readStream].	^-1!

readSmallArraySized: size 	| firstType |	size = 0 ifTrue: [| arr | self remember: (arr := self createArray: 0). ^arr].	firstType := self readType.	firstType = 16rA1 ifTrue: [| firstByte |			firstByte := self readStream next.			^ (StompConstants isTupleTag: firstByte)				ifTrue: [self readTupleBy: firstByte]				ifFalse: [self						readArraySized: size						atFirst: (ByteArray with: firstByte)]].	^self readArraySized: size atFirstTyped: firstType!

readTupleBy: tag 	tag = StompConstants value ifTrue: [^self readValue].	tag = StompConstants reference ifTrue: [^self readReference].	tag = StompConstants byteString ifTrue: [^self readByteString].	tag = StompConstants byteSymbol ifTrue: [^self readByteSymbol].	tag = StompConstants wideString ifTrue: [^self readWideString].	tag = StompConstants wideSymbol ifTrue: [^self readWideString].	!

readUIntIfMatched: type	(type <= 16r7F) ifTrue: [^ self readPositiveFixNum: type].	type = MpConstants uint8		ifTrue: [^ self readUint8].	type = MpConstants uint16		ifTrue: [^ self readUint16].	type = MpConstants uint32		ifTrue: [^ self readUint32].	type = MpConstants uint64		ifTrue: [^ self readUint64].	^-1!

readValue	| type attributesSize attribDict newInst |	type := self readType.	(type bitShift: -4) = 2r1000 ifFalse: [ | id |		id := self readObjectOf: type.		^ id < 0 			ifTrue: [self readInstanceContentClassCoded: id negated]			ifFalse: [self readInstanceContentAt: id]	].	attributesSize := type bitAnd: 16rF.	attributesSize = 1 ifTrue: [ | key value |		key := self readPositiveFixNum: self readStream next.		value := self basicReadObject.		key = StompConstants klassName ifTrue: [^ self readInstanceContentClassNamed: (self identifierStringFromBytes: value) in: nil].	].	attribDict := Dictionary new: attributesSize.	1 to: attributesSize do: [:idx |		attribDict at: (self readPositiveFixNum: self readStream next) put: self basicReadObject	].	newInst :=  self readValueWithAttributes: attribDict.	^newInst	!

readValueWithAttributes: attribDict	| clsName envName |		clsName := self identifierStringFromBytes: (attribDict at: StompConstants klassName ifAbsent: []).	envName := self identifierStringFromBytes: (attribDict at: StompConstants environmentName ifAbsent: []).	envName		ifNil: 			[envName := self context						environmentNameAt: (attribDict at: StompConstants environmentId ifAbsent: [])]		ifNotNil: [self context rememberEnvironmentName: envName].	^ self readInstanceContentClassNamed: clsName in: envName!

readWideString	^self portableUtil stringFromBytes: self basicReadObject!

readWideSymbol	^self readWideString asSymbol!

remember: anObject	^ self remember: anObject at: self context size	!

remember: anObject at: key	anObject class = StompClassNotFound ifTrue: [		self context at: key put: nil.		^ key	].	(self supportsReferenceFor: anObject) ifFalse: [^nil]. 	self context at: key put: anObject.	^ key!

settingsClass	^StompSettings!

supportsReferenceFor: anObject	^anObject stompSupportsReference: self context	!

version	"Answer the value of version"	^ version!

version: anObject	"Set the value of version"	version := anObject! !
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

addClassNameAttributeTo: attributes for: writeObject	^attributes add: (StompConstants klassName -> (self bytesFromIdentifierString: (self classNameOf: writeObject)))!

addClassSpecifierAttributesTo: attributes for: writeObject 	| writeObjectClass |	writeObjectClass := writeObject class.	self addClassNameAttributeTo: attributes for: writeObject.	self context rememberClass: writeObjectClass.			(self context includesEnvironment: writeObjectClass environment)		ifTrue: [| envId |			envId := self context environmentIdOf: writeObjectClass environment.			self addEnvironmentIdAttributeTo: attributes id: envId]		ifFalse: [self addEnvironmentNameAttributeTo: attributes for: writeObject.			self context rememberEnvironment: writeObjectClass environment].!

addEnvironmentIdAttributeTo: attributes id: envId	^attributes add: (StompConstants environmentId -> envId)!

addEnvironmentNameAttributeTo: attributes for: writeObject	| env |	self settings useEnvironment ifFalse: [^self].	(self portableUtil shouldWriteEnvironmentNameOf: writeObject) ifFalse: [^self].	env := self portableUtil environmentNameOf: writeObject.	^attributes add: (StompConstants environmentName -> (self bytesFromIdentifierString: env))!

basicWriteInstVarsOf: writeObject 		| instVarNamesWithIndices |	instVarNamesWithIndices := self writtenInstVarNamesWithIndicesOf: writeObject.	self writeMapSize: 1.	self writeArraySize: instVarNamesWithIndices size.	instVarNamesWithIndices do: [:each | super writeObject: (self bytesFromIdentifierString: (each at: 1))].	self writeArraySize: instVarNamesWithIndices size.	instVarNamesWithIndices do: [:nmAndIndex |		| nm idx val | 		nm := nmAndIndex at: 1.		idx := nmAndIndex at: 2.		val := writeObject stompInstVarNamed: nm writtenAs: (writeObject instVarAt: idx).		self writeObject: val	].!

bytesFromIdentifierString: aString 	"Usually class & variable names are single bytes"	^ self settings supportsMultibyteIdentifiers		ifTrue: [self portableUtil bytesFromString: aString]		ifFalse: [aString asByteArray]!

classNameOf: anObject	| kls |	kls := anObject class.	^ (self portableUtil isMeta: kls) ifTrue: [(self portableUtil soleInstanceOf: kls) name, '$'] ifFalse: [kls name]!

context	"Answer the value of context"	^ context ifNil: [context := StompWriteContext on: self]!

context: anObject	"Set the value of context"	context := anObject!

debugEncode: bytes	|  encoded counterDic decoder |	encoded := self encode: bytes.	self context objectsDictionary.	counterDic := Dictionary new.	self context objectsDictionary keysAndValuesDo: [:k :v | counterDic at: v put: k].	counterDic inspect.	decoder := StompReader new.	decoder decode: encoded.	decoder context objectsDictionary inspect.	^encoded!

defaultVersion	"Stomp version 1"	^1!

nextPut: anObject	self writeStompFrom: anObject!

nextPutAll: aCollectionOfObject	aCollectionOfObject do: [:each | self nextPut: each]!

portableUtil	^MpPortableUtil stomp!

putReferenceOf: anObject ifExists: aBlock	| refId |	refId := self context at: anObject ifAbsent: [].	refId notNil ifTrue: [^aBlock value: refId].		self rememberReferenceOf: anObject!

rememberReferenceOf: originalObject	^ self context rememberReferenceOf: originalObject!

settingsClass	^StompSettings!

supportsReferenceFor: anObject	^anObject stompSupportsReference: self context	!

writeAll: collObject	| sz  |	sz := collObject size. 	self writeArraySize: sz.	collObject stompDo: [:each | self nextPut: each]!

writeBitsOf: writeObject	self writeRawBytes: writeObject stompBytes	!

writeByteString: aString	self writeArraySize: 2.	self writeTag: StompConstants byteString.	self writeRawBytes: aString asByteArray!

writeByteSymbol: aSymbol	self writeArraySize: 2.	self writeTag: StompConstants byteSymbol.	self writeRawBytes: aSymbol asByteArray!

writeContent: writeObject	writeObject stompWriteContentTo: self!

writeContent: object tag: tag attributes: attribs	| sz |	sz := 2 + object stompValueContentSize. 	self writeArraySize: sz.	self writeTag: tag.	self writeMapSize: attribs size.	attribs do: [:assoc |		self writePositiveFixNum: assoc key.		super writeObject: assoc value	].	self writeContent: object!

writeEmbeddedValue: writeObject ifNotApplied: aBlock	writeObject isSymbol ifTrue: [^self writeSymbol: writeObject].	writeObject isString ifTrue: [^self writeString: writeObject].	^ aBlock value!

writeIndexFieldsOf: writeObject	self writeAll: writeObject		!

writeInstVarsOf: writeObject 	|  cachedNamesWithIndices |	self settings suppressNilWrite ifTrue: [^self writeNotNilInstVarsOf: writeObject].		(cachedNamesWithIndices := self context instVarNamesWithIndicesOf: writeObject class) ifNotNil: [		^ self writeInstVarsOf: writeObject using: cachedNamesWithIndices	].		self basicWriteInstVarsOf: writeObject !

writeInstVarsOf: writeObject using: cachedNamesWithIndices	| classId |	classId := self context classIdOf: writeObject class.	classId ifNil: [^ self basicWriteInstVarsOf: writeObject].	self writeArraySize: cachedNamesWithIndices size + 1. 	self writeObject: classId.	cachedNamesWithIndices do: [:nmAndIndex |		| nm idx val | 		nm := nmAndIndex at: 1.		idx := nmAndIndex at: 2.		val := writeObject stompInstVarNamed: nm writtenAs: (writeObject instVarAt: idx).		self writeObject: val	].!

writeMixedFieldsOf: writeObject 		| shouldWriteInstanceVariables |	writeObject class instSize = 0 ifTrue: [^self writeIndexFieldsOf: writeObject].		shouldWriteInstanceVariables := writeObject stompShouldWriteInstanceVariables.	shouldWriteInstanceVariables ifTrue: [		self writeObject: writeObject size.		self writeInstVarsOf: writeObject	].	self writeIndexFieldsOf: writeObject!

writeNotNilInstVarsOf: writeObject 	| instVarNamesWithIndices nameAndValues |	instVarNamesWithIndices := self writtenInstVarNamesWithIndicesOf: writeObject.	nameAndValues := OrderedCollection new.	instVarNamesWithIndices		do: [:nmAndIndex | 			| nm idx val | 			nm := nmAndIndex at: 1.			idx := nmAndIndex at: 2.			val := writeObject stompInstVarNamed: nm writtenAs: (writeObject instVarAt: idx).			val ifNotNil: [nameAndValues add: (Array with: (self bytesFromIdentifierString: (nm)) with: val)]].		self writeMapSize: 1.	self writeArraySize: nameAndValues size.	nameAndValues do: [:each | super writeObject: (each at: 1)].	self writeArraySize: nameAndValues size.	nameAndValues do: [:each | self writeObject: (each at: 2)].	!

writeObject: anObject	"Main dispatching method"	| writeObject |	anObject stompPrepareWrite.	(self supportsReferenceFor: anObject) ifTrue: [			self putReferenceOf: anObject ifExists: [:refId | ^self writeReference: refId].	].	writeObject := anObject stompWriteValue.		self writeValue: writeObject.	anObject stompAfterWrite	!

writePrimitiveValues: anArray	"MessagePack types only"	"Never store object refs"	^super writeObject: anArray!

writeReference: refId	self writeArraySize: 2.	self writeTag: StompConstants reference.	self writeInteger: refId!

writeStandardValue: writeObject 	| sz writeObjectClass attributes |	sz := 2 + writeObject stompValueContentSize. 	self writeArraySize: sz.	self writeTag: StompConstants value.	writeObjectClass := writeObject class.	self portableUtil popularClassMap codeAt: writeObjectClass ifPresent: [:code | 		self writeInteger: code negated.		^ self writeContent: writeObject.	].	(self context includesClass: writeObjectClass)	ifTrue: [| classId |		classId := self context classIdOf: writeObjectClass.		self writeInteger: classId.		^ self writeContent: writeObject].	attributes := OrderedCollection new.	self addClassSpecifierAttributesTo: attributes for: writeObject.	self writeMapSize: attributes size.	attributes do: [:assoc |		self writePositiveFixNum: assoc key.		super writeObject: assoc value	].	self writeContent: writeObject!

writeStompFrom: anObject 	(self settings withHeader and: [self writeStream position = 0])		ifTrue: [self writeVersion].	self writeObject: anObject!

writeString: aString	(self portableUtil isWideString: aString) ifTrue: [^self writeWideString: aString].	self writeByteString: aString!

writeSymbol: aSymbol	(self portableUtil isWideSymbol: aSymbol) ifTrue: [^self writeWideSymbol: aSymbol].	self writeByteSymbol: aSymbol!

writeTag: tagInt	"tagInt should be 0-255"	self writeStream nextPut: (2r10100001).	self writeStream nextPut: tagInt!

writeValue: writeObject 	^ self		writeEmbeddedValue: writeObject		ifNotApplied: [self				writeObject: writeObject				ifNotApplied: [self writeStandardValue: writeObject]]!

writeVersion	self writeStream nextPutAll: StompConstants header.	self writeStream nextPut: self defaultVersion "1-255"!

writeWideString: aString	self writeArraySize: 2.	self writeTag: StompConstants wideString.	self writeRawBytes: (self portableUtil bytesFromString: aString)!

writeWideSymbol: aSymbol	self writeArraySize: 2.	self writeTag: StompConstants wideSymbol.	self writeRawBytes: (self portableUtil bytesFromString: aSymbol)!

writtenInstVarNamesWithIndicesOf: writeObject 	| writeObjectClass cachedNamesIndices instVarNames transientInstVarNames instVarIndices |	writeObjectClass := writeObject class.	(cachedNamesIndices := self context instVarNamesWithIndicesOf: writeObjectClass) ifNotNil: [^cachedNamesIndices].	instVarNames := writeObjectClass allInstVarNames asOrderedCollection.	transientInstVarNames := writeObject stompTransientInstVarNames.	transientInstVarNames := transientInstVarNames collect: [:each | each asString] .	transientInstVarNames do: [:each | instVarNames remove: each ifAbsent:[]].	instVarIndices := self portableUtil instVarIndicesOf: writeObjectClass from: instVarNames.	^ self context rememberInstVarNames: instVarNames indices: instVarIndices of: writeObjectClass.	! !
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

supportsMultibyteIdentifiers	^self at: #supportsMultibyteIdentifiers ifAbsentPut: [false]!

supportsMultibyteIdentifiers: aBoolean	^self at: #supportsMultibyteIdentifiers put: aBoolean!

suppressNilWrite	^ self writeVariableDefinitionsAsReference not!

suppressNilWrite: aBoolean		^self writeVariableDefinitionsAsReference: (aBoolean==true) not!

useEnvironment	^self at: #useEnvironment ifAbsentPut: [StompPortableUtil default useEnvironmentByDefault]!

useEnvironment: aBoolean	^self at: #useEnvironment put: aBoolean!

withHeader	^self at: #withHeader ifAbsentPut: [false]!

withHeader: aBoolean	^self at: #withHeader put: aBoolean!

writeVariableDefinitionsAsReference	^self at: #writeVariableDefinitionsAsReference ifAbsentPut: [true]!

writeVariableDefinitionsAsReference: aBoolean	^self at: #writeVariableDefinitionsAsReference put: aBoolean! !
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

classAliasesDictionary	^ classAliasesDictionary ifNil: [classAliasesDictionary := IdentityDictionary new]!

classAliasesDictionary: anObject	"Set the value of classAliasesDictionary"	classAliasesDictionary := anObject!

classAt: classId	^self classIdsDictionary at: classId ifAbsent: []!

classIdsDictionary		^ classIdsDictionary ifNil: [classIdsDictionary := Dictionary new]!

classIdsDictionary: anObject	"Set the value of classIdsDictionary"	classIdsDictionary := anObject!

classNamed: localClassName in: environmentQualifier 	| qualifier isClassClass localClassNameSize className foundClass |	qualifier := self settings useEnvironment ifTrue: [environmentQualifier].		isClassClass := false.	localClassNameSize := localClassName size.	className := ((localClassName at: (localClassNameSize)) = $$)				ifTrue: [isClassClass := true.					localClassName copyFrom: 1 to: (localClassNameSize-1)]				ifFalse: [localClassName].					foundClass := self mappedClassAt: className asSymbol in: qualifier.		isClassClass ifTrue: [foundClass := foundClass class].		^ foundClass!

clearFieldsInfo	fieldsInfo := nil!

environmentNameAt: envId	^ self environmentNamesDictionary at: envId ifAbsent: [].!

environmentNamesDictionary	^environmentNamesDictionary ifNil: [environmentNamesDictionary := Dictionary new]!

environmentNamesDictionary: anObject	"Set the value of environmentNamesDictionary"	environmentNamesDictionary := anObject!

fieldsInfo	"Answer the value of mixedFieldHint"	^ fieldsInfo!

fieldsInfo: anObject	"Set the value of mixedFieldHint"	fieldsInfo := anObject!

instVarNamesWithIndicesAt: classId	| klass |	klass := self classAt: classId.	^ self instVarNamesWithIndicesDictionary at: klass ifAbsent: [#()]!

instVarNamesWithIndicesDictionary	^ instVarNamesWithIndicesDictionary ifNil: [instVarNamesWithIndicesDictionary := IdentityDictionary new]!

instVarNamesWithIndicesDictionary: anObject	"Set the value of instVarNamesWithIndicesDictionary"	instVarNamesWithIndicesDictionary := anObject!

mappedClassAt: classNameSymbol in: environmentQualifier 	| classNameSymbolOrNewClass |	classNameSymbolOrNewClass := self mappedClassNameAt: classNameSymbol asSymbol in: environmentQualifier.	^ classNameSymbolOrNewClass isBehavior 		ifTrue: [classNameSymbolOrNewClass]		ifFalse: [StompPortableUtil default classNamed: classNameSymbolOrNewClass in: environmentQualifier].!

mappedClassNameAt: classNameSymbol in: environmentQualifier 	| key |	key := environmentQualifier isNil				ifTrue: [classNameSymbol]				ifFalse: [Array with: environmentQualifier with: classNameSymbol].	^ self classAliasesDictionary		at: key		ifAbsent: [classNameSymbol]!

objectDictionaryClass	^Dictionary!

registerClassOldName: oldClassNameSymbol for: existingClass	^ self registerClassOldName: oldClassNameSymbol in: nil for: existingClass!

registerClassOldName: oldClassNameSymbol in: oldEnvironmentQualifier for: existingClass	| key |	key := oldEnvironmentQualifier isNil				ifTrue: [oldClassNameSymbol]				ifFalse: [Array with: oldEnvironmentQualifier with: oldClassNameSymbol].	^ self classAliasesDictionary at: key put: existingClass!

registerClassOldName: oldClassNameSymbol in: oldEnvironmentQualifier shapeChanger: shapeChangerClass for: existingClass	self registerClassOldName: oldClassNameSymbol in: oldEnvironmentQualifier for: existingClass.	self registerShapeChanger: shapeChangerClass for: existingClass.!

registerClassOldName: oldClassNameSymbol shapeChanger: shapeChangerClass for: existingClass	self registerClassOldName: oldClassNameSymbol for: existingClass.	self registerShapeChanger: shapeChangerClass for: existingClass.!

registerShapeChanger: shapeChangerObject for: existingClass	^self shapeChangersDictionary at: existingClass put: shapeChangerObject!

registerShapeChangerRenameBy: loadInstVarsBlock for: existingClass	^self registerShapeChangerRenameBy: loadInstVarsBlock initializeBy: nil for: existingClass!

registerShapeChangerRenameBy: loadInstVarsBlock initializeBy: loadAdditionsBlock for: existingClass	| changer |	changer := StompBlockShapeChanger loadInstVarsBlock: loadInstVarsBlock loadAdditionsBlock: loadAdditionsBlock.	^self shapeChangersDictionary at: existingClass put: changer!

rememberClass: aClass	^ self classIdsDictionary at: self classIdsDictionary size put: aClass!

rememberEnvironmentName: aString	^ self environmentNamesDictionary at: (self environmentNamesDictionary size) put: aString!

rememberInstVarNames: instVarNames indices: instVarIndices of: aClass	| ord |	ord := OrderedCollection new: instVarNames size.	instVarNames with: instVarIndices do: [:name :idx | ord add: (Array with: name with: idx)]. 	self instVarNamesWithIndicesDictionary at: aClass put: ord.	^ord!

shapeChangerFor: existingClass	shapeChangersDictionary ifNil: [^nil].	^self shapeChangersDictionary at: existingClass ifAbsent: []!

shapeChangersDictionary	^ shapeChangersDictionary ifNil: [shapeChangersDictionary := IdentityDictionary new]!

shapeChangersDictionary: anObject	"Set the value of shapeChangersDictionary"	shapeChangersDictionary := anObject! !
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

classesDictionary	^ classesDictionary ifNil: [classesDictionary := IdentityDictionary new]!

classesDictionary: anObject	"Set the value of classesDictionary"	classesDictionary := anObject!

classIdOf: aClass	^ self classesDictionary at: aClass!

environmentIdOf: aClass	^ self environmentsDictionary at: aClass!

environmentsDictionary	^ environmentsDictionary ifNil: [environmentsDictionary := IdentityDictionary new]!

environmentsDictionary: anObject	"Set the value of environmentsDictionary"	environmentsDictionary := anObject!

includesClass: aClass	^ self classesDictionary includesKey: aClass!

includesEnvironment: anEnvironment	self settings useEnvironment ifFalse: [^false].	^ self environmentsDictionary includesKey: anEnvironment!

instVarNamesWithIndicesDictionary	^ instVarNamesWithIndicesDictionary ifNil: [instVarNamesWithIndicesDictionary := IdentityDictionary new]!

instVarNamesWithIndicesDictionary: anObject	"Set the value of instVarNamesWithIndicesDictionary"	instVarNamesWithIndicesDictionary := anObject!

instVarNamesWithIndicesOf: aClass	^ self instVarNamesWithIndicesDictionary at: aClass ifAbsent: []!

rememberClass: aClass	^ self classesDictionary at: aClass put: (self classesDictionary size)!

rememberEnvironment: anEnvironment	self settings useEnvironment ifFalse: [^self].	anEnvironment name == #Smalltalk ifTrue: [^self].	^ self environmentsDictionary at: anEnvironment put: (self environmentsDictionary size)!

rememberInstVarNames: instVarNames indices: instVarIndices of: aClass	| ord |	ord := OrderedCollection new: instVarNames size.	instVarNames with: instVarIndices do: [:name :idx | ord add: (Array with: name with: idx)]. 	self instVarNamesWithIndicesDictionary at: aClass put: ord.	^ord!

rememberReferenceOf: originalObject	self at: originalObject put: self size.! !
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

loadAdditions	loadAdditionsBlock ifNotNil: [loadAdditionsBlock value: self targetInstance]!

loadAdditionsBlock	"Answer the value of loadAdditionsBlock"	^ loadAdditionsBlock!

loadAdditionsBlock: anObject	"Set the value of loadAdditionsBlock"	loadAdditionsBlock := anObject!

loadInstVarAt: varIndex named: varName put: varValue 	loadInstVarsBlock ifNotNil: [loadInstVarsBlock value: self targetInstance value: varName value: varValue]. 		super loadInstVarAt: varIndex named: varName put: varValue !

loadInstVarsBlock	"Answer the value of loadInstVarsBlock"	^ loadInstVarsBlock!

loadInstVarsBlock: anObject	"Set the value of loadInstVarsBlock"	loadInstVarsBlock := anObject! !
!StompBlockShapeChanger categoriesFor: #loadAdditions!actions!public! !
!StompBlockShapeChanger categoriesFor: #loadAdditionsBlock!accessing!public! !
!StompBlockShapeChanger categoriesFor: #loadAdditionsBlock:!accessing!public! !
!StompBlockShapeChanger categoriesFor: #loadInstVarAt:named:put:!actions!public! !
!StompBlockShapeChanger categoriesFor: #loadInstVarsBlock!accessing!public! !
!StompBlockShapeChanger categoriesFor: #loadInstVarsBlock:!accessing!public! !

!StompBlockShapeChanger class methodsFor!

loadInstVarsBlock: loadInstVarsBlock loadAdditionsBlock: loadAdditionsBlock	| inst |	inst := self new.	inst loadInstVarsBlock: loadInstVarsBlock.	inst loadAdditionsBlock: loadAdditionsBlock.	^ inst! !
!StompBlockShapeChanger class categoriesFor: #loadInstVarsBlock:loadAdditionsBlock:!instance creation!public! !

"Binary Globals"!

