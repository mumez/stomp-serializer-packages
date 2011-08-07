| package |
package := Package name: 'Stomp-Dolphin-Core'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '6'.


package classNames
	add: #StompColor;
	add: #StompDolpPopularClassMap;
	add: #StompDolpPortableUtil;
	yourself.

package methodNames
	add: #Color -> #stompWriteContentTo:;
	add: #OrderedCollection -> #stompAt:put:;
	add: #Set -> #stompAt:put:;
	add: #TimeStamp -> #stompWriteContentTo:;
	add: 'Color class' -> #stompCreateInstanceFrom:;
	add: 'Color class' -> #stompLoadContentsOnCreation;
	add: 'CompiledMethod class' -> #stompFromBytes:;
	add: 'ScaledDecimal class' -> #stompCreateInstanceFrom:;
	add: 'TimeStamp class' -> #stompCreateInstanceFrom:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: 'Stomp-Core';
	yourself).

package!

"Class Definitions"!

RGB subclass: #StompColor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompPopularClassMap subclass: #StompDolpPopularClassMap
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
StompPortableUtil subclass: #StompDolpPortableUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Color methodsFor!

stompWriteContentTo: stompWriter	| rgbArray |	rgbArray := Array with: self red  with: self green  with: self blue .	stompWriter writePrimitiveValues: rgbArray.		! !
!Color categoriesFor: #stompWriteContentTo:!*Stomp/Dolphin/Core/writing!public! !

!Color class methodsFor!

stompCreateInstanceFrom: stompReader 
	| rgbArray |
	rgbArray := stompReader readPrimitiveValues.
	^Color 
		red: rgbArray first
		green: rgbArray second
		blue: rgbArray third!

stompLoadContentsOnCreation
	^true! !
!Color class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Dolphin/Core/instance creation!public! !
!Color class categoriesFor: #stompLoadContentsOnCreation!*Stomp/Dolphin/Core/instance creation!*Stomp/Dolphin/Core/testing!public! !

!CompiledMethod class methodsFor!

stompFromBytes: rawBytes	| inst |	inst := super basicNew: rawBytes size.	inst replaceFrom: 1 to: inst size with: rawBytes startingAt: 1.	^inst	! !
!CompiledMethod class categoriesFor: #stompFromBytes:!*Stomp/Dolphin/Core/instance creation!public! !

!OrderedCollection methodsFor!

stompAt: index put: aValue
	^self stompAdd: aValue at: index! !
!OrderedCollection categoriesFor: #stompAt:put:!*Stomp/Dolphin/Core/reading!public! !

!ScaledDecimal class methodsFor!

stompCreateInstanceFrom: stompReader	| array |	array := stompReader readPrimitiveValues.	^self newFromNumber:  (array at: 1) scale: (array at: 2)! !
!ScaledDecimal class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Dolphin/Core/instance creation!public! !

!Set methodsFor!

stompAt: index put: aValue
	^self stompAdd: aValue at: index! !
!Set categoriesFor: #stompAt:put:!*Stomp/Dolphin/Core/reading!public! !

!TimeStamp methodsFor!

stompWriteContentTo: stompWriter
	stompWriter writeObject: (StompPortableUtil default nanosecondsFromDateAndTime: self)! !
!TimeStamp categoriesFor: #stompWriteContentTo:!*Stomp/Dolphin/Core/writing!public! !

!TimeStamp class methodsFor!

stompCreateInstanceFrom: stompReader	^ StompPortableUtil default timestampFromNanoseconds: stompReader readObject! !
!TimeStamp class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Dolphin/Core/instance creation!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StompColor guid: (GUID fromString: '{AA5DFA1F-ED89-4BBD-A3BB-FF1888C93AAE}')!
StompColor comment: ''!
!StompColor categoriesForClass!Drawing! !
!StompColor methodsFor!

stompWriteContentTo: stompWriter 
	| rgbArray |
	rgbArray := Array 
				with: (self red / 255) asFloat
				with: (self green / 255) asFloat
				with: (self blue / 255) asFloat.
	stompWriter writePrimitiveValues: rgbArray! !
!StompColor categoriesFor: #stompWriteContentTo:!*Stomp/Dolphin/Core/writing!public! !

!StompColor class methodsFor!

stompCreateInstanceFrom: stompReader
	| rgbArray |
	rgbArray := stompReader readPrimitiveValues.
	^ StompPortableUtil default colorFromRgbArray: rgbArray! !
!StompColor class categoriesFor: #stompCreateInstanceFrom:!*Stomp/Dolphin/Core/instance creation!public! !

StompDolpPopularClassMap guid: (GUID fromString: '{81CDE24B-A524-4CA0-BF10-0ADE85CF09DA}')!
StompDolpPopularClassMap comment: ''!
!StompDolpPopularClassMap categoriesForClass!Stomp-Dolphin-Core! !
!StompDolpPopularClassMap methodsFor!

colorClass	^ self classNamed: #StompColor!

fixedPointClass	^ self classNamed: #ScaledDecimal!

timestampClass	^ self classNamed: #TimeStamp!

uint16ArrayClass	^ self classNamed: #WORDArray!

uint32ArrayClass	^ self classNamed: #DWORDArray! !
!StompDolpPopularClassMap categoriesFor: #colorClass!factory!public! !
!StompDolpPopularClassMap categoriesFor: #fixedPointClass!factory!public! !
!StompDolpPopularClassMap categoriesFor: #timestampClass!factory!public! !
!StompDolpPopularClassMap categoriesFor: #uint16ArrayClass!factory!public! !
!StompDolpPopularClassMap categoriesFor: #uint32ArrayClass!factory!public! !

StompDolpPortableUtil guid: (GUID fromString: '{4581D3FC-92B4-4FBA-8594-AE312C893DE5}')!
StompDolpPortableUtil comment: ''!
!StompDolpPortableUtil categoriesForClass!Stomp-Dolphin-Core! !
!StompDolpPortableUtil methodsFor!

bytes: rawBytes intoOf: bitsClass	"override"
	| inst |	inst := bitsClass new: rawBytes size.	inst replaceFrom: 1 to: inst size with: rawBytes startingAt: 1.	^inst!

bytesFromString: aString	^aString asByteArray!

colorFromRgbArray: rgbArray	^StompColor red: (rgbArray first*255) truncated green: (rgbArray second*255) truncated blue: (rgbArray third*255) truncated!

instVarIndexOf: aClass for: varName 	^ self privInstVarIndexOf: aClass for: varName !

instVarNamed: varName put: value in: anObject
	"Note that when varName is invalid, just silently ignore"
	| index |
	index := self instVarIndexOf: anObject class for: varName.
	index = 0 ifTrue: [^self].
	anObject instVarAt: index put: value			!

nanosecondsFromDateAndTime: dateAndTime	"Answer the number of nanoseconds since January 1, 1901."	^(dateAndTime asMilliseconds) * 1000000!

popularClassMap
	"override"

	^StompDolpPopularClassMap default!

privInstVarIndexOf: aClass for: varName 
			
	| index superKlass |
	index :=aClass instVarNames indexOf: varName.
	superKlass := aClass superclass.
	index == 0 ifTrue: [
		^ superKlass == nil 
			ifTrue: [0]
			ifFalse: [self privInstVarIndexOf: superKlass for: varName]
	].
	^(superKlass == nil )
		ifTrue: [index]
		ifFalse: [index + superKlass instSize].
!

timestampFromNanoseconds: nanoseconds	"^ Timestamp fromNanoseconds: nanoseconds"	^ TimeStamp fromSeconds: (nanoseconds / 1000000000)!

useEnvironmentByDefault	^false! !
!StompDolpPortableUtil categoriesFor: #bytes:intoOf:!actions!public! !
!StompDolpPortableUtil categoriesFor: #bytesFromString:!converting!public! !
!StompDolpPortableUtil categoriesFor: #colorFromRgbArray:!converting!public! !
!StompDolpPortableUtil categoriesFor: #instVarIndexOf:for:!actions!public! !
!StompDolpPortableUtil categoriesFor: #instVarNamed:put:in:!actions!public! !
!StompDolpPortableUtil categoriesFor: #nanosecondsFromDateAndTime:!converting!public! !
!StompDolpPortableUtil categoriesFor: #popularClassMap!factory!public! !
!StompDolpPortableUtil categoriesFor: #privInstVarIndexOf:for:!private! !
!StompDolpPortableUtil categoriesFor: #timestampFromNanoseconds:!converting!public! !
!StompDolpPortableUtil categoriesFor: #useEnvironmentByDefault!actions!public! !

!StompDolpPortableUtil class methodsFor!

initialize	"StompDolpPortableUtil initialize"	| klsName |	super initialize.	StompPortableUtil dialectSpecificClass: self	! !
!StompDolpPortableUtil class categoriesFor: #initialize!class initialization!public! !

"Binary Globals"!

