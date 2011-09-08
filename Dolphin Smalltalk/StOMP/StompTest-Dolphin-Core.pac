| package |
package := Package name: 'StompTest-Dolphin-Core'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '3'.


package classNames
	add: #StompDolpFixtures;
	yourself.

package methodNames
	add: #StompDolpPortableUtil -> #testFixturesClass;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: 'Stomp-Core';
	add: 'Stomp-Dolphin-Core';
	add: 'StompTest-Core';
	yourself).

package!

"Class Definitions"!

StompPortableFixtures subclass: #StompDolpFixtures
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!StompDolpPortableUtil methodsFor!

testFixturesClass
	^ StompDolpFixtures! !
!StompDolpPortableUtil categoriesFor: #testFixturesClass!*StompTest/Core/factory!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

StompDolpFixtures guid: (GUID fromString: '{F024BE11-43BC-42D5-8D7C-60C6353D5020}')!
StompDolpFixtures comment: ''!
!StompDolpFixtures categoriesForClass!StompTest-Dolphin-Core! !
!StompDolpFixtures class methodsFor!

blueColor	^ StompColor blue!

double1234567890dot123456789	^1234567890.123456789!

double3dot3	^ 3.3!

float1dot2bytes	^ #[203 63 243 51 51 51 51 51 51]!

timestamp1	| timestamp tsClass |	tsClass := StompPortableUtil default popularClassMap timestampClass.	timestamp := tsClass date: (StompPortableUtil default dateFromSeconds:  3330720000) time: (Time fromSeconds: 13506). 	^timestamp !

yellowColor	^ StompColor yellow! !
!StompDolpFixtures class categoriesFor: #blueColor!fixtures!public! !
!StompDolpFixtures class categoriesFor: #double1234567890dot123456789!fixtures!public! !
!StompDolpFixtures class categoriesFor: #double3dot3!fixtures!public! !
!StompDolpFixtures class categoriesFor: #float1dot2bytes!fixtures!public! !
!StompDolpFixtures class categoriesFor: #timestamp1!fixtures!public! !
!StompDolpFixtures class categoriesFor: #yellowColor!fixtures!public! !

"Binary Globals"!

