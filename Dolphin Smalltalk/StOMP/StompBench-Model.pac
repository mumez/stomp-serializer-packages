| package |
package := Package name: 'StompBench-Model'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #StompMockAttachment;
	add: #StompMockAuthor;
	add: #StompMockBlogPost;
	add: #StompMockCategory;
	add: #StompMockComment;
	add: #StompMockOrder;
	add: #StompMockOrderItem;
	add: #StompMockSearchBody;
	add: #StompMockSearchDocument;
	add: #StompMockSearchHeader;
	add: #StompMockSearchHighlight;
	add: #StompMockSearchResponse;
	add: #StompMockSession;
	add: #StompMockTag;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #StompMockAttachment
	instanceVariableNames: 'id type data size'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockAuthor
	instanceVariableNames: 'id firstName surname nickname'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'authors'!
Object subclass: #StompMockBlogPost
	instanceVariableNames: 'id title status content creationDate modifiedDate author comments categories tags attachments'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'myCategories'!
Object subclass: #StompMockCategory
	instanceVariableNames: 'id name description'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'myCategories'!
Object subclass: #StompMockComment
	instanceVariableNames: 'id content creationDate author accepted count'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockOrder
	instanceVariableNames: 'id items'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockOrderItem
	instanceVariableNames: 'id title price'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockSearchBody
	instanceVariableNames: 'numFounds start docs'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockSearchDocument
	instanceVariableNames: 'id name price manufacturer inStock popularity timestamp categories features'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockSearchHeader
	instanceVariableNames: 'status time params'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockSearchHighlight
	instanceVariableNames: 'name features'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockSearchResponse
	instanceVariableNames: 'header body highlightings'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockSession
	instanceVariableNames: 'id expires properties'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #StompMockTag
	instanceVariableNames: 'id name parent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'tags'!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

StompMockAttachment guid: (GUID fromString: '{290BC6EB-5FCE-4C06-8E88-CD53622D5F29}')!
StompMockAttachment comment: ''!
!StompMockAttachment categoriesForClass!StompBench-Model! !
!StompMockAttachment methodsFor!

data
	
	^data!

data: anObject
	
	data := anObject!

id
	
	^id!

id: anObject
	
	id := anObject!

size
	
	^size!

size: anObject
	
	size := anObject!

type
	
	^type!

type: anObject
	
	type := anObject! !
!StompMockAttachment categoriesFor: #data!accessing!public! !
!StompMockAttachment categoriesFor: #data:!accessing!public! !
!StompMockAttachment categoriesFor: #id!accessing!public! !
!StompMockAttachment categoriesFor: #id:!accessing!public! !
!StompMockAttachment categoriesFor: #size!accessing!public! !
!StompMockAttachment categoriesFor: #size:!accessing!public! !
!StompMockAttachment categoriesFor: #type!accessing!public! !
!StompMockAttachment categoriesFor: #type:!accessing!public! !

!StompMockAttachment class methodsFor!

attachment1	| inst |	inst := self new.	inst id: 'aaa.gif'.	inst type: 'gif'.	inst data: (ByteArray new: 700*1000).	inst size: inst data size.	^inst!

attachment2	| inst |	inst := self new.	inst id: 'bbb.png'.	inst type: 'png'.	inst data: (ByteArray new: 800*1000).	inst size: inst data size.	^inst! !
!StompMockAttachment class categoriesFor: #attachment1!accessing!public! !
!StompMockAttachment class categoriesFor: #attachment2!accessing!public! !

StompMockAuthor guid: (GUID fromString: '{163424F3-F59A-4518-9755-394DB18EB0F3}')!
StompMockAuthor comment: ''!
!StompMockAuthor categoriesForClass!StompBench-Model! !
!StompMockAuthor methodsFor!

firstName
	
	^firstName!

firstName: anObject
	
	firstName := anObject!

id
	
	^id!

id: anObject
	
	id := anObject!

nickname
	
	^nickname!

nickname: anObject
	
	nickname := anObject!

surname
	
	^surname!

surname: anObject
	
	surname := anObject! !
!StompMockAuthor categoriesFor: #firstName!accessing!public! !
!StompMockAuthor categoriesFor: #firstName:!accessing!public! !
!StompMockAuthor categoriesFor: #id!accessing!public! !
!StompMockAuthor categoriesFor: #id:!accessing!public! !
!StompMockAuthor categoriesFor: #nickname!accessing!public! !
!StompMockAuthor categoriesFor: #nickname:!accessing!public! !
!StompMockAuthor categoriesFor: #surname!accessing!public! !
!StompMockAuthor categoriesFor: #surname:!accessing!public! !

!StompMockAuthor class methodsFor!

authors	"Answer the value of authors"	^ authors!

authors: anObject	"Set the value of authors"	authors := anObject!

initialize	self prepareAuthors!

prepareAuthors	authors := Dictionary new.	authors at: #ume put: (self new id: 1; firstName: 'Masashi'; surname: 'Umezawa'; nickname: 'mu').	authors at: #yama put: (self new id: 2; firstName: 'Yamada'; surname: 'Taro'; nickname: 'dokaben').	^authors! !
!StompMockAuthor class categoriesFor: #authors!accessing!public! !
!StompMockAuthor class categoriesFor: #authors:!accessing!public! !
!StompMockAuthor class categoriesFor: #initialize!class initialization!public! !
!StompMockAuthor class categoriesFor: #prepareAuthors!preparing!public! !

StompMockBlogPost guid: (GUID fromString: '{A4437A28-5A25-479E-95C4-47E0BE4AAE5A}')!
StompMockBlogPost comment: ''!
!StompMockBlogPost categoriesForClass!StompBench-Model! !
!StompMockBlogPost methodsFor!

attachments
	
	^attachments!

attachments: anObject
	
	attachments := anObject!

author
	
	^author!

author: anObject
	
	author := anObject!

categories
	
	^categories!

categories: anObject
	
	categories := anObject!

comments
	
	^comments!

comments: anObject
	
	comments := anObject!

content
	
	^content!

content: anObject
	
	content := anObject!

creationDate
	
	^creationDate!

creationDate: anObject
	
	creationDate := anObject!

id
	
	^id!

id: anObject
	
	id := anObject!

initialize
	"Initialize a newly created instance. This method must answer the receiver."
	
	super initialize.	" *** Replace this comment with the appropriate initialization code *** "
	^self!

modifiedDate
	
	^modifiedDate!

modifiedDate: anObject
	
	modifiedDate := anObject!

status
	
	^status!

status: anObject
	
	status := anObject!

tags
	
	^tags!

tags: anObject
	
	tags := anObject!

title
	
	^title!

title: anObject
	
	title := anObject! !
!StompMockBlogPost categoriesFor: #attachments!accessing!public! !
!StompMockBlogPost categoriesFor: #attachments:!accessing!public! !
!StompMockBlogPost categoriesFor: #author!accessing!public! !
!StompMockBlogPost categoriesFor: #author:!accessing!public! !
!StompMockBlogPost categoriesFor: #categories!accessing!public! !
!StompMockBlogPost categoriesFor: #categories:!accessing!public! !
!StompMockBlogPost categoriesFor: #comments!accessing!public! !
!StompMockBlogPost categoriesFor: #comments:!accessing!public! !
!StompMockBlogPost categoriesFor: #content!accessing!public! !
!StompMockBlogPost categoriesFor: #content:!accessing!public! !
!StompMockBlogPost categoriesFor: #creationDate!accessing!public! !
!StompMockBlogPost categoriesFor: #creationDate:!accessing!public! !
!StompMockBlogPost categoriesFor: #id!accessing!public! !
!StompMockBlogPost categoriesFor: #id:!accessing!public! !
!StompMockBlogPost categoriesFor: #initialize!initialize/release!public! !
!StompMockBlogPost categoriesFor: #modifiedDate!accessing!public! !
!StompMockBlogPost categoriesFor: #modifiedDate:!accessing!public! !
!StompMockBlogPost categoriesFor: #status!accessing!public! !
!StompMockBlogPost categoriesFor: #status:!accessing!public! !
!StompMockBlogPost categoriesFor: #tags!accessing!public! !
!StompMockBlogPost categoriesFor: #tags:!accessing!public! !
!StompMockBlogPost categoriesFor: #title!accessing!public! !
!StompMockBlogPost categoriesFor: #title:!accessing!public! !

!StompMockBlogPost class methodsFor!

new
	"Answer a newly created and initialized instance."
	
	^super new initialize! !
!StompMockBlogPost class categoriesFor: #new!instance creation!public! !

StompMockCategory guid: (GUID fromString: '{E17E177D-5B68-4742-B47E-1129E1970A06}')!
StompMockCategory comment: ''!
!StompMockCategory categoriesForClass!StompBench-Model! !
!StompMockCategory methodsFor!

description
	
	^description!

description: anObject
	
	description := anObject!

id
	
	^id!

id: anObject
	
	id := anObject!

name
	
	^name!

name: anObject
	
	name := anObject! !
!StompMockCategory categoriesFor: #description!accessing!public! !
!StompMockCategory categoriesFor: #description:!accessing!public! !
!StompMockCategory categoriesFor: #id!accessing!public! !
!StompMockCategory categoriesFor: #id:!accessing!public! !
!StompMockCategory categoriesFor: #name!accessing!public! !
!StompMockCategory categoriesFor: #name:!accessing!public! !

!StompMockCategory class methodsFor!

getCategories	^ myCategories !

initialize	self prepareCategories!

prepareCategories	myCategories := Dictionary new.	myCategories at: #book put: (self new id: 1; name: 'book'; description: 'some topic about book').	myCategories at: #photo put: (self new id: 2; name: 'photo'; description: 'nice photos').	myCategories at: #life put: (self new id: 3; name: 'life'; description: 'life is too short...').	^myCategories!

setCategories: collectionOfCategory	myCategories := collectionOfCategory! !
!StompMockCategory class categoriesFor: #getCategories!preparing!public! !
!StompMockCategory class categoriesFor: #initialize!class initialization!public! !
!StompMockCategory class categoriesFor: #prepareCategories!preparing!public! !
!StompMockCategory class categoriesFor: #setCategories:!preparing!public! !

StompMockComment guid: (GUID fromString: '{4324D726-F8E6-4882-8830-A33EB999046F}')!
StompMockComment comment: ''!
!StompMockComment categoriesForClass!StompBench-Model! !
!StompMockComment methodsFor!

accepted
	
	^accepted!

accepted: anObject
	
	accepted := anObject!

author
	
	^author!

author: anObject
	
	author := anObject!

content
	
	^content!

content: anObject
	
	content := anObject!

count
	
	^count!

count: anObject
	
	count := anObject!

creationDate
	
	^creationDate!

creationDate: anObject
	
	creationDate := anObject!

id
	
	^id!

id: anObject
	
	id := anObject! !
!StompMockComment categoriesFor: #accepted!accessing!public! !
!StompMockComment categoriesFor: #accepted:!accessing!public! !
!StompMockComment categoriesFor: #author!accessing!public! !
!StompMockComment categoriesFor: #author:!accessing!public! !
!StompMockComment categoriesFor: #content!accessing!public! !
!StompMockComment categoriesFor: #content:!accessing!public! !
!StompMockComment categoriesFor: #count!accessing!public! !
!StompMockComment categoriesFor: #count:!accessing!public! !
!StompMockComment categoriesFor: #creationDate!accessing!public! !
!StompMockComment categoriesFor: #creationDate:!accessing!public! !
!StompMockComment categoriesFor: #id!accessing!public! !
!StompMockComment categoriesFor: #id:!accessing!public! !

StompMockOrder guid: (GUID fromString: '{44C6F60C-FC6F-4EAF-A1E8-ED7113F57A37}')!
StompMockOrder comment: ''!
!StompMockOrder categoriesForClass!StompBench-Model! !
!StompMockOrder methodsFor!

id	"Answer the value of id"	^ id!

id: anObject	"Set the value of id"	id := anObject!

items	"Answer the value of items"	^ items!

items: anObject	"Set the value of items"	items := anObject! !
!StompMockOrder categoriesFor: #id!accessing!public! !
!StompMockOrder categoriesFor: #id:!accessing!public! !
!StompMockOrder categoriesFor: #items!accessing!public! !
!StompMockOrder categoriesFor: #items:!accessing!public! !

StompMockOrderItem guid: (GUID fromString: '{20D1DB4A-5A6A-44F0-A975-0EA401BE87D2}')!
StompMockOrderItem comment: ''!
!StompMockOrderItem categoriesForClass!StompBench-Model! !
!StompMockOrderItem methodsFor!

= other	self class = other class ifFalse: [^false].	^self id = other id!

id	"Answer the value of id"	^ id!

id: anObject	"Set the value of id"	id := anObject!

price	"Answer the value of price"	^ price!

price: anObject	"Set the value of price"	price := anObject!

title	"Answer the value of title"	^ title!

title: anObject	"Set the value of title"	title := anObject! !
!StompMockOrderItem categoriesFor: #=!comparing!public! !
!StompMockOrderItem categoriesFor: #id!accessing!public! !
!StompMockOrderItem categoriesFor: #id:!accessing!public! !
!StompMockOrderItem categoriesFor: #price!accessing!public! !
!StompMockOrderItem categoriesFor: #price:!accessing!public! !
!StompMockOrderItem categoriesFor: #title!accessing!public! !
!StompMockOrderItem categoriesFor: #title:!accessing!public! !

StompMockSearchBody guid: (GUID fromString: '{E532E94D-AA80-46EF-A089-D216B12F11E9}')!
StompMockSearchBody comment: ''!
!StompMockSearchBody categoriesForClass!StompBench-Model! !
!StompMockSearchBody methodsFor!

docs	"Answer the value of docs"	^ docs!

docs: anObject	"Set the value of docs"	docs := anObject!

numFounds	"Answer the value of numFounds"	^ numFounds!

numFounds: anObject	"Set the value of numFounds"	numFounds := anObject!

start	"Answer the value of start"	^ start!

start: anObject	"Set the value of start"	start := anObject! !
!StompMockSearchBody categoriesFor: #docs!accessing!public! !
!StompMockSearchBody categoriesFor: #docs:!accessing!public! !
!StompMockSearchBody categoriesFor: #numFounds!accessing!public! !
!StompMockSearchBody categoriesFor: #numFounds:!accessing!public! !
!StompMockSearchBody categoriesFor: #start!accessing!public! !
!StompMockSearchBody categoriesFor: #start:!accessing!public! !

StompMockSearchDocument guid: (GUID fromString: '{6140A38D-8446-42EA-BB64-8D8D69995F2E}')!
StompMockSearchDocument comment: ''!
!StompMockSearchDocument categoriesForClass!StompBench-Model! !
!StompMockSearchDocument methodsFor!

categories	"Answer the value of categories"	^ categories!

categories: anObject	"Set the value of categories"	categories := anObject!

features	"Answer the value of features"	^ features!

features: anObject	"Set the value of features"	features := anObject!

id	"Answer the value of id"	^ id!

id: anObject	"Set the value of id"	id := anObject!

inStock	"Answer the value of inStock"	^ inStock!

inStock: anObject	"Set the value of inStock"	inStock := anObject!

manufacturer	"Answer the value of manufacturer"	^ manufacturer!

manufacturer: anObject	"Set the value of manufacturer"	manufacturer := anObject!

name: anObject	"Set the value of name"	name := anObject!

popularity	"Answer the value of popularity"	^ popularity!

popularity: anObject	"Set the value of popularity"	popularity := anObject!

price	"Answer the value of price"	^ price!

price: anObject	"Set the value of price"	price := anObject!

timestamp	"Answer the value of timestamp"	^ timestamp!

timestamp: anObject	"Set the value of timestamp"	timestamp := anObject! !
!StompMockSearchDocument categoriesFor: #categories!accessing!public! !
!StompMockSearchDocument categoriesFor: #categories:!accessing!public! !
!StompMockSearchDocument categoriesFor: #features!accessing!public! !
!StompMockSearchDocument categoriesFor: #features:!accessing!public! !
!StompMockSearchDocument categoriesFor: #id!accessing!public! !
!StompMockSearchDocument categoriesFor: #id:!accessing!public! !
!StompMockSearchDocument categoriesFor: #inStock!accessing!public! !
!StompMockSearchDocument categoriesFor: #inStock:!accessing!public! !
!StompMockSearchDocument categoriesFor: #manufacturer!accessing!public! !
!StompMockSearchDocument categoriesFor: #manufacturer:!accessing!public! !
!StompMockSearchDocument categoriesFor: #name:!accessing!public! !
!StompMockSearchDocument categoriesFor: #popularity!accessing!public! !
!StompMockSearchDocument categoriesFor: #popularity:!accessing!public! !
!StompMockSearchDocument categoriesFor: #price!accessing!public! !
!StompMockSearchDocument categoriesFor: #price:!accessing!public! !
!StompMockSearchDocument categoriesFor: #timestamp!accessing!public! !
!StompMockSearchDocument categoriesFor: #timestamp:!accessing!public! !

StompMockSearchHeader guid: (GUID fromString: '{1F788D65-DEB4-44DF-9054-5539C1BE26FE}')!
StompMockSearchHeader comment: ''!
!StompMockSearchHeader categoriesForClass!StompBench-Model! !
!StompMockSearchHeader methodsFor!

params	"Answer the value of params"	^ params!

params: anObject	"Set the value of params"	params := anObject!

status	"Answer the value of status"	^ status!

status: anObject	"Set the value of status"	status := anObject!

time	"Answer the value of time"	^ time!

time: anObject	"Set the value of time"	time := anObject! !
!StompMockSearchHeader categoriesFor: #params!accessing!public! !
!StompMockSearchHeader categoriesFor: #params:!accessing!public! !
!StompMockSearchHeader categoriesFor: #status!accessing!public! !
!StompMockSearchHeader categoriesFor: #status:!accessing!public! !
!StompMockSearchHeader categoriesFor: #time!accessing!public! !
!StompMockSearchHeader categoriesFor: #time:!accessing!public! !

StompMockSearchHighlight guid: (GUID fromString: '{50CA6EE9-ED0D-49B2-BAD8-8663F1F61A22}')!
StompMockSearchHighlight comment: ''!
!StompMockSearchHighlight categoriesForClass!StompBench-Model! !
!StompMockSearchHighlight methodsFor!

features	"Answer the value of features"	^ features!

features: anObject	"Set the value of features"	features := anObject!

name: anObject	"Set the value of name"	name := anObject! !
!StompMockSearchHighlight categoriesFor: #features!accessing!public! !
!StompMockSearchHighlight categoriesFor: #features:!accessing!public! !
!StompMockSearchHighlight categoriesFor: #name:!accessing!public! !

StompMockSearchResponse guid: (GUID fromString: '{AA9CD7C7-2BCC-4501-8D2B-018468DE059E}')!
StompMockSearchResponse comment: ''!
!StompMockSearchResponse categoriesForClass!StompBench-Model! !
!StompMockSearchResponse methodsFor!

body	"Answer the value of body"	^ body!

body: anObject	"Set the value of body"	body := anObject!

header	"Answer the value of header"	^ header!

header: anObject	"Set the value of header"	header := anObject!

highlightings	"Answer the value of highlightings"	^ highlightings!

highlightings: anObject	"Set the value of highlightings"	highlightings := anObject! !
!StompMockSearchResponse categoriesFor: #body!accessing!public! !
!StompMockSearchResponse categoriesFor: #body:!accessing!public! !
!StompMockSearchResponse categoriesFor: #header!accessing!public! !
!StompMockSearchResponse categoriesFor: #header:!accessing!public! !
!StompMockSearchResponse categoriesFor: #highlightings!accessing!public! !
!StompMockSearchResponse categoriesFor: #highlightings:!accessing!public! !

StompMockSession guid: (GUID fromString: '{E3C08890-B24C-4500-9190-531A0F8BBA96}')!
StompMockSession comment: ''!
!StompMockSession categoriesForClass!StompBench-Model! !
!StompMockSession methodsFor!

expires	"Answer the value of expires"	^ expires!

expires: anObject	"Set the value of expires"	expires := anObject!

id	"Answer the value of id"	^ id!

id: anObject	"Set the value of id"	id := anObject!

properties	"Answer the value of properties"	^ properties!

properties: anObject	"Set the value of properties"	properties := anObject! !
!StompMockSession categoriesFor: #expires!accessing!public! !
!StompMockSession categoriesFor: #expires:!accessing!public! !
!StompMockSession categoriesFor: #id!accessing!public! !
!StompMockSession categoriesFor: #id:!accessing!public! !
!StompMockSession categoriesFor: #properties!accessing!public! !
!StompMockSession categoriesFor: #properties:!accessing!public! !

StompMockTag guid: (GUID fromString: '{FE1C85D3-504F-4214-942D-72FA85A0EB04}')!
StompMockTag comment: ''!
!StompMockTag categoriesForClass!StompBench-Model! !
!StompMockTag methodsFor!

id
	
	^id!

id: anObject
	
	id := anObject!

name
	
	^name!

name: anObject
	
	name := anObject!

parent
	
	^parent!

parent: anObject
	
	parent := anObject! !
!StompMockTag categoriesFor: #id!accessing!public! !
!StompMockTag categoriesFor: #id:!accessing!public! !
!StompMockTag categoriesFor: #name!accessing!public! !
!StompMockTag categoriesFor: #name:!accessing!public! !
!StompMockTag categoriesFor: #parent!accessing!public! !
!StompMockTag categoriesFor: #parent:!accessing!public! !

!StompMockTag class methodsFor!

initialize	self prepareTags!

prepareTags	tags := Dictionary new.	tags at: #lang put: (StompMockTag new id: 1; name: 'lang').	tags at: #trans put: (StompMockTag new id: 2; name: 'trans').	tags at: #smalltalk put: (StompMockTag new id: 11; name: 'smalltalk'; parent: (tags at: #lang)).	tags at: #visualworks put: (StompMockTag new id: 111; name: 'visualworks'; parent: (tags at: #smalltalk)).	tags at: #ruby put: (StompMockTag new id: 12; name: 'ruby'; parent: (tags at: #lang)).	tags at: #erlang put: (StompMockTag new id: 13; name: 'erlang'; parent: (tags at: #lang)).	tags at: #trans put: (StompMockTag new id: 21; name: 'Japanese').	tags at: #trans put: (StompMockTag new id: 22; name: 'English').		^tags!

tags	"Answer the value of tags"	^ tags!

tags: anObject	"Set the value of tags"	tags := anObject! !
!StompMockTag class categoriesFor: #initialize!class initialization!public! !
!StompMockTag class categoriesFor: #prepareTags!preparing!public! !
!StompMockTag class categoriesFor: #tags!accessing!public! !
!StompMockTag class categoriesFor: #tags:!accessing!public! !

"Binary Globals"!

