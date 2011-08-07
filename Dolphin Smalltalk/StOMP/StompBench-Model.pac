|package|package := Package name: 'StompBench-Model'.package paxVersion: 1; basicComment: ''.package basicPackageVersion: ''.package classNamesadd: #StompMockAttachment;add: #StompMockAuthor;add: #StompMockBlogPost;add: #StompMockCategory;add: #StompMockComment;add: #StompMockOrder;add: #StompMockOrderItem;add: #StompMockSearchBody;add: #StompMockSearchDocument;add: #StompMockSearchHeader;add: #StompMockSearchHighlight;add: #StompMockSearchResponse;add: #StompMockSession;add: #StompMockTag;yourself.package binaryGlobalNames: Set new.package globalAliases: Set new.package!Object subclass: #StompMockAttachment	instanceVariableNames: 'id type data size'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockAuthor	instanceVariableNames: 'id firstName surname nickname'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockBlogPost	instanceVariableNames: 'id title status content creationDate modifiedDate author comments categories tags attachments'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockCategory	instanceVariableNames: 'id name description'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockComment	instanceVariableNames: 'id content creationDate author accepted count'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockOrder	instanceVariableNames: 'id items'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockOrderItem	instanceVariableNames: 'id title price'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockSearchBody	instanceVariableNames: 'numFounds start docs'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockSearchDocument	instanceVariableNames: 'id name price manufacturer inStock popularity timestamp categories features'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockSearchHeader	instanceVariableNames: 'status time params'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockSearchHighlight	instanceVariableNames: 'name features'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockSearchResponse	instanceVariableNames: 'header body highlightings'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockSession	instanceVariableNames: 'id expires properties'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!Object subclass: #StompMockTag	instanceVariableNames: 'id name parent'	classVariableNames: ''	poolDictionaries: ''	category: 'StompBench-Model'!!StompMockOrderItem methodsFor: 'comparing'!= other	self class = other class ifFalse: [^false].	^self id = other id! !!StompMockComment methodsFor: 'accessing'!accepted
	
	^accepted! !!StompMockComment methodsFor: 'accessing'!accepted: anObject
	
	accepted := anObject! !!StompMockAttachment class methodsFor: 'accessing'!attachment1	| inst |	inst := self new.	inst id: 'aaa.gif'.	inst type: 'gif'.	inst data: (ByteArray new: 700*1000).	inst size: inst data size.	^inst! !!StompMockAttachment class methodsFor: 'accessing'!attachment2	| inst |	inst := self new.	inst id: 'bbb.png'.	inst type: 'png'.	inst data: (ByteArray new: 800*1000).	inst size: inst data size.	^inst! !!StompMockBlogPost methodsFor: 'accessing'!attachments
	
	^attachments! !!StompMockBlogPost methodsFor: 'accessing'!attachments: anObject
	
	attachments := anObject! !!StompMockBlogPost methodsFor: 'accessing'!author
	
	^author! !!StompMockComment methodsFor: 'accessing'!author
	
	^author! !!StompMockBlogPost methodsFor: 'accessing'!author: anObject
	
	author := anObject! !!StompMockComment methodsFor: 'accessing'!author: anObject
	
	author := anObject! !!StompMockAuthor class methodsFor: 'accessing'!authors	"Answer the value of authors"	^ authors! !!StompMockAuthor class methodsFor: 'accessing'!authors: anObject	"Set the value of authors"	authors := anObject! !!StompMockSearchResponse methodsFor: 'accessing'!body	"Answer the value of body"	^ body! !!StompMockSearchResponse methodsFor: 'accessing'!body: anObject	"Set the value of body"	body := anObject! !!StompMockBlogPost methodsFor: 'accessing'!categories
	
	^categories! !!StompMockCategory class methodsFor: 'accessing'!categories	"Answer the value of categories"	^ categories! !!StompMockSearchDocument methodsFor: 'accessing'!categories	"Answer the value of categories"	^ categories! !!StompMockBlogPost methodsFor: 'accessing'!categories: anObject
	
	categories := anObject! !!StompMockCategory class methodsFor: 'accessing'!categories: anObject	"Set the value of categories"	categories := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!categories: anObject	"Set the value of categories"	categories := anObject! !!StompMockBlogPost methodsFor: 'accessing'!comments
	
	^comments! !!StompMockBlogPost methodsFor: 'accessing'!comments: anObject
	
	comments := anObject! !!StompMockBlogPost methodsFor: 'accessing'!content
	
	^content! !!StompMockComment methodsFor: 'accessing'!content
	
	^content! !!StompMockBlogPost methodsFor: 'accessing'!content: anObject
	
	content := anObject! !!StompMockComment methodsFor: 'accessing'!content: anObject
	
	content := anObject! !!StompMockComment methodsFor: 'accessing'!count
	
	^count! !!StompMockComment methodsFor: 'accessing'!count: anObject
	
	count := anObject! !!StompMockBlogPost methodsFor: 'accessing'!creationDate
	
	^creationDate! !!StompMockComment methodsFor: 'accessing'!creationDate
	
	^creationDate! !!StompMockBlogPost methodsFor: 'accessing'!creationDate: anObject
	
	creationDate := anObject! !!StompMockComment methodsFor: 'accessing'!creationDate: anObject
	
	creationDate := anObject! !!StompMockAttachment methodsFor: 'accessing'!data
	
	^data! !!StompMockAttachment methodsFor: 'accessing'!data: anObject
	
	data := anObject! !!StompMockCategory methodsFor: 'accessing'!description
	
	^description! !!StompMockCategory methodsFor: 'accessing'!description: anObject
	
	description := anObject! !!StompMockSearchBody methodsFor: 'accessing'!docs	"Answer the value of docs"	^ docs! !!StompMockSearchBody methodsFor: 'accessing'!docs: anObject	"Set the value of docs"	docs := anObject! !!StompMockSession methodsFor: 'accessing'!expires	"Answer the value of expires"	^ expires! !!StompMockSession methodsFor: 'accessing'!expires: anObject	"Set the value of expires"	expires := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!features	"Answer the value of features"	^ features! !!StompMockSearchHighlight methodsFor: 'accessing'!features	"Answer the value of features"	^ features! !!StompMockSearchDocument methodsFor: 'accessing'!features: anObject	"Set the value of features"	features := anObject! !!StompMockSearchHighlight methodsFor: 'accessing'!features: anObject	"Set the value of features"	features := anObject! !!StompMockAuthor methodsFor: 'accessing'!firstName
	
	^firstName! !!StompMockAuthor methodsFor: 'accessing'!firstName: anObject
	
	firstName := anObject! !!StompMockSearchResponse methodsFor: 'accessing'!header	"Answer the value of header"	^ header! !!StompMockSearchResponse methodsFor: 'accessing'!header: anObject	"Set the value of header"	header := anObject! !!StompMockSearchResponse methodsFor: 'accessing'!highlightings	"Answer the value of highlightings"	^ highlightings! !!StompMockSearchResponse methodsFor: 'accessing'!highlightings: anObject	"Set the value of highlightings"	highlightings := anObject! !!StompMockAttachment methodsFor: 'accessing'!id
	
	^id! !!StompMockAuthor methodsFor: 'accessing'!id
	
	^id! !!StompMockBlogPost methodsFor: 'accessing'!id
	
	^id! !!StompMockCategory methodsFor: 'accessing'!id
	
	^id! !!StompMockComment methodsFor: 'accessing'!id
	
	^id! !!StompMockOrder methodsFor: 'accessing'!id	"Answer the value of id"	^ id! !!StompMockOrderItem methodsFor: 'accessing'!id	"Answer the value of id"	^ id! !!StompMockSearchDocument methodsFor: 'accessing'!id	"Answer the value of id"	^ id! !!StompMockSession methodsFor: 'accessing'!id	"Answer the value of id"	^ id! !!StompMockTag methodsFor: 'accessing'!id
	
	^id! !!StompMockAttachment methodsFor: 'accessing'!id: anObject
	
	id := anObject! !!StompMockAuthor methodsFor: 'accessing'!id: anObject
	
	id := anObject! !!StompMockBlogPost methodsFor: 'accessing'!id: anObject
	
	id := anObject! !!StompMockCategory methodsFor: 'accessing'!id: anObject
	
	id := anObject! !!StompMockComment methodsFor: 'accessing'!id: anObject
	
	id := anObject! !!StompMockOrder methodsFor: 'accessing'!id: anObject	"Set the value of id"	id := anObject! !!StompMockOrderItem methodsFor: 'accessing'!id: anObject	"Set the value of id"	id := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!id: anObject	"Set the value of id"	id := anObject! !!StompMockSession methodsFor: 'accessing'!id: anObject	"Set the value of id"	id := anObject! !!StompMockTag methodsFor: 'accessing'!id: anObject
	
	id := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!inStock	"Answer the value of inStock"	^ inStock! !!StompMockSearchDocument methodsFor: 'accessing'!inStock: anObject	"Set the value of inStock"	inStock := anObject! !!StompMockAuthor class methodsFor: 'class initialization'!initialize	self prepareAuthors! !!StompMockBlogPost methodsFor: 'initialize-release'!initialize
	"Initialize a newly created instance. This method must answer the receiver."
	
	super initialize.	" *** Replace this comment with the appropriate initialization code *** "
	^self! !!StompMockCategory class methodsFor: 'class initialization'!initialize	self prepareCategories! !!StompMockTag class methodsFor: 'class initialization'!initialize	self prepareTags! !!StompMockOrder methodsFor: 'accessing'!items	"Answer the value of items"	^ items! !!StompMockOrder methodsFor: 'accessing'!items: anObject	"Set the value of items"	items := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!manufacturer	"Answer the value of manufacturer"	^ manufacturer! !!StompMockSearchDocument methodsFor: 'accessing'!manufacturer: anObject	"Set the value of manufacturer"	manufacturer := anObject! !!StompMockBlogPost methodsFor: 'accessing'!modifiedDate
	
	^modifiedDate! !!StompMockBlogPost methodsFor: 'accessing'!modifiedDate: anObject
	
	modifiedDate := anObject! !!StompMockCategory methodsFor: 'accessing'!name
	
	^name! !!StompMockTag methodsFor: 'accessing'!name
	
	^name! !!StompMockCategory methodsFor: 'accessing'!name: anObject
	
	name := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!name: anObject	"Set the value of name"	name := anObject! !!StompMockSearchHighlight methodsFor: 'accessing'!name: anObject	"Set the value of name"	name := anObject! !!StompMockTag methodsFor: 'accessing'!name: anObject
	
	name := anObject! !!StompMockBlogPost class methodsFor: 'instance creation'!new
	"Answer a newly created and initialized instance."
	
	^super new initialize! !!StompMockAuthor methodsFor: 'accessing'!nickname
	
	^nickname! !!StompMockAuthor methodsFor: 'accessing'!nickname: anObject
	
	nickname := anObject! !!StompMockSearchBody methodsFor: 'accessing'!numFounds	"Answer the value of numFounds"	^ numFounds! !!StompMockSearchBody methodsFor: 'accessing'!numFounds: anObject	"Set the value of numFounds"	numFounds := anObject! !!StompMockSearchHeader methodsFor: 'accessing'!params	"Answer the value of params"	^ params! !!StompMockSearchHeader methodsFor: 'accessing'!params: anObject	"Set the value of params"	params := anObject! !!StompMockTag methodsFor: 'accessing'!parent
	
	^parent! !!StompMockTag methodsFor: 'accessing'!parent: anObject
	
	parent := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!popularity	"Answer the value of popularity"	^ popularity! !!StompMockSearchDocument methodsFor: 'accessing'!popularity: anObject	"Set the value of popularity"	popularity := anObject! !!StompMockAuthor class methodsFor: 'preparing'!prepareAuthors	authors := Dictionary new.	authors at: #ume put: (self new id: 1; firstName: 'Masashi'; surname: 'Umezawa'; nickname: 'mu').	authors at: #yama put: (self new id: 2; firstName: 'Yamada'; surname: 'Taro'; nickname: 'dokaben').	^authors! !!StompMockCategory class methodsFor: 'preparing'!prepareCategories	categories := Dictionary new.	categories at: #book put: (self new id: 1; name: 'book'; description: 'some topic about book').	categories at: #photo put: (self new id: 2; name: 'photo'; description: 'nice photos').	categories at: #life put: (self new id: 3; name: 'life'; description: 'life is too short...').	^categories! !!StompMockTag class methodsFor: 'preparing'!prepareTags	tags := Dictionary new.	tags at: #lang put: (StompMockTag new id: 1; name: 'lang').	tags at: #trans put: (StompMockTag new id: 2; name: 'trans').	tags at: #smalltalk put: (StompMockTag new id: 11; name: 'smalltalk'; parent: (tags at: #lang)).	tags at: #visualworks put: (StompMockTag new id: 111; name: 'visualworks'; parent: (tags at: #smalltalk)).	tags at: #ruby put: (StompMockTag new id: 12; name: 'ruby'; parent: (tags at: #lang)).	tags at: #erlang put: (StompMockTag new id: 13; name: 'erlang'; parent: (tags at: #lang)).	tags at: #trans put: (StompMockTag new id: 21; name: 'Japanese').	tags at: #trans put: (StompMockTag new id: 22; name: 'English').		^tags! !!StompMockOrderItem methodsFor: 'accessing'!price	"Answer the value of price"	^ price! !!StompMockSearchDocument methodsFor: 'accessing'!price	"Answer the value of price"	^ price! !!StompMockOrderItem methodsFor: 'accessing'!price: anObject	"Set the value of price"	price := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!price: anObject	"Set the value of price"	price := anObject! !!StompMockSession methodsFor: 'accessing'!properties	"Answer the value of properties"	^ properties! !!StompMockSession methodsFor: 'accessing'!properties: anObject	"Set the value of properties"	properties := anObject! !!StompMockAttachment methodsFor: 'accessing'!size
	
	^size! !!StompMockAttachment methodsFor: 'accessing'!size: anObject
	
	size := anObject! !!StompMockSearchBody methodsFor: 'accessing'!start	"Answer the value of start"	^ start! !!StompMockSearchBody methodsFor: 'accessing'!start: anObject	"Set the value of start"	start := anObject! !!StompMockBlogPost methodsFor: 'accessing'!status
	
	^status! !!StompMockSearchHeader methodsFor: 'accessing'!status	"Answer the value of status"	^ status! !!StompMockBlogPost methodsFor: 'accessing'!status: anObject
	
	status := anObject! !!StompMockSearchHeader methodsFor: 'accessing'!status: anObject	"Set the value of status"	status := anObject! !!StompMockAuthor methodsFor: 'accessing'!surname
	
	^surname! !!StompMockAuthor methodsFor: 'accessing'!surname: anObject
	
	surname := anObject! !!StompMockBlogPost methodsFor: 'accessing'!tags
	
	^tags! !!StompMockTag class methodsFor: 'accessing'!tags	"Answer the value of tags"	^ tags! !!StompMockBlogPost methodsFor: 'accessing'!tags: anObject
	
	tags := anObject! !!StompMockTag class methodsFor: 'accessing'!tags: anObject	"Set the value of tags"	tags := anObject! !!StompMockSearchHeader methodsFor: 'accessing'!time	"Answer the value of time"	^ time! !!StompMockSearchHeader methodsFor: 'accessing'!time: anObject	"Set the value of time"	time := anObject! !!StompMockSearchDocument methodsFor: 'accessing'!timestamp	"Answer the value of timestamp"	^ timestamp! !!StompMockSearchDocument methodsFor: 'accessing'!timestamp: anObject	"Set the value of timestamp"	timestamp := anObject! !!StompMockBlogPost methodsFor: 'accessing'!title
	
	^title! !!StompMockOrderItem methodsFor: 'accessing'!title	"Answer the value of title"	^ title! !!StompMockBlogPost methodsFor: 'accessing'!title: anObject
	
	title := anObject! !!StompMockOrderItem methodsFor: 'accessing'!title: anObject	"Set the value of title"	title := anObject! !!StompMockAttachment methodsFor: 'accessing'!type
	
	^type! !!StompMockAttachment methodsFor: 'accessing'!type: anObject
	
	type := anObject! !StompMockAuthor initialize!StompMockCategory initialize!StompMockTag initialize!