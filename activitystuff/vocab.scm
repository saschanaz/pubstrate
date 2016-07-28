(define-module (activitystuff vocab)
  #:use-module (activitystuff asobj)
  #:export ($Object $Link $Activity $IntransitiveActivity
            $Actor $Collection $OrderedCollection

            $Accept $TentativeAccept $Add $Arrive $Create
            $Delete $Follow $Ignore $Join $Leave $Like
            $Offer $Invite $Reject $TentativeReject $Remove
            $Undo $Update $Experience $View $Listen $Read
            $Move $Travel $Announce $Block $Flag $Dislike

            $Application $Group $Organization $Person
            $Process $Service

            $Relationship $Content $Article $Album $Folder
            $Story $Document $Audio $Image $Video $Note
            $Page $Question $Event $Place $Mention
            $Profile

            *core-vocab* *basic-env))


(define (as-uri identifier)
  (string-append "http://www.w3.org/ns/activitystreams#"
                 identifier))



;;; Core classes
;;; ============

(define $Object
  (make-astype
   (as-uri "Object") '() "Object"
   "Describes an object of any kind.
The Object class serves as the base class for most of the
other kinds of objects defined in the Activity Vocabulary,
include other Core classes such as Activity,
IntransitiveActivity, Actor, Collection and OrderedCollection."))

(define $Link
  (make-astype
   (as-uri "Link") '() "Link"
   "A Link is an indirect, qualified reference to a resource identified by
a URL. The fundamental model for links is established by [RFC5988].
Many of the properties defined by the Activity Vocabulary allow
values that are either instances of Object or Link. When a Link is
used, it establishes a qualified relation connecting the subject (the
containing object) to the resource identified by the href."))

(define $Activity
  (make-astype
   (as-uri "Activity") (list $Object) "Activity"
   "An Activity is a subclass of Object that describes some form of
action that may happen, is currently happening, or has already
happened. The Activity class itself serves as an abstract base
class for all types of activities. It is important to note that
the Activity class itself does not carry any specific semantics
about the kind of action being taken."))

(define $IntransitiveActivity
  (make-astype
   (as-uri "IntransitiveActivity") (list $Activity) "IntransitiveActivity"
   "Instances of IntransitiveActivity are a subclass of Activity whose
actor property identifies the direct object of the action as opposed
to using the object property."))

(define $Actor
  (make-astype
   (as-uri "Actor") (list $Object) "Actor"
   "An Actor is any entity that is capable of being the primary actor
for an Activity."))

(define $Collection
  (make-astype
   (as-uri "Collection") (list $Object) "Collection"
   "A Collection is a subclass of Object that represents ordered or
unordered sets of Object or Link instances.

Refer to the Activity Streams 2.0 Core specification for a complete
description of the Collection type."))

(define $OrderedCollection
  (make-astype
   (as-uri "OrderedCollection") (list $Collection) "OrderedCollection"
   "A subclass of Collection in which members of the logical collection
are assumed to always be strictly ordered."))

(define $CollectionPage
  (make-astype
   (as-uri "CollectionPage") (list $Collection) "CollectionPage"
   "Used to represent distinct subsets of items from a Collection.
Refer to the Activity Streams 2.0 Core for a complete description of
the CollectionPage object."))

(define $OrderedCollectionPage
  (make-astype
   (as-uri "OrderedCollectionPage") (list $OrderedCollection $CollectionPage)
   "OrderedCollectionPage"
   "Used to represent ordered subsets of items from an OrderedCollection.
Refer to the Activity Streams 2.0 Core for a complete description of
the OrderedCollectionPage object."))



;;; Extended Classes: Activity Types
;;; ================================

(define $Accept
  (make-astype
   (as-uri "Accept") (list $Activity) "Accept"
   "Indicates that the actor accepts the object.
The target property can be used in certain circumstances to indicate
the context into which the object has been accepted. For instance,
when expressing the activity, \"Sally accepted Joe into the Club\",
the \"target\" would identify the \"Club\"."))

(define $TentativeAccept
  (make-astype
   (as-uri "TentativeAccept") (list $Accept) "TentativeAccept"
   "A specialization of Accept indicating that the acceptance is tentative."))

(define $Add
  (make-astype
   (as-uri "Add") (list $Activity) "Add"
   "Indicates that the actor has added the object to the target. If the
target property is not explicitly specified, the target would need
to be determined implicitly by context. The origin can be used to
identify the context from which the object originated."))

(define $Arrive
  (make-astype
   (as-uri "Arrive") (list $IntransitiveActivity) "Arrive"
   "An IntransitiveActivity that indicates that the actor has arrived
at the location. The origin can be used to identify the context
from which the actor originated. The target typically has no defined
meaning."))

(define $Create
  (make-astype
   (as-uri "Create") (list $Activity) "Create"
   "Indicates that the actor has created the object."))

(define $Delete
  (make-astype
   (as-uri "Delete") (list $Activity) "Delete"
   "Indicates that the actor has deleted the object. If specified,
the origin indicates the context from which the object was
deleted."))

(define $Follow
  (make-astype
   (as-uri "Follow") (list $Activity) "Follow"
   "Indicates that the actor is \"following\" the object. Following is
defined in the sense typically used within Social systems in which
the actor is interested in any activity performed by or on the
object. The target and origin typically have no defined meaning."))

(define $Ignore
  (make-astype
   (as-uri "Ignore") (list $Activity) "Ignore"
   "Indicates that the actor is ignoring the object.
The target and origin typically have no defined meaning."))

(define $Join
  (make-astype
   (as-uri "Join") (list $Activity) "Join"
   "Indicates that the actor has joined the object. The target and
origin typically have no defined meaning."))

(define $Leave
  (make-astype
   (as-uri "Leave") (list $Activity) "Leave"
   "Indicates that the actor has left the object. The target and origin
typically have no meaning."))

(define $Like
  (make-astype
   (as-uri "Like") (list $Activity) "Like"
   "Indicates that the actor likes, recommends or endorses the object.
The target and origin typically have no defined meaning."))

(define $Offer
  (make-astype
   (as-uri "Offer") (list $Activity) "Offer"
   "Indicates that the actor is offering the object. If specified, the
target indicates the entity to which the object is being offered."))

(define $Invite
  (make-astype
   (as-uri "Invite") (list $Offer) "Invite"
   "A specialization of Offer in which the actor is extending an
invitation for the object to the target."))

(define $Reject
  (make-astype
   (as-uri "Reject") (list $Activity) "Reject"
   "Indicates that the actor is rejecting the object. The target and
origin typically have no defined meaning."))

(define $TentativeReject
  (make-astype
   (as-uri "TentativeReject") (list $Reject) "TentativeReject"
   "A specialization of Reject in which the rejection is considered
tentative."))

(define $Remove
  (make-astype
   (as-uri "Remove") (list $Activity) "Remove"
   "Indicates that the actor is removing the object. If specified, the
origin indicates the context from which the object is being removed."))

(define $Undo
  (make-astype
   (as-uri "Undo") (list $Activity) "Undo"
   "Indicates that the actor is undoing the object. In most cases,
the object will be an Activity describing some previously performed
action (for instance, a person may have previously \"liked\"
an article but, for whatever reason, might choose to undo that
like at some later point in time).

The target and origin typically have no defined meaning."))

(define $Update
  (make-astype
   (as-uri "Update") (list $Activity) "Update"
   "Indicates that the actor has updated the object. Note, however, that
this vocabulary does not define a mechanism for describing the
actual set of modifications made to object.

The target and origin typically have no defined meaning."))

(define $Experience
  (make-astype
   (as-uri "Experience") (list $Activity) "Experience"
   "Indicates that the actor has experienced the object. The type of
experience is not specified."))

(define $View
  (make-astype
   (as-uri "View") (list $Experience) "View"
   "Indicates that the actor has viewed the object. Viewing is a
specialization of Experience."))

(define $Listen
  (make-astype
   (as-uri "Listen") (list $Experience) "Listen"
   "Indicates that the actor has listened to the object. Listening is a
specialization of Experience."))

(define $Read
  (make-astype
   (as-uri "Read") (list $Experience) "Read"
   "Indicates that the actor has read the object. Reading is a
specialization of Experience."))

(define $Move
  (make-astype
   (as-uri "Move") (list $Activity) "Move"
   "Indicates that the actor has moved object from origin to target. If
the origin or target are not specified, either can be determined by
context."))

(define $Travel
  (make-astype
   (as-uri "Travel") (list $IntransitiveActivity) "Travel"
   "Indicates that the actor is traveling to target from origin.
Travel is an IntransitiveObject whose actor specifies the direct
object. If the target or origin are not specified, either can be
determined by context."))

(define $Announce
  (make-astype
   (as-uri "Announce") (list $Activity) "Announce"
   "Indicates that the actor is calling the target's attention the object.

The origin typically has no defined meaning."))

(define $Block
  (make-astype
   (as-uri "Block") (list $Ignore) "Block"
   "Indicates that the actor is blocking the object. Blocking is a
stronger form of Ignore. The typical use is to support social systems
that allow one user to block activities or content of other users.

The target and origin typically have no defined meaning."))

(define $Flag
  (make-astype
   (as-uri "Flag") (list $Activity) "Flag"
   "Indicates that the actor is \"flagging\" the object. Flagging is
defined in the sense common to many social platforms as reporting
content as being inappropriate for any number of reasons."))

(define $Dislike
  (make-astype
   (as-uri "Dislike") (list $Activity) "Dislike"
   "Indicates that the actor dislikes the object."))


;; Extended Classes: Actor types
;; =============================

(define $Application
  (make-astype
   (as-uri "Application") (list $Actor) "Application"
   "Describes a software application."))

(define $Group
  (make-astype
   (as-uri "Group") (list $Actor) "Group"
   "Represents a formal or informal collective of Actors."))

(define $Organization
  (make-astype
   (as-uri "Organization") (list $Actor) "Organization"
   "Represents an organization."))

(define $Person
  (make-astype
   (as-uri "Person") (list $Actor) "Person"
   "Represents an individual person."))

(define $Process
  (make-astype
   (as-uri "Process") (list $Actor) "Process"
   "Represents a series of actions taken to achieve a particular goal."))

(define $Service
  (make-astype
   (as-uri "Service") (list $Actor) "Service"
   "Represents a service of any kind."))



;;; Relationship
;;; ============

(define $Relationship
  (make-astype
   (as-uri "Relationship") (list $Object) "Relationship"
   "Describes a relationship between two individuals.
The subject and object properties are used to identify the
connected individuals.

See 3.3.1 [of ActivityStreams 2.0 Vocabulary document] Representing
Relationships Between Entities for additional information."))

(define $Content
  (make-astype
   (as-uri "Content") (list $Object) "Content"
   "Describes an entity representing any form of content. Examples
include documents, images, etc. Content objects typically are not
able to perform activities on their own, yet rather are usually the
object or target of activities."))

(define $Article
  (make-astype
   (as-uri "Article") (list $Content) "Article"
   "Represents any kind of multi-paragraph written work."))

(define $Album
  (make-astype
   (as-uri "Album") (list $Collection) "Album"
   "A type of Collection typically used to organize Image, Video or Audio
objects."))

(define $Folder
  (make-astype
   (as-uri "Folder") (list $Collection) "Folder"
   "A type of Collection typically used to organize objects such as
Documents."))

(define $Story
  (make-astype
   (as-uri "Story") (list $OrderedCollection) "Story"
   "A type of Ordered Collection usually containing Content Items
organized to \"tell a story\"."))

(define $Document
  (make-astype
   (as-uri "Document") (list $Content) "Document"
   "Represents a document of any kind."))

(define $Audio
  (make-astype
   (as-uri "Audio") (list $Document) "Audio"
   "Represents an audio document of any kind."))

(define $Image
  (make-astype
   (as-uri "Image") (list $Document) "Image"
   "An image document of any kind."))

(define $Video
  (make-astype
   (as-uri "Video") (list $Document) "Video"
   "Represents a video document of any kind."))

(define $Note
  (make-astype
   (as-uri "Note") (list $Object) "Note"
   "Represents a short work typically less than a single
paragraph in length."))

(define $Page
  (make-astype
   (as-uri "Page") (list $Document) "Page"
   "Represents a Web Page."))

(define $Question
  (make-astype
   (as-uri "Question") (list $Content $IntransitiveActivity) "Question"
   "Represents a question being asked. Question objects are unique in
that they are an extension of both Content and IntransitiveActivity.
That is, the Question object is an Activity but the direct object is
the question itself."))

(define $Event
  (make-astype
   (as-uri "Event") (list $Object) "Event"
   "Represents any kind of event."))

(define $Place
  (make-astype
   (as-uri "Place") (list $Object) "Place"
   "Represents a logical or physical location.
See 3.3.2 Representing Places [of ActivityStreams 2.0 Vocabulary
document] for additional information."))

(define $Mention
  (make-astype
   (as-uri "Mention") (list $Link) "Mention"
   "A specialized Link that represents an @mention."))

(define $Profile
  (make-astype
   (as-uri "Profile") (list $Content) "Profile"
   "A Profile is a content object that describes another Object,
typically used to describe Actor, objects. The describes property
is used to reference the object being described by the profile."))


(define *core-vocab*
  (list $Object $Link $Activity $IntransitiveActivity $Actor $Collection
        $OrderedCollection $CollectionPage $OrderedCollectionPage
        $Accept $TentativeAccept $Add $Arrive $Create $Delete
        $Follow $Ignore $Join $Leave $Like $Offer $Invite $Reject
        $TentativeReject $Remove $Undo $Update $Experience $View
        $Listen $Read $Move $Travel $Announce $Block $Flag $Dislike
        $Application $Group $Organization $Person $Process $Service
        $Relationship $Content $Article $Album $Folder $Story $Document
        $Audio $Image $Video $Note $Page $Question $Event $Place $Mention
        $Profile))

(define *basic-env*
  (make-asenv #:vocabs *core-vocab*
              ;; #:shortids (shortids-from-vocab *core-vocab*)
              ;; #:c-accessors (shortids-from-vocab *core-vocab*)
              ))
