(define-module (activitystuff as-shorthand)
  #:use-module (activitystuff asobj)
  #:use-module (activitystuff vocab)
  #:export (object link activity intransitive-activity actor
            collection ordered-collection

            accept tentative-accept add arrive create delete
            arrive follow ignore join leave like offer invite
            reject tentative-reject remove undo update experience
            view listen read move travel announce block flag
            dislike

            application group person process service

            relationship content article album folder story
            document audio image video note page question
            event place mention profile))

(define-syntax-rule (as-shortcut name astype)
  (begin
    (define (name . kwargs)
      (apply make-as astype *basic-env* kwargs))
    (if (astype-notes astype)
        (set-procedure-property! name 'documentation
                                 (astype-notes astype)))))

(as-shortcut object $Object)
(as-shortcut link $Link)
(as-shortcut activity $Activity)
(as-shortcut intransitive-activity $IntransitiveActivity)
(as-shortcut actor $Actor)
(as-shortcut collection $Collection)
(as-shortcut ordered-collection $OrderedCollection)

(as-shortcut accept $Accept)
(as-shortcut tentative-accept $TentativeAccept)
(as-shortcut add $Add)
(as-shortcut arrive $Arrive)
(as-shortcut create $Create)
(as-shortcut delete $Delete)
(as-shortcut arrive $Arrive)
(as-shortcut follow $Follow)
(as-shortcut ignore $Ignore)
(as-shortcut join $Join)
(as-shortcut leave $Leave)
(as-shortcut like $Like)
(as-shortcut offer $Offer)
(as-shortcut invite $Invite)
(as-shortcut reject $Reject)
(as-shortcut tentative-reject $TentativeReject)
(as-shortcut remove $Remove)
(as-shortcut undo $Undo)
(as-shortcut update $Update)
(as-shortcut experience $Experience)
(as-shortcut view $View)
(as-shortcut listen $Listen)
(as-shortcut read $Read)
(as-shortcut move $Move)
(as-shortcut travel $Travel)
(as-shortcut announce $Announce)
(as-shortcut block $Block)
(as-shortcut flag $Flag)
(as-shortcut dislike $Dislike)

(as-shortcut application $Application)
(as-shortcut group $Organization)
(as-shortcut person $Person)
(as-shortcut process $Process)
(as-shortcut service $Service)

(as-shortcut relationship $Relationship)
(as-shortcut content $Content)
(as-shortcut article $Article)
(as-shortcut album $Album)
(as-shortcut folder $Folder)
(as-shortcut story $Story)
(as-shortcut document $Document)
(as-shortcut audio $Audio)
(as-shortcut image $Image)
(as-shortcut video $Video)
(as-shortcut note $Note)
(as-shortcut page $Page)
(as-shortcut question $Question)
(as-shortcut event $Event)
(as-shortcut place $Place)
(as-shortcut mention $Mention)
(as-shortcut profile $Profile)
