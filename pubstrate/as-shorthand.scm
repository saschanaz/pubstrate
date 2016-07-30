(define-module (pubstrate as-shorthand)
  #:use-module (pubstrate asobj)
  #:use-module (pubstrate vocab)
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

(define-syntax-rule (as-shorthand name astype)
  (begin
    (define (name . kwargs)
      (apply make-as astype *basic-env* kwargs))
    (if (astype-notes astype)
        (set-procedure-property! name 'documentation
                                 (astype-notes astype)))))

(as-shorthand object $Object)
(as-shorthand link $Link)
(as-shorthand activity $Activity)
(as-shorthand intransitive-activity $IntransitiveActivity)
(as-shorthand actor $Actor)
(as-shorthand collection $Collection)
(as-shorthand ordered-collection $OrderedCollection)

(as-shorthand accept $Accept)
(as-shorthand tentative-accept $TentativeAccept)
(as-shorthand add $Add)
(as-shorthand arrive $Arrive)
(as-shorthand create $Create)
(as-shorthand delete $Delete)
(as-shorthand arrive $Arrive)
(as-shorthand follow $Follow)
(as-shorthand ignore $Ignore)
(as-shorthand join $Join)
(as-shorthand leave $Leave)
(as-shorthand like $Like)
(as-shorthand offer $Offer)
(as-shorthand invite $Invite)
(as-shorthand reject $Reject)
(as-shorthand tentative-reject $TentativeReject)
(as-shorthand remove $Remove)
(as-shorthand undo $Undo)
(as-shorthand update $Update)
(as-shorthand experience $Experience)
(as-shorthand view $View)
(as-shorthand listen $Listen)
(as-shorthand read $Read)
(as-shorthand move $Move)
(as-shorthand travel $Travel)
(as-shorthand announce $Announce)
(as-shorthand block $Block)
(as-shorthand flag $Flag)
(as-shorthand dislike $Dislike)

(as-shorthand application $Application)
(as-shorthand group $Organization)
(as-shorthand person $Person)
(as-shorthand process $Process)
(as-shorthand service $Service)

(as-shorthand relationship $Relationship)
(as-shorthand content $Content)
(as-shorthand article $Article)
(as-shorthand album $Album)
(as-shorthand folder $Folder)
(as-shorthand story $Story)
(as-shorthand document $Document)
(as-shorthand audio $Audio)
(as-shorthand image $Image)
(as-shorthand video $Video)
(as-shorthand note $Note)
(as-shorthand page $Page)
(as-shorthand question $Question)
(as-shorthand event $Event)
(as-shorthand place $Place)
(as-shorthand mention $Mention)
(as-shorthand profile $Profile)
