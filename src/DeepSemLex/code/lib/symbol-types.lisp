;;;; important types of symbols
(in-package :dsl)

;;; lists from querying old lexicon (probably not actually complete)

(deftype pos ()
  '(member adj adv art conj cv FP infinitival-to N name neg number-unit ordinal prep pro punc quan uttword V value ^ ^o ^s))

(deftype syn-cat ()
  '(or pos (member adjp advbl CP NP number PP pred S utt VP VP- word
		   ;; HACK: include t to represent things like (% ?sc), which
		   ;; happen in templates
		   t
		   )))

(deftype syn-arg ()
  '(member lsubj lobj liobj lcomp subcat subcat2 post-subcat premod argument))

(deftype syn-feat ()
  '(member
  ; key		; values (unless just + -)
    abbrev
    adj-s-prepost
    agr		; 1p 1s 2p 2s 3p 3s
    allow-before
    allow-deleted-comp
    allow-post-n1-subcat
    allow-pre-mod
    atype 	; post pre pre-vp
    aux
    auxname	; - passive progr perf
    cardinality
    case	; - obj sub
    changesem
    comparative	; + superl
    comp-op	; - less
    compar-op
    conj
    contraction
    diectic
    disj
    ellipsis
    else-word
    ;; Form wasn't originally an explicit feature, just passed around inside
    ;; LXM, but I think it makes sense as a feature. Note that this only covers
    ;; inflections, not cross-POS derivations like :nom, :result, and :ly
    form	; none 12s123pbase 3s ing past pastpart sing plur er est
    functn	; acceptability-val compare-val linear-scale
    gap
    generated
    ground-oblig
    implicit-arg
    indef-only
    lex		; us you
    mass	; bare mass count
    modal
    name
    nname
    neg
    negatable
    npmod
    nobarespec
    npagr	; ???
    number
    numerical-form	; numercal-both numerical-comp
    operator	; one-of
    particle
    particle-role-map ; result manner (sem roles?)
    pointer
    pos		; (see deftype pos)
    poss
    prefix
    pro		; + indef recip
    ptype	; (prepositions)
;    qcomp	; (constits (blech))
;    qof		; (ditto)
    qtype	; wh
    quant
    refl
    seq
    sing-lf-only
    skip
    sort	; attribute-unit binary-constraint classifier disc double-subcat else number-unit operator other-reln pp-word pred substance-unit unit-measure
    status	; definite-plural indefinite indefinite-plural quantifier
    stem	; he her him it me one she they we who you
    subcat	; - s pp np vp adjp
    subcat1	;
    subcat2	;
    subcat3	;
    unaccusative
    vform	; base fut pres ing past pastpart passive
    wh 		; q r (question, relative clause)
    ^s-plural
    ))

;;; sem-feat subtypes

(deftype phys-obj-feat ()
  '(member object-function origin form mobility group spatial-abstraction intentional information container kr-type trajectory))

(deftype situation-feat ()
  '(member aspect time-span cause trajectory locative intentional information container kr-type type origin iobj))

(deftype abstr-obj-feat ()
  '(member measure-function scale intentional information container gradability kr-type object-function origin intensity orientation))

(deftype proposition-feat ()
  '(member intentional information container gradability kr-type origin))

(deftype time-feat ()
  '(member time-function scale time-scale kr-type))

(in-package :vn)

  (common-lisp::deftype sem-feat ()
    ;; this list generated by getting all the unique type attributes of SELRESTR
    ;; elements in the VerbNet data
    ;; VN 3.2
    ;'(common-lisp::member abstract animal animate body_part comestible communication concrete currency dest dest_conf dest_dir dir elongated force garment human int_control loc location machine nonrigid organization path plural pointy refl region scalar solid sound spatial src state substance time vehicle))
    ;; VN 3.2b
    '(common-lisp::member abstract animal animate biotic body_part comestible communication concrete currency dest dest_conf dest_dir dir elongated force garment human int_control loc location machine nonrigid organization path plural pointy refl region scalar solid sound spatial src state substance time vehicle))

  (common-lisp::deftype sem-role ()
    ;; VN 3.2
    ;'(common-lisp::member Agent Asset Attribute Beneficiary Cause Co-Agent Co-Patient Co-Theme Destination Experiencer Extent Goal Initial_Location Instrument Location Material Patient Pivot Predicate Product Recipient Result Source Stimulus Theme Time Topic Trajectory Value Scale)
    ;; VN 3.2b
    '(common-lisp::member Actor Agent Asset Attribute Beneficiary Cause Co-Agent Co-Patient Co-Theme Context Destination Experiencer Extent Goal Initial_Location Instrument Location Manner Material Path Patient Pivot Predicate Product Recipient Reflexive Result Source Stimulus Theme Time Topic Trajectory Value)
    )

(common-lisp::in-package :dsl)

(deftype sem-feat ()
  '(or (member type) phys-obj-feat situation-feat abstr-obj-feat
       proposition-feat time-feat vn::sem-feat))

;;; feature list types

(deftype feats (&optional (feat-type 'symbol))
  `(alist :from ,feat-type :to (maybe-disj symbol)))

(in-package :ont)

(common-lisp::deftype sem-role ()
  ;; this list found by grepping templates and OM files (see get-ont-sem-roles.sh)
  '(common-lisp::member affected affected1 affected-result agent agent1 along arg0 arg1 assoc-with beneficiary between cause cognizer content contents criterion donor effect event-of-change experiencer extent figure figure1 formal formal1 goal goal-reln goods ground ground1 indefinite kind location manner money neutral neutral1 neutral2 norole one-of path position-reln pro pro-det pro-set quantifier reason result scale source source-reln standard stimulus value
    ;; some I was surprised were no longer there
    of of1 of2
    ;; some I didn't delete because maybe they're just not used *yet*
    ;; (feel free to delete them if you know better) --wdebeaum
    affected-create affected-create1 affected-result1 co-agent co-result co-theme effect-implicit from-loc material obj-val place predicate property purpose-implicit result-val spatial-loc stative theme time-duration-rel to-loc via
    ;; some extras output by src/Systems/gloss/test.lisp
    co-theme patient partner means method
    ))

(common-lisp::in-package :dsl)

(deftype sem-role ()
  '(or vn::sem-role ont::sem-role))

