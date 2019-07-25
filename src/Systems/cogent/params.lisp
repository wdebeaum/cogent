;;PARSER, TEXTTAGGER and WORDFINDER settings

(setf parser::*include-parse-tree-in-messages* '(w::lex)) ; for WebParser

(setq *use-texttagger* T)
(setq parser::*filter-and-preparse-input* nil)
;;(setf (parser::flexible-semantic-matching parser::*chart*) t)

;;;; Parser options
(setq parser::*parser-init-settings*
      '((parser::*parser-display-enabled* nil)
	(parser::*in-system* :cogent)
	;; average number of letters in a word (not critical)
	(parser::*word-length* 8)
	;; boost factor based on length of input covered
	(parser::*score-length-multiplier* .4)
	;; not clear this is helpful
	(parser::*score-corner-multiplier* 0)
	;; indicate we should use POS information
	(parser::*use-tags-as-filter* t)
	;; penalty multiplier for lex entries that do not match POS tags
	(parser::*bad-tag-multiplier* .9)
	;; penalty for referential-sem lex items
	(parser::*referential-sem-penalty* .98)
	;; constituents that we expect in the skeleton
	(parser::*skeleton-constit-cats* '(W::NP W::CP W::VP W::ADVBL W::PP W::S))
	;; boost constituents that match the skeleton (from stat. parser)
	(parser::*skeleton-boost-factor* .2)   ;; new boosting scheme - 20% of difference to 1.0
	;; penalty for crossing skeleton constituent boundaries
	((setf (parser::barrier-penalty parser::*chart*) .99))
	;;
	(parser::*use-senses-as-filter* t)
	;;
	(parser::*bad-sense-multiplier* .96)
	;;
	(parser::*no-positions-in-lf* nil)
	;;
	(parser::*beam-pruning-on* nil)   ; no pruning
	;;
	(parser::*pruning-frequency* 500)
	;;
	(parser::*beam-width* 20)
	;; max number of constituents built before stopping
	((parser::setmaxnumberentries 3000))
	;;
	((parser::setmaxchartsize 4000))
	;;
	;;(parser::*kr-type-info-desired* '(:WNsense :DRUM :wordnet))
	;;
	((setf (parser::flexible-semantic-matching parser::*chart*) t))
	;; boost content words that have domain specific info attached
	(parser::*domain-boosting-factor* 1.01)
	;; have the parser remove tagged constituents that are subparts of other terms with domain info
	(parser::*filter-texttagger-input* t)
	;; number of interpretations to obtain before stopping
	((setf (parser::number-parses-to-find parser::*chart*) 2))
	;; number of interpretations returned
	((setf (parser::number-parses-desired parser::*chart*) 1))
	;; disfavor speech acts not common in text
	
	((parser::customize-cost-table 
	  '((ont::SA_QUERY 1.2) 
	     (ont::SA_IDENTIFY 1.3) 
	     (ont::SA_pred-fragment 2) 
	     (ont::SA_request 1) 
	     (ont::SA_YN-QUESTION 1) 
	     (w::ADJP 1.2) 
	     (w::advbl 1.3)
	     (ont::SA_CONFIRM 1) 
	     (ont::SA_WH-QUESTION 1) 
	     (ont::SA_TELL 1)
	     (w::CP 2) 
	     (w::VP 1.1) 
	     (w::punc .5))))
	))
(parser::initialize-settings) 

;; dialogue manager, eg: textIM, simpleIM, extractIM...
(setq im::*current-dialog-manager* #'im::SequentialLFTransformIM)   ;;#'im::simpleIM)
(setq im::*cps-control* t)
;(setq im::*substitute-types-in-pros* t)
(setq im::*substitute-types-in-pros* nil)
(setq im::*compute-force-from-tmas* t)
;(setq im::*max-allowed-utts-in-turn* 2) ;; we're being a little generous to try to pick up more referring expressions
(setq im::*max-allowed-utts-in-turn* 3) ;; we're being a little generous to try to pick up more referring expressions
(setq im::*external-name-resolution* nil)  ;; will eventually be set to T when we do reference resolution in context
(setq im::*show-lf-graphs* t)
(setq im::*lf-output-by-utterance* t)
(setq im::*max-cover-with-strict-disjoint-extractions* nil)
(setq im::*eliminate-subevents* nil)
(setq im::*allow-optional-lfs* t) ;; set to t for optional term matching
(setq im::*output-format* 'im::lf-term)
;(setq im::*output-format* 'im::LF)

;(setq im::*symbol-map* nil)

(setq *print-pretty* t)

;;;; LxM options
;; use WordFinder?
(setq lxm::*use-wordfinder* t)
;; we are trying to really depend on the Stanford parser (for now!)
(setq lxm::*use-tagged-senses-only* t)
;; don't use wordnet if we have domain-specific info from TextTagger
(setq lxm::*no-wf-senses-for-words-tagged-with-ont-types* t)
;; don't use wordnet if we have TRIPS entries  
(setq  lxm::*use-trips-and-wf-senses* nil) 

;;;; LOGGING options
(setq logging::*logging-enabled* nil)
(setq logging2::*logging-enabled* nil)

;;  DM settings
(setq dagent::*silent-failures* nil)  ;; don't ignore utterance failure
(setq dagent::*using-alarms* t)   ;; no alarms
(setq dagent::*disabled-wizard* t)  ;; no wizard
(dagent::trace-on 1)

; just the default user
(setq dagent::*users* (list (cons "desktop" (dagent::make-user :name "desktop" :channel-id 'dagent::desktop))))

(setq im::*extraction-sequence* '((im::preprocessRules) (im::cogentRules)))
(setq im::*substitute-terms-in-extraction* t)
