(in-package "IM")

(reset-im-rules 'cogentRules)  ;; this allows you to edit this file and reload it without having to reload the entire system

(mapcar #'(lambda (x) (add-im-rule x 'cogentRules))  ;; sets of rules are tagged so they can be managed independently 
	'(

;;;;;;;;;;;;;;;

#|	  
          ((ONT::F ?ev ?!t)   
           -rule-generic>
           60
           (ONT::EVENT ?ev ?!t
            :rule -rule-generic
            )
           )

          (((? reln ONT::QUANTIFIER ONT::KIND ONT::A ONT::INDEF-PLURAL ONT::THE ONT::THE-SET ONT::INDEF-SET ONT::BARE ONT::SM ONT::PRO ONT::PRO-SET) ?ev ?!t)   
           -rule-generic2>
           60
           (ONT::TERM ?ev ?!t
            :rule -rule-generic2
            )
           )
|#
	  
;;;;;;;;;;;;;;;
	  
	  )
	)
