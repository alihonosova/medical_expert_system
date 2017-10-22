(clear) 
(deffunction  ask   (?question  $?allowed) 
  (printout  t  ?question  ?allowed)  
  (bind  ?answer  (read)) 
?answer 
) 
 
(deffunction  ask-allowed   (?question $?allowed)  
        (bind   ?answer (ask ?question)) 
  (while (not (member  ?answer $?allowed) )  
   do  
    (printout t "Reenter, please" crlf) 
    (bind ?answer (ask ?question)) 
 ) 
?answer 
) 
(deffunction ask-yes-no   (?question) 
  (bind  ?response   (ask-allowed  ?question yes no))  
   (eq ?response yes) 
) 

(deftemplate sympthom (slot name) (multislot type) (slot question) (slot answer))
(deffacts example (sympthom (name temperature) (type flu stomack) (question "Is there temperature? - yes - 1) no - 2) why - 3) ") (answer unknown))
		  (sympthom (name pain) (type stomack) (question "Is there pain? - yes - 1) no - 2) why - 3)") (answer unknown))
		  (sympthom (name weaknesses) (type flu) (question "Is there weaknesses? - yes - 1) no - 2) why - 3)") (answer unknown))
)

(defrule ask_flu
(not(not_flu))
?f1 <- (sympthom(name ?name) (type $? flu $?) (question ?question) (answer unknown))
=>
(if (eq (ask-allowed ?question 1 2 3) 1)
then 
(modify ?f1 (answer yes))
else
(modify ?f1 (answer no))
(assert( not_flu ))
)
)

(defrule ask_stomack
(not_flu)
?f1 <- (sympthom(name ?name) (type $? stomack $?) (question ?question) (answer unknown))
=>

(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then  
(modify ?f1 (answer yes))
else (if (eq ?response 2)
then
(assert(not_stomack))
(modify ?f1 (answer no))
else 
(if (eq ?response 3)
then
(assert(stomack_answer)))
)
)
)

(defrule stomack_answer
(sympthom(name ?name) (type $? stomack $?) (question ?question) (answer ?answer))
=>
(printout t ?name crlf)
)

(defrule flu 
(sympthom (name temperature) (type flu stomack) (question "Is there temperature? - yes - 1) no - 2) why - 3) ") (answer yes))
(sympthom (name weaknesses) (type flu) (question "Is there weaknesses? - yes - 1) no - 2) why - 3)") (answer yes))
=>
(assert(suggest "This is flu"))
)

(defrule stomack 
(sympthom (name temperature) (type flu stomack) (question "Is there temperature? - yes - 1) no - 2) why - 3) ") (answer yes))
(sympthom (name pain) (type stomack) (question "Is there pain? - yes - 1) no - 2) why - 3)") (answer yes))
=>
(assert(suggest "This is stomack"))
)

(defrule PrintSuggest    
 (suggest ?x) 
 => 
  (printout t ?x crlf) 
) 
(defrule NoSuggest    
  (declare (salience -10)) 
  (not (suggest ?)) 
 => 
  (printout t "Sorry, there is no suggest." crlf) 
)