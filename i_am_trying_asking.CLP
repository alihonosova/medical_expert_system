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
(deffacts example 
	(sympthom (name temperature) (type flu) (question "Is there temperature? - yes - 1) no - 2) why - 3) ") (answer unknown))
	(sympthom (name pain_in_muscles) (type flu) (question "Is there pain in muscles? - yes - 1) no - 2) why - 3)") (answer unknown))
	(sympthom (name cought) (type flu) (question "Is there cought? - yes - 1) no - 2) why - 3)") (answer unknown))
	(sympthom (name weaknesses) (type flu) (question "Is there weaknesses? - yes - 1) no - 2) why - 3)") (answer unknown))
	(sympthom (name vomit) (type flu gastritis) (question "Is there vomit? - yes - 1) no - 2) why - 3)") (answer unknown))
	(sympthom (name weaknesses) (type flu) (question "Is there weaknesses? - yes - 1) no - 2) why - 3)") (answer unknown))
	(sympthom (name pain_in_stomack) (type gastritis) (question "Is there pain in stomack? - yes - 1) no - 2) why - 3)") (answer unknown))
)

(defrule ask_flu
(not(not_flu))
?f1 <- (sympthom(name ?name) (type $? flu $?) (question ?question) (answer unknown))
=>
(bind ?response (ask-allowed ?question 1 2 3))
(if (eq ?response 1)
then 
(modify ?f1 (answer yes))
else (if (eq ?response 2)
then
(assert(not_flu))
(modify ?f1 (answer no))
else 
(if (eq ?response 3)
then
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a flu" crlf))
)
)
)

(defrule ask_gastritis
(not_flu)
?f1 <- (sympthom(name ?name) (type $? gastritis $?) (question ?question) (answer unknown))
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
(modify ?f1 (answer unknown))
(printout t "There is suggest that you have a problem with your stomack" crlf))
)
)
)

(defrule flu 
(sympthom (name temperature) (type flu) (question "Is there temperature? - yes - 1) no - 2) why - 3) ") (answer yes))
(sympthom (name pain_in_muscles) (type flu) (question "Is there pain in muscles? - yes - 1) no - 2) why - 3)") (answer yes))
(sympthom (name cought) (type flu) (question "Is there cought? - yes - 1) no - 2) why - 3)") (answer yes))
(sympthom (name weaknesses) (type flu) (question "Is there weaknesses? - yes - 1) no - 2) why - 3)") (answer yes))
(sympthom (name vomit) (type flu gastritis) (question "Is there vomit? - yes - 1) no - 2) why - 3)") (answer yes))
=>
(assert(suggest "This is flu"))
)

(defrule gastritis 
(sympthom (name vomit) (type flu gastritis) (question "Is there vomit? - yes - 1) no - 2) why - 3)") (answer yes))
(sympthom (name pain_in_stomack) (type gastritis) (question "Is there pain in stomack? - yes - 1) no - 2) why - 3)") (answer yes))
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