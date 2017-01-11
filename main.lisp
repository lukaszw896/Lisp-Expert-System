;;;(defvar *example* (read))
;;;(format t "~s~%" *example*)

;;Since lists cannot be passed as arguments into functions I created  global lists. Maybe it's stupid and hard to read but it's easy to implement for newbee.

;;list containing all symptoms checked during expert system session
(defparameter *checkedSymptomsList* (list 
		"Dummy?"
		"false"))

(defparameter *sublist* nil)

;;global list for symptoms of checked problem
(defparameter *symptomList* nil)

(defparameter *fileLoadingList* (list nil))

;;list used for different tasks
(defparameter *tmpList* nil)

(defparameter *problemList* nil)
	#||(list 
		(list "Lack of power cable"
		"Computer turns on?"
		"false"
		"Pluged in power cable?"
		"false")
		(list "Damaged power brick"
		"Computer turns on?"
		"true"
		"Computer is restarting?"
		"true"
		)
		(list "Lack of network card"
		"Internet connection?"
		"false"
		"Existance of ethernet port?"
		"false")
	)
)||#

(defun analyseProblem()
	(defparameter problem (car *sublist*))
	(setf *symptomList* (cdr *sublist*))
	 ;;(format t "Problem: ~s~%" problem)
	(checkSymptoms problem)
)

;;Passes argument further if format is ok
(defun validateAnswerInput(userInput)
	(cond ((equalp "false" userInput)
		  userInput)
		  ((equalp "true" userInput)
		  userInput)
		  ((progn 
			(format t "Answer is not in the correct format. Please type true or false.")
			(defparameter userInput (read-line))
			(validateAnswerInput userInput)
		  )))
)

(defun checkPreviousSymptoms (symptom)
	(setf *tmpList* *checkedSymptomsList*)
	(traverseTroughPreviusSymptoms symptom)
	(if *tmpList*
		(progn 
			(car *tmpList*)
		)
	)
)

(defun traverseTroughPreviusSymptoms(symptom)
	(if *tmpList*
		(if (equalp (car *tmpList*) symptom)
			(setf *tmpList* (cdr *tmpList*))
			(progn
				(setf *tmpList* (cddr *tmpList*))
				(traverseTroughPreviusSymptoms symptom)
			)
		)
	)
)

(defun checkSymptoms(problem)
	(if *symptomList*
		(progn 
			(defparameter symptom (car *symptomList*))
			(defparameter symptomValue (cadr *symptomList*))
			(setf previousValue (checkPreviousSymptoms symptom))
			(if previousValue
				(progn
					(if (equalp symptomValue previousValue)
						(progn 
							(setf *symptomList* (cddr *symptomList*))
							(checkSymptoms problem)
						)
					)
				)
				(progn
					(format t "Symptom ~s~%" symptom)
					(format t "Type 'true' or 'false'.~%")
					(defparameter userInput (read-line))
					;;If user answer to symptom the same as DB than check further symptoms of problem. Go to another problem otherwise
					(if (equalp symptomValue (validateAnswerInput userInput))
						(progn 
							(setf *symptomList* (cddr *symptomList*))
							(push symptom (cdr (last *checkedSymptomsList*)))
							(push userInput (cdr (last *checkedSymptomsList*)))
							(checkSymptoms problem)
						)
						(progn 
							(push symptom (cdr (last *checkedSymptomsList*)))
							(push userInput (cdr (last *checkedSymptomsList*)))
						)
					)
				)
			)
		)
		(progn 
			(format t "YOUR PROBLEM: ~s~%" problem)
			(terpri)
			
		)
	)
)

(defun printMenu()
	(format t "******* Choose action: **********~%")
	(format t "*********************************~%")
	(format t "* 1.Add problem to the database *~%")
	(format t "* 2.Start expert system         *~%")
	(format t "* 3.Exit program                *~%")
	(format t "*********************************~%")
)

(defun printAddProblemMenu()
	(format t "******* Choose action: **********~%")
	(format t "*********************************~%")
	(format t "* 1.Print all problems          *~%")
	(format t "* 2.Print all symptoms          *~%")
	(format t "* 3.Add problem to DB           *~%")
	(format t "*********************************~%")
)
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *problemList* out))))
	  
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *problemList* (read in)))))
	 
(defun printAllProblems()

)

(defun printAllSymptoms()

)

(defun getProblemFromUser()
	(format t "**Please type name of the problem you want to add to DB**~%")
	(setf problemName (read-line))
	(setf *tmpList* (list problemName))
	(getSymptomName)
)

(defun getSymptomName()
	(format t "**Please type symptom you want to add to DB (To end adding type end)**~%")
	(setf symptomName (read-line))
	(if (equalp symptomName "end")
		(progn
			(push *tmpList* (cdr (last *problemList*)))
			(save-db "ESDB.cs")
		)		
		(progn 
			(push symptomName (cdr (last *tmpList*)))
			(getSymptomValue)
			(getSymptomName)
		)
	)		
)

(defun getSymptomValue()
	(format t "**Type the value of symptom - true or false**~%")
	(setf symVal (validateAnswerInput (read-line)))
	(push symVal (cdr (last *tmpList*)))
)
	 
(defun addProblem()
	(printAddProblemMenu)
	(setf action (read))
	(if(numberp action)
		(cond   ((= action 1)
				(printAllProblems))
				((= action 2)
				(printAllSymptoms))
				((= action 3)
				(getProblemFromUser))
				((wrongInput))
		)
		(wrongInput))
)



(defun startExpertSystem()
	(catch 'err 
		(dolist (myList *problemList*)
			(setf *sublist* myList)
			(analyseProblem)
			(if *symptomList* 
			()
			(throw 'err "ERROR"))
		)
		(format t "Sorry but we weren't able to find your problem.~%")
		(terpri)
	)
)

(defun exitProgram()
	(format t "Exiting the program!~%")
	(quit)
)

(defun wrongInput()
(format t "Input could not be processed. Please try again~%")
)

(defun startAction(action)
	(if(numberp action)
		(cond   ((= action 1)
				(getProblemFromUser))
				((= action 2)
				(startExpertSystem))
				((= action 3)
				(exitProgram))
				((wrongInput))
		)
		(wrongInput))
)

(defun menuLoop()
(printMenu)
(setf menuAction (read))
(terpri)
(startAction menuAction)
(setf *checkedSymptomsList* (list "Dummy?" "false"))
(menuLoop)
) 

(load-db "dbSaveTest.txt")
(menuLoop)
