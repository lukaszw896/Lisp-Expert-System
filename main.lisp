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
(defmacro prints (input-string)
	`(format t "~s~%" ,input-string)
)

(defmacro ifp (condition &body body1)
	`(if ,condition
		(progn ,@body1)
	)
)

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
			(prints "Answer is not in the correct format. Please type true or false.")
			(defparameter userInput (read-line))
			(validateAnswerInput userInput)
		  )))
)

(defun checkPreviousSymptoms (symptom)
	(setf *tmpList* *checkedSymptomsList*)
	(traverseTroughPreviusSymptoms symptom)
	(ifp *tmpList* (car *tmpList*))
	#||(if *tmpList*
		(progn 
			(car *tmpList*)
		)
	)||#
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
					(prints "Type 'true' or 'false'.")
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
	(prints "******* Choose action: **********")
	(prints "*********************************")
	(prints "* 1.Add problem to the database *")
	(prints "* 2.Start expert system         *")
	(prints "* 3.Print macroexpand           *")
	(prints "* 4.Exit program                *")
	(prints "*********************************")
)

(defun printAddProblemMenu()
	(prints "******* Choose action: **********")
	(prints "*********************************")
	(prints "* 1.Print problems and symptoms *")
	(prints "* 2.Add problem to DB           *")
	(prints "*********************************")
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
			(save-db "dbSaveTest.txt")
		)		
		(progn 
			(push symptomName (cdr (last *tmpList*)))
			(getSymptomValue)
			(getSymptomName)
		)
	)		
)

(defun getSymptomValue()
	(prints "**Type the value of symptom - true or false**")
	(setf symVal (validateAnswerInput (read-line)))
	(push symVal (cdr (last *tmpList*)))
)
	 
(defun addProblem()
	(printAddProblemMenu)
	(setf action (read))
	(if(numberp action)
		(cond   ((= action 1)
				(progn 
				(setf *tmpList* *problemList*)
				(printAllProblemsAndSymptoms *tmpList*)))
				((= action 2)
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
		(prints "Sorry but we weren't able to find your problem.")
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
				(addProblem))
				((= action 2)
				(startExpertSystem))
				((= action 3)
				(prints (macroexpand '(printAllProblemsAndSymptoms *tmpList*))))
				((= action 4)
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

(defmacro printAllProblemsAndSymptoms (condition)
	`(ifp ,condition 
		(prints (car *tmpList*))
		(setf *tmpList* (cdr *tmpList*))
		(printAllProblemsAndSymptoms ,condition))
	#||`(if ,condition
		(progn 
		(prints (car *tmpList*))
		(setf *tmpList* (cdr *tmpList*))
		(printAllProblemsAndSymptoms ,condition))
		)
		||#
)

(load-db "dbSaveTest.txt")
(menuLoop)

;;(format t "~s~%"(macroexpand '(prints "Test")))
;;(format t "~s~%"(macroexpand-1 '(printAllProblemsAndSymptoms *tmpList*)))

;;(prints (read-line))
