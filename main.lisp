;;;(defvar *example* (read))
;;;(format t "~s~%" *example*)

(defvar *menuAction* nil)

;;Since lists cannot be passed as arguments into functions I created  global lists. Maybe it's stupid and hard to read but it's easy to implement for newbee.

;;list containing all symptoms checked during expert system session
(defparameter *checkedSymptomsList* nil)

(defparameter *sublist* nil)

;;global list for symptoms of checked problem
(defparameter *symptomList* nil)

(defparameter *fileLoadingList* (list nil))

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

(defun checkSymptoms(problem)
	(if *symptomList*
		(progn 
			(defparameter symptom (car *symptomList*))
			(defparameter symptomValue (cadr *symptomList*))
			(format t "Symptom ~s~%" symptom)
			(format t "Type 'true' or 'false'.~%")
			(defparameter userInput (read-line))
			;;If user answer to symptom the same as DB than check further symptoms of problem. Go to another problem otherwise
			(if (equalp symptomValue (validateAnswerInput userInput))
				(progn 
					(setf *symptomList* (cddr *symptomList*))
					(checkSymptoms problem)
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
	  
(defun addProblem()
 #||(get-file "testDb.txt")
 (format t "~s~%" *fileLoadingList*)
 (setf *problemList* (cdr *problemList*))
 (format t "~s~%" *problemList*)||#
 (save-db "dbSaveTest.txt")
)

(defun startExpertSystem()
	(catch 'err 
		(dolist (tmpList *problemList*)
			(setf *sublist* tmpList)
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
				(addProblem))
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
(setf *menuAction* (read))
(terpri)
(startAction *menuAction*)
(menuLoop)
) 

(load-db "dbSaveTest.txt")
(menuLoop)
