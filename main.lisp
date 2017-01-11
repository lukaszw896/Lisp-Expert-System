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


(defparameter *problemList* 
	(list 
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
)

(defun attach1 (x)
  (push x (cdr (last *fileLoadingList*)))
 )

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do
          (attach1 line))))

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
		(format t "YOUR PROBLEM: ~s~%" problem)
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

(defun addProblem()
 (get-file "testDb.txt")
 (format t "~s~%" *fileLoadingList*)
 (format t "~s~%" *problemList*)
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


(menuLoop)
