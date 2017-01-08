;;;(defvar *example* (read))
;;;(format t "~s~%" *example*)

(defvar *menuAction* nil)

(defun printMenu()
	(format t "******* Choose action: **********~%")
	(format t "*********************************~%")
	(format t "* 1.Add problem to the database *~%")
	(format t "* 2.Start expert system         *~%")
	(format t "* 3.Exit program                *~%")
	(format t "*********************************~%")
)

(defun addProblem()

)

(defun startExpertSystem()

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
(startAction *menuAction*)
(menuLoop)
) 


(menuLoop)
