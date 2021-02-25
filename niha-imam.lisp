;;;; PROJECT 4: CONNECT FOUR
;;;; Niha Imam
;;;;
;;;;
;;;; This file contains the heuristic board evaluation function
;;;; and the alpha-beta searcher for a connect four game




;; need for speed and some light debugging
(declaim (optimize (compilation-speed 0)
                   (debug 1)
                   (space 0)
                   (speed 3)))

(defpackage :niha-imam
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :niha-imam)




(defun alpha-beta (game current-depth max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta)
  "Does alpha-beta search.
  is-maxs-turn-p takes one argument (a game) and returns true if it's max's turn in that game.  
  expand takes one argument (a game) and gives all the immediate child games for this game.
  terminal-p takes one argument (a game) and returns true if the game is over or not.
  evaluate takes TWO arguements (a game and the is-maxs-turn-p function) and provides a quality
  assessment of the game for 'max', from min-wins (-1000) to max-wins (1000)."

  ;; if terminal(s) or depth >= max depth
  (if (or (funcall terminal-p game) (> current-depth max-depth))
      ;; return evaluate
      (funcall evaluate game is-maxs-turn-p)

      ;;else do more stuff
      ;;if ismaxturn(s) then
      (if (funcall is-maxs-turn-p game)
	  ;;its max's turn
	  (progn
	    ;;for every child in children also added a terminating statement
	    (loop for child in (funcall expand game) until (>= alpha beta) do
		 ;;set alpha to the max of alpha and alpha-beta
		 (setf alpha (max alpha (alpha-beta child (incf current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta))))
	    ;;if alpha>=beta return beta else return alpha
	    (if (>= alpha beta) beta alpha))
	  ;;its mins turn
	  (progn
	    ;;for every child in children also added a terminating statement
	    (loop for child in (funcall expand game) until (>= alpha beta) do
		 ;;set beta to the min of beta and alpha-beta
		 (setf beta (min beta (alpha-beta child (incf current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta))))
	    ;;if alpha>=beta return alpha else return beta
	    (if (>= alpha beta) alpha beta)))))





(defun horizontal (game &optional is-maxs-turn-p)
  "Helper function for the evaluate heuristic.
   Goes over every row on the board and adds up scores"

  ;;keep track of positive and negatives
  (let ((score 0) (pos 0) (neg 0)
	(curr 22) (last 22))
    ;;loop through every row
    (loop for y from 0 to (1- (height game)) do
	 (setf pos 0)
	 (setf neg 0)
	 (loop for x from 0 to (1- (width game)) do
	      ;;add points to the score based on positive or negative one
	      (setf curr (aref (board game) x y))
	      (incf score curr)
	      ;;if more than one in a row occur
	      (if (and (/= curr 0) (= last curr))
		  (progn
		    ;;if more than one black occurs add 10
		    (if (> curr 0) (progn (incf pos) (incf score 10)))
		    ;;if more than one red occurs take away 5
		    (if (< curr 0) (progn (decf neg) (decf score 5)))
		    ;;if its max's turn add more points as an incentive
		    (if (and (> curr 0) (funcall is-maxs-turn-p game))
			;;if the next block and the block before is empty add 25 points
			(if (or (and (>= (- x 2) 0) (equal empty (aref (board game) (- x 2) y)))
				(and (< (+ x 2) (width game)) (equal empty (aref (board game) (+ x 2) y))))
			    (progn
			      (if (= pos 1) (incf score 25))
			      (if (= pos 2) (incf score 50)))))
		    ;;when position is 1- the num in a row we know we have enountered a n in a row and won
		    (if (= pos (1- (num-in-a-row game))) (setf score 1000))
		    (if (= neg (1- (num-in-a-row game))) (setf score -1000))))
	      (setf last curr)))
    score))


(defun vertical (game &optional is-maxs-turn-p)
  "Helper function for the evaluate heuristic.
   Goes over every column on the board and adds up scores"

  ;;keep track of positive and negatives
  (let ((score 0) (pos 0) (neg 0)
	(curr 22) (last 22))
    ;;loop through every column
    (loop for x from 0 to (1- (width game)) do
	 (setf pos 0)
	 (setf neg 0)
	 (loop for y from 0 to (1- (height game)) do
	      ;;add points to the score based on positive or negative one
	      (setf curr (aref (board game) x y))
	      (incf score curr)
	      ;;if more than one in a column occur
	      (if (and (/= curr 0) (= last curr))
		  (progn
		    ;;if more than one black occurs add 10
		    (if (> curr 0) (progn (incf pos) (incf score 10)))
		    ;;if more than one red occurs take away 5
		    (if (< curr 0) (progn (decf neg) (decf score 5)))
		    ;;if its max's turn add more points as an incentive
		    (if (and (> curr 0) (funcall is-maxs-turn-p game))
			;;if the next block and the block before is empty add 25 points
			(if (or (and (>= (- y 2) 0) (equal empty (aref (board game) x (- y 2))))
				(and (< (+ y 2) (height game)) (equal empty (aref (board game) x (+ y 2)))))
			    (progn
			      (if (= pos 1) (incf score 25))
			      (if (= pos 2) (incf score 50)))))
		    ;;when position is 1- the num in a row we know we have enountered a n in a row and won
		    (if (= pos (1- (num-in-a-row game))) (setf score 1000))
		    (if (= neg (1- (num-in-a-row game))) (setf score -1000))))
	      (setf last curr)))
    score))


(defun down-left (game &optional is-maxs-turn-p)
  "Helper function for the evaluate heuristic.
   Goes over every diagonal going down left on the board and adds up scores"

  ;;keep track of positive and negatives
  (let ((score 0) (pos 0) (neg 0)
	(curr-x 22) (curr-y 22)
	(curr 22) (last 22))
    ;;loop through every x y diagonal
    (loop for x from (1- (width game)) downto 0 do
	 (loop for y from 0 to (1- (height game)) do
	      (if (< x (1- (width game))) (setf y (1- (height game))))
	      (setf curr-x x)
	      (setf curr-y y)
	      (setf pos 0)
	      (setf neg 0)
	      (loop while (and (>= curr-y 0) (>= curr-x 0)) do
		   ;;add points to the score based on positive or negative one
		   (setf curr (aref (board game) curr-x curr-y))
		   (setf score (+ score curr))
		   ;;if more than one in a diagonal occur
		   (if (and (/= curr 0) (= last curr))
		       (progn
			 ;;if more than one black occurs add 10
			 (if (> curr 0) (progn (incf pos) (incf score 10)))
			 ;;if more than one red occurs take away 5
			 (if (< curr 0) (progn (incf neg) (decf score 5)))
			 ;;if its max's turn add more points as an incentive
			 (if (and (> curr 0) (funcall is-maxs-turn-p game))
			     ;;if the next block and the block before is empty add 25 points
			     (if (or (and (>= (- x 1) 0) (>= (- y 1) 0) (equal empty (aref (board game) (- x 1) (- y 1))))
				     (and (< (+ x 2) (width game)) (< (+ y 2) (height game)) (equal empty (aref (board game) (+ x 2) (+ y 2)))))
				 (progn
			      (if (= pos 1) (incf score 25))
			      (if (= pos 2) (incf score 50)))))
			 ;;when position is 1- the num in a row we know we have enountered a n in a row and won
			 (if (= pos (1- (num-in-a-row game))) (setf score 1000))
			 (if (= neg (1- (num-in-a-row game))) (setf score -1000))))
		   (setf last curr)
		   (decf curr-x)
		   (decf curr-y))))
    score))


(defun down-right (game &optional is-maxs-turn-p)
  "Helper function for the evaluate heuristic.
   Goes over every diagonal going down right on the board and adds up scores"

  ;;keep track of positive and negatives
  (let ((score 0) (pos 0) (neg 0)
	(curr-x 22) (curr-y 22)
        (curr 22) (last 22))
    ;;loop through every x y diagonal
    (loop for x from 0 to (1- (width game)) do
	 (loop for y from 0 to (1- (height game)) do
	      (if (> x 0) (setf y (1- (height game))))
	      (setf curr-x x)
	      (setf curr-y y)
	      (setf pos 0)
	      (setf neg 0)
	      (loop while (and (> curr-y 0) (< curr-x (width game))) do
		   ;;add points to the score based on positive or negative one
		   (setf curr (aref (board game) curr-x curr-y))
		   (setf score (+ score curr))
		   ;;if more than one in a diagonal occur
		   (if (and (/= curr 0) (= last curr))
		       (progn
			 ;;if more than one black occurs add 10
			 (if (> curr 0) (progn (incf pos) (incf score 10)))
			 ;;if more than one red occurs take away 10
			 (if (< curr 0) (progn (incf neg) (decf score 5)))
			 ;;if its max's turn add more points as an incentive
			 (if (and (> curr 0) (funcall is-maxs-turn-p game))
			     ;;if the next block and the block before is empty add 25 points
			     (if (or (and (>= (- x 2) 0) (< (+ y 2) (height game)) (equal empty (aref (board game) (- x 2) (+ y 2))))
				     (and (< (+ x 1) (width game)) (>= (- y 1) 0) (equal empty (aref (board game) (+ x 1) (- y 1)))))
				 (progn
			      (if (= pos 1) (incf score 25))
			      (if (= pos 2) (incf score 50)))))
			 ;;when position is 1- the num in a row we know we have enountered a n in a row and won
			 (if (= pos (1- (num-in-a-row game))) (setf score 1000))
			 (if (= neg (1- (num-in-a-row game))) (setf score -1000))))
		   (setf last curr)
		   (incf curr-x)
		   (decf curr-y))))
    score))




(defun evaluate (game is-maxs-turn-p)
  "Returns an evaluation, between min-wins and max-wins inclusive, for the game.
  is-maxs-turn-p is a function which, when called and passed the game, returns true
  if it's max's turn to play."
  
  (let ((end (game-over game)))
    (if (null end) ;; game not over yet
	(progn
	  ;;increment all the values returned from the helpers
	  (let ((final 0))
	    (incf final (horizontal game is-maxs-turn-p))
	    (incf final (vertical game is-maxs-turn-p))
	    (incf final (down-right game is-maxs-turn-p))
	    (incf final (down-left game is-maxs-turn-p))

	    ;;if final is >1000 or <-1000 we return the winner
	    (if (>= final 1000) (return-from evaluate max-wins))
	    (if (<= final -1000) (return-from evaluate min-wins))

	    ;;else we return the final score
	    (return-from evaluate final)))
	
	(if (= 0 end)  ;; game is a draw
	    0
	    ;; else, the game is over but not a draw.  Return its value.
	    ;;
	    ;; this is a deep-math way of saying the following logic:
	    ;;
	    ;; if black, then if turn=black and ismaxsturn, then max-wins
	    ;; if black, then if turn=red and ismaxsturn, then min-wins
	    ;; if black, then if turn=black and !ismaxsturn, then min-wins
	    ;; if black, then if turn=red and !ismaxsturn, then max-wins
	    ;; if red, then if turn=black and ismaxsturn, then min-wins
	    ;; if red, then if turn=red and ismaxsturn, then max-wins
	    ;; if red, then if turn=black and !ismaxsturn, then max-wins
	    ;; if red, then if turn=red and !ismaxsturn, then min-wins
	    ;;
	    ;; keep in mind that black=1, red=-1, max-wins=1000, red-wins = -1000

	    (* end (turn game) max-wins (if (funcall is-maxs-turn-p game) 1 -1))))))




;; I've decided to make this function available to you

(defun make-computer-move (game depth)
  "Makes a move automatically by trying alpha-beta on all possible moves and then picking
  the one which had the highest value for max., and making that move and returning the new game."

  (let* ((max (turn game)))
    (max-element (all-moves game)
		 (lambda (g)
		   (alpha-beta g 0 depth
			       (lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
			       (lambda (gm) (all-moves gm)) ;; expand
			       (lambda (gm) (game-over gm)) ;; terminal-p
			       #'evaluate ;; evaluate
			       min-wins
			       max-wins)))))

;; go back to cl-user
(in-package :cl-user)
