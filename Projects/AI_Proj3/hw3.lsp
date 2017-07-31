;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly effect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; effect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "C:\\Users\\Garrett\\Desktop\\hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "C:\\Users\\Garrett\\Desktop\\a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (load-a-star)
  (reload)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;(goal-test (set-square (set-square p1 2 2 0) 5 4 0))

;goal-test takes a state
;returns t if it is a goal state, nil otherwise
(defun goal-test (s)
	(cond
		((null s) t) ;if it reaches the end without failing, it must be a goal state
		((null (goal-test-helper (first s))) nil) ;if the helper returned that the row contained a box/goal, return nil
		(t (goal-test (rest s))) ;otherwise continue to the next row
	)
)
;goal-test-helper takes a row of a state
;returns nil if it finds a box
;returns t otherwise
(defun goal-test-helper (row)
	(cond
		((null row) t) ;if the function gets through every element, but does not fail, there must be no boxes
		((isBox (first row)) nil) ;if the element is equal to a box return nil
		(t (goal-test-helper (rest row))) ;otherwise move to the next column
	)
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

;next-states takes a state
;returns a list of a maximum of four states that can be reached from the current state
(defun next-states (s)
	(cleanUpList
		(append ;try to make a move in any direction and append all
			(list (try-move s 1))
			(list (try-move s 2))
			(list (try-move s 3))
			(list (try-move s 4))
		)
	)
)
;1 2 3 4 represent UP DOWN LEFT RIGHT for the direction
;try-move takes a state and a direction
;returns nil if the keeper cannot move, otherwise a valid state with the moved boxes/keeper
;wraps the try-move-keeper function
(defun try-move (s dir)
	(let*
		(
			(KROW (second (getKeeperPosition s 0))) ;get the keepers row
			(KCOL (first (getKeeperPosition s 0))) ;get the keepers col
		)
		(try-move-keeper s KROW KCOL dir)
	)
)
;try-move-keeper takes a state, row, column, and a direction
;returns nil if the keeper cannot move, oterhwise a valid state with the moved box/keeper
(defun try-move-keeper (s r c dir)
	(let*
		(
			(ROW ;;set where the row that is being moved to
				(cond
					((equal dir 1)
						(- r 1)
					)
					((equal dir 2)
						(+ r 1)
					)
					(t r)
				)
			)
			(COL ;;set where the col that is being moved to
				(cond
					((equal dir 3)
						(- c 1)
					)
					((equal dir 4)
						(+ c 1)
					)
					(t c)
				)
			)
			(CURRSQUARE (get-square s r c)) ;;get what the current square is
			(CURRSQUARE-IF-MOVED ;;get what the current square will be if the keeper can move
				(cond
					((isKeeper CURRSQUARE) 0) ;;it will be blank if it can move and is just the keeper
					((isKeeperStar CURRSQUARE) 4) ;;it will be a goal if it can move and is keeper/goal
				)
			)
			(NEXTSQUARE (get-square s ROW COL)) ;;get what the square that is being moved to is
			(NEXTSQUARE-IF-MOVED ;;get what the square that is being moved to will be if the keeper can move
				(cond
					((null NEXTSQUARE) nil)
					((isBox NEXTSQUARE) 3) ;;set to keeper
					((isBlank NEXTSQUARE) 3) ;;set to keeper
					((isStar NEXTSQUARE) 6) ;;set to keeper star
					((isBoxStar NEXTSQUARE) 6) ;;set to keeper star
					(t nil) ;;otherwise, it cannot be moved to so return nil
				)
			)
		)
		(cond
			((null NEXTSQUARE-IF-MOVED) nil) ;;if there was an issue moving to the next, return nil
			(
				(or 
					(isBox NEXTSQUARE) ; if the the next contains a box or boxstar, check if the box can be moved
					(isBoxStar NEXTSQUARE) ; 
				)
				(let*
					((BOX-MOVED-STATE (try-move-box s ROW COL dir))) ; check if the box could be moved
					(cond
						((null BOX-MOVED-STATE) nil) ; if the box cannot be moved, then return nil
						;move keeper and change both squares
						(t (set-square (set-square BOX-MOVED-STATE r c CURRSQUARE-IF-MOVED) ROW COL NEXTSQUARE-IF-MOVED)) 
					)
				)
			)
			(t
				;move the keeper and change both squares
				(set-square (set-square s r c CURRSQUARE-IF-MOVED) ROW COL NEXTSQUARE-IF-MOVED) 
			)
		)
	)
)
;try-move-box takes a state, row, column, and a direction
;returns nil if the box cannot move, oterhwise a valid state with the moved box
(defun try-move-box (s r c dir)
	(let*
		(
			(ROW ;;set where the row that is being moved to
				(cond
					((equal dir 1)
						(- r 1)
					)
					((equal dir 2)
						(+ r 1)
					)
					(t r)
				)
			)
			(COL ;;set where the col that is being moved to
				(cond
					((equal dir 3)
						(- c 1)
					)
					((equal dir 4)
						(+ c 1)
					)
					(t c)
				)
			)
			(CURRSQUARE (get-square s r c)) ;;get what the current square is
			(CURRSQUARE-IF-MOVED ;;get what the current square will be if the box can move
				(cond
					((isBox CURRSQUARE) 0) ;;it will be blank if it can move and is just the box
					((isBoxStar CURRSQUARE) 4) ;;it will be a goal if it can move and is box/goal
				)
			)
			(NEXTSQUARE (get-square s ROW COL)) ;;get what the square that is being moved to is
			(NEXTSQUARE-IF-MOVED ;;get what the square that is being moved to will be if the box can move
				(cond
					((null NEXTSQUARE) nil)
					((isBlank NEXTSQUARE) 2) ;;set to box
					((isStar NEXTSQUARE) 5) ;;set to box star
					(t nil) ;;otherwise, it cannot be moved to so return nil
				)
			)
		)
		(cond
			((null NEXTSQUARE-IF-MOVED) nil) ;;if there was an issue moving to the next, return nil
			(t
				(set-square (set-square s r c CURRSQUARE-IF-MOVED) ROW COL NEXTSQUARE-IF-MOVED) ;move the box ad change both squares
			)
		)
	)
)
;;upper left tile is (0, 0) for indexing of rows/cols
;get-square takes a state s, row number r, col number c
;returns the tile type of the tile at r,c
(defun get-square (s r c)
	(cond
		(
			(or
				(< r 0) ;return nil if the row or col is negative
				(< c 0)
			)
			nil
		)
		(t
			(first (nthcdr c (first (nthcdr r s)))) ; get the tile at (r,c)
		)
	)
)
;get-square takes a state s, row number r, col number c, tiletype v
;returns a state with the tile changed
(defun set-square (s r c v)
	(cond
		(
			(or ;return nil if the row or column are negative
				(< r 0)
				(< c 0)
			)
			nil
		)
		(t
			(let*
				(
					(FIRST-ROWS (firstn r s)) ;the first rows before row r
					(FIRST-COLS (firstn c (first (nthcdr r s)))) ;the first cols of row r before c
					(END-ROWS (nthcdr (+ r 1) s)) ;the last rows after r
					(END-COLS (nthcdr (+ c 1) (first (nthcdr r s)))) ;the last cols of row r after c
				)
				(append	;append the state and return it
					FIRST-ROWS
					(list 
						(append
							FIRST-COLS
							(list v)
							END-COLS
						)
					)
					END-ROWS
				)
			)
		)
	)
)
;firstn takes a number n and a list L
;returns a list of the first n elements in L
(defun firstn (n L)
	(cond
		((null L) nil)
		((<= n 0) nil)
		((not (listp L)) L)
		(t 
			(append 
				(list (first L))
				(firstn  (- n 1) (rest L))
			)
		)
	)
)
; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; Yes, this is admissable because minimum it will take atleast an equivelant number of moves
; to the number of boxes in order to complete the level
(defun h1 (s)
		(cond
			((null s) 0) ;if the state is null, return zero
			(t (+ (h1-helper (first s)) (h1 (rest s)))) ;otherwise move on to the next row, adding the amount of boxes in the current one
		)
)
;h1-helper takes a row of the state
; returns the number of boxes in that row
(defun h1-helper (row)
	(cond
		((null row) 0) ;if the row is empty, there can be no boxes
		((isBox (first row)) (+ 1 (h1-helper (rest row)))) ;if the first element is a box, increment the count to the rest of the row
		(t (h1-helper (rest row))) ;if it's not a box, keep counting the rest of the row
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

;Admissible heuristic
(defun h903696573 (s)
	(let*
		(
			(BOXES-AND-GOALS (getBoxesAndGoals s 0)) ;get a list of the boxes, goals, and keeper pos
			(BOXLIST (first BOXES-AND-GOALS)) ;get a list of just the boxes
			(GOALLIST (second BOXES-AND-GOALS)) ;get a list of just the goals
			(KEEPER (third BOXES-AND-GOALS)) ;get a list of just the keeper
			(ADDED 
				(+
					(getAddedShortestDist BOXLIST GOALLIST) ;add the shortest distance between the boxes and goals
					(getAddedShortestDist KEEPER BOXLIST) ;get the distance from the keeper to the closest box
				)
			)
		)
		;ADDLIST1
		(cond
			((< 0 ADDED) ;subtract one if possible to take into account the keeper moving to the box
				(- ADDED 1)
			)
			(t ADDED)
		)
	)
)
;;h903696573 helper functions
;getBoxesAndGoals takes a state s, and a start row number r
; returns a list of a list of box positions (r,c), list of goal positions (r,c) and
;list of keeper position (r,c)
(defun getBoxesAndGoals (s r)
	(cond
		((null s) nil) ;returns nil if the state is empty
		(t
			(let*
				(
					(BOXES-GOALS
						(getBoxesAndGoals (rest s) (+ r 1))
					)
					(NEXT-BOXES-GOALS
						(getBoxesAndGoals-helper (first s) r 0)
					)
					(BOXES
						(first BOXES-GOALS)
					)
					(GOALS
						(second BOXES-GOALS)
					)
					(KEEPER
						(third BOXES-GOALS)
					)
					(NEXT-BOXES
						(first NEXT-BOXES-GOALS)
					)
					(NEXT-GOALS
						(second NEXT-BOXES-GOALS)
					)
					(NEXT-KEEPER
						(third NEXT-BOXES-GOALS)
					)
				)
				;append the list of positions at the row to those of the next row
				(list (append BOXES NEXT-BOXES) (append GOALS NEXT-GOALS) (append KEEPER NEXT-KEEPER))
			)
		)
	)
)
;getBoxesAndGoals-helper takes a state's row s, current row r, and start column c
; returns a list of a list of box positions(r,c), goal positions (r,c), and keeper position(r,c)
; from that row
(defun getBoxesAndGoals-helper (s r c)
	(cond
		((null s) nil)
		(t
			(let*
				(
					(REST-BOXES-GOALS
						(getBoxesAndGoals-helper (rest s) r (+ c 1))
					)
					(REST-BOXES
						(first REST-BOXES-GOALS)
					)
					(REST-GOALS
						(second REST-BOXES-GOALS)
					)
					(REST-KEEPER
						(third REST-BOXES-GOALS)
					)
				)
				(cond
					((isBox (first s)) ;if its a box, add the position to the box list
						(list (append (list (list r c)) REST-BOXES) REST-GOALS REST-KEEPER)
					)
					((isStar (first s)) ;if its a goal, add the position to the goal llist
						(list REST-BOXES (append (list (list r c)) REST-GOALS) REST-KEEPER)
					)
					((isKeeper (first s)) ;if its a keeper, add the position to the keeper list
						(list REST-BOXES REST-GOALS (append (list (list r c)) REST-KEEPER))
					)
					(t (list REST-BOXES REST-GOALS REST-KEEPER)) ;otherwise step forward
				)
			)
		)
	)
)
;getShortestDist takes a position (r,c) p, and a list of positions
; returns the shortest distance between p and the closest point in pL
(defun getShortestDist (p pL)
	(cond
		((null pL) 9999) ;if pL has reached the end, return an arbitrarily large distance
		(t 
			(let* 
				(
					(CURRDIST (dist p (first pL))) ;get distance from p to the first element
					(NEXTDIST (getShortestDist p (rest pL))) ;check the rest
				)
				(cond
					(
						(or
							(null NEXTDIST) ;if rest of the distances didnt work or were larger
							(< CURRDIST NEXTDIST)
						)
						CURRDIST ;then return the distance that was calculated first
					)
					(t NEXTDIST)
				)
			)
		)
	)
)
;getAddedShortestDist takes a list of distances pL1 and pL2
;returns the added distance for everypoint in pL1 to the closest point in pL2
(defun getAddedShortestDist (pL1 pL2)
	(cond 
		(
			(or ;if either list is null, return 0
				(null pL1)
				(null pL2)
			)
			0
		)
		(t ;otherwise continue adding through the lists
			(+ (getShortestDist (first pL1) pL2) (getAddedShortestDist (rest pL1) pL2))
		)
	)
)
;dist takes a postion (r,c) d1 and a position (r,c) d2
;returns the number of moves it takes to move between both
(defun dist (d1 d2)
	(let*
		(
			(ROWD-RAW ;the raw distance between the rows
				(- (first d1) (first d2))
			)
			(COLD-RAW ;the raw distance between the columns
				(- (second d1) (second d2))
			)
			(ROWD
				(cond ;makes the raw row data positive
					((< ROWD-RAW 0) (- 0 ROWD-RAW))
					(t ROWD-RAW)
				)
			)
			(COLD
				(cond ;makes the raw col data positive
					((< COLD-RAW 0) (- 0 COLD-RAW))
					(t COLD-RAW)
				)
			)
		)
		(+ COLD ROWD) ;add the two distances to get the total distance
	)
)
;addDist list of positions (r,c) dList1 and list of positions (r,c) dList2 and adds them successively
; returns a total distance between all points ie distance between (first dlist2) and (first dlist1) to (second dList2) and (second dList1) etc
(defun addDist (dList1 dList2)
	(cond
		((null dList1) 0) ;if the first list is empty, return 0
		((null dList2) 0) ;if the second list is empty, return 0
		(t 
			(+ 
				(dist (first dList1) (first dList2)) ;get the distance between the first two points
				(addDist (rest dList1) (rest dList2)) ;get the distance between rest
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
)