; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(cond
		;if s equals the final state, return true
		((equal s '(3 3 NIL)) T)
		(T nil)
	)
 )

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(let*
		;Variable Declarations
		(
			(CURRSIDE_M (- (first s) m)) ;declare variables for the remaining people on the current side
			(CURRSIDE_C (- (second s) c))
			(BOAT_M m) ;declare variables for the people on the boat
			(BOAT_C c)
			(OTRSIDE_M (- 3 (+ CURRSIDE_M BOAT_M))) ;declare variables for people on the other side
			(OTRSIDE_C (- 3 (+ CURRSIDE_C BOAT_C)))
			(TOTAL_M (+ (+ CURRSIDE_M BOAT_M) OTRSIDE_M)) ;count the total amount of people that the variables imply
			(TOTAL_C (+ (+ CURRSIDE_C BOAT_C) OTRSIDE_C))
			(NEWSIDE (not (third s))) ;set the new side to not the current side
		)
		;Conditional
		(cond
			;verify that the total number of cannibals and missionaries is valid
			(	
				(not (and 
					;if the sides start out uneven, then also return nil
					(or (<= (+ BOAT_C CURRSIDE_C) (+ BOAT_M CURRSIDE_M)) (equal (+ BOAT_C CURRSIDE_C) 0) (equal (+ BOAT_M CURRSIDE_M) 0))
					;Make sure that the total number of Missionaries and Cannibals is 3 each
					(equal TOTAL_M 3)
					(equal TOTAL_C 3)
					;Make sure that the number of missionaries and cannibals on the current wside is valid
					(>= CURRSIDE_M 0)
					(<= CURRSIDE_M 3)
					(>= CURRSIDE_C 0)
					(<= CURRSIDE_C 3)

					;Make sure that the number of missionaries and cannibals on the other side is valid
					(>= OTRSIDE_M 0)
					(<= OTRSIDE_M 3)
					(>= OTRSIDE_C 0)
					(<= OTRSIDE_C 3)

					;Make sure that the number of missionaries and cannibals on the boat is valid
					(>= BOAT_M 0)
					(<= BOAT_M 2)
					(>= BOAT_C 0)
					(<= BOAT_C 2)
					(<= (+ BOAT_M BOAT_C) 2)
					(>= (+ BOAT_M BOAT_C) 1)
					
					;Make sure that the number of cannibals does not outnumber the number of missionaries anywhere
					(or (<= CURRSIDE_C CURRSIDE_M) (equal CURRSIDE_C 0) (equal CURRSIDE_M 0))
					(or (<= OTRSIDE_C OTRSIDE_M) (equal OTRSIDE_C 0) (equal OTRSIDE_M 0))
					(or (<= BOAT_C BOAT_M) (equal BOAT_C 0) (equal BOAT_M 0))
					;Make sure that moving the Ms and Cs will not cause any outnumbering
					(or (<= (+ BOAT_C OTRSIDE_C) (+ BOAT_M OTRSIDE_M)) (equal (+ BOAT_C OTRSIDE_C) 0) (equal (+ BOAT_M OTRSIDE_M) 0))
				))
				nil
			)
			;construct a list of the state if everything is valid
			(t (list (list (+ OTRSIDE_M BOAT_M) (+ OTRSIDE_C BOAT_C) NEWSIDE)))
		)
	)
 )

 ;;test func
 (defun next-state-test ()
	(cond
		((and
			(equal (next-state '(3 3 T) 1 1) '((1 1 nil)))
			(equal (next-state '(2 2 T) 1 1) '((2 2 nil)))
			(equal (next-state '(1 1 T) 1 1) '((3 3 nil)))
			(equal (next-state '(3 2 T) 1 0) '((1 1 nil)))
			(equal (next-state '(3 1 T) 2 0) '((2 2 nil)))
			(equal (next-state '(3 3 T) 0 1) '((0 1 nil)))
			(equal (next-state '(3 3 T) 0 2) '((0 2 nil)))
			(equal (next-state '(3 3 nil) 1 1) '((1 1 T)))
			(equal (next-state '(2 2 nil) 1 1) '((2 2 T)))
			(equal (next-state '(1 1 nil) 1 1) '((3 3 T)))
			(equal (next-state '(3 2 nil) 1 0) '((1 1 T)))
			(equal (next-state '(3 1 nil) 2 0) '((2 2 T)))
			(equal (next-state '(3 3 nil) 0 1) '((0 1 T)))
			(equal (next-state '(3 3 nil) 0 2) '((0 2 T)))
			
			(equal (next-state '(3 3 T) 2 1) nil)
			(equal (next-state '(1 1 nil) 0 0) nil)
			
			(equal (next-state '(3 3 T) 1 0) nil)
			(equal (next-state '(3 1 T) 1 0) nil)
			(equal (next-state '(3 1 T) 2 0) '((2 2 nil)))
			;()
			;()
		) t)
		(t nil)
	)
 )
 
; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
	(append
		;add together a list of all possible next states
		;because "next-state" will return nil, the can all just be appended
		(next-state s 0 1)
		(next-state s 0 2)
		(next-state s 1 0)
		(next-state s 2 0)
		(next-state s 1 1)
	)	
 )

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond
		((null states) nil)
		;if the first element is the state that is being looked for, return true
		((equal s (first states)) T)
		;otherwise, look further into the stack
		(t (on-path s (rest states)))
	)
 )

; MULT-DFS is a uhelper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
	(cond
		;if there are no more states to check for this path, return nil
		((null states) nil)
		;if the first state listed is the final state, append it to the path and return it
		((final-state (first states)) (cons (first states) path))
		;if the first state has already been checked before, move on to the rest of the states
		((on-path (first states) path) (mult-dfs (rest states) path))
		;otherwise look at the rest of the states
		(t 
			(let*
				(
					;define RETURNED to be the path to goal from the first state
					(RETURNED 
						(mult-dfs 
							(succ-fn (first states)) 
							(cons (first states) path)
						)
					)
				)
				(cond
					;if the path is nil, then try the next state
					((equal RETURNED nil) 
						(mult-dfs 
							(rest states)
							path
						)
					)
					;otherwise return the successful path
					(t RETURNED)
				)
			)
		)
	)
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	(cond
		;If the starting point is the goal state, return the path to the goal state
		((final-state s) (cons s path))
		;If the current state has already been looked at, then return 
		((on-path s path) nil)
		;otherwise do a depth first search starting at the node
		(t (mult-dfs (list s) path))
	)
)


; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
