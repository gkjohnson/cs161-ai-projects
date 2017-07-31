;upper left corner of board is 0,0
;a valid list of states is a list of column values with the order indicating the row number

;reload
(defun reload ()
	(load "C:\\Users\\Garrett\\Desktop\\hw4.lsp")
)

;QUEENS
;takes N QUEENS number value N
;RETURNS nil if no solution can be found, otherwise a goalstate is returned
(defun QUEENS (N)
	(cond
		((<= N 0) ;return nil if N is less than or equal to 0
			nil
		)
		(t
			(QUEENS-AUX N nil)
		)
	)
)
;QUEENS-AUX
;takes N QUEENS number N and valid state state
;RETURNS nil if a goal state was not found down the branch, returns a goal state if it was
(defun QUEENS-AUX (N state)
	(cond
		( ;if the board has been filled, check if it is a goal
			(equal N (length state))
			(goal-state state 0)
		)
		(t ;otherwise check the next states
			(QUEENS-NEXT N (next-states N state))
		)
	)
)

;QUEENS-NEXT
;takes N QUEENS number value N and list of states 
;RETURNS nil if a goal could not be found the branch - returns a goal state if it was found
(defun QUEENS-NEXT (N next-states)
	(cond
		( ; if next-states is nil, then there cannot be a goal state here
			(null next-states)
			nil
		)
		(t
			(let*
				( ;check down the first states branch
					(RETURNEDSTATE (QUEENS-AUX N (first next-states)))
				)
				(cond
					( ;if the state did not lead to a goal check the next state
						(null RETURNEDSTATE)
						(QUEENS-NEXT N (rest next-states)) 
					)
					(t RETURNEDSTATE)
				)
			)
		)
	)
)

;next-states
;takes an N QUEEN number value N and valid state state
;RETURNS a list of successive states
(defun next-states (N state)
	(cond
		( ; if the size is 0, there are no states
			(equal N 0)
			nil
		)
		( ; if the potential state is not valid, then try the next potential state
			(null (checkIfValid (length state) (- N 1) state 0))
			(next-states (- N 1) state)
		)
		(t
			(cons ; if the state works, then store it in the list
				(append state (list (- N 1)))
				(next-states (- N 1) state)
			)
		)
	)
)


;checkIfValid
;takes a row number newRow, col number new Col, valid state (non nil), and row number to start checking at currRow (0 when called)
;RETURNS t if it is a valid state, nil otherwise
(defun checkIfValid (newRow newCol state currRow)
	(cond
		((null state) t)
		(
			(OR
				(equal
					(abs-val (- newRow currRow)) ;if the pieces are on the same diagonal
					(abs-val (- newCol (first state)))
				)
				(equal (- newCol (first state)) 0) ;or same row or column
				(equal (- newRow currRow) 0)
			)
			nil ;it is not valid
		)
		(t ;check the next value
			(checkIfValid newRow newCol (rest state) (+ currRow 1))
		)
	)
)

;goal-state
;takes a valid (non nil) state and a row to start at (0 when called)
;assumes that the state is correctly formed and fully filled and checks if any queen
;can attack another
;RETURNS nil if it is not a valid state, returns the state if it is
(defun goal-state (state startRow)
	(cond
		((null state) t) ;if the end was reached
		( ;if the row was valid
			(checkIfValid startRow (first state) (rest state) (+ startRow 1))
			(cond
				( ;check the rest of the state
					(goal-state (rest state) (+ startRow 1))
					state
				)
				(t nil)
			)
		)
		(t
			nil
		)
	)
)

;abs-val
;takes a number num
;RETURNS a positive version of it
(defun abs-val (num)
	(cond
		((< num 0)
			(- 0 num)
		)
		(t
			num
		)
	)
)


