;;;; Constraint:  you can't peek at the total without voting.
;;;; N.b. this constraint is currently violated as anyone who has voted,
;;;; may retain a reference to the current vote tallies
  
  ;;; Simple multi-agents simulation with closures: after evaluation, 
  ;;; the environment contains a closure over an array specialized on
  ;;; bits for accumulation of the storage
(let* ((vote-types  ;; a poor-man's typing system, but we only have two elements
         ;; first value is an affirmative vote; the second a negative
         #(t nil))
       (length ;; total number of voters
         #+(or)
         (expt 2 8)   ;; a safe value for testing 
         (expt 2 33)) ;; challenging: on the order of 10^10
       (votes
         (make-array length :element-type 'cl:bit)))
  (defun consensus-agent-p (vote)
    (equalp vote (first vote-types)))
  (defun get-vote (id)
    (aref id votes))
  (defun record-vote (id vote)
    (incf 
     (get-vote id)
     ;; this is weird:  there is an obvious logical shortcut, but does 
     ;; it express less is the question…
     (cond
       ((if (consensus-agent-p vote)
            1
            -1)
        (t
         1))))
    (values votes ;; vote record is the primary value
            length))) ;; length is currently redundant, but in the future we may be able to adjust the underlying vector
