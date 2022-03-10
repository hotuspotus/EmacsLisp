;;; The store is simulated by an association list.  The key is the offset that
;;; has been allocated to an identifier in the AST.

                                       

(defun store (offset value alist)
  "Insert the value for this offset, replacing the previous value (if any)."
  (cond
   ((null alist)             (list (cons offset value)))    ; ((offset . value))
   ((eq offset (caar alist)) (cons (cons offset value) (cdr alist)))
   (t                        (cons (car alist)
                                   (store offset value (cdr alist))))
   )
  )

(defun lookup (offset alist)
  "Return the value associated with this offset, or raise an error."
  (cond
   ((null alist)             (user-error "UNINITIALISED %s" offset) (exit))
   ((eq (caar alist) offset) (cdar alist))
   (t                        (lookup offset (cdr alist)))
   )
  )



;;; Accessors for the various fields in an AST node

(defun position (ast)
  "The position stored in an AST node"
  (cadar ast)
  )

(defun kind (ast)
  (caar ast)
)

(defun pick (n l)
  ( cond
    ( (null l) l)
    ( (eq n 0) (car l))
    ( t ( pick (- n 1) (cdr l)))
  )
 )

(defun operand (n ast)
  (pick n (cadr ast))
)

;; (setq ast '((PLUS pos) (( (VARIABLE pos) (b 1) ) ((INT_LITERAL pos) (77) ))))
;; (kind ast)
;; (position ast)
;; (operand 0 ast)
;; (operand 1 ast)
;; (operand 1 (operand 0 ast))
;; (kind (operand 1 ast))


;;; The interpreter itself.

(defun exp (ast alist)
  "Evaluate an expression (given this alist to represent the variable store)."
  (cond
   ((eq (kind ast) 'BOOL_LITERAL) (operand 0 ast))
   ((eq (kind ast) 'INT_LITERAL)  (print(operand 0 ast))(operand 0 ast))
   ((eq (kind ast) 'UMINUS)       (* -1 (exp (operand 0 ast) alist)))
   ((eq (kind ast) 'AND)          (and (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'OR)           (or (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ( (and (eq (kind ast) 'NOT) (eq (exp (operand 0 ast) alist) nil)  'True))
   ((eq (kind ast) 'NOT)          (not (exp (operand 0 ast) alist)))
   
   ((eq (kind ast) 'EQ)           (eq (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'NE)           (/= (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'LT)           (< (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'LE)           (<= (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'GT)           (> (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'GE)           (>= (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'PLUS)         (+ (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'MINUS)        (- (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'MULT)         (* (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   ((eq (kind ast) 'DIV)          (/ (exp (operand 0 ast) alist) (exp (operand 1 ast)alist)))
   )
  )

;; (lookup (operand 1 '((VARIABLE 'pos) (a 9))) (store 9 77 ()))
;; (exp '((VARIABLE 'pos) (a 9)) (store 9 77 ()))
;; (exp '((PLUS 'p) ( ( (VARIABLE 'p) (a 9)) ((INT_LITERAL 'p) (2))) )(store 9 77 ()))

(defun stmts (ast alist)
  "Interpret a statement or a sequence of statenents, return the store."
  ;; SEQ evaluates the right operand with the store returned by the left one.
  ;; DECL is simply skipped.
  ;; ASSIGNMENT evaluates the right operand and stores the result under the
  ;;            name of the second operand.
  ;; IF and WHILE are handled separately.
  ;; PRINT just evaluates and outputs its operand.
  (cond
   ((eq (kind ast) 'SEQ)          (stmts (operand 1 ast)
                                         (stmts (operand 0 ast) alist)
                                         ))
   ((eq (kind ast) 'DECL)         alist)
   ((eq (kind ast) 'ASSIGNMENT)   (store (operand 1 (operand 0 ast))
                                         (exp (operand 1 ast) alist)
                                         alist
                                         ))
   ((eq (kind ast) 'IF)           (if_stmt    ast alist))
   ((eq (kind ast) 'WHILE)        (while_stmt ast alist))
   ((eq (kind ast) 'PRINT_BOOL)   (progn
                                    (print (exp (operand 0 ast) alist))
                                    alist
                                    ))
   ((eq (kind ast) 'PRINT_INT)    (progn
                                    (print (exp (operand 0 ast) alist))
                                    alist
                                    ))
   )
  )

(defun if_stmt (ast alist)
  "Evaluate the AST for an IF node, returning the updated store."
  (if (eq 'True (exp (operand 0 ast) alist))      ; is condition true?
      (stmts (operand 1 ast) alist)               ; the "then" branch
    (stmts (operand 2 ast) alist)                 ; the "else" branch
    )
  )

(defun while_stmt (ast alist)
  "Evaluate the AST for a WHILE node, returning the updated store."
  (if (eq 'True (exp (operand 0 ast) alist))      ; is condition true?
      ;; yes: evaluate this ast again, in the store updated by the body
      (while_stmt ast (stmts (operand 1 ast) alist))
    ;; no: just return the store
    alist
    )
  )

(defun interpret (ast)
  "Interpret this AST."
  (stmts ast ())
  )



(defun load_data (buffer-name)
  "Load the data from this buffer into variable `data`."
  (setq data (read (get-buffer buffer-name)))
  )

(defun run ()
  "Run the interpreter on data in `data`."
  (interpret data)
  )

;; Evaluate the following two expressions, after changing the buffer name to
;; the one you want.
;; NOTE: The buffer with data must be loaded first, and the cursor must be at
;;       the beginning.
;;
;; (load_data "print.ast")
;; (run)
