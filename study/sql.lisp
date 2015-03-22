
(eval-when (:compile-toplevel :load-toplevel) 

  (defun mkstr-downcase (&rest args)
    (string-downcase (with-output-to-string (s)
		       (dolist (a args) (princ a s)))))
  
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

(defmacro defsqlfun (name args &body body)
  `(defun ,(symb 'SQL- name) ,args
     ,@body))

(defsqlfun insert (table &rest values)
  (cl-mysql:query (format nil "insert into ~(~a~) values(~{\"~a\"~^,~});" table values)))

(defsqlfun select-all (table)
  (cl-mysql:query (format nil "select * from ~(~a~);" table)))

(defsqlfun delete-all (table)
  (cl-mysql:query (format nil "delete from ~(~a~);" table)))

(defsqlfun select (table fields conds)
  (if (consp fields)
      (my-sql:query (format nil "select ~(~{~a~^,~}~) from ~(~a~) where ~a;" fields table conds))
      (my-sql:query (format nil "select ~(~a~) from ~(~a~) where ~a;" fields table conds))))

(defsqlfun delete (table )
  (my-sql:query (format nil "delete from")))
)
