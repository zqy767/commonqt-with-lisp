
(defpackage #:dumper731
  (:use #:cl)) 
(in-package #:dumper731) 
(defparameter *post-invoke-debugger-hook* nil) 
(defparameter *system-load-output* *standard-output*) 
(defvar *logfile-output*) 
(defun dump-file-debugger (condition previous-hook)
  "The function to call if there are errors when loading the dump file."
  (declare (ignore previous-hook))
  (format *system-load-output* "~&Fatal ~A:~%  ~A~%" (type-of condition)
          condition)
  (print (macroexpand-1 '(backtrace-as-list)) *logfile-output*)
  (macroexpand-1 '(quit 111))) 
(setf sb-ext:*invoke-debugger-hook* 'dump-file-debugger) 
(defparameter *logfile-output* (make-broadcast-stream)) 
(unless (find-symbol "SAVE-RUNTIME-OPTIONS" '#:sb-impl)
  (error "This SBCL, ~A, does not support :SAVE-RUNTIME-OPTIONS"
         (lisp-implementation-version))) 
(with-open-file
    (stream "learning" :direction :output :if-does-not-exist :create :if-exists
     :append)
  (write-line "buildapp write check" stream)) 
(delete-file "learning") 
(require '#:asdf) 
(progn
 (defun system-search-table (&rest pathnames)
   (let ((table (make-hash-table :test 'equalp)))
     (dolist (pathname pathnames table)
       (setf (gethash (pathname-name pathname) table) (probe-file pathname)))))
 (defvar *asdf-systems-table*)
 (defun system-search-function (name) (gethash name *asdf-systems-table*))
 (defun load-system (name)
   "Load ASDF system identified by NAME."
   (let ((*standard-output* *logfile-output*)
         (*error-output* *logfile-output*)
         (*compile-verbose* t)
         (*compile-print* t))
     (handler-bind ((warning
                     (lambda (condition)
                       (when *compile-file-truename*
                         (unless (typep condition 'style-warning)
                           (error "Compilation failed: ~A in ~A" condition
                                  *compile-file-truename*))))))
       (format *system-load-output* ";; loading system ~S~%" name)
       (asdf/operate:oos 'asdf/lisp-action:load-op name)
       t)))) 
(setf *asdf-systems-table* (system-search-table #P"/home/leo/study/study.asd")) 
(push 'system-search-function
      asdf/find-system:*system-definition-search-functions*) 
(progn
 (defparameter *load-search-paths* (list *default-pathname-defaults*))
 (defun load-file (file)
   "Search for FILE in *LOAD-SEARCH-PATHS* and, if found, load
it. If an exact filename is not found, file.lisp is also tried."
   (dolist (path *load-search-paths*)
     (let* ((p1 (merge-pathnames file path))
            (p2
             (merge-pathnames (make-pathname :type "lisp" :defaults file)
                              path))
            (truename (or (probe-file p1) (probe-file p2))))
       (when truename
         (format *system-load-output* ";; loading file ~S~%" truename)
         (return-from load-file
           (let ((*standard-output* *logfile-output*)
                 (*package* (find-package '#:cl-user)))
             (load truename :verbose t :print t))))))
   (error "File ~S not found" file))) 
(progn
 (let ((previous-hook sb-ext:*invoke-debugger-hook*)
       (sb-ext:*invoke-debugger-hook* sb-ext:*invoke-debugger-hook*))
   (progn (load-system "study"))
   (unless (eql sb-ext:*invoke-debugger-hook* previous-hook)
     (setf *post-invoke-debugger-hook* sb-ext:*invoke-debugger-hook*)))) 
(ignore-errors (close *logfile-output*)) 
(setf sb-ext:*invoke-debugger-hook* *post-invoke-debugger-hook*) 
(setf asdf/find-system:*system-definition-search-functions*
        (remove 'system-search-function
                asdf/find-system:*system-definition-search-functions*)) 
(in-package #:cl-user) 
(delete-package '#:dumper731) 
(sb-ext:gc :full t) 
(sb-ext:save-lisp-and-die #P"learning" :executable t :save-runtime-options t) 