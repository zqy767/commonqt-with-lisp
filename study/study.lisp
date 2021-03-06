;;;; study.lisp

(in-package #:study)
(named-readtables:in-readtable :qt)

;;; "study" goes here. Hacks and glory await!
(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *x* nil)
  (defparameter *y* nil)   

  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defstruct data 
    china 
    japan 
    right 
    total 
    rate)

  (defun read-from-file ()
    (let ((str (open "data_for_learning.db" :if-does-not-exist nil)))
      (if str
	  (with-standard-io-syntax 
	    (read str))
	  nil)))

  (defparameter *db* nil))

(defun find-data (data)
  (find (list (data-china data) (data-japan data)) *db* 
	:test #'equal
	:key #'(lambda (a) (list (data-china a) (data-japan a)))))

(defun find-japan-data (data)
  (find (princ-to-string (data-japan data)) *db*
	:test #'string-equal
	:key #'(lambda (a) (princ-to-string (data-japan a)))))

(defun find-china-data (data)
  (find (princ-to-string (data-china data)) *db*
	:test #'string-equal
	:key #'(lambda (a) (princ-to-string (data-china a)))))

(defun data-scored? (data)
  (find-data data))

(defun japan-data-scored? (data)
  (find-japan-data data))

(defmacro defqtclass (name direct-slots qt-superclass  (&key slots signals override) &body body)  
  `(progn 
     (defclass ,name ()
       ,direct-slots
       (:metaclass qt-class)
       (:qt-superclass ,qt-superclass)
       (:slots ,@slots)
       (:signals ,@signals)
       (:override ,@override))
     
     (defmethod initialize-instance :after ((instance ,name) &key)
		(new instance)		
		,@body)))

(defmacro turn (before after &key (size-x 600) (size-y 540))
  `(defmethod ,(symb "TURN_TO_" (string-upcase `,after)) ((instance ,before))
       (let ((,after (make-instance ',after)))
	 ,(or *x*
	      *y*
	      (setf *x* 100 *y* 100))
	 (#_setGeometry ,after ,*x* ,*y* ,size-x ,size-y)
	 (#_hide instance)
	 (#_show ,after))))

(defun set-font (font objects)
  (when objects
    (#_setFont (car objects) font)
    (set-font font (cdr objects))))

(defun add-widget (layout objects)
  (when objects
    (#_addWidget layout (car objects))
    (add-widget layout (cdr objects))))
  

(defqtclass welcome () "QWidget" (:slots (("turn()" turn_to_menu)))
  (#_setFixedSize instance 600 540)
  (turn welcome menu)
  
  (let ((words (#_new QLabel "给杨女神的礼物，希望你能喜欢～>=<~" instance))
	(font (#_new QFont "Times" 25 (#_Bold "QFont")))
	(enter (#_new QPushButton "进入" instance)))
    (connect enter "clicked()" instance "turn()")
    (#_resize enter 200 100)
    (set-font font (list enter words))
    (#_move words 10 100)
    (#_move enter 200 440)))

(defmethod close-event (instance event)
  (save instance))



(defqtclass menu () "QWidget" 
    (:slots (("turnInput()" turn_to_input) 
	     ("turnLook()" turn_to_look)
	     ("save()" save)
	     ("turnDeleteWord()" turn_to_delete_word)
	     ("turnLearning()" turn_to_learning)
	     ("setXY()" (lambda (this)
			  (setf *x* (#_x this))
			  (setf *y* (#_y this)))))
     :signals ()
     :override (("closeEvent" close-event))) 
  (turn menu input)
  (turn menu look :size-x 750)
  (turn menu learning)
  (turn menu delete_word)

  (let ((input (#_new QPushButton "输入" instance))
	(look (#_new QPushButton "查看" instance))
	(learning (#_new QPushButton "检测" instance))
	(delete-word (#_new QPushButton "删除" instance))
	(font (#_new QFont "Times" 25 (#_Bold "QFont")))
	(layout (#_new QVBoxLayout)))
    (set-font font (list input look learning delete-word))
    (add-widget layout (list input look learning delete-word))
    (connect input "clicked()" instance "setXY()")
    (connect look "clicked()" instance "setXY()")
    (connect learning "clicked()" instance "setXY()")
    (connect delete-word "clicked()" instance "setXY()")
    (connect input "clicked()" instance "turnInput()")
    (connect look "clicked()" instance "turnLook()")
    (connect learning "clicked()" instance "turnLearning()")
    (connect delete-word "clicked()" instance "turnDeleteWord()")
    (#_setLayout instance layout)))




(defqtclass input 
    ((chinese :accessor chinese)
     (japanese :accessor japanese))
    "QWidget"
    (:slots (("save()" save)
	     ("turnMenu()" turn_to_menu)
	     ("saveBuffer()" save-buffer))
     :override (("closeEvent" close-event)))
  (turn input menu)

  (with-slots ((chinese chinese) (japanese japanese)) instance
    (let ((font (#_new QFont "Times" 15 (#_Bold "QFont")))
	  (chinese-label (#_new QLabel "输入中文" instance))
	  (japanese-label (#_new QLabel "输入日语" instance))
	  (next (#_new QPushButton "下一个" instance))
	  (back (#_new QPushButton "返回" instance))
	  (layout (#_new QVBoxLayout))
	  (vlayout (#_new QHBoxLayout)))
      (setf chinese (#_new QLineEdit "" instance)
	    japanese (#_new QLineEdit "" instance))
      (set-font font (list chinese chinese-label japanese japanese-label back next))
      (connect next "clicked()" instance "saveBuffer()")
      (connect back "clicked()" instance "save()")
      (connect back "clicked()" instance "turnMenu()")
      (add-widget layout (list chinese-label chinese japanese-label japanese))
      (add-widget vlayout (list next back))
      (#_addLayout layout vlayout)
      (#_setLayout instance layout))))
     
(defun save-buffer (instance)
  (let* ((chinese (read-from-string  (#_text (chinese instance))))
	 (japanese (read-from-string (#_text (japanese instance))))
	 (data (make-data  :china chinese :japan japanese :right 0 :total 0 :rate 0)))
    (cond  
      ((japan-data-scored? data) (setf (data-china (find-china-data data)) (symb (data-china (find-china-data data)) "," chinese)))
      ((not (data-scored? data)) (push data *db*)))
    (#_setText (chinese instance) "")
    (#_setText (japanese instance) "")))


(defun save (instance)
  (declare (ignore instance))
  (with-open-file (str (make-pathname :name "data_for_learning.db")
		       :direction :output
		       :if-exists :overwrite
		       :if-does-not-exist :create)
    (with-standard-io-syntax
      (princ *db* str))))




 
(defqtclass look 
    ()
    "QWidget"
    (:slots (("turnMenu()" turn_to_menu))
     :override (("closeEvent" close-event)))
  (turn look menu)
  
  (let ((layout (#_new QVBoxLayout))
	(table (make-instance 'look_table))
	(back (#_new QPushButton "back" instance)))
    (connect back "clicked()" instance "turnMenu()")
    (add-widget layout (list back table))
    (#_setLayout instance layout)))


(defqtclass look_table 
    ((chinese :accessor chinese)
     (japanese :accessor japanese))
    "QTableWidget"
    ()
  
  (#_setColumnCount instance 5)
  (#_setDefaultSectionSize (#_horizontalHeader instance) 150)
  (let ((header '("汉语" "日语" "答对次数" "答题次数" "正确率"))
	(font (#_font (#_horizontalHeader instance))))
    (#_setHorizontalHeaderLabels instance header)
    (#_setBold font t)
    (#_setFont (#_horizontalHeader instance) font)
    
    (#_setStretchLastSection (#_horizontalHeader instance) t)
    (#_setDefaultSectionSize (#_verticalHeader instance) 25)
    (#_setFrameShape instance (#_QFrame::NoFrame))
    (#_setShowGrid instance nil)
    (#_setEditTriggers instance (#_QAbstractItemView::NoEditTriggers))
    (#_setSelectionBehavior instance (#_QAbstractItemView::SelectRows))
    (#_setStyleSheet instance "selection-background-color:lightblue;")
    (#_setStyleSheet (#_horizontalHeader instance) "QHeaderView::section{background:skyblue;}")
    (#_setHighlightSections (#_horizontalHeader instance) nil))
  (mapcar #'(lambda (a) (add-item a instance)) *db*))

(defun add-item (db instance)
  (let ((row-count (#_rowCount instance))
	(db-list (list (format nil "~:(~a~)" (data-china db)) (format nil "~:(~a~)"(data-japan db)) (data-right db) (data-total db) (data-rate db)))
	(color (#_new QColor "gray")))
    (#_insertRow instance row-count)
    (do ((n 0 (1+ n))
	 (object (car db-list) (nth (1+ n) db-list)))
	((> n 4))
      (let ((item (#_new QTableWidgetItem)))
	(#_setText item (princ-to-string object))
	(#_setItem instance row-count n item)
	(#_setTextColor item color)))))





(defqtclass learning
    ((input :accessor input)
     (right :accessor right)
     (china-mean :accessor china-mean)
     (judge :accessor judge)
     (random-num :accessor random-num))
  "QWidget"
  (:slots (("turnMenu()" turn_to_menu)
	   ("judgeIt()" judge-it)
	   ("createNew()" new-one))
   :override (("closeEvent" close-event)))
  (turn learning menu)

  (with-slots ((china-mean china-mean) (input input) (right right) (judge judge) (random-num random-num)) instance
    (setf random-num (random (length *db*)))
    (let* ((chinese-label (#_new QLabel "汉语是" instance))
	   (china (data-china (nth random-num *db*)))
	   (japanese-label (#_new QLabel "对应的日语" instance))
	   (next (#_new QPushButton "下一个" instance))
	   (font (#_new QFont "Times" 15 (#_Bold "QFont")))
	   (layout (#_new QVBoxLayout))
	   (vlayout (#_new QHBoxLayout))
	   (confirm (#_new QPushButton "确认" instance))
	   (back (#_new QPushButton "返回" instance)))
      (setf input (#_new QLineEdit "" instance))
      (setf right (data-japan (nth random-num *db*)))
      (setf china-mean (#_new QLabel (mkstr "<font color='red'>" (princ-to-string china) "</font>") instance))
      (setf judge (#_new QLabel "" instance))
      (set-font font (list chinese-label china-mean input japanese-label next judge confirm back))
      (add-widget layout (list chinese-label china-mean japanese-label input judge))
      (add-widget vlayout (list confirm next back))
      (#_addLayout layout vlayout)
      (#_setLayout instance layout)
      (connect back "clicked()" instance "save()")
      (connect back "clicked()" instance "turnMenu()")
      (connect confirm "clicked()" instance "judgeIt()")
      (connect next "clicked()" instance "createNew()"))))

(defmethod judge-it ((instance learning))
  (with-slots ((input input) (right right) (judge judge) (random-num random-num)) instance
    (if (string-equal
 	 (#_text input)
 	 (princ-to-string right))
	(progn
	  (#_setText judge "<font color='green'>女神答对啦!!好棒！！ </font>")
	  (let ((data (nth random-num *db*)))
	    (incf (data-right data))
	    (incf (data-total data))
	    (if (data-total data)
		(setf (data-rate data) (/ (data-right data) (data-total data))))))
	(progn 
	  (#_setText judge (format nil "<font color='red'>女神加油！正确答案是 ~a </font>" right))
	  (let ((data (nth random-num *db*)))
	    (incf (data-total (nth random-num *db*)))
	    (if (data-total data)
		(setf (data-rate data) (/ (data-right data) (data-total data)))))))))

(defmethod new-one ((instance learning))
  (with-slots ((china-mean china-mean) (input input) (right right) (judge judge) (random-num random-num)) instance 
    (setf random-num (random (length *db*)))
    (#_setText china-mean (mkstr "<font color='red'>" (princ-to-string (data-china (nth random-num *db*))) "</font>"))
    (setf right (data-japan (nth random-num *db*)))
    (#_setText input "")
    (#_setText judge "")))


(defqtclass delete_word
    ((japanese :accessor japanese)
     (word :accessor word)
     (con :accessor con))
    "QWidget"
    (:slots (("tryDelete()" try-delete)
	     ("turnMenu()" turn_to_menu))
     :override (("closeEvent" close-event)))
  (turn delete_word menu)
  (let ((confirm (#_new QPushButton "确认" instance))
	(back (#_new QPushButton "返回" instance))
	(layout (#_new QVBoxLayout))
	(font (#_new QFont "Times" 15 (#_Bold "QFont"))))
    (setf (japanese instance) (#_new QLabel "请女神输入日文~:-)" instance)
	  (word instance) (#_new QLineEdit "" instance)
	  (con instance) (#_new QLabel "" instance))
    (add-widget layout (list (japanese instance) (word instance) (con instance) confirm back))
    (set-font font (list (japanese instance) (word instance) (con instance) confirm back))
    (#_setLayout instance layout)
    (connect back "clicked()" instance "turnMenu()")
    (connect confirm "clicked()" instance "tryDelete()")))

(defmethod try-delete ((instance delete_word))
  (if 
   (japan-data-scored? (make-data :japan (#_text (word instance))))
   (progn 
     (#_setText (con instance) (format nil "<font color='red'> ~a</font> 被删除啦啦啦啦" (#_text (word instance))))
     (setf *db* (remove (find-japan-data (make-data :japan (#_text (word instance)))) *db*)))
   (#_setText (con instance) (format nil "TAT <font color='red'>~a</font> 没被找到" (#_text (word instance)))))
  (#_setText (word instance) "")
  (save instance))
  

(defun main (argc &rest argv)
  (setf *db* (read-from-file))
  (with-main-window (window (make-instance 'welcome))))
