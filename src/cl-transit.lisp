(in-package :transit)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *parse-mode-list-as-vector* nil)
(defvar *parse-mode-object-as-hash* nil)
(defvar *parse-mode-null-object* nil)
(defvar *parse-mode-true-object* t)
(defvar *parse-mode-false-object* nil)

(defclass transit-wrapper-mixin ()
  ((value :type string
          :initarg :value
          :reader transit-wrapper-mixin/value)))

(defmethod print-object ((obj transit-wrapper-mixin) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (if (slot-boundp obj 'value)
                            (slot-value obj 'value)
                            :not-bound))))

(defclass uuid (transit-wrapper-mixin)
  ())

(defclass url (transit-wrapper-mixin)
  ())

(defun decode-transit-string (string)
  (let ((tag (aref string 1)))
    (ecase tag
      (#\~ (subseq string 2))
      (#\_ (if (not (= (length string) 1))
               (error "Garbage at the of a null element: ~s" string)
               *parse-mode-null-object*))
      (#\s (subseq string 2))
      (#\? (if (not (= (length string) 3))
               (error "Unexpected boolean value: ~s" string)
               (ecase (aref string 2)
                 (#\t *parse-mode-true-object*)
                 (#\f *parse-mode-false-object*))))
      (#\i (parse-integer (subseq string 2)))
      (#\d (parse-float:parse-float (subseq string 2) :type 'long-double))
      (#\b (cl-base64:base64-string-to-usb8-array (subseq string 2)))
      (#\: (intern (string-upcase (subseq string 2)) "KEYWORD"))
      (#\f (error "decimal not supported"))
      (#\n (parse-integer (subseq string 2)))
      (#\m (let ((v (parse-integer (subseq string 2))))
             (multiple-value-bind (secs msecs)
                 (truncate v 1000)
               (local-time:unix-to-timestamp secs :nsec (* msecs 1000000)))))
      (#\t (local-time:parse-rfc3339-timestring (subseq string 2)))
      (#\u (make-instance 'uuid :value (subseq string 2)))
      (#\r (make-instance 'url :value (subseq string 2)))
      (#\c (if (/= (length string) 3)
               (error "Unexpected char value: ~s" string)
               (aref string 2))))))

(defun parse-transit-string-element (string)
  (if (and (plusp (length string))
           (eql (aref string 0) #\~))
      (decode-transit-string string)
      string))

(defun parse-object (js)
  (if *parse-mode-object-as-hash*
      (let ((hash (make-hash-table :test #'equal)))
        (loop
           for key = (json-streams:json-read js)
           until (eq key :end-object)
           do (let ((v (parse-element js)))
                (setf (gethash key hash) v)))
        hash)
      ;; ELSE: Parse as alist
      (loop
         for key = (json-streams:json-read js)
         until (eq key :end-object)
         collect (let ((v (parse-element js)))
                   (cons key v)))))

(defun parse-array (js)
  (if *parse-mode-list-as-vector*
      (let ((result (make-array 10 :adjustable t :fill-pointer 0)))
        (loop
           for v = (json-streams:json-read js)
           until (eq v :end-array)
           do (vector-push-extend (parse-element js v) result))
        result)
      ;; ELSE: Parse as normal list
      (loop
         for v = (json-streams:json-read js)
         until (eq v :end-array)
         collect (parse-element js v))))

(defun parse-element (js &optional initial-element)
  (let ((first-element (or initial-element (json-streams:json-read js))))
    (etypecase first-element
      (string (parse-transit-string-element first-element))
      (number first-element)
      (null *parse-mode-null-object*)
      (keyword (ecase first-element
                 (:true *parse-mode-true-object*)
                 (:false *parse-mode-false-object*)
                 (:begin-object (parse-object js))
                 (:begin-array (parse-array js))
                 (:eof (error "Premature EOF")))))))

(defun parse-stream (s)
  (let ((json-stream (json-streams:make-json-input-stream s)))
    (parse-element json-stream)))
