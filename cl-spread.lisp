;;; Common Lisp bindings for libspread using CFFI
;;; Thomas Munro <munro@ip9.org>
;;;
;;; See README for some more information.

(defpackage :cl-spread
  (:nicknames :spread)
  (:use :common-lisp :cffi)
  (:export :connect :disconnect :multicast :receive :join :leave
           :envelope-message :envelope-sender
           :with-mailbox 
            #+sbcl :wait
           :regular-message? :membership-message? :reg-membership-message?
           :transition-message?))

(in-package :cl-spread)

(define-foreign-library libtspread
  (:unix (:default "libtspread")))

(use-foreign-library libtspread)

;; Defaults
(defparameter +default-spread-name+ "4803")
(defconstant +max-groups+ 20 "Maxmimum size of group list for RECEIVE.")
(defconstant +max-message+ 1024 "Default largest message we can RECEIVE.")

;; Bit masks
(defconstant +UNRELIABLE_MESS+ #x01)
(defconstant +RELIABLE_MESS+ #x02)
(defconstant +FIFO_MESS+ #x04)
(defconstant +CASUAL_MESS+ #x08)
(defconstant +AGREED_MESS+ #x10)
(defconstant +SAFE_MESS+ #x20)
(defconstant +REGULAR_MESS+ #x3f)
(defconstant +SELF_DISCARD+ #x40)
(defconstant +REG_MEMB_MESS+ #x1000)
(defconstant +TRANSITION_MESS+ #x2000)
(defconstant +MEMBERSHIP_MESS+ #x3f00)
(defconstant +CAUSED_BY_JOIN+ #x100)
(defconstant +CAUSED_BY_LEAVE+ #x200)
(defconstant +CAUSED_BY_DISCONNECT+ #x400)
(defconstant +CAUSED_BY_NETWORK+ #x800)
(defconstant +REJECT_MESS+ #x400000)

;; Fixed array sizes
(defconstant +MAX_GROUP_NAME+ 32)

;; Return codes
(defconstant +ACCEPT_SESSION+ 1)
(defconstant +ILLEGAL_SPREAD+ -1)
(defconstant +COULD_NOT_CONNECT+ -2)
(defconstant +REJECT_QUOTA+ -3)
(defconstant +REJECT_NO_NAME+ -4)
(defconstant +REJECT_ILLEGAL_NAME+ -5)
(defconstant +REJECT_NOT_UNIQUE+ -6)
(defconstant +REJECT_VERSION+ -7)
(defconstant +CONNECTION_CLOSED+ -8)
(defconstant +REJECT_AUTH+ -9)
(defconstant +ILLEGAL_SESSION+ -11)
(defconstant +ILLEGAL_SERVICE+ -12)
(defconstant +ILLEGAL_MESSAGE+ -13)
(defconstant +ILLEGAL_GROUP+ -14)
(defconstant +BUFFER_TOO_SHORT+ -15)
(defconstant +GROUPS_TOO_SHORT+ -16)
(defconstant +MESSAGE_TOO_LONG+ -17)

;;; Raw C functions 

(defcfun ("SP_connect" %sp-connect) :int
  (spread-name :string)     ; const char *spread_name
  (private-name :string)    ; const char *private_name
  (priority :int)           ; int priority
  (group-membership :int)   ; int group_membership
  (mailbox :pointer)        ; mailbox *mbox
  (private-group :pointer)) ; char *private_group

(defcfun ("SP_disconnect" %sp-disconnect) :int
  (mailbox :int))

(defcfun ("SP_join" %sp-join) :int
  (mailbox :int)
  (group :string))

(defcfun ("SP_leave" %sp-leave) :int
  (mailbox :int)
  (group :string))

(defcfun ("SP_multicast" %sp-multicast) :int
  (mailbox :int)
  (service-type :int)
  (group :string)
  (message-type :int16)
  (message-length :int)
  (message :string))

(defcfun ("SP_multigroup_multicast" %sp-multigroup-multicast) :int
  (mailbox :int)
  (service-type :int)
  (num-groups :int)
  (group :pointer)
  (message-type :int16)
  (message-length :int)
  (message :string))

(defcfun ("SP_receive" %sp-receive) :int
  (mailbox :int)                ; maibox mbox
  (service :pointer)            ; service *service_type
  (sender :pointer)             ; char sender[MAX_GROUP_NAME]
  (max-groups :int)             ; int max_groups
  (num-groups :pointer)         ; int *num_groups
  (groups :pointer)             ; char groups[][MAX_GROUP_NAME]
  (message-type :pointer)       ; int16 *mess_type
  (endian-mismatch :pointer)    ; int *endian_mismatch
  (max-length :int)             ; int max_mess_len
  (message :pointer))           ; char *mess

;;; Slightly friendlier Lisp interface on top of the C functions

(defstruct mailbox
  "A mailbox connected to a Spread daemon."
  (fileno)
  (private-group))

(defun make-random-name ()
  "Generate a random private-name.  TODO make really unique..."
  (format nil "user-~a" (random 100000)))

(defmacro with-mailbox ((name &rest arguments) &body body)
  "A macro to manage a scoped connection to the Spread deamon."
  `(let ((,name (connect ,@arguments)))
    (unwind-protect
      (progn ,@body)
      (disconnect ,name))))

(defun connect (&key (spread-name +default-spread-name+)
                     (private-name (make-random-name))
                     (priority nil)
                     (group-membership nil))
  "Create a new mailbox.  See the Spread documentation for the meaning
   of the optional parameters."
  (with-foreign-objects ((mbox :int)
                         (group :char +MAX_GROUP_NAME+))
    (let ((result (%sp-connect spread-name 
                               private-name
                               (if priority 1 0)
                               (if group-membership 1 0)
                               mbox 
                               group)))
      (cond ((= result +ACCEPT_SESSION+)
             (make-mailbox :fileno (mem-ref mbox :int)
                           :private-group (foreign-string-to-lisp group)))
            ((= result +COULD_NOT_CONNECT+)
             (error "spread:connect -- could not connect"))
            ((= result +CONNECTION_CLOSED+)
             (error "spread:connect -- mailbox closed"))
            ((= result +REJECT_VERSION+)
             (error "spread:connect -- reject version"))
            ((= result +REJECT_NO_NAME+)
             (error "spread:connect -- reject no name"))
            ((= result +REJECT_ILLEGAL_NAME+)
             (error "spread:connect -- reject illegal name"))
            ((= result +REJECT_NOT_UNIQUE+)
             (error "spread:connect -- not unique"))
            (t (error "spread:connect -- undocumented error ~a" result))))))

(defun disconnect (mailbox)
  "Disconnect a mailbox."
  (let ((result (%sp-disconnect (mailbox-fileno mailbox))))
    (cond ((>= result 0) t)
          ((= result +ILLEGAL_SESSION+)
           (error "spread:disconnect -- illegal session"))
          (t (error "spread:disconnect -- undocumented error ~a" result)))))

(defun join (mailbox group)
  "Join a group."
  (let ((result (%sp-join (mailbox-fileno mailbox) group)))
    (cond ((>= result 0) t)
          ((= result +ILLEGAL_GROUP+)
           (error "spread:join -- illegal group"))
          ((= result +ILLEGAL_SESSION+)
           (error "spread:join -- illegal session"))
          ((= result +CONNECTION_CLOSED+)
           (error "spread:join -- mailbox closed"))
          (t (error "spread:join -- undocumented error ~a" result)))))

(defun leave (mailbox group)
  "Leave a group."
  (let ((result (%sp-leave (mailbox-fileno mailbox) group)))
    (cond ((>= result 0) t)
          ((= result +ILLEGAL_GROUP+)
           (error "spread:leave -- illegal group"))
          ((= result +ILLEGAL_SESSION+)
           (error "spread:leave -- illegal session"))
          ((= result +CONNECTION_CLOSED+)
           (error "spread:leave -- mailbox closed"))
          (t (error "spread:leave -- undocumented error ~a" result)))))

(defun service-type->number (symbol)
  "Convert a service-type symbol into an integer constant."
  (case symbol
    (:unreliable +UNRELIABLE_MESS+)
    (:reliable +RELIABLE_MESS+)
    (:fifo +FIFO_MESS+)
    (:casual +CASUAL_MESS+)
    (:agreed +AGREED_MESS+)
    (:safe +SAFE_MESS+)
    (:regular +REGULAR_MESS+)
    (t (error "unknown service type ~a" symbol))))

(defun multicast (mailbox service group message-type message self-discard)
  "Send a message to a group."
  (let ((result (%sp-multicast (mailbox-fileno mailbox)
                               (+ (service-type->number service)
                                  (if self-discard +SELF_DISCARD+ 0))
                               group
                               message-type
                               (length message)
                               message)))
    (cond ((>= result 0) result)
          ((= result +ILLEGAL_SESSION+)
           (error "spread:multicast -- illegal session"))
          ((= result +ILLEGAL_MESSAGE+)
           (error "spread:multicast -- illegal message"))
          ((= result +CONNECTION_CLOSED+)
           (error "spread:multicast -- mailbox closed"))
          (t (error "spread:multicast -- undocumented error ~a" result)))))

(defstruct envelope
  "An envelope containing incoming messages and assocated meta-data."
  (service-type)
  (sender)
  (message-type)
  (groups)
  (endian-mismatch)
  (message))

(defun %regular-message? (service-type)
  "Bitfield predicate for regular messages (ie incoming data)."
  (and (not (zerop (logand service-type +REGULAR_MESS+)))
       (zerop (logand service-type +REJECT_MESS+))))

(defun %membership-message? (service-type)
  "Bitfield predicate for membership change notification messages."
  (and (not (zerop (logand service-type +MEMBERSHIP_MESS+)))
       (zerop (logand service-type +REJECT_MESS+))))

(defun %transition-message? (service-type)
  "Bitfield predicate for transition notification messages."
  (not (zerop (logand service-type +TRANSITION_MESS+))))

(defun %reg-membership-message? (service-type)
  "Bitfield predicate for 'reg' membership change notification messages."
  (not (zerop (logand service-type +REG_MEMB_MESS+))))

(defun regular-message? (envelope)
  "Check if an envelope contains a regular data message."
  (%regular-message? (envelope-service-type envelope)))

(defun membership-message? (envelope)
  "Check if an envelope contains a membership message."
  (%membership-message? (envelope-service-type envelope)))

(defun transition-message? (envelope)
  "Check if an envelope contains a transition message."
  (%transition-message? (envelope-service-type envelope)))

(defun reg-membership-message? (envelope)
  "Check if an envelope contains a reg membership message."
  (%reg-membership-message? (envelope-service-type envelope)))

(defun extract-groups (groups count)
  "Makes a list of Lisp strings from multidimensional C array."
  (loop for i from 0 below count
        collect (foreign-string-to-lisp groups 
                                        :offset (* i +MAX_GROUP_NAME+)
                                        :max-chars +MAX_GROUP_NAME+)))

#+sbcl                                              ;; SBCL on Unix
(defun wait (mailbox seconds)
  "Wait until data is ready and returns T, or time out and returns NIL.
   TODO: find out how to do this portably.  MAILBOX-FILENO gives you a 
   file descriptor for a socket, so you can use that with select, poll,
   etc etc and multiplex with other IO without any help from this package
   anyway so I probably don't even need to provide this..."
  (sb-sys:wait-until-fd-usable (mailbox-fileno mailbox) :input seconds))

(defun receive (mailbox &optional (max-size +max-message+))
  "Fetch the next message.  Blocks until a message is available, but you
   can make sure it never blocks by using [non-portable] file descriptor 
   readiness tests on the fileno (see WAIT).  The result is an envelope
   struct which must be tested to see what type of message it contains
   (in the C API there was no such concept, because the C function uses
   output parameters which doesn't really work in Lisp)."
  (with-foreign-objects ((service-type :int)
                         (sender :char +MAX_GROUP_NAME+)
                         (num-groups :int)
                         (groups :char (* +MAX_GROUP_NAME+ +max-groups+))
                         (message-type :int16)
                         (endian-mismatch :int)
                         (message :char max-size))
    (let ((result (%sp-receive (mailbox-fileno mailbox)
                               service-type
                               sender
                               +max-groups+
                               num-groups
                               groups
                               message-type
                               endian-mismatch
                               +max-message+
                               message)))
      (cond ((>= result 0)
             (cond 
               ((%regular-message? (mem-ref service-type :int))
                (make-envelope :service-type (mem-ref service-type :int)
                               :sender (foreign-string-to-lisp sender)
                               :message-type (mem-ref message-type :int16)
                               :groups (extract-groups groups   
                                         (mem-ref num-groups :int))
                               :endian-mismatch 
                               (zerop (mem-ref endian-mismatch :int))
                               :message (foreign-string-to-lisp message 
                                                                :count result)))
               ((%transition-message? (mem-ref service-type :int))
                (make-envelope :service-type (mem-ref service-type :int)
                               :sender (foreign-string-to-lisp sender)))
               ((%reg-membership-message? (mem-ref service-type :int))
                (make-envelope :service-type (mem-ref service-type :int)
                               :sender (foreign-string-to-lisp sender)
                               :groups nil
                               :num-groups (mem-ref num-groups :int)
                               :index message-type))
               (t
                (make-envelope :service-type service-type))))
            ((= result +ILLEGAL_SESSION+)
             (error "spread:receive -- illegal session"))
            ((= result +ILLEGAL_MESSAGE+)
             (error "spread:receive -- illegal message"))
            ((= result +CONNECTION_CLOSED+)
             (error "spread:receive -- mailbox closed"))
            ((= result +BUFFER_TOO_SHORT+)
             (error "spread:receive -- buffer too short"))
            ((= result +GROUPS_TOO_SHORT+)
             (error "spread:receive -- groups too short"))
            (t (error "spread:receive -- undocumented error ~a" result))))))


