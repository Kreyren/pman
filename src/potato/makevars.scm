;;; The Project Manager ("pman") -- GNU Guile-based solution for project management
;;; Copyright (C) 2022 Jacob Hrbek <kreyren@rixotstudio.cz>
;;;
;;; This file is Free/Libre Open-Source Software; you may copy, redistribute and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
;;; This file is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public license along with this project. If not, see <http://www.gnu.org/licenses>
;;;
;;; This file incorporates work covered by the following copyright and permission notice:
;;;
;;; Copyright (C) 2017-2021 Mike Gran <spk121@yahoo.com>
;;;
;;; Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE

(define-module (potato makevars)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (potato exceptions)
  #:use-module (potato builtins)
  #:use-module (potato text)
  #:export (initialize-makevars
	    %makevars
	    %elevate-environment?
	    lazy-assign    ?=
	    assign         :=
	    reference      $    Q
	    reference-func $$
	    dump-makevars))

;;; Commentary:
;;;
;;; Used to handle the 'makevars' functionality meaning assigning and managing environmental variables into variables used by guile
;;;
;;; Code:

;; Defines the priority level
;; There are five priority levels
;;   1. defined - in the script itself
;;   2. command-line
;;   3. makeflags - in the MAKEFLAGS environment variable
;;   4. env - specified in the environment
;;   5. built-in - one of the built-in macros
;;
;; The lower priority level always win, unless the '-e' flag was set
;; If the '-e' flag is set level 1 doesn't override level 3 and 4.
(define %level-name '("unknown"
		      "script"
		      "command-line"
		      "makeflags"
		      "environment"
		      "built-in"))

;; WTF(Krey): No idea.. something with ASCII? If ASCII is supported?
(define %ascii? #f)

;; WTF(Krey)
(define %makevars (make-hash-table))

;; WTF(Krey)
(define %elevate-environment? #f)

;; WTF(Krey)
(define %strict #f)

;; WTF(Krey)
(define %verbose? #t)

;; WTF(Krey)
(define (debug spec . args)
  "Output to handle debugging messages"
  (when %verbose?
    (apply format (append (list #t spec) args))))

(define (split-at-equals str)
  "Splits the string at the first equals sign, ignoring later equals signs."
  (let ((idx (string-index str #\=)))
    (if (and idx (> idx 0))
	(cons (substring str 0 idx)
	      (substring str (1+ idx)))
	;; else
	#f)))

(define (string-append-with-spaces lst)
  "Appends the strings in lst, adding spaces in between."
  (fold
   (lambda (elem prev)
     (string-append prev " " elem))
   (car lst)
   (cdr lst)))

(define (override? old-priority new-priority)
  "The logic of whether which makemacro priority levels can override
others."
  (if %elevate-environment?
      (if (and (or (= old-priority 2) (= old-priority 3) (= old-priority 4))
	       (= new-priority 1))
	  #f
	  ;; else
	  (<= new-priority old-priority))
      ;; else
      (<= new-priority old-priority)))

(define* (makevars-set key
		       #:optional (new-val "") (new-priority 1))
  "Maybe add key / val to %makevars hash table, if there is sufficient
priority."
  ;; Note that VAL can be either a string or a procedure.  If it is a
  ;; procedure, it is converted into a promise to be evaluated later.
  (let* ((val&priority (hash-ref %makevars key))
	 (old-val (if (pair? val&priority) (cdr val&priority) #f))
	 (old-priority (if (pair? val&priority) (cdr val&priority) #f)))
    (when (or (not old-val)
	      (override? old-priority new-priority))
      (if (procedure? new-val)
	  (hash-set! %makevars key (cons (delay new-val) new-priority))
	  (hash-set! %makevars key (cons new-val new-priority)))
      (when %verbose? (print-makevar key))))
  *unspecified*)

(define (makevars-add-keyvals keyvals)
  "Adds any suitable macros passed in from the command line, which here are expected to be a list of key / val string pairs."
  (for-each
   (lambda (entry)
     (let ((key (car entry))
	   (val (cdr entry)))
       (unless (or (string=? key "SHELL")
		   (string=? key "MAKEFLAGS"))
	 (makevars-set key val 2))))
   keyvals))

(define (makevars-add-makeflags)
  "Adds any suitable environment variables found in the MAKEFLAGS environment variable to the macro store"
  (let ((makeflags (getenv "MAKEFLAGS")))
    (when makeflags
      (for-each
       (lambda (entry)
	 (let* ((keyval (split-at-equals entry))
		(key (if keyval (car keyval) #f))
		(val (if keyval (cdr keyval) #f)))
	   (unless (or (not (string? key))
		       (string=? key "SHELL")
		       (string=? key "MAKEFLAGS"))
	     (makevars-set key val 3))))
       (string-split makeflags #\space)))))

(define (makevars-add-environment)
  "Adds any suitable environment variables to the macro store, but not the value of MAKEFLAGS or SHELL."
  (for-each
   (lambda (entry)
     (let* ((keyval (split-at-equals entry))
	    (key (if keyval (car keyval) #f))
	    (val (if keyval (cdr keyval) #f)))
       (unless (or (string=? key "SHELL")
		   (string=? key "MAKEFLAGS"))
	 (makevars-set key val 4))))
   (environ)))

(define (makevars-add-builtins)
  "Adds the default macros to the store"
  (for-each
   (lambda (keyval)
     (makevars-set (car keyval) (cdr keyval) 5))
   builtin-makevars))

;; FIXME-DOCS(Krey)
(define (print-makevar key)
  (let ((val (hash-ref %makevars key)))
    (let ((keyval-string
	   (if (zero? (string-length (car val)))
	       (string-copy key)
	       (string-append key " " (right-arrow) " " (car val)))))
      ;; Replace any control characters in VAL, like newline or tab
      (set! keyval-string
	    (string-fold
	     (lambda (c str)
	       (string-append str
			      (if (char<? c #\space)
				  (C0 c)
				  (string c))))
	     ""
	     keyval-string))
      ;; Truncate
      (if (> (string-length keyval-string) 60)
	  (if %ascii?
	      (set! keyval-string
		    (string-append (substring keyval-string 0 57) "..."))
	      (set! keyval-string
		    (string-append (substring keyval-string 0 59) "…"))))
      (let* ((space (make-string (- 64 (string-length keyval-string))
				 #\space))
	     (priority (cdr val))
	     (source-string (list-ref '("unknown"
					"script"
					"command line"
					"MAKEFLAGS"
					"environment"
					"built-in")
				      priority)))
	(display "Var:  ")
	(display keyval-string)
	(display space)
	(display source-string)
	(newline)))))

(define (dump-makevars)
  "Write out a list of the current makevars."
  (when (not (zero? (hash-count (const #t) %makevars)))
    (display (underline))
    (display "Makevars")
    (display (default))
    (newline)
    (let ((keyvals
	   (sort
	    (hash-map->list cons %makevars)
	    (lambda (a b)
	      (string<? (car a) (car b))))))
      (for-each
       (lambda (keyval)
	 (let ((key (car keyval))
	       (val (cdr keyval)))
	   (let ((keyval-string
		  (if (zero? (string-length (car val)))
		      (string-copy key)
		      (string-append key " " (right-arrow) " " (car val)))))
	     ;; Replace any control characters in VAL, like newline or tab
	     (set! keyval-string
		   (string-fold
		    (lambda (c str)
		      (string-append str
				     (if (char<? c #\space)
					 (C0 c)
					 (string c))))
		    ""
		    keyval-string))
	     ;; Truncate
	     (if (> (string-length keyval-string) 60)
		 (if %ascii?
		     (set! keyval-string
			   (string-append (substring keyval-string 0 57) "..."))
		     (set! keyval-string
			   (string-append (substring keyval-string 0 59) "…"))))
	     (let* ((space (make-string (- 64 (string-length keyval-string))
					#\space))
		    (priority (cdr val))
		    (source-string (list-ref '("unknown"
					       "script"
					       "command line"
					       "MAKEFLAGS"
					       "environment"
					       "built-in")
					     priority)))
	       (display "  ")
	       (display keyval-string)
	       (display space)
	       (display source-string)
	       (newline)))))
       keyvals))))

;; FIXME-DOCS(Krey)
(define (initialize-makevars keyvals
			     environment?
			     elevate-environment?
			     builtins?
			     strict?
			     verbosity
			     ascii?)
  (set! %elevate-environment? elevate-environment?)
  (hash-clear! %makevars)
  (set! %strict strict?)
  (set! %verbose? (= verbosity 3))
  (set! %ascii? ascii?)
  (when builtins?
    (makevars-add-builtins))
  (when (or environment? elevate-environment?)
    (makevars-add-environment)
    (makevars-add-makeflags))
  (makevars-add-keyvals keyvals))

;;; API

(define* (lazy-assign key #:optional (val ""))
  "This procedure sets an entry in the %makevars hash table.
KEY must be a string or a thunk that evaluates to a string. Likewise
VAL.
    If KEY is a thunk, it is immediately evaluated to a string to use as the key in the hash table entry.
    If VAL is a thunk, it is stored as a *promise* to be evaluated later. The promise will be evaluated the first time this key is
referenced.
    If VAL is not given, the empty string will be used."
  (when (procedure? key)
    (set! key (key)))
  (unless (string? key)
    (set! key (format #f "~a" key)))
  (makevars-set key (delay val)))

(define-syntax ?=
  (lambda (stx)
    (syntax-case stx ()
      ((_ key val)
       #'(lazy-assign (symbol->string (syntax->datum #'key)) val))
      ((_ key)
       #'(lazy-assign (symbol->string (syntax->datum #'key)))))))

(define* (assign key #:optional (val ""))
  "This procedure sets an entry in the %makevars hash table.
KEY must be a string or a thunk that evaluates to a string. Likewise
VAL.
    If KEY and/or VAL is a thunk, it is immediately evaluated to a string to use as the key in the hash table entry.
    If VAL is not given, the empty string will be used."
  (when (procedure? key)
    (set! key (key)))
  (unless (string? key)
    (set! key (format #f "~a" key)))
  (when (procedure? val)
    (set! val (val)))
  (unless (string? val)
    (set! val (format #f "~a" val)))
  (makevars-set key val))

(define-syntax :=
  (lambda (stx)
    (syntax-case stx ()
      ((_ key val)
       #'(assign (symbol->string (syntax->datum #'key)) val))
      ((_ key)
       #'(assign (symbol->string (syntax->datum #'key)))))))

(define* (reference key quoted? #:optional (transformer #f))
  "Looks up KEY in the %makevars hash table. KEY may be a string
or a procedure that evaluates to a string.
     If the value of the key in the hash table was a *promise* it will be forced, evaluated, and set to that result.
     If no transformer is supplied, the looked up value will be returned.
     TRANSFORMER, if supplied, should be a procedure of one string argument that returns a string. If a transformer is supplied, it will be applied to every space-separated token in the looked-up value."
  (when (and (not (string? key))
	     (not (procedure? key)))
    (bad-key-type "reference" (list key)))
  (when (procedure? key)
    (set! key (key))
    (unless (string? key)
      (bad-proc-output "reference" key)))
  (when (not (string? key))
    (set! key (format #t "~a" key)))
  (let* ((val&priority (hash-ref %makevars key))
	 (val (if (pair? val&priority) (car val&priority) #f))
	 (priority (if (pair? val&priority) (cdr val&priority) #f)))
    (if (not val)
	(if %strict
	    (error (format #t "There is no makevar for key ~a~%~!" key))
	    ;; else
	    (if quoted?
		"\"\""
		""))
	;; else
	(begin
	  (cond
	   ((promise? val)
	    (set! val (force val))
	    (cond
	     ((string? val)
	      ;; noop
	      #t)
	     ((procedure? val)
	      (set! val (val)))
	     (else
	      (set! val (format #f "~a" val)))))
	   ((string? val)
	    ;; noop
	    #f)
	   (else
	    (set! val (format #f "~a" val))))
	  (hash-set! %makevars key (cons val priority))
	  (when %verbose? (print-makevar key))
	  (when (procedure? transformer)
	    (set! val (string-append-with-spaces
		       (map transformer
			    (string-tokenize val)))))
	  (if quoted?
	      (string-append "\"" val "\"")
	      val)))))

;; FIXME-DOCS(Krey)
(define-syntax $
  (lambda (stx)
    (syntax-case stx ()
      ((_ key transformer)
       #'(reference (symbol->string (syntax->datum #'key)) #f transformer))
      ((_ key)
       #'(reference (symbol->string (syntax->datum #'key)) #f)))))

;; FIXME-DOCS(Krey)
(define-syntax Q
  (lambda (stx)
    (syntax-case stx ()
      ((_ key transformer)
       #'(reference (symbol->string (syntax->datum #'key)) #t transformer))
      ((_ key)
       #'(reference (symbol->string (syntax->datum #'key)) #t)))))

(define (reference-func key)
  "Looks up KEY in the %makevars hash table. KEY shall be a string or a procedure that evaluates to a string.
   If the value of the key in the hash table was a *promise*, a procedure will be returned that, when called, will call that promise.
   If the value of the key is a string, a procedure will be returned that, when called, returns that string."
  (when (and (not (string? key))
	     (not (procedure? key)))
    (bad-key-type "reference" (list key)))
  (when (procedure? key)
    (set! key (key))
    (unless (string? key)
      (bad-proc-output "reference" key))
  (let* ((val&priority (hash-ref %makevars key))
	 (val (if (pair? val&priority) (cdr val&priority) #f)))
    (if (not val)
	#f
	;; else
	(begin
	  (if (promise? val)
	      (lambda ()
		(let ((VAL (force val)))
		  ;; FIXME(spk121): put verbose print here?
		  VAL))
	      ;; else
	      (lambda ()
		val)))))))

;; FIXME-DOCS(Krey)
(define-syntax $$
  (lambda (stx)
    (syntax-case stx ()
      ((_ key)
       #'(reference-func (symbol->string (syntax->datum #'key)))))))

;; makevards.scm ends here
