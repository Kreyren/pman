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

(define-module (potato parse)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 receive)
  #:use-module (system vm trace)
  #:use-module (potato exceptions)
  #:use-module (potato makevars)
  #:use-module (potato rules)
  #:use-module (potato text)
  #:use-module (potato parse-lib)
  #:export (parse _readline))

;;; Commentary:
;;;
;;; Functionality to parse non-guile makefiles
;;;
;;; Code:

;; A makefile can contain rules, macro definitions, include lines, and comments.

(define (parse filename)
  "FIXME-DOCS"
  (with-input-from-file filename _eval #:guess-encoding #t))

(define (_eval)
  "FIXME-DOCS"
  (let ((filenames #f)
	(ignoring #f)
	(commands '()))
    ;; FIXME-QA(Krey): `(while #t` is not robust enough, specify the logic to lesser the chance of infinite loops
    (while #t
      (receive (line nlines)
	  (read-line-handle-escaped-newline)
	(cond
	 ((zero? nlines)
	  (break))

	 ((string-starts-with? line #\tab)
	  ;; Shell-command lines
	  (when filenames
	    (when ignoring
	      (continue))
	    (set! commands (append commands (list line)))))

	 (else
	  (display
	   (string-trim-both
	    (string-remove-comments
	     (string-collapse-continuations line #t))))
	  (newline)))))))

(define (string-parse-variable-definition str i)
  "Parse a string as a variable definition"
  (let loop ((i (string-next-token str)))
    (cond
     ((= i (string-length str))
      (values i 'null))

     ((char=? (string-ref str i) #\#)
      ;; Comments aren't variable definitions.
      (values i 'null))

     ((char=? (string-ref str i) #\$)
      ;; This begins a variable expansion reference.
      (let* ((openparen (false-if-exception (string-ref str (1+ i))))
	     (closeparen (if (eqv? openparen #\()
			     #\)
			     (if (eqv? openparen #\{)
				 #\}
				 #f))))
	(if (not closeparen)
	    (values i 'null)

	    ;; else, skip over the matching closeparen
	    (begin
	      (let ((count 0))
		(while #t
		  (set! i (1+ i))
		  (when (char=? (string-ref str i) openparen)
		    (set! count (1+ count)))
		  (when (char=? (string-ref str i) closeparen)
		    (set! count (1- count))
		    (when (zero? count)
		      (set! i (1+ i))
		      (break)))))

	      ;; Any whitespace before the operator?
	      (when (char-set-contains? char-set:blank (string-ref str i))
		(set! wspace #t)
		(set! i (string-next-token str i)))

	      (cond
	       ((eqv? (string-ref str i) #\=)
		(values (1+ i) 'recursive))
	       ((and (eqv? (string-ref str i) #\:)
		     (eqv? (string-ref str (1+ i)) #\=))
		(values (+ i 2) 'simple))
	       ((and (eqv? (string-ref str i) #\+)
		     (eqv? (string-ref str (1+ i)) #\=))
		(values (+ i 2) 'append))
	       ((and (eqv? (string-ref str i) #\?)
		     (eqv? (string-ref str (1+ i)) #\=))
		(values (+ i 2) 'conditional))
	       (else
		(values i 'null)))))))
     (else
      (values i 'null)))))

;; STUB(Krey): Unfinished code by original author, kept here in case we need to finish it in the future
#|
(define (parse-var-assignment line)
  (let ((i (string-next-token line 0)))
    (if (= i (string-length line))
	#f
	;; else
	(while #t
|#

;;; parse.scm ends here
