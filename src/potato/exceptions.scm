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

(define-module (potato exceptions)
  #:use-module (ice-9 exceptions)
  #:export (bad-key-type
	    bad-value-type
	    bad-proc-output
	    invalid-macro
	    not-a-regular-file
	    not-a-procedure
	    no-read-access-to-file))

;;; Commentary:
;;;
;;; Dedicated to handling 'exceptions' as in termination of the program
;;;
;;; Code:

;; FIXME-QA(Krey): This code is using 'bad' which is hard to understand without context, thus should be changed on 'unexpected'

(define (make-bad-key-type origin irritants)
  "Exception used when bad key is parsed to 'make' functionality"
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   ;; FIXME-TRANSLATE(Krey)
   (make-exception-with-message "Wrong type for key. Expecting string or procedure.")
   (make-exception-with-irritants irritants)))

(define (bad-key-type origin irritant)
  "Generic exception for unexpected key"
  (raise-exception (make-bad-key-type origin irritant)))

(define (make-bad-value-type origin irritants)
  "Exception for unexpected value in the 'make' functionality"
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   ;; FIXME-TRANSLATE(Krey)
   (make-exception-with-message "Wrong type for value. Expecting string or procedure.")
   (make-exception-with-irritants irritants)))

(define (bad-value-type origin irritant)
  "Generic exception for unexpected value"
  (raise-exception (make-bad-value-type origin irritant)))

(define (make-bad-proc-output origin irritants)
  "Exception for unexpected process output in 'make' functionality"
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Procedure does not evaluate to a string.")
   (make-exception-with-irritants irritants)))

(define (bad-proc-output origin irritant)
  "Generic exception for unexpected process output"
  (raise-exception (make-bad-proc-output origin irritant)))

(define (make-invalid-macro origin irritants)
  "Exception for invalid macro in 'make' functionality"
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Invalid macro format")
   (make-exception-with-irritants irritants)))

(define (invalid-macro origin irritant)
  "Generic exception for invalid macro"
  (raise-exception (make-invalid-macro origin irritant)))

(define (make-not-a-regular-file origin irritants)
  "Exception for not regular file in 'make' functionality"
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Not a regular file")
   (make-exception-with-irritants irritants)))

(define (not-a-regular-file origin irritant)
  "Generic exception for when regular file is not parsed"
  (raise-exception (make-not-a-regular-file origin irritant)))

(define (make-not-a-procedure origin irritants)
  "Exception for when regular file is not parsed in the 'make' functionality"
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Not a procedure")
   (make-exception-with-irritants irritants)))

(define (not-a-procedure origin irritant)
  "Generic exception for when procedure is not parsed"
  (raise-exception (make-not-a-procedure origin irritant)))

(define (make-no-read-access-to-file origin irritants)
  "Exception for when we don't have the expected read access to the file in 'make' functionality"
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Do not have permissions to read file")
   (make-exception-with-irritants irritants)))

(define (no-read-access-to-file origin irritant)
  "Generic exception for when we don't have the excepted read access to the file"
  (raise-exception (make-no-read-access-to-file origin irritant)))

;;; exceptions.scm end here
