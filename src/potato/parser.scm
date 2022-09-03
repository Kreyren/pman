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

(define-module (potato parser)
  #:use-module (ice-9 peg)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (system vm trace)
  #:use-module (potato exceptions)
  #:use-module (potato makevars)
  #:use-module (potato rules)
  #:use-module (potato text)
  #:export (parse))

;;; Commentary:
;;;
;;; Core functionality of the parsed for non-guile makefiles
;;;
;;; Code:

;; A makefile can contain rules, macro definitions, include lines,
;; and comments.

(define (parse filename)
  "FIXME-DOCS"
  (with-input-from-file filename parse-input #:guess-encoding #t))

(define (last-char str)
  "FIXME-DOCS"
  (string-ref str (1- (string-length str))))

(define (parse-input)
  "FIXME-DOCS"
  (while #t
    (let loop ((line "")
	       (str (read-line)))
      (cond
       ((eof-object? str)
	(break))
       ((char=? (last-char str) #\\)
	(loop (string-append line str) (read-line)))
       (else
	(parse-line (string-append line str)))))))

;; For include lines
(define-peg-pattern I_TOK none "include")
(define-peg-pattern I_SPACE none (or " " "\t"))
(define-peg-pattern I_FILENAME_CHAR body (or (range #\a #\z)
					     (range #\A #\Z)
					     (range #\0 #\9)
					     "_" "-" "."))
(define-peg-pattern I_FILENAME all (+ I_FILENAME_CHAR))
(define-peg-pattern I_NL none "\n")
(define-peg-pattern I_COMMENT none (and "#" (* peg-any)))
(define-peg-pattern INCLUDE all (and I_TOK
				     (+ (and (* I_SPACE)
					     I_FILENAME))
				     (* I_SPACE)
				     (? I_COMMENT)))

;; For comment lines
(define-peg-pattern C_SPACE none (or " " "\t"))
(define-peg-pattern C_COMMENT none (and "#" (* peg-any)))
(define-peg-pattern COMMENT none (or C_COMMENT
				     (and (+ C_SPACE) (not-followed-by peg-any))))

(define (parse-line line)
  "FIXME-DOCS"
  (write (peg:tree (match-pattern INCLUDE line)))
  (newline)
  (write (peg:tree (match-pattern COMMENT line)))
  (newline)
  (cond
   ((line-is-include? line)
    (format #t "INCLUDE:   ~S~%" line))
   ((line-is-comment? line)
    (format #t "COMMENT:   ~S~%" line))
   ((line-is-macro? line)
    (format #t "MACRO:     ~S~%" line))
   ((line-is-special-target? line)
    (format #t "SPECIAL:   ~S~%" line))
   ((line-is-inference-rule? line)
    (format #t "INFERENCE: ~S~%" line))
   ((line-is-rule? line)
    (format #t "RULE:      ~S~%" line))
   (else
    (format #t "UNKNOWN:   ~S~%" line))))

(define (line-is-include? line)
  "FIXME-DOCS"
  (and (> (string-length line) 8)
       (string= line "include " 0 8)))

(define (line-is-comment? line)
  "FIXME-DOCS"
  (or (string-null? (string-trim-both line char-set:whitespace))
      (char=? (string-ref line 0) #\#)))

(define (line-is-macro? line)
  "FIXME-DOCS"
  (let ((len (string-length line)))
    (let loop ((i 0))
      (if (>= i len)
	  #f
	  ;; else
	  (let ((c (string-ref line i)))
	    (cond
	     ((and (zero? i)
		   (not (char-is-pcs? c)))
	      #f)
	     ((and (not (zero? i))
		   (char=? #\= c))
	      #t)
	     ((not (char-is-pcs-or-space? c))
	      #f)
	     (else
	      (loop (+ i 1)))))))))

(define (line-is-special-target? line)
  "FIXME-DOCS"
  (or (and (>= (string-length line) 8)
	   (string= line ".DEFAULT" 0 8))
      (and (>= (string-length line) 8)
	   (string= line ".IGNORE" 0 7))
      (and (>= (string-length line) 6)
	   (string= line ".POSIX"))
      (and (>= (string-length line) 9)
	   (string= line ".PRECIOUS" 0 9))
      (and (>= (string-length line) 9)
	   (string= line ".SCCS_GET" 0 9))
      (and (>= (string-length line) 7)
	   (string= line ".SILENT" 0 7))))

(define (line-is-rule? line)
  "FIXME-DOCS"
  (let ((len (string-length line)))
    (let loop ((i 0))
      (if (>= i len)
	  #f
	  ;; else
	  (let ((c (string-ref line i)))
	    (cond
	     ((and (zero? i)
		   (not (char-is-pcs? c)))
	      #f)
	     ((and (not (zero? i))
		   (char=? #\: c))
	      #t)
	     ((not (char-is-pcs-or-space? c))
	      #f)
	     (else
	      (loop (+ i 1)))))))))

(define (line-is-inference-rule? line)
  "FIXME-DOCS"
  (let ((len (string-length line)))
    (let loop ((i 0)
	       (dot-count 0))
      (if (>= i len)
	  #f
	  ;; else
	  (let ((c (string-ref line i)))
	    (cond
	     ((and (zero? i)
		   (not (char=? #\. c)))
	      #f)
	     ((and (not (zero? i))
		   (char=? #\: c))
	      (if (or (= dot-count 1)
		      (= dot-count 2))
		  #t
		  #f))
	     ((not (char-is-pcs? c))
	      #f)
	     (else
	      (loop (+ i 1)
		    (+ dot-count
		       (if (char=? c #\.)
			   1
			   0))))))))))

(define (char-is-pcs? c)
  "FIXME-DOCS"
  (or (and (char<=? #\a c) (char>=? #\z c))
      (and (char<=? #\A c) (char>=? #\Z c))
      (and (char<=? #\0 c) (char>=? #\9 c))
      (char=? #\. c)
      (char=? #\_ c)))

(define (char-is-pcs-or-space? c)
  "FIXME-DOCS"
  (or (char-is-pcs? c)
      (char=? #\space c)))

;;; parser.scm ends here
