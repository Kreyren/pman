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

(define-module (potato text)
  #:export (underline
	    default
	    right-arrow
	    left-arrow
	    ellipses
	    C0
	    red green
	    lquo
	    rquo
	    initialize-text))

;;; Commentary:
;;;
;;; File handling the text formatting in the repository
;;;
;;; Code:

(define %fancy #t)
(define (initialize-text ascii)
  "FIXME-DOCS"
  (set! %fancy (not ascii)))

(define (default)
  "FIXME-DOCS"
  (if %fancy
      (string #\escape #\[ #\0 #\m)
      ""))

(define (bold)
  "FIXME-DOCS"
  (if %fancy
      (string #\escape #\[ #\1 #\m)
      ""))

(define (underline)
  "FIXME-DOCS"
  (if %fancy
      (string #\escape #\[ #\4 #\m)
      ""))

(define (red)
  "FIXME-DOCS"
  (if %fancy
      (string #\escape #\[ #\3 #\1 #\m)
      ""))

(define (green)
  "FIXME-DOCS"
  (if %fancy
      (string #\escape #\[ #\3 #\2 #\m)
      ""))

(define (blue)
  "FIXME-DOCS"
  (if %fancy
      (string #\escape #\[ #\3 #\4 #\m)
      ""))

(define (important)
  "FIXME-DOCS"
  (if %fancy
      "⚠"                               ; U+26A0 WARNING SIGN
      "!!!"))

(define (stop)
  "FIXME-DOCS"
  (if %fancy
      "🛑"                               ; U+26A0 WARNING SIGN
      "XXX"))

(define (right-arrow)
  "FIXME-DOCS"
  (if %fancy
      "→" "->"))

(define (left-arrow)
  "FIXME-DOCS"
  (if %fancy
      "←" "<-"))

(define (ellipses)
  "FIXME-DOCS"
  (if %fancy "…" "..."))

(define (QED)
  "FIXME-DOCS"
  (if %fancy "∎" "QED"))                ; U+220E END OF PROOF

(define (C0 c)
  "FIXME-DOCS"
  (if %fancy
      ;; Replace control codes with control pictures
      (string (integer->char (+ #x2400 (char->integer c))))
      (list-ref '("<NUL>" "<SOH>" "<STX>" "<ETX>" "<EOT>" "<ENQ>"
		  "<ACK>" "<BEL>" "<BS>"  "<HT>"  "<LF>"
		  "<VT>" "<FF>" "<CR>" "<SO>" "<SI>"
		  "<DLE>" "<DC1>" "<DC2>" "<DC3>" "<DC4>"
		  "<NAK>" "<SYN>" "<ETB>" "<CAN>" "<EM>"
		  "<SUB>" "<ESC>" "<FS>" "<GS>" "<RS>"
		  "<US>")
		(char->integer c))))

(define (lquo)
  "FIXME-DOCS"
  (if %fancy (string #\“) (string #\")))

(define (rquo)
  "FIXME-DOCS"
  (if %fancy (string #\”) (string #\")))

(define (BOL)
  "go to beginning of line"
  (if %fancy (string #\escape #\[ #\G) "\n"))

#|
in quiet mode it is just
☐ target -> parent (when building)
☒ target -> parent   (on pass)
⚠ target -> parent   (on fail but continue)
🛑 target -> parent  (on stop)
∎                    (on successful completion)

in normal mode it is
?  target -> parent
☐ recipe truncated to 70 cols, using C0 control pics
  etc
then
☒ target -> parent   (on pass)
|#

;; text.scm ends here
