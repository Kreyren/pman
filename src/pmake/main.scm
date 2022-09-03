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
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-module (make main)
  #:use-module (ice-9 getopt-long)
  #:export (main))

;;; Commentary:
;;;
;;; Backend for the `pman` command to process the command line arguments using the standardized 'getopts-long' as described in the GNU Guile reference manual <https://www.gnu.org/software/guile/manual/html_node/getopt_002dlong.html>.
;;;
;;; Code:

;; Declare exit codes
(define EXIT_SUCCESS 0)
(define EXIT_NOT_UP_TO_DATE 1)
(define EXIT_FAILURE 2)

(define option-spec
  '((environment-overrides (single-char #\e))
    (makefile (single-char #\f) (value #t))
    (ignore-errors (single-char #\i))
    (keep-going (single-char #\k))
    (dry-run (single-char #\n))
    (print-data-base (single-char #\p))
    (question (single-char #\q))
    (no-builtin-rules (single-char #\r))
    (stop (single-char #\S))
    (silent (single-char #\s))
    (touch (single-char #\t))))

(define (main args)
  (let ((options (getopt-long args option-spec)))
    (write options)
    (newline)
    EXIT_SUCCESS))

;;; make.scm ends here
