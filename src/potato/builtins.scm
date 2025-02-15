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


(define-module (potato builtins)
  #:export (builtin-makevars))

;;; Commentary:
;;;
;;; Dedicated to the declaration of built-in functionality such as variables, rules, tasks, etc..
;;;
;;; Code:

(define builtin-makevars
  '(("MAKE" . "make")
    ("AR"   . "ar")
    ("ARFLAGS" . "-rv")
    ("YACC" . "yacc")
    ("YFLAGS" . "")
    ("LEX" . "lex")
    ("LFLAGS" . "")
    ("LDFLAGS" . "")
    ("CC" . "cc")
    ("CFLAGS" . "-g -O2")
    ("FC" . "gfortran")
    ("FFLAGS" . "-O1")
    ("GUILE" . "guile")
    ("GUILD" . "guild")
    ("GFLAGS" . "-W2")))

;;; builtins.scm ends here
