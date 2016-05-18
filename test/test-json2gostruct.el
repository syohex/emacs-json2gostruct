;;; test-json2gostruct.el --- test for json2gostruct

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'json2gostruct)

(ert-deftest vector-type ()
  "Detecting vector type"
  (should (eq (json2gostruct--vector-type [1 2 3]) 'int))
  (should (eq (json2gostruct--vector-type [1.1 2.2 3.3]) 'float64))
  (should (eq (json2gostruct--vector-type [t t t t t]) 'boolean))
  (should (eq (json2gostruct--vector-type [nil nil nil]) 'boolean))
  (should (eq (json2gostruct--vector-type ["a" "b"]) 'string))
  ;; fallback type
  (should (eq (json2gostruct--vector-type [1 t 3]) 'string)))

(ert-deftest encode ()
  "Encoding json to go struct"
  (let ((input "{
\"a\": [1,2,3],
\"b\": [{\"name\": \"tom\", \"age\": 10}]
}")
        (expect "type Test struct {
\ta []int `json:\"a\"`
\tb []struct {
\t\tname string `json:\"name\"`
\t\tage int `json:\"age\"`
\t} `json:\"b\"`
}
"))
    (let ((got (json2gostruct--encode (json-read-from-string input) "Test" 0)))
      (should (string= got expect)))))

;;; test-json2gostruct.el ends here
