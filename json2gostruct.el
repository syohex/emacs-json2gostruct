;;; json2gostruct.el --- Convert JSON to go struct definition -*- lexical-binding: t -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/
;; Version: 0.01
;; Package-Requires: ((emacs "24.3"))

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

;;; Commentary:

;; Convert JSON to golang struct definition

;;; Code:

(require 'json)
(require 'cl-lib)

(defun json2gostruct--vector-type (v)
  (if (zerop (length v))
      'string
    (let ((first-type (type-of (aref v 0))))
      (or (and (cl-every (lambda (a) (eq (type-of a) first-type)) v)
               first-type)
          'string))))

(defun json2gostruct--encode-element (json name nest)
  (cl-loop with ret = nil
           with indent = (cl-loop repeat nest
                                  concat "\t")
           for (key . val) in json
           do
           (let ((varname (concat indent (symbol-name key) " "))
                 (type (cl-typecase val
                         (string "string")
                         (integer "int")
                         (float "float64")
                         (vector (let ((type (json2gostruct--vector-type val)))
                                   (format "[]%s"
                                           (if (eq type 'cons)
                                               (json2gostruct--encode (aref val 0) name nest)
                                             (symbol-name type)))))
                         (symbol (if (or (eq val t) (not val))
                                     "boolean"
                                   (error "Invalid input %s" val)))
                         (cons (json2gostruct--encode val name nest)))))
             (let ((tag (format " `json:\"%s\"`" key)))
               (push (concat varname type tag) ret)))
           finally return ret))

(defun json2gostruct--encode (json name nest)
  (unless (listp json)
    (error "Support only object type"))
  (let ((top-level-p (zerop nest))
        (indent (cl-loop repeat nest
                         concat "\t"))
        ret)
    (push (if top-level-p
              (format "type %s struct {" name)
            "struct {") ret)
    (setq ret (append (json2gostruct--encode-element json name (1+ nest)) ret))
    (push (if top-level-p "}\n" (concat indent "}")) ret)
    (mapconcat #'identity (reverse ret) "\n")))

;;;###autoload
(defun json2gostruct (struct-name)
  (interactive
   (list (read-string "Struct name: ")))
  (let* ((beg (or (and (use-region-p) (region-beginning)) (point-min)))
         (end (or (and (use-region-p) (region-end)) (point-max)))
         (json (json-read-from-string (buffer-substring-no-properties beg end))))
    (let ((go-struct (json2gostruct--encode json struct-name 0)))
      (with-current-buffer (get-buffer-create "*json2gostruct*")
        (read-only-mode -1)
        (erase-buffer)
        (insert go-struct)
        (read-only-mode +1)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(provide 'json2gostruct)

;;; json2gostruct.el ends here
