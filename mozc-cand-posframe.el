;;; mozc-cand-posframe.el --- Posframe frontend for mozc.el -*- lexical-binding: t -*-

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (posframe "0.5.0") (mozc "20180101.800") (s "1.12"))
;; Keywords: i18n, tooltip
;; URL: https://github.com/akirak/mozc-posframe

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;     * Neither the name of Google Inc. nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; To use this library, set `mozc-candidate-style' to "posframe".
;;
;;     (setq mozc-candidate-style 'posframe)

;;; Code:

(require 'mozc)
(require 'posframe)
(require 's)

(defgroup mozc-posframe nil
  "Posframe frontend for mozc.el."
  :group 'mozc
  :group 'posframe
  :prefix "mozc-cand-posframe-")

(defconst mozc-cand-posframe-buffer "*mozc cand posframe*")

(if (posframe-workable-p)
    (add-to-list 'mozc-candidate-dispatch-table
                 '(posframe (clean-up . mozc-cand-posframe-clean-up)
                            (clear . mozc-cand-posframe-clear)
                            (update . mozc-cand-posframe-update)))
  (error "Posframe won't run on this Emacs session"))

(defun mozc-cand-posframe-clean-up ()
  "Clean up the current-index candidate session."
  (mozc-cand-posframe-clear))

(defun mozc-cand-posframe-clear ()
  "Clear the candidate window."
  (posframe-delete mozc-cand-posframe-buffer))

(defface mozc-cand-posframe-normal-face
  '((t (:inherit default
                 :background "#dcd4be"
                 :foreground "#222222")))
  "Face for normal candidates and the entire child frame."
  :group 'mozc-posframe)

(defface mozc-cand-posframe-focused-face
  '((t (:inherit mozc-cand-posframe-normal-face
                 :background "#fff37a"
                 :foreground "#111111")))
  "Face for selected candidates."
  :group 'mozc-posframe)

(defface mozc-cand-posframe-footer-face
  '((t (:inherit mozc-cand-posframe-normal-face
                 :foreground "#203152")))
  "Face for extra information area in the child frame."
  :group 'mozc-posframe)

(cl-defstruct mozc-cand-posframe-candidate lstr rstr width)

(defcustom mozc-cand-posframe-separator " "
  "Separator between the left column and the right column."
  :type 'string)

(defun mozc-cand-posframe--make-item (candidate)
  "Make a struct from CANDIDATE."
  (let* ((value (mozc-protobuf-get candidate 'value))
         (desc (mozc-protobuf-get candidate 'annotation 'description))
         (shortcut (mozc-protobuf-get candidate 'annotation 'shortcut))
         (lstr (concat (when shortcut
                         (format "%s. " shortcut))
                       value))
         (rstr desc))
    (make-mozc-cand-posframe-candidate
     :lstr lstr :rstr rstr :width (string-width (if rstr
                                                    (concat lstr mozc-cand-posframe-separator rstr)
                                                  lstr)))))

(defun mozc-cand-posframe-update (candidates)
  "Update the candidate window using posframes.
CANDIDATES must be the candidates field in a response protobuf."
  (let* ((current-index (mozc-protobuf-get candidates 'focused-index))
         (total (mozc-protobuf-get candidates 'size))
         (index-visible (mozc-protobuf-get candidates 'footer 'index-visible))
         (source (mozc-protobuf-get candidates 'candidate))
         candidate
         (before-current (let (result)
                           (when current-index
                             (while (and (setq candidate (pop source))
                                         (< (mozc-protobuf-get candidate 'index) current-index))
                               (push (mozc-cand-posframe--make-item candidate) result)))
                           (nreverse result)))
         (current (when (and current-index
                             candidate
                             (= (mozc-protobuf-get candidate 'index) current-index))
                    (mozc-cand-posframe--make-item candidate)))
         (after-current (mapcar #'mozc-cand-posframe--make-item source))
         (x-pixel-offset (- (car (window-text-pixel-size nil
                                                         (overlay-start mozc-preedit-overlay)
                                                         (overlay-end mozc-preedit-overlay)))))
         (posframe-width (apply #'max (mapcar #'mozc-cand-posframe-candidate-width
                                              (append before-current
                                                      (when current
                                                        (list current))
                                                      after-current))))
         (modeline (when (and index-visible current-index total
                              (> total 1))
                     (s-pad-left posframe-width " "
                                 (format "%d/%d" (1+ current-index) total)))))
    (with-current-buffer (get-buffer-create mozc-cand-posframe-buffer)
      (goto-char (point-min))
      (cl-labels ((format-candidate (cand)
                                    (concat (mozc-cand-posframe-candidate-lstr cand)
                                            (if (mozc-cand-posframe-candidate-rstr cand)
                                                (concat mozc-cand-posframe-separator
                                                        (make-string (- posframe-width
                                                                        (mozc-cand-posframe-candidate-width cand))
                                                                     ?\s)
                                                        (mozc-cand-posframe-candidate-rstr cand))
                                              "")
                                            "\n"))
                  (put-face-overlay (begin end face)
                                    (overlay-put (make-overlay begin end) 'face face)))
        (when before-current
          (insert (mapconcat #'format-candidate before-current "")))
        (put-face-overlay (point-min) (point) 'mozc-cand-posframe-normal-face)
        (when current
          (let ((begin (point)))
            (insert (format-candidate current))
            (put-face-overlay begin (point) 'mozc-cand-posframe-focused-face)))
        (when after-current
          (let ((begin (point)))
            (insert (mapconcat #'format-candidate after-current ""))
            (put-face-overlay begin (point) 'mozc-cand-posframe-normal-face))))
      (delete-region (point) (point-max))
      (when modeline
        (setq-local mode-line-format
                    (list (propertize modeline 'face 'mozc-cand-posframe-footer-face)))))
    (posframe-show mozc-cand-posframe-buffer
                   :foreground-color (face-foreground 'mozc-cand-posframe-normal-face nil t)
                   :background-color (face-background 'mozc-cand-posframe-normal-face nil t)
                   :poshandler 'posframe-poshandler-point-bottom-left-corner
                   :respect-mode-line (not (null modeline))
                   :x-pixel-offset x-pixel-offset)))

(provide 'mozc-cand-posframe)
;;; mozc-cand-posframe.el ends here
