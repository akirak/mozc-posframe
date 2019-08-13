;;; mozc-cand-posframe.el --- Posframe frontend for mozc.el -*- lexical-binding: t -*-

;; Keywords: mule, multilingual, input method, tooltip

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (posframe "0.5.0") (mozc "0"))
;; Keywords: mule, multilingual, input method
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
  (error "posframe won't run on this Emacs session."))

(defun mozc-cand-posframe-clean-up ()
  "Clean up the current candidate session."
  (mozc-cand-posframe-clear))

(defun mozc-cand-posframe-clear ()
  "Clear the candidate window."
  (posframe-delete mozc-cand-posframe-buffer))

(defface mozc-cand-posframe-normal-face
  '((t (:inherit popup-face)))
  "Face for normal candidates and the entire child frame."
  :group 'mozc-posframe)

(defface mozc-cand-posframe-focused-face
  '((t (:inherit popup-tip-face)))
  "Face for selected candidates."
  :group 'mozc-posframe)

(defface mozc-cand-posframe-footer-face
  '((t (:inherit popup-summary-face)))
  "Face for extra information area in the child frame."
  :group 'mozc-posframe)

(defun mozc-cand-posframe-update (candidates)
  "Update the candidate window using posframes.
CANDIDATES must be the candidates field in a response protobuf."
  (let* ((focused-index (mozc-protobuf-get candidates 'focused-index))
         (size (mozc-protobuf-get candidates 'size))
         (sep "  ")
         (index-visible (mozc-protobuf-get candidates 'footer 'index-visible))
         focused-line
         (candidates (cl-loop for candidate being the elements of (mozc-protobuf-get candidates 'candidate)
                              using (index linenum)
                              collect (let* ((index (mozc-protobuf-get candidate 'index))
                                             (value (mozc-protobuf-get candidate 'value))
                                             (desc (mozc-protobuf-get candidate 'annotation 'description))
                                             (shortcut (mozc-protobuf-get candidate 'annotation 'shortcut))
                                             (lstr (concat (when shortcut
                                                             (format "%s. " shortcut))
                                                           value))
                                             (rstr (or desc "")))
                                        (when (eq index focused-index)
                                          (setq focused-line linenum))
                                        (list lstr
                                              rstr
                                              (string-width (concat lstr sep rstr))))))
         (x-pixel-offset (- (car (window-text-pixel-size nil
                                                         (overlay-start mozc-preedit-overlay)
                                                         (overlay-end mozc-preedit-overlay)))))
         (has-stat (and index-visible focused-index size))
         (posframe-width (apply #'max (mapcar #'caddr candidates)))
         (posframe-height (+ (length candidates)
                             (if has-stat 1 0)))
         (abovep (> (+ posframe-height (mozc-posn-y mozc-preedit-posn-origin))
                    (window-height))))
    (with-current-buffer (get-buffer-create mozc-cand-posframe-buffer)
      (goto-char (point-min))
      (insert (mapconcat (lambda (cand)
                           (concat (car cand)
                                   sep
                                   (make-string (- posframe-width
                                                   (caddr cand))
                                                ?\s)
                                   (cadr cand)))
                         candidates
                         "\n")
              (if has-stat
                  (let ((s (format "%d/%d" (1+ focused-index) size)))
                    (concat "\n"
                            (make-string (- posframe-width
                                            (string-width s))
                                         ?\s)
                            s))
                ""))
      (delete-region (point) (point-max))
      (remove-overlays)
      (when focused-line
        (goto-char (point-min))
        (forward-line focused-line)
        (let ((beg (point))
              (end (progn
                     (forward-line)
                     (1- (point)))))
          (overlay-put (make-overlay beg end) 'face 'mozc-cand-posframe-focused-face)))
      (when has-stat
        (goto-char (point-max))
        (let ((end (point))
              (beg (progn
                     (beginning-of-line)
                     (point))))
          (overlay-put (make-overlay beg end) 'face 'mozc-cand-posframe-footer-face))))
    (posframe-show mozc-cand-posframe-buffer
                   :foreground-color (face-foreground 'mozc-cand-posframe-normal-face nil t)
                   :background-color (face-background 'mozc-cand-posframe-normal-face nil t)
                   :poshandler
                   (if abovep
                       'posframe-poshandler-point-bottom-left-corner
                     'posframe-poshandler-point-top-left-corner)
                   :x-pixel-offset x-pixel-offset
                   ;; :y-pixel-offset (frame-char-height)
                   :width posframe-width
                   :height posframe-height)))

(provide 'mozc-cand-posframe)
;;; mozc-cand-posframe.el ends here
