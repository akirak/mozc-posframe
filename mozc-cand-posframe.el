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

(require 'cl-lib)
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

(defcustom mozc-cand-posframe-y-pixel-offset 0
  "Vertical offset for the posframe."
  :type 'integer
  :group 'mozc-posframe)

(defun mozc-cand-posframe-clean-up ()
  "Clean up the current-index candidate session."
  (mozc-cand-posframe-clear))

(defun mozc-cand-posframe-clear ()
  "Clear the candidate window."
  (posframe-delete mozc-cand-posframe-buffer))

(defface mozc-cand-posframe-normal-face
  '((((class color) (min-colors 88) (background dark)) :background "#191a1b")
    (((class color) (min-colors 88) (background light)) :background "#f0f0f0")
    (t :background "gray"))
  "Face for normal candidates and the entire child frame.")

(defface mozc-cand-posframe-focused-face
  '((((class color) (min-colors 88) (background dark))
     :background "#00415e" :foreground "white")
    (((class color) (min-colors 88) (background light))
     :background "#c0efff" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face for selected candidates.")

(defface mozc-cand-posframe-footer-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "#00415e" :background "#191a1b")
    (((class color) (min-colors 88) (background light))
     :foreground "#c0efff" :background "#f0f0f0")
    (t :background "gray"))
  "Face for extra information area in the child frame.")

(defface mozc-cand-posframe-annotations-face
  '((t (:inherit completions-annotations)))
  "Face for candidate annotations.")

(defface mozc-cand-posframe-border-face
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "Face used for the child frame border.")

(cl-defstruct mozc-cand-posframe-candidate lstr rstr width)

(defcustom mozc-cand-posframe-min-width 20
  "Minimun width of the child frame."
  :type 'integer
  :group 'mozc-posframe)

(defcustom mozc-cand-posframe--width-padding 4
  "Width padding."
  :type 'integer
  :group 'mozc-posframe)

(defcustom mozc-cand-posframe-mode-line nil
  "If non-nil, display mode-line."
  :type 'boolean
  :group 'mozc-posframe)

(defcustom mozc-cand-posframe-border-width 1
  "Border width."
  :type 'integer
  :group 'mozc-posframe)

(cl-defun mozc-cand-posframe--frame-width (width min-width &key padding)
  "Return frame width.
If WIDTH >= MIN-WIDTH, return WIDTH + PADDING
IF MIN-WIDTH > WIDTH, return MIN-WIDTH + PADDING"
  (let ((pad (or padding mozc-cand-posframe--width-padding)))
    (cond
     ((>= width min-width) (+ width pad))
     (t  (+ min-width pad)))))

(cl-defun mozc-cand-posframe-draw (cand &key cand-max-width face)
  "Format posframe CAND entry.

CAND-MAX-WIDTH: length of the longest candidate
FACE: candidate entry face"
  (let* ((posframe-width (mozc-cand-posframe--frame-width cand-max-width
                                                          mozc-cand-posframe-min-width))
         (lstr (mozc-cand-posframe-candidate-lstr cand))
         (rstr (mozc-cand-posframe-candidate-rstr cand))
         (lstr-width (string-width lstr)))
    (let ((begin nil))
      (setq begin (point))
      (insert " ")
      (insert lstr)
      (overlay-put (make-overlay begin (point)) 'face face)
      (setq begin (point))
      (insert (s-pad-left (- posframe-width lstr-width)
                          " "
                          ""))
      (overlay-put (make-overlay begin (point-at-eol))
                   'face `(nil :inherit mozc-cand-posframe-annotations-face
                               :background ,(face-attribute face :background))))
    (insert "\n")))

(cl-defun mozc-cand-posframe-draw-info (index total &key cand-max-width)
  "Format `mozc-cand-posframe` mode-line.

INDEX: current candidate index
TOTAL: candidates total
CAND-MAX-WIDTH: length of the longest candidate"
  (let* ((posframe-width (mozc-cand-posframe--frame-width cand-max-width
                                                          mozc-cand-posframe-min-width)))
    (s-pad-right posframe-width " "
                 (format " %d/%d" index total))))

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
     :lstr lstr
     :rstr rstr
     :width (string-width lstr))))

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
         (x-pixel-offset (+ (- (car (window-text-pixel-size nil
                                                            (overlay-start mozc-preedit-overlay)
                                                            (overlay-end mozc-preedit-overlay))))
                            (line-number-display-width t)))
         (cand-max-width (apply #'max (mapcar #'mozc-cand-posframe-candidate-width
                                              (append before-current
                                                      (when current
                                                        (list current))
                                                      after-current))))
         (modeline (when (and index-visible current-index total
                              (> total 1) mozc-cand-posframe-mode-line)
                     (mozc-cand-posframe-mode-line (1+ current-index) total :cand-max-width cand-max-width))))
    (with-current-buffer (get-buffer-create mozc-cand-posframe-buffer)
      (goto-char (point-min))
      (cl-labels ((draw-default (cand)
                    (mozc-cand-posframe-draw cand
                                             :cand-max-width cand-max-width
                                             :face 'mozc-cand-posframe-normal-face))
                  (draw-focused (cand)
                    (mozc-cand-posframe-draw cand
                                             :cand-max-width cand-max-width
                                             :face 'mozc-cand-posframe-focused-face)))
        (when before-current
          (cl-map 'list #'draw-default before-current))
        (when current
          (draw-focused current))
        (when after-current
          (cl-map 'list #'draw-default after-current)))
      (delete-region (point) (point-max))
      (when modeline
        (setq-local mode-line-format
                    (list (propertize modeline 'face `(:inherit mozc-cand-posframe-footer-face
                                                                :box nil))))))
    (posframe-show mozc-cand-posframe-buffer
                   :foreground-color (face-foreground 'mozc-cand-posframe-normal-face nil t)
                   :background-color (face-background 'mozc-cand-posframe-normal-face nil t)
                   :poshandler 'posframe-poshandler-point-bottom-left-corner
                   :lines-truncate t
                   :border-width mozc-cand-posframe-border-width
                   :border-color (face-background 'mozc-cand-posframe-border-face nil t)
                   :max-width (mozc-cand-posframe--frame-width cand-max-width
                                                               mozc-cand-posframe-min-width
                                                               :padding (- mozc-cand-posframe--width-padding 2))
                   :respect-mode-line (not (null modeline))
                   :x-pixel-offset x-pixel-offset
                   :y-pixel-offset mozc-cand-posframe-y-pixel-offset)))

(provide 'mozc-cand-posframe)
;;; mozc-cand-posframe.el ends here
