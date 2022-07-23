;;; tamil99.el --- Tamil99 keyboard layout           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Visuwesh

;; Author: Visuwesh <visuweshm@gmail.com>
;; Keywords: i18n, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Emacs port of the Tamil99 keyboard layout.
;; For a description of the layout, visit http://www.tamilvu.org/ta/tkbd-index-341488
;; (direct link http://www.tamilvu.org/tkbd/Tamil_Unicode_G.O.zip).

;;; Code:
(require 'quail)

(defvar tamil99-translation-rules
  '(("q" . "ஆ") ("w" . "ஈ") ("e" . "ஊ") ("r" . "ஐ") ("t" . "ஏ") ("y" . "ள") ("u" . "ற") ("i" . "ன") ("o" . "ட") ("p" . "ண") ("[" . "ச") ("]" . "ஞ")
    ("Q" . "ஸ") ("W" . "ஷ") ("E" . "ஜ") ("R" . "ஹ") ("T" . ["க்ஷ"]) ("Y" . ["ஸ்ரீ"]) ("U" . "ஶ") ("I" . "ன") ("O" . "[") ("P" . "]")
    ("a" . "அ") ("s" . "இ") ("d" . "உ") ("f" . "்") ("g" . "எ") ("h" . "க") ("j" . "ப") ("k" . "ம") ("l" . "த") (";" . "ந") ("'" . "ய")
    ("A" . "௹") ("S" . "௺") ("D" . "௸") ("F" . "ஃ") ("G" . "எ") ("H" . "க") ("J" . "ப") ("K" . "\"") ("L" . ":") (":" . ";") ("\"" . "'")
    ("z" . "ஔ") ("x" . "ஓ") ("c" . "ஒ") ("v" . "வ") ("b" . "ங") ("n" . "ல") ("m" . "ர") ("/" . "ழ")
    ("Z" . "௳") ("X" . "௴") ("C" . "௵") ("V" . "௶") ("B" . "௷") ("N" . "ௐ") ("M" . "/")))

;; Separate to speed `assoc-default' calls in the translation func.
(defvar tamil99-extra-translations
  '(("^2" . "½") ("^3" . "¾") ("^4" . "¼") ("^7" . "‘") ("^8" . "’")
    ("^9" . "“") ("^0" . "”") ("^." . "•") ("^C" . "©") ("^S" . " ") ;; NBSP.
    ("^^" . "^") ("^#" . "₹")
    ("^f" . "்") ("^q" . "ா") ("^s" . "ி") ("^w" . "ீ") ("^d" . "ு")
    ("^e" . "ூ") ("^g" . "ெ") ("^t" . "ே") ("^r" . "ை") ("^c" . "ொ")
    ("^x" . "ோ") ("^z" . "ௌ")
    ("^#0#" . "௦") ("^#1#" . "௧") ("^#2#" . "௨") ("^#3#" . "௩") ("^#4#" . "௪")
    ("^#5#" . "௫") ("^#6#" . "௬") ("^#7#" . "௭") ("^#8#" . "௮") ("^#9#" . "௯")
    ("^#10#" . "௰") ("^#100#" . "௱") ("^#1000#" . "௲")
    ;; TODO: Is 1/16, 3/16, 3/80 correct?
    ("^#18#" . "𑿌") ("^#14#" . "𑿐") ("^#12#" . "𑿑") ("^#34#" . "𑿓")
    ("^#132#" . "𑿅") ("^#116#" . "𑿊") ("^#316#" . "𑿎") ("^#140#" . "𑿄")
    ("^#120#" . "𑿈") ("^#110#" . "𑿋") ("^#320#" . "𑿍") ("^#15#" . "𑿏")
    ("^#1320#" . "𑿀") ("^#1160#" . "𑿁") ("^#180#" . "𑿂") ("^#380#" . "𑿆")))

(defun tamil99--lookup-translation (key)
  (let ((trans (assoc-default key tamil99-translation-rules)))
    (if (vectorp trans)
        (aref trans 0)
      trans)))

(defvar tamil99-vowel-signs
  '(("அ" . nil) ("ஆ" . ?ா) ("இ" . ?ி) ("ஈ" . ?ீ)
    ("உ" . ?ு) ("ஊ" . ?ூ) ("எ" . ?ெ) ("ஏ" . ?ே)
    ("ஐ" . ?ை) ("ஒ" . ?ொ) ("ஓ" . ?ோ) ("ஔ" . ?ௌ)))

(defsubst tamil99-vowel-keyp (key)
  (member key '("q" "w" "e" "r" "t" "a" "s" "d" "g" "g" "G" "z" "x" "c")))

(defsubst tamil99-consonant-keyp (key)
  (member key '("y" "u" "i" "o" "p" "[" "]" "U" "I" "Q" "W" "E" "R" "T" "h"
                "j" "k" "l" ";" "'" "H" "J" "v" "b" "n" "m" "/" "N")))

(defsubst tamil99-consonantp ()
  ;; Consonants in Tamil unicode block is between KA and HA.
  (<= ?க (char-before (point)) ?ஹ))

(defsubst tamil99-vowel-sign (key)
  (string (assoc-default (tamil99--lookup-translation key) tamil99-vowel-signs)))

(defvar tamil99-hard-soft-pairs
  '(("க" . ?ங) ("ச" . ?ஞ) ("த" . ?ந) ("ட" . ?ண) ("ப" . ?ம) ("ற" . ?ன)))

(defsubst tamil99-soft-hard-pairp (key)
  (eq (assoc-default key tamil99-hard-soft-pairs)
      (char-before (point))))

(defvar-local tamil99--delink-flag nil)
(put 'tamil99--delink-flag 'permanent-local t)

;; The control flag FLAG may be a number, nil or t.
;; If a number, then that length of quail-current-key is to be
;; translated.
;; If nil, then translation is not complete, and should resume.
;; If t, then translation is complete.
;; quail-current-key is the string to be translated, quail-current-str
;; is the final translation.
(defun tamil99-update-translation (flag)
  ;; TODO: Do we need to handle f specially as well?  I.e., if the
  ;; previous character is not a consonant, should it insert ஃ (F)?
  (cond
   ((eq flag t)
    (let ((key quail-current-key))
      (cond
       ((and (equal key "W")
             (and (eq (char-before (point)) ?்)
                  (eq (char-before (1- (point))) ?க)))
        ;; We need a ZWNJ if the previous character is க், otherwise ஷ (W)
        ;; combines with it to produce க்ஷ (T).
        (setq quail-current-str (string #x200c quail-current-str) ; 200c = ZWNJ
              tamil99--delink-flag nil))
       ((and (tamil99-vowel-keyp key) (tamil99-consonantp))
        ;; Check if delink flag needs to be set.
        ;; TODO: Ask Srinivasan if it is possible to insert அ after a consonant.
        (if (equal key "a")
            (setq tamil99--delink-flag t
                  quail-current-str "")
          (if (null tamil99--delink-flag)
              ;; If no delink flag is set, then insert the vowel sign.
              (setq quail-current-str (tamil99-vowel-sign key))
            ;; Reset the flag.
            (setq tamil99--delink-flag nil))))
       ((tamil99-consonant-keyp key)
        ;; If delink flag is set, then don't do any fancy pants stuff.
        (if tamil99--delink-flag
            (setq tamil99--delink-flag nil)
          ;; If a hard-soft consonant pair is encountered or if the
          ;; previous consonant is same as the current consonant, then
          ;; add a pulli.
          (when (or (tamil99-soft-hard-pairp key)
                    ;; FIXME: This naive check will definitely fail
                    ;; when there's a க்ஷ before and we are inserting
                    ;; ஷ.
                    (equal (string (char-before (point)))
                           (tamil99--lookup-translation key)))
            (setq quail-current-str (concat "்" (if (characterp quail-current-str)
                                                   (string quail-current-str)
                                                 quail-current-str))
                  tamil99--delink-flag t))))
       (t (setq tamil99--delink-flag nil)))))
   ;; Copy-pasted from `quail-update-translation'.
   ((null flag)
    (unless quail-current-str
      (setq quail-current-str
	    (if (quail-kbd-translate)
		(quail-keyseq-translate quail-current-key)
	      quail-current-key))
      (if (and input-method-exit-on-first-char
	       (quail-simple))
	  (setq flag t))))
   ((numberp flag)
    (let ((len (length quail-current-key)))
      (if (= flag 0)
          (setq quail-current-str
	        (if (quail-kbd-translate)
		    (quail-keyseq-translate quail-current-key)
	          quail-current-key)))
      (or input-method-exit-on-first-char
          (while (> len flag)
	    (setq len (1- len))
	    (quail-add-unread-command-events
	     (aref quail-current-key len)))))))
  flag)

(quail-define-package "tamil99" "Tamil" "t99" t
  "Tamil99 keyboard layout."
  nil nil t t t nil nil nil #'tamil99-update-translation nil t)

(progn
  ;; Clear translation rules.  Placeholder for now whilst testing.
  (let ((quail-current-package (assoc "tamil99" quail-package-alist)))
    (setf (nth 2 quail-current-package) '(nil)))
  (dolist (r (append tamil99-translation-rules
                     tamil99-extra-translations))
    (quail-defrule (car r) (cdr r) "tamil99")))

(defun tamil99--clear-delink-flag ()
  (when (equal current-input-method "tamil99")
    (setq tamil99--delink-flag nil)))

(add-hook 'input-method-activate-hook #'tamil99--clear-delink-flag)

(provide 'tamil99)
;;; tamil99.el ends here
