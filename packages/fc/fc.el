;;; fc.el --- a flashcard wrapper to make it more friendly.

;; Copyright (C) 2006  Damien Elmes <emacs AT ichi2.net>

;; Author: Damien Elmes <emacs AT ichi2.net>
;; Keywords: flashcard, japanese

;; fc is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; fc is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fc; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains a bunch of convenience routines for flashcard
;; and flashcard-sm5 to make the user interface friendlier. It
;; includes English and Japanese user interfaces, but can be used to
;; study anything.

;; See http://ichi2.net/flashcard for a screenshot.

;; Terminology:
;; C-g                 Control+g
;; M-x                 Alt+x
;; M-x foo bar         Alt+x, foo [Enter] bar

;; Installation
;; -------------------------------------------------------------------

;; Win32: simple answer, download win32 binaries from
;; http://ichi2.net/flashcard/. Long answer, download meadow 3.0 and
;; follow the instructions below.

;; Unix/Mac: emacs22+ is recommended. It's possible to
;; use flashcard in emacs21.4 (the stable version available in most
;; cases), but you need to install mule-ucs and make sure it's
;; enabled. On Debian, it's not enabled by default, but you can enable
;; it by doing 'export DEB_MULEUCS_UNICODE=on' in a shell before
;; starting emacs. If you get messages about unsafe encodings and
;; being unable to convert to utf-8, it's because you haven't got
;; mule-ucs installed properly. In debian, you're better off with the
;; 'emacs-snapshot' package instead.

;; Next, download the following two files, and along with this file,
;; place them in a directory somewhere - maybe c:/meadow on Windows,
;; ~/jp on unix

;; * http://ichi2.net/emacs/flashcard/flashcard.el
;; * http://ichi2.net/flashcard/flashcard-sm5.el

;; Now create an empty .emacs in c:/meadow/_emacs or ~/.emacs

;; For Meadow, at the bottom of the file, add these lines, and make
;; sure to remove the leading semi-colons.

;; (setq fc-base "c:/meadow")
;; (setq fc-default-lang 'en)
;; (add-to-list 'load-path "c:/meadow")
;; (load "c:/meadow/fc")

;; For unix, add the following lines

;; (setq fc-base "~/jp")
;; (setq fc-default-lang 'en)
;; (add-to-list 'load-path "~/jp")
;; (load "~/jp/fc")

;; If you want to see the Japanese user interface instead of English,
;; change `en' to `jp'.

;; After you start emacs again, the following files will (eventually)
;; be created:

;; * tango.deck:     The main file which flashcard uses
;; * tango.pending:  Stores entries which haven't been imported yet.
;;                   This is easy to hand-edit.
;; * tango.imported: Stores entries which have been imported, for
;;                   future reference. This file has been largely
;;                   obsoleted by the deck editor, but it serves as a
;;                   rudimentary backup.

;; Usage
;; -------------------------------------------------------------------

;; fc is configured for studying Japanese by default. When you hit
;; [F7], it prompts you for a kanji compound, the hiragana reading,
;; and an English description. If you want to use fc to study
;; something other than Japanese, try and create your own version of
;; `fc-add-entry'.

;; [F7] Adds a new word to tango.pending. If you make a mistake in the
;; entry you can hit C-g to abort, or you can edit tango.pending
;; before you start a quiz, to change your entry.

;; [F9] Starts the quiz (automatically importing any pending words).

;; When you want to stop the quiz, just close the window. Saving
;; should be automatic. You may be prompted to save the deck - just
;; say yes.

;; When the quiz begins, you'll see a question. You can type the
;; answer if you want, but you don't have to - I just say it aloud.
;; When you're ready to see the answer, hit enter. You'll be shown the
;; answer and prompted to enter a number from 0-5, indicating how well
;; you remembered. The numbers are as follows:

;; 0: didn't remember at all
;; 1-2: remembered a little/half remembered
;; 3: almost remembered (made a mistake, but was close)
;; 4: remembered (maybe after thinking for a while)
;; 5: remembered easily

;; Customizing colours and fonts
;; -------------------------------------------------------------------

;; To make the question & answer faces bigger, put the following lines
;; in _emacs/.emacs:

;; (set-face-attribute 'flashcard-question-face nil :height 1.5)
;; (set-face-attribute 'flashcard-answer-face nil :height 1.5)

;; You can change the number to make the font bigger or smaller.

;; The default colours and fonts are pretty difficult to see,
;; especially under Meadow. To change the foreground/background
;; colour, type: 'M-x customize-face default'. Select a different
;; foreground and background and 'save for future sessions'.

;; To change the colours of the question and answer face in the same
;; way. Type 'M-x customize-face flashcard-question-face' or 'M-x
;; customize-face flashcard-answer-face'. You can change the height
;; (try 1.5), and the colours (HTML colour codes like "#cca" work).

;; Automatically starting the quiz
;; -------------------------------------------------------------------

;; If you don't use emacs/meadow for anything else, you can get it to
;; start quizzing automatically when you start it up. Add the
;; following lines to the bottom of _emacs or .emacs:

;; (setq inhibit-startup-message t)
;; (find-file "/path/to/tango.deck")

;; JLPT word lists
;; -------------------------------------------------------------------

;; I've generated word lists for the four JLPT levels - you can
;; download them from http://ichi2.net/flashcard/

;; Note that I really recommend entering your own words, as words
;; learnt in context tend to be remembered a lot easier.

;; To use the word lists, unzip the files, place them somewhere, then
;; type C-x C-f /path/to/one-file, instead of hitting [F9].

;; Comments/questions/problems
;; -------------------------------------------------------------------

;; The fastest way to reach me is as `resolve', on
;; irc.openprojects.net, channel #nihongo. You can also email me if
;; you'd like.

(require 'flashcard)
(require 'flashcard-sm5)
(require 'time-date)

(defvar fc-show-header-line t
  "*Show header line?")

(defvar fc-english-mode nil
  "*Make it easier for English learners?")

(defvar fc-default-lang 'en
  "*The default language to use.
Currently `en' and `jp' are supported.")

(defun fc-input-english ()
  "Turn on English input mode.
Redefine this if you want.")

(defun fc-input-japanese ()
  "Turn on Japanese input mode.
Redefine this if you want.")

(defface fc-answered-face '((t (:foreground "#ccccff"
                                :height 1.5)))
  "The face to show the previous answer in.")

(defvar fc-font-sizes '(1.5 . 3.0)
  "If English is found, display in the first size.
If no English is found, display in the second.")

(defvar fc-quality-end-marker nil
  "Marker for the end of the quality advice.")

(global-set-key (kbd "<f9>") 'fc-import)
(global-set-key (kbd "<f8>") 'fc-edit)
(if fc-english-mode
    (global-set-key (kbd "<f7>") 'fc-add-entry-en)
  (global-set-key (kbd "<f7>") 'fc-add-entry))

;; tallying
(defvar fc-correct 0
  "Number of correct answers in a run.")
(defvar fc-incorrect 0
  "Number of incorrect answers in a run.")
(defvar fc-pending-num 0
  "Number of answers pending in a run.")
(make-variable-buffer-local 'fc-correct)
(make-variable-buffer-local 'fc-incorrect)

(add-to-list 'auto-mode-alist '("\\.deck\\'" . flashcard-mode))
(add-hook 'flashcard-mode-hook 'fc-reset-tallies)
(add-hook 'flashcard-mode-hook 'fc-show-header-line)
(add-hook 'flashcard-mode-hook 'flashcard-method-sm5)
(add-hook 'flashcard-positive-feedback-functions
          'fc-show-correct-tally)
(add-hook 'flashcard-pre-question-hook 'fc-pre-question)
(add-hook 'flashcard-post-question-hook 'fc-maybe-save)
(add-hook 'flashcard-post-question-hook 'fc-resize-question)
(add-hook 'flashcard-sm5-check-answer-hook 'fc-show-recall-help)
(add-hook 'flashcard-deck-finished-hook 'fc-deck-finished)

(setq flashcard-coding-system 'utf-8)
(setq fc-deck (concat fc-base "/tango.deck"))
(setq fc-pending (concat fc-base "/tango.pending"))
(setq fc-imported (concat fc-base "/tango.imported"))

(defun fc-ensure-flashcard-buffer ()
  (unless (eq major-mode 'flashcard-mode)
    (error "You're not in a flashcard buffer!")))

(defun fc-reset-tallies ()
  "Reset the tally counters."
  (setq fc-correct 0)
  (setq fc-incorrect 0)
  (setq fc-pending-num (fc-find-pending-number)))

(defun fc-maybe-save ()
  "Save the buffer after every 10 questions."
  (if fc-english-mode
      (save-buffer)
    (when (zerop (% (+ fc-incorrect
                       fc-correct) 10))
      (save-buffer))))

(defun fc-pre-question ()
  "Handle pre-question display."
  (fc-hide-recall-help)
  (goto-char (point-max))
  (if (fc-clear-old-questions)
      (progn
        (insert "----------------------------------")
        (insert "----------------------------------\n\n"))
    (insert "\n"))
  (insert fc-question-prompt)
  (set-marker flashcard-marker (point)))

(defun fc-clear-old-questions ()
  "Remove all but the last answer.
True if we removed anything."
  (save-excursion
    (let ((q (text-property-any (point-min) (point-max)
                                'face 'flashcard-question-face))
          (a (text-property-any (point-min) (point-max)
                                'face 'flashcard-answer-face))
          removed)
      (unless (or (not q) (not a))
        ;; kill space between question and answer.
        (goto-char (next-single-property-change q 'face))
        (unless (= (point) a)
          (delete-region (point) a))
        (backward-delete-char 1)
        (insert " = ")
        (set-text-properties (point-at-bol) (point-at-eol)
                             (list 'face 'fc-answered-face))
        ;; tidy up before question
        (delete-region (point-min) q)
        (setq removed t))
      (goto-char (point-min))
      (when fc-show-header-line
        (insert "\n"))
      removed)))

(defun fc-get-previous-answer ()
  (forward-word 1)
  (forward-char)
  (unless (looking-at "(")
    (when (re-search-forward "\\(.*\\) (" nil t)
      (match-string 1))))

(defun fc-change-font-size (beg end)
  "If we're looking at a kanji, make it big."
  (let* ((str (buffer-substring beg end))
         (size
          (if (or (fc-contains-english-p str)
                  fc-english-mode)
              (car fc-font-sizes)
            (cdr fc-font-sizes))))
    (set-face-attribute
     (get-text-property 0 'face str) nil
     :height size)))

(defun fc-resize-question ()
  "Resize the question."
  (save-excursion
    (forward-line -2)
    (fc-change-font-size (point) (line-end-position))))

(defun fc-contains-english-p (str)
  (not (null (string-match "[A-Za-z]" str))))

(defun fc-show-recall-help (card answer)
  "Add help about recall quality."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "The correct answer is:\n" nil t)
    (replace-match fc-answer-prompt)
    (fc-change-font-size (point) (line-end-position))
    (goto-char (point-max))
    ;; make sure the tabs below line up
    (setq tab-width 4)
    (insert
     (format fc-quality-prompt
             (fc-find-delay card 4) (fc-find-delay card 5)))
    (setq fc-quality-end-marker (point))
    (set-marker flashcard-marker (point-max))))

(defun fc-find-delay (card quality)
  "Number of days until next occurrance of card at QUALITY."
  (let* ((count (flashcard-card-get-note card 'sm-count))
         (ease (flashcard-card-get-note card 'sm-ease))
         (oldI (flashcard-card-get-note card 'sm-interval))
         (ease (cond
                ;; only modify ease for a score of 3 or better
                ((or (< quality 3) (>= 0 quality)) ease)
                ((eq 3 quality)
                 (- ease 1))
                ((eq 4 quality) ease)
                ((eq 5 quality)
                 (+ ease 1))
                (t
                 (error "Invalid quality response."))))
         (ease (if (< ease 0)
                   0
                 ease))
         (ease (if (> ease flashcard-sm5-matrix-size)
                   flashcard-sm5-matrix-size
                 ease)))
    (flashcard-sm5-get-PI count ease oldI)))

(defun fc-hide-recall-help ()
  "Remove the recall quality help.
Assumes the recall display message is 9 lines long."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward
           (nth 0 (if (>= emacs-major-version 22)
                      (split-string fc-quality-prompt "\n" t)
                    (split-string fc-quality-prompt "\n")))
           nil t)
      (forward-line -1)
      (let ((top (point)))
        (forward-line 9)
        (delete-region top (point))))))

(defun fc-show-correct-tally (grade)
  "Update the tallies and show them."
  (if (< grade 4)
      (setq fc-incorrect (1+ fc-incorrect))
    (setq fc-correct (1+ fc-correct))
    (setq fc-pending-num (1- fc-pending-num))
    (if (= fc-pending-num 0)
        ;; time has passed and new cards may have become available
        (setq fc-pending-num (fc-find-pending-number))))
  (save-excursion
    (let* ((total (+ fc-correct fc-incorrect))
           (perc-correct (* 100 (/ fc-correct (float total))))
           (perc-incorrect (* 100 (/ fc-incorrect (float total)))))
      ;; put the tally before the scheduling information
      (flashcard-insert
       (format
        fc-tally-display
        fc-correct
        perc-correct
        fc-incorrect
        perc-incorrect
        (length (flashcard-deck-cards flashcard-deck))
        fc-pending-num)))))

(defun fc-find-pending-number ()
  "Count the number of pending entries in the deck."
  (let ((cards (flashcard-deck-cards flashcard-deck))
        (pending 0))
    (while cards
      (when (time-less-p (flashcard-card-get-note
                          (car cards) 'sm-next-time) (current-time))
        (setq pending (1+ pending)))
      (setq cards (cdr cards)))
    pending))

(defun fc-deck-finished ()
  "Handle output when the deck's finished."
  (fc-clear-old-questions)
  (fc-hide-recall-help)
  (let ((fc-show-header-line t))
    (fc-show-header-line))
  (goto-char (point-min))
  (when (search-forward "The next card will be ready" nil t)
    (goto-char (point-at-bol))
    (when (> (point) 2)
      ;; insert a separator if there was a previous question
      (insert "\n----------------------------------")
      (insert "----------------------------------\n\n")))
  (save-buffer))

;; create two entries, which look like:
;; (kanji->kana+english) 潰す : つぶす - to smash/waste s/th
;; (english->kanji+kana) to smash/waste s/th : 潰す
(defun fc-add-entry ()
  "Add a word to `fc-deck'"
  (interactive)
  (save-excursion
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          kanji furigana desc)
      (find-file fc-pending)
      (goto-char (point-max))
      (fc-input-japanese)
      (setq kanji
            (read-string "漢字+送り仮名 (Kanji): " nil nil nil t))
      (setq furigana (fc-get-furigana kanji))
      (fc-input-english)
      (setq desc (read-string "Description: " nil nil nil t))
      (insert (format "%s : (%s) %s\n%s : %s (%s)\n"
                      kanji furigana desc
                      desc kanji furigana))
      (save-buffer)
      (message "Word added!"))))

(defun fc-get-furigana (kanji)
  "Get the furigana for KANJI.
If kakasi.el is installed, do it automatically.
Otherwise, prompt for it."
  (if (require 'kakasi nil t)
      (kakasi-command-hiragana kanji)
    (read-string "振り仮名 (Hiragana): " nil nil nil t)))

(defun fc-add-entry-en ()
  "Add a word to `fc-deck'"
  (interactive)
  (save-excursion
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          kanji furigana desc)
      (switch-to-buffer (get-buffer-create "*help*"))
      (delete-region (point-min) (point-max))
      (insert "
最近習った英単語や英語の表現を入力してください。

例: visit somewhere
例: go to somewhere

例: dye one's hair
例: he was soaked

覚えるため、一個の単語より文が良い。上記の例に「visit」と「go」しか入力
しないと「to」が必要かどうか忘れる可能性がある。

文字を入力すれば一番下の所に表示される。
もし間違えたら[ESC]を三回押して，その後[F7]を押してください。")
      (setq en (read-string "英語の単語や表現: " nil nil nil t))
      (delete-region (point-min) (point-max))
      (insert "
次は意味を入力してください。
もちろん、英語で意味を書ければ、英語で書いたほうが良い。

例: he was very very wet
例: 彼はびしょびしょだった。（私が台風の中で帰ったときみたいに）

経験したことを書いたほうが記憶に残る。

文字を入力すれば一番下の所に表示される。
もし間違えたら[ESC]を三回押して、その後[F7]を押してください。")
      (setq jp (read-string "意味　(和/英): " nil nil nil t))
      (find-file fc-pending)
      (goto-char (point-max))
      (insert (format "%s : %s\n%s : %s\n"
                      en jp jp en))
      (save-buffer)
      (kill-buffer (current-buffer))
      (fc-import)
      (message "Word added!"))))

(defun fc ()
  "Start quizzing with `fc-deck'"
  (interactive)
  (find-file fc-deck)
  (goto-char (point-min))
  ;; allow the user to quit easily at this point, as nothing's changed
  (set-buffer-modified-p nil)
  (goto-char flashcard-marker))

(defun fc-show-header-line ()
  "Show the header line if we're in a flashcard buffer."
  (if (eq major-mode 'flashcard-mode)
      (if fc-show-header-line
          (setq header-line-format
                fc-header-line-format)
        (setq header-line-format nil))))

(defun fc-import ()
  "Import words from `fc-pending' and start quizzing."
  (interactive)
  (fc)
  (let ((len (length (flashcard-deck-cards flashcard-deck)))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (when (file-exists-p fc-pending)
      (flashcard-import-from-colon-file flashcard-deck
                                        fc-pending))
    ;; new cards found?
    (unless (eq (length (flashcard-deck-cards flashcard-deck))
                len)
      ;; flashcard should be doing this, but for some reason it fails
      ;; to sometimes
      (set-buffer-modified-p t)
      (fc-move-pending)
      (flashcard-shuffle-deck)
      (flashcard-sm5-sort-pending)
      (save-buffer)
      (kill-buffer (current-buffer))
      (fc))))

;; move all entries from .pending to .imported
(defun fc-move-pending ()
  (save-excursion
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          text)
      (find-file fc-pending)
      (setq text (buffer-substring (point-min) (point-max)))
      (delete-region (point-min) (point-max))
      (save-buffer)
      (kill-buffer (current-buffer))
      (find-file fc-imported)
      (goto-char (point-max))
      (insert text)
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun fc-export-current-deck (file)
  (interactive
   (list (read-file-name "Export to file: " nil nil nil
                         "tango.exported")))

  (fc-ensure-flashcard-buffer)
  (let ((cards (flashcard-deck-cards flashcard-deck)))
    (with-temp-buffer
      (dolist (card cards)
        (insert (format "%s : %s\n" (flashcard-card-question card)
                        (flashcard-card-answer card))))
      (sort-lines nil (point-min) (point-max))
      (write-file file)))
  (message "Exported deck to %s" file))

;; protect against empty elements by redefining these flashcard
;; functions
(defun flashcard-card-question (card)
  "Return the question of CARD."
  (let ((res (aref card 1)))
    (if (string= res "")
        "[nothing entered]"
      (replace-regexp-in-string "^\\s-*$" "[nothing entered]" res))))

(defun flashcard-card-answer (card)
  "Return the question of CARD."
  (let ((res (aref card 2)))
    (if (string= res "")
        "[nothing entered]"
      (replace-regexp-in-string "^\\s-*$" "[nothing entered]" res))))

(defun fc-edit ()
  (interactive)
  (unless (eq major-mode 'flashcard-mode)
    (fc-import))
  (flashcard-edit-current-deck))

(defun fc-select-deck ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*help*"))
  (delete-region (point-min) (point-max))
  (insert "\nSelect deck to use:\n\n")
  (let ((list (directory-files fc-base t ".deck$"))
        (n 0))
    (setq list (sort list (lambda (x y) (not (funcall 'string< x y)))))
    (dolist (l list)
      (setq l (replace-regexp-in-string "tango.deck" "<personal deck>" l))
      (insert (format "%1d" n) ": " (file-name-nondirectory l) "\n")
      (setq n (1+ n)))
    (let ((char (read-char-exclusive "Enter deck number: ")))
      (find-file (nth (- char 48) list)))))

(defun fc-disable-keys-in-edit ()
  "Disable some keys while in the deck editor."
  (local-set-key (kbd "<f8>") 'ignore)
  (local-set-key (kbd "<f7>") 'ignore))

(add-hook 'flashcard-edit-mode-hook 'fc-disable-keys-in-edit)

(defvar fc-question-prompt nil)
(defvar fc-answer-prompt nil)
(defvar fc-quality-prompt nil)
(defvar fc-tally-display nil)
(defvar fc-header-line-format nil)

(defun fc-lang-jp ()
  (interactive)
  (setq fc-question-prompt
        "次の単語または表現を訳したら[Enter]を押してください。\n\n")
  (setq fc-answer-prompt "正解は ")
  (setq fc-quality-prompt
        "\nどのぐらい覚えていましたか。下から数字を選んでください。

0: 全然覚えてない
1: 少ししか覚えてない
2: 半分しか覚えてない
3: 大体覚えたが微妙に違う
4: 集中したら思い出した					次は約%3.0f日後に
5: すぐ思い出した						次は約%3.0f日後に\n")
  (setq fc-tally-display
        (concat
         "今回は 正解: %d (%.1f%%), 間違え: %d (%.1f%%), "
         "全て: %d, 残り: %d+\n"))
  (setq fc-header-line-format
        (concat "[F7] 新しい単語や表現を入力する "
                "[F8] 登録された単語を編集する "
                "[F9] テストを始める！"))

  ;; change flashcard messages
  (setq flashcard-edit-header-line-format
        "[Ctrl+c Ctrl+k] 指定した行を消去 [F9] 保存する")
  (setq flashcard-message-empty
        "\n単語が登録されていません。[F7]を押して単語を入力してください。
入力したら[F9]を押すとテストが始まります。\n\n")
  (setq flashcard-message-finished
        "\nおめでとうございます！現在練習する単語や表現はありません。
新しい単語や表現を入力するかまた明日来てください。\n\n")
  (fc-show-header-line))

(defun fc-lang-en ()
  (interactive)
  (setq fc-question-prompt
        (concat "Say the answer to the following question, "
                "then press enter.\n\n"))
  (setq fc-answer-prompt "The answer: ")
  (setq fc-quality-prompt
        "\nHow well did you remember? Please choose a number.

0: Incorrect; didn't remember at all
1: Incorrect; remembered only a little
2: Incorrect; remembered only half
3: Incorrect, but almost correct
4: Correct; had to think about it			Next in ~%3.0f days
5: Correct; remembered quickly				Next in ~%3.0f days\n")
  (setq fc-tally-display
        (concat
         "Correct: %d (%.1f%%), Incorrect: %d (%.1f%%), "
         "Total: %d, Remaining: %d+\n"))
  (setq fc-header-line-format
        (concat "[F7] Add a new card "
                "[F8] Edit the card deck "
                "[F9] Begin testing! "))

  ;; change flashcard messages
  (setq flashcard-edit-header-line-format
        "[Ctrl+c Ctrl+k] Delete current line [F9] Save")
  (setq flashcard-message-empty
        "\nNo cards have been created. Press [F7] to add a new card.
After you've added a new card, the test will begin automatically.")
  (setq flashcard-message-finished
        "\nCongratulations! You have finished for now. Please add
a new card, or come back tomorrow.")
  (fc-show-header-line))

;; default to English
(cond
 ((eq fc-default-lang 'en)
  (fc-lang-en))
 ((eq fc-default-lang 'jp)
  (fc-lang-jp))
 (t (error "Invalid language!")))

(provide 'fc)

;; Local Variables:
;; coding: utf-8
;; End:
