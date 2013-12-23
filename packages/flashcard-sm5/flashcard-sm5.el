;;; flashcard-sm5.el --- SuperMemo algorithm for Emacs flashcard

;; Copyright (C) 2006  David Smith

;; Author: David Smith <davidsmith@acm.org>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is meant to be a replacement for the Leitner method
;; implementation already included with flashcard. It should produce
;; better results while supporting a much larger database of cards.

;; The basic usage is still the same as with Leitner, but after
;; entering or thinking of an answer, the user must input a number
;; from 0 to 5 (inc) to indicate how well he or she personally
;; remembered the item. This number will be a primary factor in
;; determining the date to show the card again. 0 indicates no recall,
;; 3 indicates near-recall (made a mistake, but was close), 4
;; indicates recall and 5 indicates the answer was recalled easily.

;; There are parts of this that may be wrong. I've tried to label
;; those parts clearly but someone else should really read the design
;; doc for SM5 and compare.

;; The optimal factors matrix is stored in the deck file. From the SM5
;; design doc, propagation of the optimal factors matrix is
;; recommended, but not implemented here yet. I would like to do so
;; but it seems like it would be quite slow and the docs are slightly
;; contradictory so I have left it out for now.

;; The searching procedures used for the next card could be optimized.
;; One idea is to use a balanced tree to store pending cards, but
;; generation of this list takes considerable time on large decks, and
;; generating the list a bit at the time isn't very elegant.

;; UPDATE: 08/14. Rereading the SM5 documentation, I've updated
;; the procedure for calculating the next interval. It seems to
;; make the interval-matrix irrelevant but it might be a
;; mistake. I'll test for a few days.

;; UPDATE: 09/01. I think I've fixed the problem with the
;; calcuation of the interval after the first time a card is
;; asked that was introduced in the previous update. Please
;; test and report. Also, I've incorporated a patch from Damien
;; Elmes for reading characters.

;;; Code:
(require 'flashcard)

(defvar flashcard-sm5-version "0.8"
  "The version string for flashcard-sm5.el")

(defgroup flashcard-method-sm5 nil
  "The SuperMemo(TM) 5 method."
  :prefix "flashcard-method-sm5-"
  :group 'flashcard-methods)

(defvar flashcard-sm5-initial-ease 10
  "The initial ease setting")
(defvar flashcard-sm5-matrix-size 20
  "The size of the matrices")
(defvar flashcard-sm5-optimal-factors nil
  "The Optimal Factors matrix")
(defvar flashcard-sm5-rate 0.8
  "Optimal Factors rate of change setting")
(defvar flashcard-sm5-initial-interval 4.0
  "The initial interval in days. Note that this only applies to
the first card you ever see in the deck as this will be
constantly refined by the algorithm.")
(defvar flashcard-sm5-check-answer-hook nil
  "Called after the answer is shown, before asking for a number.")
(make-variable-buffer-local 'flashcard-sm5-optimal-factors)
(make-variable-buffer-local 'flashcard-sm5-rate)

(defun flashcard-sm5-get-element (mat x y)
  (aref (aref mat x) y))

(defun flashcard-sm5-set-element (mat x y val)
  (aset (aref mat x) y val))

(defun flashcard-sm5-randomize (PI OF)
  (let* ((a 0.047)
        (b 0.092)
        (p (/ (sin (random t)) 2))
        (m (* (/ -1.0 b) (log (- 1.0 (* (/ b a) (abs p)))) (if (< 0 p) -1.0 1.0))))
    (* PI (+ 1.0 (* (- OF 1.0) (/ (+ 100.0 m) 100.0))))))

(defun flashcard-sm5-get-OF (count ease)
  (flashcard-sm5-get-element flashcard-sm5-optimal-factors count ease))

(defun flashcard-sm5-update-intervals (interval oldOF count ease quality)
  (let ((factor (flashcard-sm5-optimal-factor
                 interval quality oldOF (flashcard-sm5-get-OF count ease))))
    (flashcard-sm5-set-element
     flashcard-sm5-optimal-factors count ease factor)))

(defun flashcard-sm5-get-PI (count ease oldI &optional OF)
  (let ((OF (or OF (flashcard-sm5-get-OF count ease))))
    (if (eq count 0)
        (flashcard-sm5-randomize OF 1.0)
      (flashcard-sm5-randomize oldI OF))))

(defun flashcard-sm5-create-matrix ()
  (let ((matrix (make-vector flashcard-sm5-matrix-size nil)))
    (dotimes (i flashcard-sm5-matrix-size)
      (aset matrix i (make-vector flashcard-sm5-matrix-size nil)))
    matrix))

(defun flashcard-sm5-create-optimal-factors ()
  (let* ((first-row (make-vector flashcard-sm5-matrix-size flashcard-sm5-initial-interval))
         (levels (mapcar (lambda (x) (+ 1.01 (* x 0.05)))
                        (sm5-number-sequence 0 flashcard-sm5-matrix-size)))
         (matrix (flashcard-sm5-create-matrix)))
    (aset matrix 0 first-row)
    (dotimes (i (- flashcard-sm5-matrix-size 1))
      (aset matrix (+ 1 i) (vconcat levels)))
    matrix))

(defun flashcard-sm5-optimal-factor (oldI quality usedOF oldOF)
  (let* ((mod5 (max 1.05 (/ (1+ oldI) oldI)))
         (mod2 (min 0.75 (/ (- oldI 1) oldI)))
         (mod (max 0.05
                   (if (> quality 4)
                       ;; SM5 does (1+ (* (1- mod5) (- quality 4))) but
                       ;; if quality > 4, it has to be 5, which makes the above
                       ;; just equal to (1+ (1- mod5 1)) which is just mod5, sigh.
                       ;; so before anyone thinks I'm doing this wrong, there's
                       ;; five lines of comments to explain why I'm just writing mod5...
                       mod5
                     (- 1 (* (/ (- 1 mod2) 2.0) (- 4 quality))))))
         (res (* usedOF mod)))
    (if (> quality 4)
        (if (< res oldOF)
            (setq res oldOF))
      (if (> res oldOF)
          (setq res oldOF)))
    (max 1.01 (+ (* res flashcard-sm5-rate) (* oldOF (- 1 flashcard-sm5-rate))))))

(defun flashcard-sm5-get-optimal-factors (deck)
  "Get the optimal factors from the current deck.
Do nothing if we already have optimal factors loaded."
  (unless flashcard-sm5-optimal-factors
    (setq flashcard-sm5-optimal-factors
	  (or (flashcard-deck-get-note deck 'sm5-optimal-factors)
	      (flashcard-sm5-create-optimal-factors)))))

(defun flashcard-method-sm5-get-card (deck)
  "Return a new card from DECK according to the SM5 algorithm."
  ;; first, try and load matrices from this deck, on the off chance
  ;; we're switching from a leitner deck or similar.
  (flashcard-sm5-get-optimal-factors deck)
  (let* ((card (flashcard-sm5-next-card)))
    (if (stringp card)
        (flashcard-insert card)
      card)))

(defun flashcard-method-sm5-initialize-card (card)
  "Initialize CARD to be used with the SM5 method. For this, we
have to remember the time to ask the card next, which initialy is
the current-time, the initial ease, and the number of times this
card has been asked."
  (unless (flashcard-card-get-note card 'sm-next-time)
    (flashcard-card-set-note card 'sm-next-time (current-time)))
  (unless (flashcard-card-get-note card 'sm-interval)
    (flashcard-card-set-note card 'sm-interval flashcard-sm5-initial-interval))
  (unless (flashcard-card-get-note card 'sm-factor)
    (flashcard-card-set-note card 'sm-factor flashcard-sm5-initial-interval))
  (unless (flashcard-card-get-note card 'sm-ease)
    (flashcard-card-set-note card 'sm-ease flashcard-sm5-initial-ease))
  (unless (flashcard-card-get-note card 'sm-count)
    (flashcard-card-set-note card 'sm-count 0)))

(defun flashcard-method-sm5-answered (card quality)
  "Set the next time CARD will be asked based on the response from the user."
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
                 ease))
         (interval (flashcard-sm5-get-PI count ease oldI))
         (last-time (flashcard-card-get-note card 'sm-next-time)))
    ;; always update the intervals
    (flashcard-sm5-update-intervals
     oldI (flashcard-card-get-note card 'sm-factor)
     count ease quality)
    ;; set the ease factor
    (flashcard-card-set-note card 'sm-ease ease)
    (flashcard-deck-set-note flashcard-deck 'sm5-optimal-factors
                             flashcard-sm5-optimal-factors)
    (flashcard-card-set-note card 'sm-factor (flashcard-sm5-get-OF count ease))
    (when (< quality 3)
      ;; score less than 3, restart repetitions but maintain ease
      (flashcard-card-set-note card 'sm-count 0))
    (if (< quality 4)
        ;; score less than 4, reschedule for inside of 10 minutes (review)
        (progn
          (flashcard-card-set-note
           card 'sm-next-time
           (sm5-time-add (current-time) (seconds-to-time 600)))
          (flashcard-insert "Answer was incorrect. You'll see this card again within 10 minutes.\n"))
      ;; score 4 or higher, then increase count and schedule for next interval.
      (flashcard-card-set-note card 'sm-count
                               (1+ count))
      (flashcard-card-set-note card 'sm-next-time
                               (sm5-time-add (current-time)
                                         (days-to-time interval)))
      (flashcard-card-set-note card 'sm-interval interval)
      (flashcard-insert
       (format "Scheduling this card at %0.2f days from now\n" interval)))))

(defun flashcard-method-sm5-check-answer (card answer)
  "Insert the answer, ask the user for a quality point."
  (flashcard-insert "The correct answer is:\n"
                    (propertize (flashcard-card-answer card)
                                'face 'flashcard-answer-face
                                'rear-nonsticky t) "\n")
  (run-hook-with-args 'flashcard-sm5-check-answer-hook card answer)
  (let (char)
    (while (not char)
      (setq char
            (read-char-exclusive "Quality of your answer (0-5): "))
      (if (and (>= char 48) (<= char 53))
          (setq char (- char 48))
        (setq char nil)))
    char))

(defun flashcard-method-sm5 ()
  "Use the SuperMemo 5 method in flashcard."
  (interactive)
  (message "Setting up the SM5 method...")
  (setq flashcard-method 'sm5
        flashcard-method-get-card-function 'flashcard-method-sm5-get-card
        flashcard-method-answered-function 'flashcard-method-sm5-answered
        flashcard-method-initialize-card-function 'flashcard-method-sm5-initialize-card
        flashcard-method-check-answer-function 'flashcard-method-sm5-check-answer
        flashcard-method-correct-p-function 'identity
        flashcard-sm5-optimal-factors nil)
  (add-hook 'flashcard-edit-save-hook 'flashcard-sm5-sort-pending)
  (flashcard-sm5-get-optimal-factors flashcard-deck)
  (flashcard-deck-initialize flashcard-deck)
  (message "Setting up the SM5 method...ok"))

(defun flashcard-sm5-next-card ()
  "Return the next pending card.
If there are no pending cards, return a string indicating when the
next card will be ready."
  (let ((cards (flashcard-deck-cards deck))
        next-card
        earliest-card)
    (while cards
      ;; if the card's expired
      (if (time-less-p (flashcard-card-get-note
                        (car cards) 'sm-next-time) (current-time))
          (progn
            (setq next-card (car cards))
            (setq cards nil))
        ;; keep track of the earliest card, too
        (unless earliest-card (setq earliest-card (car cards)))
        (if (time-less-p (flashcard-card-get-note (car cards) 'sm-next-time)
                         (flashcard-card-get-note earliest-card
                                                  'sm-next-time))
            (setq earliest-card (car cards)))
        ;; loop
        (setq cards (cdr cards))))
    (if next-card
        next-card
      ;; return a string indicating the date of the next card
      (format "The next card will be ready at %s, %0.2f days from now\n\n"
              (format-time-string
               "%R %D" (flashcard-card-get-note
                        earliest-card 'sm-next-time))
              (/
               (time-to-seconds
                (sm5-time-subtract (flashcard-card-get-note
                                earliest-card 'sm-next-time)
                               (current-time)))
               86400)))))              ; (* 60 60 24)

(defun flashcard-sm5-sort-pending ()
  "Sort the deck prioritizing seen/recently pending cards.
Cards which have already been seen are prioritized over unseen cards.
Cards within a 20 minute window of now are moved to the top."
  (aset flashcard-deck 1
        (sort (flashcard-deck-cards flashcard-deck)
              'flashcard-sm5-pending-first)))

(defun flashcard-sm5-pending-first (a b)
  "True if A is seen and B unseen, or A was scheduled recently."
  ;; if a's been seen
  (unless (eql (flashcard-card-get-note a 'sm-interval)
               flashcard-sm5-initial-interval)
    (if (eql (flashcard-card-get-note b 'sm-interval)
             flashcard-sm5-initial-interval)
        ;; and b's not seen, a is first
        t
      ;; both are seen
      (and (flashcard-sm5-card-recently-scheduled-p a)
           (not (flashcard-sm5-card-recently-scheduled-p b))))))

(defsubst flashcard-sm5-card-recently-scheduled-p (card)
  "True if A was scheduled recently (and B was not)."
  (let ((sched (time-to-seconds
                (flashcard-card-get-note card 'sm-next-time)))
        (current (time-to-seconds (current-time))))
    (<= (abs (- current sched)) 1200))) ; 20 minutes - (* 60 20)

;;; emacs-21 compatibility
;;; required for: sm5-time-add sm5-time-subtract sm5-number-sequence
(require 'time-date)
(if (fboundp 'time-add)
  (defalias 'sm5-time-add 'time-add)
  (defun sm5-time-add (t1 t2)
    (seconds-to-time (+ (time-to-seconds t1) (time-to-seconds t2)))))
(if (fboundp 'time-subtract)
  (defalias 'sm5-time-subtract 'time-subtract)
  (defun sm5-time-subtract (t1 t2)
    (seconds-to-time (- (time-to-seconds t1) (time-to-seconds t2)))))
(if (fboundp 'number-sequence)
  (defalias 'sm5-number-sequence 'number-sequence)
  (defun sm5-number-sequence (from &optional to inc)
    (if (or (not to) (= from to))
        (list from)
      (or inc (setq inc 1))
      (when (zerop inc) (error "The increment can not be zero"))
      (let (seq (n 0) (next from))
        (if (> inc 0)
            (while (<= next to)
              (setq seq (cons next seq)
                    n (1+ n)
                    next (+ from (* n inc))))
          (while (>= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc)))))
        (nreverse seq)))))
;;; end emacs-21 compatibility

(provide 'flashcard-sm5)
;;; flashcard-sm5.el ends here
