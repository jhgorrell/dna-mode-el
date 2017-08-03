;;; dna-primer.el --- Custom primer function for dhepag
;;
;; ~/share/emacs/pkg/dna/dna-primer.el ---
;;
;; $Id: dna-primer.el,v 1.7 2013/08/21 18:05:15 harley Exp $
;;
;;; Commentary:
;; * custom primer code converted from Excel written by Dene Littler,
;;   as provided by Sirano Dhe-Paganon
;;
;; * The interactive functions are:
;;   - dna-primer-buffer = computes the primer for a buffer.
;;   - dna-primer-region = computes the primer for region.
;;   - dna-primer-process-table = processes buffer of tab seperated data.
;;
;; * To test:
;;   - M-x dna-primer-test-gen-data
;;   - C-x b *dna-primer-data*
;;   - M-x dna-primer-process-table
;;

(require 'dna-mode)

;; Original excel code provided by dhepag@gmail.com,
;; which was ported to elisp.

;; 'declaring variables
;; Dim rownumber, q, w, x, tm, z As Integer
;; Dim nucseq, colb, colc, cold, cole, revseq As String
;;
;; ' Col A - ORF Name
;; ' Col B - DNA Seq
;; ' Col C - DNA Seq Length
;; ' Col D - DNA Forward Primer
;; ' Col E - Fwd Primer Tm
;; ' Col F - Fwd Primer Length
;; ' Col G - DNA Reverse Primer
;; ' Col H - Rev Primer Tm
;; ' Col I - Rev Primer Length
;;
;; rownumber = 1
;; Do
;;   ' forward primer part
;;   x = 1
;;   tm = 0
;;   q = 0
;;   nucseq = Cells(rownumber, 2).Value
;;   Do Until q = 2 ' q must be satisfied tm >= 58 and C or G end) for it to go on
;;     q = 0
;;     colb = Mid(nucseq, x, 1)
;;     If colb = "A" Then tm = tm + 2
;;     If colb = "T" Then tm = tm + 2
;;     If colb = "C" Then tm = tm + 4
;;     If colb = "G" Then tm = tm + 4
;;      x = x + 1
;;     If tm >= 60 Then q = q + 1
;;     If tm >= 72 Then q = q + 1
;;     If colb = "G" Then q = q + 1
;;     If colb = "C" Then q = q + 1
;;   Loop
;;   colc = Left(nucseq, x - 1)
;;   colc = "ttgtatttccagggc" + colc
;;   Cells(rownumber, 3).Value = Len((Cells(rownumber, 2).Value))
;;   Cells(rownumber, 4).Value = colc     ' column 4 is length forward primer
;;   Cells(rownumber, 5).Value = tm       ' column 5 is length Tm
;;   Cells(rownumber, 6).Value = Len(colc) ' column 6 is length of primer
;;
;;   ' reverse primer part
;;   ' TAG stop codon, the T will be part of the
;;   x = 1
;;   tm = 0
;;   q = 0
;;   revseq = ""
;;   z = Len(nucseq)
;;   Do Until q = 2
;;     q = 0
;;     colb = Mid(nucseq, z - (x - 1), 1)
;;     If colb = "A" Then tm = tm + 2: revseq = revseq + "T"
;;     If colb = "T" Then tm = tm + 2: revseq = revseq + "A"
;;     If colb = "C" Then tm = tm + 4: revseq = revseq + "G"
;;     If colb = "G" Then tm = tm + 4: revseq = revseq + "C"
;;     x = x + 1
;;     If tm >= 60 Then q = q + 1
;;     If tm >= 72 Then q = q + 1
;;     If colb = "G" Then q = q + 1
;;     If colb = "C" Then q = q + 1
;;   Loop
;;   revseq = "caagcttcgtcatca" + revseq
;;   Cells(rownumber, 7).Value = revseq
;;   Cells(rownumber, 8).Value = (tm + 2)
;;   Cells(rownumber, 9).Value = Len(revseq)
;;   rownumber = rownumber + 1
;;   If Cells(rownumber, 1).Value = "" Then w = 1
;; Loop Until w = 1""

;;;;

;;; Code:

(defun dna-primer-func (seq dir prefix)
  "Function to compute a forward or reverse primer for SEQ in DIR with PREFIX as the start."
  (let ((x 0)
        (tm 0)
        (seq-len (length seq))
        (base nil) ;;
        (q 0))
    (while (and (< q 2) (< x seq-len))
      ;;
      (cond
       ((equal dir 'f)
        (setq base (elt seq x)))
       ((equal dir 'r)
        (setq base (elt seq (- seq-len x 1))))
       (t
        (error "Not 'f or 'r")))
      ;;
      (setq base (upcase base))

      (cond
       ((= base ?A)
        (setq tm (+ tm 2)))
       ((= base ?T)
        (setq tm (+ tm 2)))
       ((= base ?C)
        (setq tm (+ tm 4)))
       ((= base ?G)
        (setq tm (+ tm 4))))
      ;;
      (setq q 0)
      (if (<= 60 tm)
        (setq q (+ q 1)))
      (if (<= 72 tm)
        (setq q (+ q 1)))
      (if (or (= base ?C) (= base ?G))
        (setq q (+ q 1)))
      ;; next
      (setq x (+ x 1))
      nil)
    ;; @todo what to do if too short?
    (cond
     ((equal dir 'f)
      (setq primer (substring seq 0 x)))
     ((equal dir 'r)
      (setq primer (dna-revcomp-string (substring seq (- seq-len x)))))
     (t
      (error "")))
    (when prefix
      (setq primer (concat prefix primer)))
    ;;
    (list primer tm)))

(defvar dna-primer-fwd-prefix "ttgtatttccagggc")
(defun dna-primer-fwd (seq)
  "Generate the forward primer for SEQ.
Return (PRIMER TM) for the forward direction."
  (dna-primer-func seq 'f dna-primer-fwd-prefix))

;; (dna-primer-fwd "acgtacgtacgtacgtacgtacgtacgtacgt")

(defvar dna-primer-rev-prefix dna-primer-fwd-prefix)
(defun dna-primer-rev (seq)
  "Generate the reverse primer for SEQ.
Return (PRIMER TM) for the reverse direction."
  (dna-primer-func seq 'r dna-primer-rev-prefix))

;; (dna-primer-rev "acgtacgtacgtacgtacgtacgtacgtacgt")
;; (dna-primer-rev (make-string 40 ?t))

(defun dna-primer (seq)
  "Generate the forward and reverse primers for SEQ.
Return both in a list (FWD-primer FWD-melt REV-primer REV-melt)."
  (let ((val-fwd (dna-primer-fwd seq))
        (val-rev (dna-primer-rev seq)))
    (append val-fwd val-rev)))
;; (dna-primer (make-string 40 ?t))


(defun dna-primer-results-to-string (results)
  (concat
   (format "fwd-primer: %s\n" (nth 0 results))
   (format "fwd-tm:     %s\n" (nth 1 results))
   (format "rev-primer: %s\n" (nth 2 results))
   (format "rev-tm:     %s\n" (nth 3 results))))

(defun dna-primer-region (r-start r-end)
  "Treat the buffer as one big sequence and replace it with the output."
  (interactive "r")
  (let ((results
         (dna-primer
          (dna-clean-string
           (buffer-substring-no-properties r-start r-end))))
        (out-buf (get-buffer-create "*dna-primer-ouput*")))
    (switch-to-buffer-other-window out-buf)
    (erase-buffer)
    (insert (dna-primer-results-to-string results))
    (shrink-window-if-larger-than-buffer)
    nil))

(defun dna-primer-buffer ()
  "Treat the entire buffer as a dna-string and run dna-primer on it."
  (interactive)
  (dna-primer-region (point-min) (point-max)))

;;;;;;;;;;

(defun dna-primer-process-table-line ()
  "Process the current line and go to the next.
Return nil when at end of buffer."
  (beginning-of-line)
  (skip-chars-forward "^\t" (point-at-eol))
  (when (looking-at "\t")
    (forward-char 1)
    (let ((result (dna-primer
                   (dna-clean-string
                    (buffer-substring-no-properties (point) (point-at-eol))))))
      (insert
       (format "%s\t%s\t%s\t%s\t"
               (nth 0 result)
               (nth 1 result)
               (nth 2 result)
               (nth 3 result)))))
  (end-of-line)
  (if (< (point) (point-max))
    (progn
      (forward-char 1)
      t)
    nil))

(defun dna-primer-process-table ()
  "Process a table of data in a buffer.
The format of the input data is:

NAME tab SEQUENCE

The output format is:

NAME tab FWD-PRIMER tab FWD-TM tab REV-PRIMER tab REV-TM tab SEQUENCE
"
  (interactive)
  (goto-char (point-min))
  (while (dna-primer-process-table-line)
    t))

;;;;;;;;;;

;; Check the version of emacs, before we attempt to use 'assert
;; Xemacs has 'Assert.
(when (and
       (fboundp 'emacs-version)
       (string-match "^GNU" (emacs-version)))
  ;; for assert.
  (require 'cl)

  (defun dna-primer-test-regression-check ()
    (let* ((seq "acgtacgtacgtacgtacgtacgtacgtacgt")
           (qes (dna-revcomp-string seq)))
      ;;
      (assert
     (equal
      (dna-primer-fwd seq)
      '("ttgtatttccagggcacgtacgtacgtacgtacgtac" 66)))
      (assert
       (equal
        (dna-primer-rev seq)
        '("ttgtatttccagggcacgtacgtacgtacgtacgtac" 66)))
      ;; ok!
      t))
  nil)
;; (progn (eval-buffer) (dna-primer-regression-check))

(defun dna-primer-test-gen-data ()
  "Generate a buffer of test data.
This is suitable for testing dna-primer-process-table with."
  (interactive)
  (save-excursion
    (let ((buf (get-buffer-create "*dna-primer-data*")))
      (set-buffer buf)
      (erase-buffer)
      (dotimes (i 10)
        (insert
         (concat
          (format "name%02d" i)
          "\t"
          (dna-make-random-sequence (+ 40 (random 20)))
          "\n")))
      (switch-to-buffer-other-window buf))))
;; (dna-primer-test-gen-data)

;; (progn (eval-buffer))
(provide 'dna-primer)

;;; dna-primer.el ends here
