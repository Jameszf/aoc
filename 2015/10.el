;;; -*- lexical-binding: t; -*-


;; Sorry about the mess in this solution. This was the first problem where
;; I needed to optimize my code for it to run in a reasonable amount of time.
;; I am choosing to leave a lot of code commented for posterity.
;; ==========================================================================
;; Things I have learned:
;;  1. The built-in profiler is very handy.
;;  2. Enabling lexical binding gives (small?) performance boost.
;;  3. Indexing lists is (unsurprisingly) not as efficient as indexing arrays
;;     (O(n) vs O(1)).
;;  4. The interpreter (?) does not cache certain things in loop (e.g.
;;     (while (length str)) <--- not cached so (length str) called each loop.
;;  5. Elisp recursion sucks (no tail-end + slow + small recursion limit).
;;  6. Using let in loops isn't too bad w/ lexical binding (still worse
;;     performance).
;;  7. Things like elt or aref have negligible performance differences (both
;;     are built-in functions and perform at similar speeds).

(defun string-to-list (str)
  (mapcar (lambda (x) (string-to-number (string x))) str))


(defun count-consecutive-chars (str char start)
  (let ((idx start)
	(size (length str)))
    (while (and (< idx size) (= (elt str idx) char))
      (setq idx (+ idx 1)))
    idx))


;; Recursive implemention of count-consecutive-chars
;; Runs 66% longer than iterative version.
;; (defun count-consecutive-chars-2 (str char size idx)
;;   (if (and (< idx size) (= (elt str idx) char))
;;       (count-consecutive-chars-2 str char size (+ idx 1))
;;     idx))


;; First solution (very slow)
;; Reason: 75% of runtime was on Automatic GC x_x
;; 
;; (defun look-and-say (sequence)
;;   (let ((ans "")
;; 	(idx 0))
;;     (while (< idx (length sequence))
;;       (setq firstChar (elt sequence idx))
;;       (setq stopIdx (count-consecutive-chars sequence firstChar idx))
;;       (setq ans (concat ans
;; 			(number-to-string (- stopIdx idx))
;; 			(string firstChar)))
;;       (setq idx stopIdx))
;;     ans))


;; (defun partOne ()
;;   (let ((sequence "1113222113"))
;;     (dotimes (i 33)
;;       (message (number-to-string i))
;;       (setq sequence (look-and-say sequence)))
;;     (message (length sequence))))


(defun look-and-say-B (sequence)
  (let ((ans nil)
	(idx 0)
	(size (length sequence)))
    (while (< idx size)
      (setq firstChar (elt sequence idx))
      (setq stopIdx (count-consecutive-chars sequence firstChar idx))
      (setq ans (cons firstChar (cons (- stopIdx idx) ans)))
      (setq idx stopIdx))
    (reverse ans)))


;; Alternative version of look-and-say
;; Substitutes 2 setq function calls with an inner let*.
;; Slightly slower and more GC calls.
;; (defun look-and-say-B2 (sequence)
;;   (let ((ans nil)
;; 	(idx 0)
;; 	(size (length sequence)))
;;     (while (< idx size)
;;       (let* ((firstChar (elt sequence idx))
;; 	     (stopIdx (count-consecutive-chars sequence firstChar idx)))
;; 	(setq ans (cons firstChar (cons (- stopIdx idx) ans)))
;; 	(setq idx stopIdx)))
;;     (reverse ans)))


;; Used for testing different implements of certain functions.
;; (defun look-and-say-B2 (sequence)
;;   (let ((ans nil)
;; 	(idx 0)
;; 	(size (length sequence)))
;;     (while (< idx size)
;;       (setq firstChar (elt sequence idx))
;;       (setq stopIdx (count-consecutive-chars-2 sequence firstChar (length sequence) idx))
;;       (setq ans (cons firstChar (cons (- stopIdx idx) ans)))
;;       (setq idx stopIdx))
;;     (reverse ans)))


(defun partOneB ()
  (let ((sequence (string-to-list "1113222113")))
    (dotimes (i 45)
      (setq sequence (look-and-say-B (vconcat sequence))))
    (message (number-to-string (length sequence)))))


;; Used for testing different implements of certain functions.
;; (defun partOneB2 ()
;;   (let ((sequence (string-to-list "1113222113")))
;;     (dotimes (i 45)
;;       (setq sequence (look-and-say-B2 (vconcat sequence))))
;;     (message (number-to-string (length sequence)))))

;; (message (number-to-string (count-consecutive-chars "1111411" ?1)))
;; (message (look-and-say-B (string-to-list "111221")))
;; (partOne)
;; (message (string-to-list "1113222113"))


(defun partTwo ()
  (let ((sequence (string-to-list "1113222113")))
    (dotimes (i 50)
      (message (number-to-string i))
      (setq sequence (look-and-say-B (vconcat sequence))))
    (message (number-to-string (length sequence)))))


(partOneB)
(partTwo)

;; (message "%s" (benchmark-run (partOneB))) ;; (2.690501325 19 1.800297406000027)
;; (message "%s" (benchmark-run (partOneB2))) ;; (3.100737997 23 2.1714247929999146)
;; (message "%d" (count-consecutive-chars-2 (vconcat (string-to-list "111111")) 1 6 0))
