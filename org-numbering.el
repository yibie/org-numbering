;;; org-numbering.el --- Flexible numbering system for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Maintainer: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/org-numbering
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: org-mode, outlines, numbering

;; This file is not part of GNU Emacs.

;; The MIT License (MIT)

;; Copyright (c) 2024 Yibie
;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;; The Software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability,
;; fitness for a particular purpose and noninfringement. In no event shall the authors or copyright holders be liable for any claim, damages or other
;; liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings
;; in the Software.

;;; Commentary:

;; This package provides various numbering schemes for org-numbering.
;; Currently supported schemes:
;; - Decimal (1, 2, 3...)
;; - Alphabetic (a, b, c...)
;; - Roman numerals (I, II, III...)
;; - Chinese numerals (一、二、三...)
;; - Upper alpha (A, B, C...)
;; - Circled numbers (①, ②, ③...)
;; - Parenthesized letters (⒜, ⒝, ⒞...)
;; - Bullet points (•)
;; - Dashes (-)
;; - Squares (□)
;; - Greek letters (α, β, γ...)
;; - Parenthesized numbers ((1), (2), (3)...)
;; - Parenthesized roman ((i), (ii), (iii)...)
;; - Katakana (ア、イ、ウ...)
;; - Iroha order (イ、ロ、ハ...)
;; - Chapter style (第一章、第二章...)
;; - Parenthesized Chinese (（一）、（二）...)
;; - Extended circled (⑴, ⑵, ⑶...)
;; - White circled (○１、○２、○３...)

;; Example mixed numbering schemes for different styles:

;; 1. Academic Paper Style (English):
;;    (setq org-numbering-level-scheme
;;          '((1 . ((scheme . decimal)      ; 1.
;;                  (combine . nil)))
;;            (2 . ((scheme . decimal)      ; 1.1
;;                  (combine . t)))
;;            (3 . ((scheme . decimal)      ; 1.1.1
;;                  (combine . t)))
;;            (4 . ((scheme . alpha)        ; a)
;;                  (combine . nil)))
;;            (5 . ((scheme . paren-num)    ; (1)
;;                  (combine . nil)))))

;; 2. German Style:
;;    (setq org-numbering-level-scheme
;;          '((1 . ((scheme . decimal)      ; 1.
;;                  (combine . nil)))
;;            (2 . ((scheme . upper-alpha)  ; A.
;;                  (combine . nil)))
;;            (3 . ((scheme . alpha)        ; a)
;;                  (combine . nil)))
;;            (4 . ((scheme . greek)        ; α)
;;                  (combine . nil)))
;;            (5 . ((scheme . dash)         ; -
;;                  (combine . nil)))))

;; 3. Chinese Academic Style:
;;    (setq org-numbering-level-scheme
;;          '((1 . ((scheme . chapter)      ; 第一章
;;                  (combine . nil)))
;;            (2 . ((scheme . decimal)      ; 1.1
;;                  (combine . t)))
;;            (3 . ((scheme . paren-chinese) ; （一）
;;                  (combine . nil)))
;;            (4 . ((scheme . extended-circled) ; ⑴
;;                  (combine . nil)))
;;            (5 . ((scheme . white-circled)   ; ○１、
;;                  (combine . nil)))))

;; 4. Japanese Document Style:
;;    (setq org-numbering-level-scheme
;;          '((1 . ((scheme . decimal)      ; 1.
;;                  (combine . nil)))
;;            (2 . ((scheme . katakana)     ; ア、
;;                  (combine . nil)))
;;            (3 . ((scheme . circled)      ; ①
;;                  (combine . nil)))
;;            (4 . ((scheme . iroha)        ; イ、
;;                  (combine . nil)))
;;            (5 . ((scheme . square)       ; □
;;                  (combine . nil)))))

;;; Code:

(require 'org)
(require 'org-numbering-schemes)

;;;; Core Data Structures

(defgroup org-numbering nil
  "Org-mode heading numbering system."
  :group 'org
  :prefix "org-numbering-")

(defcustom org-numbering-style 'single
  "Numbering style to use.
'single  - Single number (e.g., 1. or 一、)
'combine - Combined numbers (e.g., 1.1 or 一.一)"
  :type '(choice (const :tag "Single number" single)
                (const :tag "Combined numbers" combine))
  :group 'org-numbering)
 
(defcustom org-numbering-level-scheme
  '((1 . ((scheme . decimal)     ; Use decimal scheme
          (combine . nil)))      ; Do not use combined numbering
    (2 . ((scheme . roman)       ; Use roman scheme
          (combine . t)))        ; Use combined numbering, becomes 1.I
    (3 . ((scheme . alpha)       ; Use alpha scheme
          (combine . t)))        ; Use combined numbering, becomes 1.I.a
    (4 . ((scheme . decimal)     ; Use decimal scheme
          (combine . nil))))     ; Do not use combined numbering, remains 1
  "Defines the numbering scheme for each heading level.
Each level can be set with the following properties:
  - scheme: The numbering scheme used (decimal, roman, alpha, etc.)
  - combine: Whether to use combined numbering (t indicates combining with parent numbering, nil indicates using separate numbering)"
  :type '(alist :key-type integer
                :value-type (alist :key-type symbol :value-type sexp))
  :group 'org-numbering)

(defcustom org-numbering-separator "."
  "Separator for combined numbers."
  :type 'string
  :group 'org-numbering)

(defvar org-numbering-context (make-hash-table :test 'equal)
  "Context for numbering, including counters and state.")

(defvar org-numbering-numbers (make-hash-table :test 'equal)
  "Hash table to store current numbers for each level and parent combination.")

;;;; Core Functions

(defun org-numbering--get-level-scheme (level)
  "Get numbering scheme for LEVEL."
  (let ((level-config (cdr (assq level org-numbering-level-scheme))))
    (cdr (assq 'scheme level-config))))

(defun org-numbering--get-level-combine (level)
  "Get combine setting for LEVEL."
  (let ((level-config (cdr (assq level org-numbering-level-scheme))))
    (cdr (assq 'combine level-config))))

(defun org-numbering--increment (scheme current)
  "Get next number in SCHEME sequence after CURRENT.
If CURRENT is nil, return the first number in the sequence."
  (let* ((props (gethash scheme org-numbering-schemes))
         (increment-fn (plist-get props :increment-fn)))
    (when increment-fn
      (funcall increment-fn current))))

(defun org-numbering--format (scheme number)
  "Format NUMBER according to SCHEME."
  (when (and scheme number)
    (let* ((props (gethash scheme org-numbering-schemes))
           (format-fn (plist-get props :format-fn)))
      (when format-fn
        (let ((formatted (funcall format-fn number)))
          (when formatted
            (message "DEBUG: Formatting: scheme=%s number=%s formatted=%s" 
                    scheme number formatted)
            formatted))))))

(defun org-numbering--parse (scheme-name str)
  "Parse STR according to SCHEME-NAME."
  (let* ((scheme (gethash scheme-name org-numbering-schemes))
         (parse-fn (plist-get scheme :parse-fn)))
    (when parse-fn
      (funcall parse-fn str))))

(defun org-numbering--validate (scheme-name number)
  "Validate NUMBER according to SCHEME-NAME."
  (let* ((scheme (gethash scheme-name org-numbering-schemes))
         (validate-fn (plist-get scheme :validate-fn)))
    (when validate-fn
      (funcall validate-fn number))))

(defun org-numbering--reset-numbers ()
  "Reset number counters.
Called when entering a new subtree to ensure numbers start fresh."
  (clrhash org-numbering-numbers))

(defun org-numbering--get-number (level parent-point)
  "Get number for current heading.
LEVEL is the current level
PARENT-POINT is the parent node position, used to distinguish branches."
  (let* ((key (cons level parent-point))
         (current (gethash key org-numbering-numbers)))
    ;; Reset counter for new branch or level
    (unless current
      (setq current 0))
    (puthash key (1+ current) org-numbering-numbers)
    current))

(defun org-numbering--make-combined-number (level parent-points numbers)
    "Generates the combined number for the current heading.
  LEVEL is the current level
  PARENT-POINTS is the list of parent heading positions
  NUMBERS is the list of numbers to combine"
  (let* ((current-num (car numbers))
         (parent-nums (cdr numbers))
         (combine (org-numbering--get-level-combine level)))
    (if (not combine)
        ;; If the current level does not use combined numbering, only return the current number
        (org-numbering--format 
         (org-numbering--get-level-scheme level) 
         current-num)
      ;; If combined numbering is used, combine the parent numbers and the current number
      (string-join
       (nconc
        (cl-loop for num in parent-nums
                for l from 1
                for scheme = (org-numbering--get-level-scheme l)
                when (org-numbering--get-level-combine l)
                collect (org-numbering--format scheme num))
        (list (org-numbering--format 
               (org-numbering--get-level-scheme level) 
               current-num)))
       org-numbering-separator))))

;;;; Basic Numbering Schemes

(defun org-numbering--get-heading-level ()
  "Get the level of current heading."
  (save-excursion
    (org-back-to-heading t)
    (looking-at org-complex-heading-regexp)
    (- (match-end 1) (match-beginning 1))))

(defun org-numbering--get-current-number ()
  "Get the current heading's number if it exists.
Returns (scheme . number) cons or nil if no number is found."
  (save-excursion
    (org-back-to-heading t)
    (looking-at org-complex-heading-regexp)
    (let* ((todo (match-string-no-properties 2))
           (priority (match-string-no-properties 3))
           (title (match-string-no-properties 4)))
      (when title
        (let ((scheme (org-numbering--get-level-scheme
                      (org-numbering--get-heading-level))))
          (when scheme
            ;; Skip special markers before the title (dates, statuses, etc.)
            (when (string-match "^\\(?:\\[[^]]+\\]\\s-+\\)*\\([^[:space:]]+\\)\\s-+\\(.*\\)" title)
              (let ((potential-number (match-string 1 title)))
                (let ((number (org-numbering--parse scheme potential-number)))
                  (when (and number
                            (org-numbering--validate scheme number))
                    (cons scheme number)))))))))))

(defun org-numbering--make-level-key (level parent-chain)
  "Creates a unique key for the heading level.
LEVEL is the heading level.
PARENT-CHAIN is the parent heading chain."
  (let ((immediate-parent (car parent-chain)))  ; Get the immediate parent heading
    (if immediate-parent
        (cons level (cdr immediate-parent))  ; Use the parent heading's position
      (cons level 0))))  ; If there is no parent heading, use 0 as the base point

(defun org-numbering--get-next-number (level parent-key current-number headings count-strategy)
  "Get next number for LEVEL based on COUNT-STRATEGY.
PARENT-KEY is the key of parent heading.
CURRENT-NUMBER is the current number being processed.
HEADINGS is the list of all collected headings.
COUNT-STRATEGY can be 'global or 'local."
  (let* ((scheme (org-numbering--get-level-scheme level))
         (increment-fn (plist-get (org-numbering--get-scheme (plist-get scheme :scheme))
                                :increment-fn)))
    (if (eq count-strategy 'global)
        ;; Global counting: increment based on all headings at this level
        (let ((same-level-headings
               (seq-filter (lambda (h)
                           (= (plist-get h :level) level))
                         headings)))
          (if (null same-level-headings)
              (funcall increment-fn nil)
            (funcall increment-fn (plist-get (car (last same-level-headings)) :number))))
      ;; Local counting: increment based on siblings only
      (let ((siblings
             (seq-filter (lambda (h)
                          (and (= (plist-get h :level) level)
                               (equal (plist-get h :parent-key) parent-key)))
                        headings)))
        (if (null siblings)
            (funcall increment-fn nil)
          (funcall increment-fn (plist-get (car (last siblings)) :number)))))))

(defun org-numbering--update-heading-number (level)
  "Update heading number at point"
  (message "Trying to update heading at point %d" (point))
  (save-excursion
    (org-back-to-heading t)  
    (when (looking-at org-complex-heading-regexp)
      (let* ((stars (match-string-no-properties 1))
             (todo (match-string-no-properties 2))
             (priority (match-string-no-properties 3))
             (title (match-string-no-properties 4))
             (tags (match-string-no-properties 5)))
        (message "Matched heading: stars='%s' todo='%s' priority='%s' title='%s' tags='%s'"
                 stars todo priority title tags)
        (when title
          (let* ((title-begin (match-beginning 4))
                 (title-end (match-end 4))
                 (scheme (org-numbering--get-level-scheme level))
                 (number (gethash (point) org-numbering-numbers)))
            (message "Got number %s for scheme %s" number scheme)
            ;; delete old title
            (goto-char title-begin)
            (delete-region title-begin title-end)
            ;; remove old number (if exists)
            (if (string-match "^\\(?:[IVXLCDM]+[.]\\|[a-z][.]\\|[0-9]+[.]\\|[一二三四五六七八九十百千万亿]+、\\)?\\s-*\\(.*\\)" title)
                (setq title (match-string 1 title))
              (setq title (string-trim title)))
            ;; insert new title (with new number)
            (when (and number scheme)
              (let ((formatted-number (org-numbering--format scheme number)))
                (when formatted-number
                  (insert (concat formatted-number " " title)))))))))))

;;;; Heading Collection and Numbering

(defun org-numbering--collect-headings (start end min-level)
  "Collect heading information within the specified range.
START and END define the search range
MIN-LEVEL specifies the minimum heading level"
  (let ((headings nil)
        (current-parents (make-hash-table :test 'eql))
        (start-level (save-excursion
                      (goto-char start)
                      (when (org-at-heading-p)
                        (org-outline-level)))))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward org-heading-regexp end t))
        (let* ((level (org-outline-level))
               (current-point (line-beginning-position)))
          (message "DEBUG: Found heading at %d with level %d (min-level=%d, start-level=%s)"
                  current-point level min-level (or start-level "nil"))
          ;; check if it belongs to current subtree: only check level
          (when (and (>= level min-level)  ; level not less than min-level
                    (or (not start-level)   ; if not from heading, process all headings
                        (>= level start-level)))  ; otherwise only process siblings or deeper levels
            (let* ((parent-point (save-excursion
                                 (org-up-heading-safe)
                                 (point)))
                   (title (org-get-heading t t t t))
                   (components (org-heading-components)))
              (message "DEBUG: Including heading - point=%d level=%d parent=%d title='%s'"
                      current-point level parent-point title)
              ;; update parent title record
              (puthash level current-point current-parents)
              ;; clear records of deeper levels
              (maphash (lambda (k _)
                        (when (> k level)
                          (remhash k current-parents)))
                      current-parents)
              ;; build parent title chain
              (let ((parent-chain nil)
                    (curr-level (1- level)))
                (while (> curr-level 0)
                  (let ((parent (gethash curr-level current-parents)))
                    (when parent
                      (push (cons curr-level parent) parent-chain)))
                  (setq curr-level (1- curr-level)))
                (message "DEBUG: Parent chain for point %d: %S" current-point parent-chain)
                ;; add heading info, including more metadata
                (setq headings
                      (append headings
                             (list (list :point current-point
                                       :level level
                                       :parent parent-point
                                       :title title
                                       :parents parent-chain
                                       :todo (nth 2 components)
                                       :priority (nth 3 components)
                                       :tags (nth 5 components)))))))))))
    (message "DEBUG: Final collected headings: %S" headings)
    headings))

(defun org-numbering--calculate-numbers (headings)
  "Calculate numbers for collected headings.
HEADINGS is a list of heading info, each element is a property list.
When combine is t, use global counting, otherwise use local counting."
  (message "DEBUG: Calculate-numbers starting with %d headings"
          (if (listp headings) (length headings) 1))
  (let ((numbers (make-hash-table :test 'eql))
        (counters (make-hash-table :test 'equal))  ; use equal to compare keys
        (headings-list (if (listp headings) headings (list headings))))
    ;; calculate numbers for each heading
    (dolist (heading headings-list)
      (let* ((point (plist-get heading :point))
             (level (plist-get heading :level))
             (parent (plist-get heading :parent))
             (title (plist-get heading :title))
             (scheme (org-numbering--get-level-scheme level))
             (combine (org-numbering--get-level-combine level))
             ;; choose counter key based on combine setting and level
             (counter-key (cond
                          (combine 
                           (cons level parent))  ; combine is t, use (level . parent) as key
                          ((= level 1) 
                           'top-level)  ; top level share one counter
                          (t 
                           (cons level (if (= parent 0) 
                                         'local  ; if no parent, use special mark
                                       parent)))))  ; other headings use (level . parent) as key
             (current (gethash counter-key counters))
             (next (when scheme (org-numbering--increment scheme current))))
        (message "DEBUG: Processing heading: point=%d level=%d scheme=%s title='%s' combine=%s key=%S"
                point level scheme title combine counter-key)
        (when (and scheme next)
          (message "DEBUG: Counter state: key=%S current=%s next=%s"
                  counter-key current next)
          (puthash counter-key next counters)  ; update counter
          (puthash point (cons scheme next) numbers))))  ; store number
    (message "DEBUG: Calculate-numbers final state: counters=%S numbers=%S"
             counters numbers)
    numbers))

(defun org-numbering--apply-numbers (numbers headings)
  "Apply the calculated numbers to the headings.
NUMBERS is a hash table containing (point . (scheme . number))
HEADINGS is a property list containing heading information"
  (message "DEBUG: Apply-numbers starting with %d numbers and %d headings"
          (hash-table-count numbers) (length headings))
  (save-excursion
    ;; process headings from back to front to avoid position change affecting
    (dolist (heading (reverse headings))
      (let* ((point (plist-get heading :point))
             (scheme-and-number (gethash point numbers))
             (level (plist-get heading :level))
             (parents (plist-get heading :parents)))
        (message "DEBUG: Processing heading at point %d with data %S parents=%S"
                point scheme-and-number parents)
        (when scheme-and-number
          (let ((scheme (car scheme-and-number))
                (number (cdr scheme-and-number)))
            (goto-char point)
            (when (org-at-heading-p)
              (let* ((todo (plist-get heading :todo))
                     (priority (plist-get heading :priority))
                     (title (plist-get heading :title))
                     (tags (plist-get heading :tags))
                     (combine (org-numbering--get-level-combine level))
                     ;; get all parent numbers
                     (parent-numbers
                      (when combine
                        (cl-loop for parent in parents
                                for parent-level = (car parent)
                                for parent-point = (cdr parent)
                                for parent-scheme-and-number = (gethash parent-point numbers)
                                when parent-scheme-and-number
                                collect (cons parent-level parent-scheme-and-number))))
                     ;; generate combined number
                     (combined-number
                      (if (and combine parent-numbers)
                          ;; when combine is t, use separator to connect pure numbers
                          (concat
                           (string-join
                            (mapcar
                             (lambda (num)
                               (let ((parent-scheme (cadr num))
                                     (parent-number (cddr num)))
                                 (org-numbering--format parent-scheme parent-number)))
                             parent-numbers)
                            org-numbering-separator)
                           org-numbering-separator
                           (org-numbering--format scheme number))
                        ;; when combine is nil, use formatted number directly
                        (org-numbering--format scheme number))))
                ;; delete old title
                (delete-region (line-beginning-position) (line-end-position))
                ;; rebuild title line
                (when combined-number
                  (message "DEBUG: Inserting formatted heading: level=%d number=%s title=%s"
                          level combined-number title)
                  ;; build basic title (without tags)
                  (let* ((template (alist-get scheme org-numbering-format-style))
                         (formatted-number (format template combined-number))
                         (heading-text
                          (concat
                           (make-string level ?*)  ; star
                           (if todo (concat " " todo) "")  ; TODO status
                           (if priority  ; priority
                               (concat " [#" (char-to-string priority) "]")
                             "")
                           " "
                           (string-trim formatted-number)  ; remove extra spaces
                           " "  ; add space between number and title
                           (org-numbering--clean-title title))))  ; use new clean function
                    ;; insert title and align tags
                    (if tags
                        (progn
                          (insert heading-text)
                          (org-align-tags))
                      (insert heading-text))))))))))))

(defun org-numbering--clean-title (title)
  "Clean all numbering formats in the title."
  (let ((clean-title
         (replace-regexp-in-string
          (concat
           "^\\(?:"
           ;; combined number format (like 1.1.a)
           "\\(?:[0-9.]+[a-z]\\|"
           ;; multi-level number (like 1.1)
           "[0-9.]+\\|"
           ;; single letter number (like a.) and repeated letter numbers (like a. a.)
           "\\(?:[a-z][.]\\s-*\\)+\\|"
           ;; single number (like 1.) and repeated number numbers (like 1. 1.)
           "\\(?:[0-9]+[.]\\s-*\\)+\\|"
           ;; roman numerals
           "[IVXLCDM]+\\|"
           ;; chinese numerals
           "[一二三四五六七八九十百千万亿]+[、.]"
           "\\)"
           "[[:space:]]*"
           "\\)"
           "\\(.*\\)")
          "\\1"
          title)))
    ;; keep date format
    (if (string-match "^\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-+[A-Za-z]\\{3\\}\\)\\]\\s-*\\(.*\\)" clean-title)
        (concat "[" (match-string 1 clean-title) "] " (match-string 2 clean-title))
      ;; recursive clean to avoid multiple numbering
      (if (string= clean-title title)
          clean-title
        (org-numbering--clean-title clean-title)))))

;;;; Main Numbering Functions

(defun org-numbering--process-headings (headings)
  "Process heading list, return numbered headings.
HEADINGS is a list of headings collected by org-numbering--collect-headings.
Return (numbers . headings) where numbers is a hash table of numbers."
  (message "DEBUG: Process-headings received headings of type %s, value: %S"
          (type-of headings) headings)
  ;; reset numbering state
  (org-numbering--reset-numbers)
  ;; calculate numbers
  (let ((numbers (org-numbering--calculate-numbers headings)))
    (message "DEBUG: Process-headings calculated %d numbers" (hash-table-count numbers))
    (cons numbers headings)))

(defun org-numbering--number-subtree ()
  "Add numbering to the current subtree."
  (save-excursion
    (org-back-to-heading t)
    (let* ((start-level (org-outline-level))
           (start (point))
           (end (save-excursion
                  ;; move to next sibling or higher heading, or end of buffer
                  (org-back-to-heading t)
                  (org-end-of-subtree t t)  ; add t t to ensure all subheadings are included
                  (point)))
           ;; collect headings
           (all-headings (org-numbering--collect-headings start end start-level)))
      (message "DEBUG: all-headings type: %s, value: %S, start=%d, end=%d" 
              (type-of all-headings) all-headings start end)
      ;; process headings
      (let* ((result (org-numbering--process-headings all-headings))
             (numbers (car result))
             (processed-headings (cdr result)))
        (message "DEBUG: Number-subtree processing %d headings of type %s"
                (length all-headings) (type-of all-headings))
        ;; apply numbers
        (org-numbering--apply-numbers numbers processed-headings)))))

(defun org-numbering--number-region (start end)
  "Add numbering to headings in the specified region."
  (save-excursion
    (let* ((headings (org-numbering--collect-headings start end 1))
           (result (org-numbering--process-headings headings))
           (numbers (car result))
           (processed-headings (cdr result)))
      (org-numbering--apply-numbers numbers processed-headings))))

(defun org-numbering--number-buffer ()
  "Add numbering to all headings in the current buffer."
  (org-numbering--number-region (point-min) (point-max)))

;;;###autoload
(defun org-numbering-number ()
  "Smartly number headings based on context.
If there is a selected region, number headings in the region.
If on a heading, number the subtree.
Otherwise, number all headings in the buffer."
  (interactive)
  (cond
   ((use-region-p)
    (org-numbering--number-region (region-beginning) (region-end)))
   ((org-at-heading-p)
    (org-numbering--number-subtree))
   (t
    (org-numbering--number-buffer))))

;;;###autoload
(define-minor-mode org-numbering-mode
  "Minor mode for flexible numbering in org-mode."
  :lighter " OrgNum"
  :keymap (let ((map (make-sparse-keymap)))
           map)
  :group 'org-numbering
  (if org-numbering-mode
      (message "Org-numbering mode enabled")
    (message "Org-numbering mode disabled")))

(provide 'org-numbering)



;;; org-numbering.el ends here
