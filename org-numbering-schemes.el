;;; org-numbering-schemes.el --- Numbering schemes for org-numbering -*- lexical-binding: t -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Maintainer: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/org-numbering
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: org-mode, outlines, numbering

;;; Commentary:

;;; Code:

;;;; Core Data Structures

(defgroup org-numbering-schemes nil
  "Numbering schemes for org-mode heading numbering system."
  :group 'org-numbering
  :prefix "org-numbering-schemes-")

(defvar org-numbering-schemes
  (make-hash-table :test 'eq)
  "Registry of numbering schemes.
Each scheme is a plist with the following functions:
- :increment-fn - Get next number in sequence
- :format-fn    - Format number for display
- :parse-fn     - Parse number from string
- :validate-fn  - Validate number")

(defcustom org-numbering-format-style
  '((decimal . "%s. ")           ; 1. Title (with space)
    (alpha . "%s) ")             ; a) Title (with space)
    (roman . "%s. ")             ; I. Title (with space)
    (chinese . "%s、")           ; 一、Title (no space)
    (upper-alpha . "%s. ")       ; A. Title (with space)
    (circled . "%s ")            ; ① Title (with space)
    (parenthesized . "%s ")      ; ⒜ Title (with space)
    (bullet . "%s ")             ; • Title (with space)
    (dash . "%s ")               ; - Title (with space)
    (square . "%s ")             ; □ Title (with space)
    (greek . "%s) ")             ; α) Title (with space)
    (paren-num . "%s ")          ; (1) Title (with space)
    (paren-roman . "%s ")        ; (i) Title (with space)
    (katakana . "%s、")          ; ア、Title (no space)
    (iroha . "%s、")             ; イ、Title (no space)
    (chapter . "%s ")             ; 第一章Title (no space)
    (paren-chinese . "%s")       ; （一）Title (no space)
    (extended-circled . "%s ")   ; ⑴ Title (with space)
    (white-circled . "%s、"))    ; ○１、Title (no space)
  "Format templates for each numbering scheme.
The %s will be replaced with the formatted number.
Note that CJK schemes don't add extra space after the number."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-numbering-schemes)

;;;; Scheme Registration

(defun org-numbering-register-scheme (name &rest props)
  "Register a new numbering scheme.
NAME is the scheme identifier.
PROPS is a plist with :increment-fn, :format-fn, :parse-fn and :validate-fn."
  (puthash name props org-numbering-schemes))

(defun org-numbering--get-scheme (name)
  "Get scheme properties for NAME."
  (gethash name org-numbering-schemes))

;;;; Basic Numbering Schemes

;; Decimal scheme (1, 2, 3...)
(defun org-numbering--decimal-increment (num)
  "Get next number after NUM.
If NUM is nil, start from 1."
  (1+ (if (numberp num) num 0)))

(defun org-numbering--decimal-format (num)
  "Format decimal number NUM."
  (number-to-string num))

(defun org-numbering--decimal-parse (str)
  "Parse decimal number from STR."
  (when (string-match "^\\([0-9]+\\)[.]?$" str)
    (string-to-number (match-string 1 str))))

(defun org-numbering--decimal-validate (num)
  "Validate decimal number NUM."
  (and (numberp num) (>= num 1)))

;; Alpha scheme (a, b, c...)
(defun org-numbering--alpha-increment (alpha)
  "Get next letter after ALPHA.
If ALPHA is nil, start from 'a'.
Examples: nil->a, a->b, z->a."
  (let ((next-char
         (if (null alpha)
             ?a
           (1+ (string-to-char alpha)))))
    (if (> next-char ?z)
        "a"  ; Wrap around after 'z'
      (char-to-string next-char))))

(defun org-numbering--alpha-format (alpha)
  "Format alphabetic character ALPHA."
  alpha)

(defun org-numbering--alpha-parse (str)
  "Parse alphabetic character from STR.
Return nil if STR is not a valid alpha character.
Handles format like 'a.', 'b.', etc."
  (when (string-match "^\\([a-z]\\)[.]?$" str)
    (match-string 1 str)))

(defun org-numbering--alpha-validate (alpha)
  "Validate alphabetic character ALPHA.
Must be a single lowercase letter a-z."
  (and (stringp alpha)
       (= (length alpha) 1)
       (string-match "^[a-z]$" alpha)))

;; Uppercase Alpha scheme (A, B, C...)
(defun org-numbering--upper-alpha-increment (alpha)
  "Get next uppercase letter after ALPHA.
If ALPHA is nil, start from 'A'.
Examples: nil->A, A->B, Z->A."
  (let ((next-char
         (if (null alpha)
             ?A
           (1+ (string-to-char alpha)))))
    (if (> next-char ?Z)
        "A"  ; Wrap around after 'Z'
      (char-to-string next-char))))

(defun org-numbering--upper-alpha-format (alpha)
  "Format uppercase alphabetic character ALPHA."
  alpha)

(defun org-numbering--upper-alpha-parse (str)
  "Parse uppercase alphabetic character from STR.
Return nil if STR is not a valid uppercase alpha character.
Handles format like 'A.', 'B.', etc."
  (when (string-match "^\\([A-Z]\\)[.]?$" str)
    (match-string 1 str)))

(defun org-numbering--upper-alpha-validate (alpha)
  "Validate uppercase alphabetic character ALPHA.
Must be a single uppercase letter A-Z."
  (and (stringp alpha)
       (= (length alpha) 1)
       (string-match "^[A-Z]$" alpha)))

;; Roman numeral scheme (I, II, III...)
(defconst org-numbering--roman-values
  '((1000 . "M") (900 . "CM")
    (500  . "D") (400 . "CD")
    (100  . "C") (90  . "XC")
    (50   . "L") (40  . "XL")
    (10   . "X") (9   . "IX")
    (5    . "V") (4   . "IV")
    (1    . "I"))
  "Mapping between decimal numbers and Roman numerals.")

(defun org-numbering--to-roman (num)
  "Convert decimal number NUM to Roman numerals."
  (let ((result ""))
    (dolist (pair org-numbering--roman-values result)
      (let ((val (car pair))
            (roman (cdr pair)))
        (while (>= num val)
          (setq result (concat result roman)
                num (- num val)))))))

(defun org-numbering--from-roman (str)
  "Convert Roman numeral STR to decimal number.
Return nil if STR is not a valid Roman numeral."
  (when (and str (string-match "^[IVXLCDM]+$" str))
    (let ((num 0)
          (prev-val 0))
      (dolist (ch (reverse (string-to-list str)) num)
        (let ((curr-val
               (cond
                ((eq ch ?I) 1)
                ((eq ch ?V) 5)
                ((eq ch ?X) 10)
                ((eq ch ?L) 50)
                ((eq ch ?C) 100)
                ((eq ch ?D) 500)
                ((eq ch ?M) 1000))))
          (setq num (if (>= curr-val prev-val)
                       (+ num curr-val)
                     (- num curr-val))
                prev-val curr-val))))))

(defun org-numbering--roman-increment (roman)
  "Get next Roman numeral after ROMAN.
If ROMAN is nil, start from I."
  (let ((current-num (if roman
                        (org-numbering--from-roman roman)
                      0)))
    (message "Roman increment: current=%s num=%d" roman current-num)
    (let ((next (org-numbering--to-roman (1+ current-num))))
      (message "Roman increment: next=%s" next)
      next)))

(defun org-numbering--roman-format (roman)
  "Format Roman numeral ROMAN."
  roman)

(defun org-numbering--roman-parse (str)
  "Parse Roman numeral from STR.
Return nil if STR is not a valid Roman numeral.
Handles format like 'I.', 'II.', etc."
  (when (string-match "^\\([IVXLCDM]+\\)[.]?$" str)
    (let ((roman (match-string 1 str)))
      ;; Verify it's a valid Roman numeral by converting back and forth
      (let ((num (org-numbering--from-roman roman)))
        (when (and num (string= roman (org-numbering--to-roman num)))
          roman)))))

(defun org-numbering--roman-validate (roman)
  "Validate Roman numeral ROMAN."
  (and (stringp roman)
       (string-match "^[IVXLCDM]+$" roman)
       ;; Additional validation: check if it's a valid Roman numeral
       (let ((num (org-numbering--from-roman roman)))
         (and num (string= roman (org-numbering--to-roman num))))))

;; Chinese numeral scheme (一、二、三...)
(defconst org-numbering--chinese-values
  '((0 . "零") (1 . "一") (2 . "二") (3 . "三") (4 . "四")
    (5 . "五") (6 . "六") (7 . "七") (8 . "八") (9 . "九"))
  "Mapping between Arabic numbers and Chinese numerals.")

(defconst org-numbering--chinese-units
  '((1 . "") (10 . "十") (100 . "百") (1000 . "千")
    (10000 . "万") (100000000 . "亿"))
  "Chinese number units in ascending order.")

(defun org-numbering--to-chinese (num)
  "Convert number NUM to Chinese numerals."
  (let ((result ""))
    (cond
     ((< num 0) (concat "负" (org-numbering--to-chinese (abs num))))
     ((zerop num) "零")
     (t
      (let ((units (reverse org-numbering--chinese-units))
            (prev-zero nil))
        ;; 从大到小处理每个单位
        (dolist (unit-pair units)
          (let* ((unit-val (car unit-pair))
                 (unit-str (cdr unit-pair))
                 (quotient (/ num unit-val))
                 (remainder (mod num unit-val)))
            (when (> quotient 0)
              ;; 处理当前单位
              (unless (and (= quotient 1) (= unit-val 10))
                (setq result (concat result (cdr (assq quotient org-numbering--chinese-values)))))
              (unless (string= unit-str "")  ; 不添加空单位
                (setq result (concat result unit-str)))
              ;; 更新剩余值
              (setq num remainder
                    prev-zero nil)
              ;; 处理零
              (when (and (> remainder 0)
                        (< remainder (/ unit-val 10))
                        (not prev-zero))
                (setq result (concat result "零")
                      prev-zero t)))))
        ;; 处理最后的个位数
        (when (> num 0)
          (setq result (concat result (cdr (assq num org-numbering--chinese-values))))))
      result))))

(defun org-numbering--from-chinese (str)
  "Convert Chinese numeral STR to number.
Return nil if STR is not a valid Chinese numeral."
  (when str
    (let ((num 0)
          (curr-num 0)
          (num-map (mapcar (lambda (pair)
                            (cons (cdr pair) (car pair)))
                          org-numbering--chinese-values))
          (unit-map (mapcar (lambda (pair)
                             (cons (cdr pair) (car pair)))
                           org-numbering--chinese-units)))
      (catch 'invalid
        ;; 处理负数
        (when (string-prefix-p "负" str)
          (let ((result (org-numbering--from-chinese (substring str 1))))
            (when result
              (throw 'invalid (- result)))))
        
        ;; 处理每个字符
        (let ((chars (string-to-list str))
              (len (length str)))
          (dotimes (i len)
            (let* ((ch (nth i chars))
                   (ch-str (char-to-string ch))
                   (digit (cdr (assoc ch-str num-map)))
                   (unit (cdr (assoc ch-str unit-map))))
              (cond
               ;; 找到数字
               (digit
                (setq curr-num digit))
               ;; 找到单位
               (unit
                (when (zerop curr-num)
                  (setq curr-num 1))
                (setq num (+ num (* curr-num unit))
                      curr-num 0))
               ;; 跳过 "零"
               ((= ch ?零)
                (setq curr-num 0))
               ;; 无效字符
               (t (throw 'invalid nil))))))
        
        ;; 添加剩余的数字
        (when (> curr-num 0)
          (setq num (+ num curr-num)))
        num))))

(defun org-numbering--chinese-increment (chinese)
  "Get next Chinese numeral after CHINESE.
If CHINESE is nil, start from 一."
  (org-numbering--to-chinese
   (1+ (if chinese
           (org-numbering--from-chinese chinese)
         0))))

(defun org-numbering--chinese-format (chinese)
  "Format Chinese numeral CHINESE."
  chinese)

(defun org-numbering--chinese-parse (str)
  "Parse Chinese numeral from STR.
Return nil if STR is not a valid Chinese numeral.
Handles format like '一、', '二、', etc."
  (when (string-match "^\\([负零一二三四五六七八九十百千万亿]+\\)\\(、\\)?$" str)
    (let ((chinese (match-string 1 str)))
      ;; Verify it's a valid Chinese numeral by converting back and forth
      (let ((num (org-numbering--from-chinese chinese)))
        (when (and num (string= chinese (org-numbering--to-chinese num)))
          chinese)))))

(defun org-numbering--chinese-validate (chinese)
  "Validate Chinese numeral CHINESE."
  (and (stringp chinese)
       (string-match "^[负零一二三四五六七八九十百千万亿]+$" chinese)
       ;; Additional validation: check if it's a valid Chinese numeral
       (let ((num (org-numbering--from-chinese chinese)))
         (and num (string= chinese (org-numbering--to-chinese num))))))

;; Circled number scheme (①, ②, ③...)
(defconst org-numbering--circled-numbers
  '((1 . "①") (2 . "②") (3 . "③") (4 . "④") (5 . "⑤")
    (6 . "⑥") (7 . "⑦") (8 . "⑧") (9 . "⑨") (10 . "⑩")
    (11 . "⑪") (12 . "⑫") (13 . "⑬") (14 . "⑭") (15 . "⑮")
    (16 . "⑯") (17 . "⑰") (18 . "⑱") (19 . "⑲") (20 . "⑳"))
  "Mapping between numbers and their circled representations.")

(defun org-numbering--circled-increment (circled)
  "Get next circled number after CIRCLED.
If CIRCLED is nil, start from ①.
Wraps around after ⑳."
  (let* ((num (if circled
                  (car (rassoc circled org-numbering--circled-numbers))
                0))
         (next-num (1+ num)))
    (if (> next-num 20)
        "①"  ; Wrap around after ⑳
      (cdr (assq next-num org-numbering--circled-numbers)))))

(defun org-numbering--circled-format (circled)
  "Format circled number CIRCLED."
  circled)

(defun org-numbering--circled-parse (str)
  "Parse circled number from STR.
Return nil if STR is not a valid circled number.
Handles format like '①.', '②.', etc."
  (when (string-match "^\\([①-⑳]\\)[.]?$" str)
    (match-string 1 str)))

(defun org-numbering--circled-validate (circled)
  "Validate circled number CIRCLED."
  (and (stringp circled)
       (= (length circled) 1)
       (string-match "^[①-⑳]$" circled)))

;; Parenthesized letter scheme (⒜, ⒝, ⒞...)
(defconst org-numbering--parenthesized-letters
  '((?a . "⒜") (?b . "⒝") (?c . "⒞") (?d . "⒟") (?e . "⒠")
    (?f . "⒡") (?g . "⒢") (?h . "⒣") (?i . "⒤") (?j . "⒥")
    (?k . "⒦") (?l . "⒧") (?m . "⒨") (?n . "⒩") (?o . "⒪")
    (?p . "⒫") (?q . "⒬") (?r . "⒭") (?s . "⒮") (?t . "⒯")
    (?u . "⒰") (?v . "⒱") (?w . "⒲") (?x . "⒳") (?y . "⒴")
    (?z . "⒵"))
  "Mapping between ASCII letters and their parenthesized representations.")

(defun org-numbering--parenthesized-increment (letter)
  "Get next parenthesized letter after LETTER.
If LETTER is nil, start from ⒜.
Wraps around after ⒵."
  (let* ((curr-char (if letter
                        (car (rassoc letter org-numbering--parenthesized-letters))
                      ?`))  ; 使用 ?` 因为它是 ?a 的前一个字符
         (next-char (1+ curr-char)))
    (if (> next-char ?z)
        (cdr (assq ?a org-numbering--parenthesized-letters))  ; 回到 ⒜
      (cdr (assq next-char org-numbering--parenthesized-letters)))))

(defun org-numbering--parenthesized-format (letter)
  "Format parenthesized letter LETTER."
  letter)

(defun org-numbering--parenthesized-parse (str)
  "Parse parenthesized letter from STR.
Return nil if STR is not a valid parenthesized letter.
Handles format like '⒜.', '⒝.', etc."
  (when (string-match "^\\([⒜-⒵]\\)[.]?$" str)
    (match-string 1 str)))

(defun org-numbering--parenthesized-validate (letter)
  "Validate parenthesized letter LETTER."
  (and (stringp letter)
       (= (length letter) 1)
       (string-match "^[⒜-⒵]$" letter)))

;; Bullet point scheme (•)
(defconst org-numbering--bullet-symbol "•"
  "Bullet point symbol.")

(defun org-numbering--bullet-increment (_)
  "Always return bullet point symbol."
  org-numbering--bullet-symbol)

(defun org-numbering--bullet-format (bullet)
  "Format bullet point BULLET."
  bullet)

(defun org-numbering--bullet-parse (str)
  "Parse bullet point from STR."
  (when (string-match (concat "^" (regexp-quote org-numbering--bullet-symbol) "$") str)
    org-numbering--bullet-symbol))

(defun org-numbering--bullet-validate (bullet)
  "Validate bullet point BULLET."
  (string= bullet org-numbering--bullet-symbol))

;; Dash scheme (-)
(defconst org-numbering--dash-symbol "-"
  "Dash symbol.")

(defun org-numbering--dash-increment (_)
  "Always return dash symbol."
  org-numbering--dash-symbol)

(defun org-numbering--dash-format (dash)
  "Format dash DASH."
  dash)

(defun org-numbering--dash-parse (str)
  "Parse dash from STR."
  (when (string-match (concat "^" (regexp-quote org-numbering--dash-symbol) "$") str)
    org-numbering--dash-symbol))

(defun org-numbering--dash-validate (dash)
  "Validate dash DASH."
  (string= dash org-numbering--dash-symbol))

;; Square scheme (□)
(defconst org-numbering--square-symbol "□"
  "Square symbol.")

(defun org-numbering--square-increment (_)
  "Always return square symbol."
  org-numbering--square-symbol)

(defun org-numbering--square-format (square)
  "Format square SQUARE."
  square)

(defun org-numbering--square-parse (str)
  "Parse square from STR."
  (when (string-match (concat "^" (regexp-quote org-numbering--square-symbol) "$") str)
    org-numbering--square-symbol))

(defun org-numbering--square-validate (square)
  "Validate square SQUARE."
  (string= square org-numbering--square-symbol))

;; Greek letter scheme (α, β, γ...)
(defconst org-numbering--greek-letters
  ["α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ" 
   "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" 
   "φ" "χ" "ψ" "ω"]
  "List of Greek letters.")

(defun org-numbering--greek-increment (letter)
  "Get next Greek letter after LETTER.
If LETTER is nil, start from α."
  (let* ((idx (if letter
                  (cl-position letter org-numbering--greek-letters :test #'string=)
                -1))
         (next-idx (1+ idx)))
    (if (>= next-idx (length org-numbering--greek-letters))
        (aref org-numbering--greek-letters 0)  ; Wrap around to α
      (aref org-numbering--greek-letters next-idx))))

(defun org-numbering--greek-format (letter)
  "Format Greek letter LETTER."
  letter)

(defun org-numbering--greek-parse (str)
  "Parse Greek letter from STR."
  (when (string-match "^\\([α-ω]\\)[.]?$" str)
    (match-string 1 str)))

(defun org-numbering--greek-validate (letter)
  "Validate Greek letter LETTER."
  (and (stringp letter)
       (cl-position letter org-numbering--greek-letters :test #'string=)))

;; Parenthesized number scheme ((1), (2), (3)...)
(defun org-numbering--paren-num-increment (num)
  "Get next parenthesized number after NUM.
If NUM is nil, start from (1)."
  (format "(%d)" (1+ (if num
                         (string-to-number (replace-regexp-in-string "[()]" "" num))
                       0))))

(defun org-numbering--paren-num-format (num)
  "Format parenthesized number NUM."
  num)

(defun org-numbering--paren-num-parse (str)
  "Parse parenthesized number from STR."
  (when (string-match "^(\\([0-9]+\\))$" str)
    (format "(%s)" (match-string 1 str))))

(defun org-numbering--paren-num-validate (num)
  "Validate parenthesized number NUM."
  (and (stringp num)
       (string-match "^([0-9]+)$" num)))

;; Parenthesized roman scheme ((i), (ii), (iii)...)
(defun org-numbering--paren-roman-increment (roman)
  "Get next parenthesized roman numeral after ROMAN.
If ROMAN is nil, start from (i)."
  (format "(%s)" 
          (downcase
           (org-numbering--to-roman
            (1+ (if roman
                    (org-numbering--from-roman 
                     (upcase (replace-regexp-in-string "[()]" "" roman)))
                  0))))))

(defun org-numbering--paren-roman-format (roman)
  "Format parenthesized roman numeral ROMAN."
  roman)

(defun org-numbering--paren-roman-parse (str)
  "Parse parenthesized roman numeral from STR."
  (when (string-match "^(\\([ivxlcdm]+\\))$" str)
    (format "(%s)" (match-string 1 str))))

(defun org-numbering--paren-roman-validate (roman)
  "Validate parenthesized roman numeral ROMAN."
  (and (stringp roman)
       (string-match "^(\\([ivxlcdm]+\\))$" roman)
       (org-numbering--from-roman (upcase (replace-regexp-in-string "[()]" "" roman)))))

;; Katakana scheme (ア、イ、ウ...)
(defconst org-numbering--katakana
  ["ア" "イ" "ウ" "エ" "オ" 
   "カ" "キ" "ク" "ケ" "コ"
   "サ" "シ" "ス" "セ" "ソ"
   "タ" "チ" "ツ" "テ" "ト"
   "ナ" "ニ" "ヌ" "ネ" "ノ"
   "ハ" "ヒ" "フ" "ヘ" "ホ"
   "マ" "ミ" "ム" "メ" "モ"
   "ヤ" "ユ" "ヨ"
   "ラ" "リ" "ル" "レ" "ロ"
   "ワ" "ヲ" "ン"]
  "List of Katakana characters.")

(defun org-numbering--katakana-increment (kana)
  "Get next Katakana after KANA.
If KANA is nil, start from ア."
  (let* ((idx (if kana
                  (cl-position kana org-numbering--katakana :test #'string=)
                -1))
         (next-idx (1+ idx)))
    (if (>= next-idx (length org-numbering--katakana))
        (aref org-numbering--katakana 0)  ; Wrap around to ア
      (aref org-numbering--katakana next-idx))))

(defun org-numbering--katakana-format (kana)
  "Format Katakana KANA."
  kana)

(defun org-numbering--katakana-parse (str)
  "Parse Katakana from STR."
  (when (string-match "^\\([ァ-ヶ]\\)[.]?$" str)
    (match-string 1 str)))

(defun org-numbering--katakana-validate (kana)
  "Validate Katakana KANA."
  (and (stringp kana)
       (cl-position kana org-numbering--katakana :test #'string=)))

;; Iroha scheme (イ、ロ、ハ...)
(defconst org-numbering--iroha
  ["イ" "ロ" "ハ" "ニ" "ホ" 
   "ヘ" "ト" "チ" "リ" "ヌ"
   "ル" "ヲ" "ワ" "カ" "ヨ"
   "タ" "レ" "ソ" "ツ" "ネ"
   "ナ" "ラ" "ム" "ウ" "ヰ"
   "ノ" "オ" "ク" "ヤ" "マ"
   "ケ" "フ" "コ" "エ" "テ"
   "ア" "サ" "キ" "ユ" "メ"
   "ミ" "シ" "ヱ" "ヒ" "モ"
   "セ" "ス"]
  "List of characters in Iroha order.")

(defun org-numbering--iroha-increment (char)
  "Get next character in Iroha order after CHAR.
If CHAR is nil, start from イ."
  (let* ((idx (if char
                  (cl-position char org-numbering--iroha :test #'string=)
                -1))
         (next-idx (1+ idx)))
    (if (>= next-idx (length org-numbering--iroha))
        (aref org-numbering--iroha 0)  ; Wrap around to イ
      (aref org-numbering--iroha next-idx))))

(defun org-numbering--iroha-format (char)
  "Format Iroha character CHAR."
  char)

(defun org-numbering--iroha-parse (str)
  "Parse Iroha character from STR."
  (when (string-match "^\\([ァ-ヶ]\\)[.]?$" str)
    (match-string 1 str)))

(defun org-numbering--iroha-validate (char)
  "Validate Iroha character CHAR."
  (and (stringp char)
       (cl-position char org-numbering--iroha :test #'string=)))

;; Chapter scheme (第一章, 第二章...)
(defconst org-numbering--chapter-prefix "第"
  "Chapter prefix in Chinese.")

(defconst org-numbering--chapter-suffix "章"
  "Chapter suffix in Chinese.")

(defun org-numbering--chapter-increment (chapter)
  "Get next chapter number after CHAPTER.
If CHAPTER is nil, start from 第一章."
  (let* ((num (if chapter
                  (org-numbering--from-chinese 
                   (replace-regexp-in-string 
                    (concat org-numbering--chapter-prefix "\\|" org-numbering--chapter-suffix) 
                    "" chapter))
                0)))
    (concat org-numbering--chapter-prefix
            (org-numbering--to-chinese (1+ num))
            org-numbering--chapter-suffix)))

(defun org-numbering--chapter-format (chapter)
  "Format chapter number CHAPTER."
  chapter)

(defun org-numbering--chapter-parse (str)
  "Parse chapter number from STR."
  (when (string-match (concat "^" org-numbering--chapter-prefix 
                             "\\([一二三四五六七八九十百千万亿]+\\)" 
                             org-numbering--chapter-suffix) str)
    (concat org-numbering--chapter-prefix
            (match-string 1 str)
            org-numbering--chapter-suffix)))

(defun org-numbering--chapter-validate (chapter)
  "Validate chapter number CHAPTER."
  (and (stringp chapter)
       (string-match (concat "^" org-numbering--chapter-prefix 
                            "[一二三四五六七八九十百千万亿]+" 
                            org-numbering--chapter-suffix "$") chapter)))

;; Parenthesized Chinese scheme (（一）, （二）, （三）...)
(defun org-numbering--paren-chinese-increment (chinese)
  "Get next parenthesized Chinese numeral after CHINESE.
If CHINESE is nil, start from （一）."
  (format "（%s）"
          (org-numbering--to-chinese
           (1+ (if chinese
                   (org-numbering--from-chinese
                    (replace-regexp-in-string "[（）]" "" chinese))
                 0)))))

(defun org-numbering--paren-chinese-format (chinese)
  "Format parenthesized Chinese numeral CHINESE."
  chinese)

(defun org-numbering--paren-chinese-parse (str)
  "Parse parenthesized Chinese numeral from STR."
  (when (string-match "^（\\([一二三四五六七八九十百千万亿]+\\)）$" str)
    (format "（%s）" (match-string 1 str))))

(defun org-numbering--paren-chinese-validate (chinese)
  "Validate parenthesized Chinese numeral CHINESE."
  (and (stringp chinese)
       (string-match "^（[一二三四五六七八九十百千万亿]+）$" chinese)))

;; Extended circled number scheme (⑴, ⑵, ⑶...)
(defconst org-numbering--extended-circled-numbers
  '((1 . "⑴") (2 . "⑵") (3 . "⑶") (4 . "⑷") (5 . "⑸")
    (6 . "⑹") (7 . "⑺") (8 . "⑻") (9 . "⑼") (10 . "⑽")
    (11 . "⑾") (12 . "⑿") (13 . "⒀") (14 . "⒁") (15 . "⒂")
    (16 . "⒃") (17 . "⒄") (18 . "⒅") (19 . "⒆") (20 . "⒇"))
  "Mapping between numbers and their parenthesized circle representations.")

(defun org-numbering--extended-circled-increment (circled)
  "Get next extended circled number after CIRCLED.
If CIRCLED is nil, start from ⑴.
Wraps around after ⒇."
  (let* ((num (if circled
                  (car (rassoc circled org-numbering--extended-circled-numbers))
                0))
         (next-num (1+ num)))
    (if (> next-num 20)
        "⑴"  ; Wrap around after ⒇
      (cdr (assq next-num org-numbering--extended-circled-numbers)))))

(defun org-numbering--extended-circled-format (circled)
  "Format extended circled number CIRCLED."
  circled)

(defun org-numbering--extended-circled-parse (str)
  "Parse extended circled number from STR."
  (when (string-match "^[⑴-⒇]$" str)
    (match-string 0 str)))

(defun org-numbering--extended-circled-validate (circled)
  "Validate extended circled number CIRCLED."
  (and (stringp circled)
       (= (length circled) 1)
       (string-match "^[⑴-⒇]$" circled)))

;; White circled number scheme (○１、○２、○３...)
(defun org-numbering--white-circled-increment (num)
  "Get next white circled number after NUM.
If NUM is nil, start from ○１."
  (format "○%s" 
          (char-to-string
           (+ (- ?１ 1)
              (1+ (if num
                      (- (string-to-char (substring num 1)) ?０)
                    0))))))

(defun org-numbering--white-circled-format (num)
  "Format white circled number NUM."
  num)

(defun org-numbering--white-circled-parse (str)
  "Parse white circled number from STR."
  (when (string-match "^○\\([０-９]+\\)$" str)
    (format "○%s" (match-string 1 str))))

(defun org-numbering--white-circled-validate (num)
  "Validate white circled number NUM."
  (and (stringp num)
       (string-match "^○[０-９]+$" num)))

;;;; Register Default Schemes

;; Register decimal scheme
(org-numbering-register-scheme 'decimal
                              :increment-fn #'org-numbering--decimal-increment
                              :format-fn #'org-numbering--decimal-format
                              :parse-fn #'org-numbering--decimal-parse
                              :validate-fn #'org-numbering--decimal-validate)

;; Register alpha scheme
(org-numbering-register-scheme 'alpha
                              :increment-fn #'org-numbering--alpha-increment
                              :format-fn #'org-numbering--alpha-format
                              :parse-fn #'org-numbering--alpha-parse
                              :validate-fn #'org-numbering--alpha-validate)

;; Register roman scheme
(org-numbering-register-scheme 'roman
                              :increment-fn #'org-numbering--roman-increment
                              :format-fn #'org-numbering--roman-format
                              :parse-fn #'org-numbering--roman-parse
                              :validate-fn #'org-numbering--roman-validate)

;; Register chinese scheme
(org-numbering-register-scheme 'chinese
                              :increment-fn #'org-numbering--chinese-increment
                              :format-fn #'org-numbering--chinese-format
                              :parse-fn #'org-numbering--chinese-parse
                              :validate-fn #'org-numbering--chinese-validate)

;; Register uppercase alpha scheme
(org-numbering-register-scheme 'upper-alpha
                              :increment-fn #'org-numbering--upper-alpha-increment
                              :format-fn #'org-numbering--upper-alpha-format
                              :parse-fn #'org-numbering--upper-alpha-parse
                              :validate-fn #'org-numbering--upper-alpha-validate)

;; Register circled number scheme
(org-numbering-register-scheme 'circled
                              :increment-fn #'org-numbering--circled-increment
                              :format-fn #'org-numbering--circled-format
                              :parse-fn #'org-numbering--circled-parse
                              :validate-fn #'org-numbering--circled-validate)

;; Register parenthesized letter scheme
(org-numbering-register-scheme 'parenthesized
                              :increment-fn #'org-numbering--parenthesized-increment
                              :format-fn #'org-numbering--parenthesized-format
                              :parse-fn #'org-numbering--parenthesized-parse
                              :validate-fn #'org-numbering--parenthesized-validate)

;; Register bullet scheme
(org-numbering-register-scheme 'bullet
                              :increment-fn #'org-numbering--bullet-increment
                              :format-fn #'org-numbering--bullet-format
                              :parse-fn #'org-numbering--bullet-parse
                              :validate-fn #'org-numbering--bullet-validate)

;; Register dash scheme
(org-numbering-register-scheme 'dash
                              :increment-fn #'org-numbering--dash-increment
                              :format-fn #'org-numbering--dash-format
                              :parse-fn #'org-numbering--dash-parse
                              :validate-fn #'org-numbering--dash-validate)

;; Register square scheme
(org-numbering-register-scheme 'square
                              :increment-fn #'org-numbering--square-increment
                              :format-fn #'org-numbering--square-format
                              :parse-fn #'org-numbering--square-parse
                              :validate-fn #'org-numbering--square-validate)

;; Register Greek letter scheme
(org-numbering-register-scheme 'greek
                              :increment-fn #'org-numbering--greek-increment
                              :format-fn #'org-numbering--greek-format
                              :parse-fn #'org-numbering--greek-parse
                              :validate-fn #'org-numbering--greek-validate)

;; Register parenthesized number scheme
(org-numbering-register-scheme 'paren-num
                              :increment-fn #'org-numbering--paren-num-increment
                              :format-fn #'org-numbering--paren-num-format
                              :parse-fn #'org-numbering--paren-num-parse
                              :validate-fn #'org-numbering--paren-num-validate)

;; Register parenthesized roman scheme
(org-numbering-register-scheme 'paren-roman
                              :increment-fn #'org-numbering--paren-roman-increment
                              :format-fn #'org-numbering--paren-roman-format
                              :parse-fn #'org-numbering--paren-roman-parse
                              :validate-fn #'org-numbering--paren-roman-validate)

;; Register katakana scheme
(org-numbering-register-scheme 'katakana
                              :increment-fn #'org-numbering--katakana-increment
                              :format-fn #'org-numbering--katakana-format
                              :parse-fn #'org-numbering--katakana-parse
                              :validate-fn #'org-numbering--katakana-validate)

;; Register iroha scheme
(org-numbering-register-scheme 'iroha
                              :increment-fn #'org-numbering--iroha-increment
                              :format-fn #'org-numbering--iroha-format
                              :parse-fn #'org-numbering--iroha-parse
                              :validate-fn #'org-numbering--iroha-validate)

;; Register chapter scheme
(org-numbering-register-scheme 'chapter
                              :increment-fn #'org-numbering--chapter-increment
                              :format-fn #'org-numbering--chapter-format
                              :parse-fn #'org-numbering--chapter-parse
                              :validate-fn #'org-numbering--chapter-validate)

;; Register paren-chinese scheme
(org-numbering-register-scheme 'paren-chinese
                              :increment-fn #'org-numbering--paren-chinese-increment
                              :format-fn #'org-numbering--paren-chinese-format
                              :parse-fn #'org-numbering--paren-chinese-parse
                              :validate-fn #'org-numbering--paren-chinese-validate)

;; Register extended-circled scheme
(org-numbering-register-scheme 'extended-circled
                              :increment-fn #'org-numbering--extended-circled-increment
                              :format-fn #'org-numbering--extended-circled-format
                              :parse-fn #'org-numbering--extended-circled-parse
                              :validate-fn #'org-numbering--extended-circled-validate)

;; Register white-circled scheme
(org-numbering-register-scheme 'white-circled
                              :increment-fn #'org-numbering--white-circled-increment
                              :format-fn #'org-numbering--white-circled-format
                              :parse-fn #'org-numbering--white-circled-parse
                              :validate-fn #'org-numbering--white-circled-validate)

(provide 'org-numbering-schemes)

;;; org-numbering-schemes.el ends here 