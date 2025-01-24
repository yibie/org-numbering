# org-numbering

A flexible and customizable numbering system for Org mode headings, supporting various international numbering schemes.

![org-numbering](https://github.com/yibie/org-numbering/blob/main/assets/figure1.gif)

## Features

- Multiple numbering schemes:
  - Basic: Decimal (1, 2, 3...), Alphabetic (a, b, c...), Roman numerals (I, II, III...)
  - CJK: Chinese numerals (一、二、三...), Katakana (ア、イ、ウ...), Iroha order (イ、ロ、ハ...)
  - Symbols: Circled numbers (①, ②, ③...), Bullet points (•), Squares (□)
  - Special: Greek letters (α, β, γ...), Parenthesized formats ((1), (i), （一）)
  - Extended: Chapter style (第一章), White circled (○１)

- Flexible configuration:
  - Independent numbering scheme for each heading level
  - Support for combined numbering (e.g., 1.1, 1.1.1)
  - Customizable separator for combined numbers

## Installation

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/yibie/org-numbering.git
   ```

2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/org-numbering")
   (require 'org-numbering)
   ```

## Usage

### Basic Commands

- `M-x org-numbering-number`: Apply numbering based on context
  - On a heading: Number the current subtree
  - With active region: Number headings in the region
  - Blank line: Number all headings in the buffer

### Configuration

The package provides two main customization variables:

#### `org-numbering-level-scheme`

This variable defines the numbering scheme for each heading level. It's an alist where:
- Key: The heading level (1 for top level, 2 for second level, etc.)
- Value: An alist with two properties:
  - `scheme`: The numbering scheme to use (see Available Numbering Schemes below)
  - `combine`: Whether to combine with parent numbering
    - `t`: Use combined numbering (e.g., "1.1", "1.1.1")
    - `nil`: Use independent numbering (e.g., "1", "1", "1")

Example with explanation:
```elisp
(setq org-numbering-level-scheme
      '((1 . ((scheme . decimal)      ; Level 1: Use decimal (1, 2, 3...)
              (combine . nil)))       ; Don't combine with parent (no parent exists)
        (2 . ((scheme . decimal)      ; Level 2: Use decimal
              (combine . t)))         ; Combine with parent -> becomes 1.1, 1.2...
        (3 . ((scheme . alpha)        ; Level 3: Use lowercase letters
              (combine . nil)))       ; Don't combine -> just a, b, c...
        (4 . ((scheme . roman)        ; Level 4: Use Roman numerals
              (combine . t)))))       ; Combine -> becomes 1.1.a.i
```

This configuration would result in:
```org
* 1. First Level
** 1.1 Second Level
*** a. Third Level
**** 1.1.a.i Fourth Level
```

#### `org-numbering-separator`

Defines the separator used in combined numbers. Default is "." (period).

Examples:
```elisp
(setq org-numbering-separator ".")    ; 1.1.1
(setq org-numbering-separator "-")    ; 1-1-1
(setq org-numbering-separator " ")    ; 1 1 1
```

### Configuration Examples

1. Academic Paper Style (English):
```elisp
(setq org-numbering-level-scheme
      '((1 . ((scheme . decimal)      ; (scheme . decimal) means decimal numbering
              (combine . nil)))       ; <= here `nil` means not combine
        (2 . ((scheme . decimal)      ; 1.1
              (combine . t)))         ; <= here `t` means combine
        (3 . ((scheme . decimal)      ; 1.1.1
              (combine . t)))         ; <= here `t` means combine
        (4 . ((scheme . alpha)        ; a)
              (combine . nil)))
        (5 . ((scheme . paren-num)    ; (1)
              (combine . nil)))))
```


2. Chinese Academic Style:
```elisp
(setq org-numbering-level-scheme
      '((1 . ((scheme . chapter)      ; 第一章
              (combine . nil)))
        (2 . ((scheme . decimal)      ; 1.1
              (combine . t)))
        (3 . ((scheme . paren-chinese) ; （一）
              (combine . nil)))
        (4 . ((scheme . extended-circled) ; ⑴
              (combine . nil)))
        (5 . ((scheme . decimal)       ; 1.
              (combine . nil)))))
```

3. Japanese Document Style:
```elisp
(setq org-numbering-level-scheme
      '((1 . ((scheme . decimal)      ; 1.
              (combine . nil)))
        (2 . ((scheme . katakana)     ; ア、
              (combine . nil)))
        (3 . ((scheme . circled)      ; ①
              (combine . nil)))
        (4 . ((scheme . iroha)        ; イ、
              (combine . nil)))
        (5 . ((scheme . square)       ; □
              (combine . nil)))))
```

### Available Numbering Schemes

| Scheme Name | Example | Description |
|-------------|---------|-------------|
| decimal | 1, 2, 3 | Arabic numerals |
| alpha | a, b, c | Lowercase letters |
| roman | I, II, III | Roman numerals |
| chinese | 一、二、三 | Chinese numerals |
| upper-alpha | A, B, C | Uppercase letters |
| circled | ①, ②, ③ | Circled numbers |
| parenthesized | ⒜, ⒝, ⒞ | Parenthesized letters |
| bullet | • | Bullet points |
| dash | - | Dashes |
| square | □ | Squares |
| greek | α, β, γ | Greek letters |
| paren-num | (1), (2), (3) | Parenthesized numbers |
| paren-roman | (i), (ii), (iii) | Parenthesized roman |
| katakana | ア、イ、ウ | Katakana |
| iroha | イ、ロ、ハ | Iroha order |
| chapter | 第一章 | Chapter style |
| paren-chinese | （一）、（二） | Parenthesized Chinese |
| extended-circled | ⑴, ⑵, ⑶ | Extended circled |
| white-circled | ○１、○２ | White circled |

### Tricks

#### Switch between different numbering schemes

You can define different numbering schemes and switch between them in your configuration:

```
(defvar my/org-numbering-academic-scheme
  '((1 . ((scheme . decimal)      ; 1.
          (combine . nil)))
    (2 . ((scheme . decimal)      ; 1.1
          (combine . t)))
    (3 . ((scheme . decimal)      ; 1.1.1
          (combine . t)))
    (4 . ((scheme . alpha)        ; a)
          (combine . nil)))
    (5 . ((scheme . paren-num)    ; (1)
          (combine . nil))))
  "Academic paper style numbering scheme.")

(defvar my/org-numbering-chinese-scheme
  '((1 . ((scheme . chapter)      ; 第一章
          (combine . nil)))
    (2 . ((scheme . decimal)      ; 1.1
          (combine . t)))
    (3 . ((scheme . paren-chinese) ; （一）
          (combine . nil)))
    (4 . ((scheme . extended-circled) ; ⑴
          (combine . nil)))
    (5 . ((scheme . white-circled)   ; ○１、
          (combine . nil))))
  "Chinese academic style numbering scheme.")

(defun my/org-numbering-toggle-scheme ()
  "Toggle between academic and Chinese numbering schemes."
  (interactive)
  (if (equal org-numbering-level-scheme my/org-numbering-academic-scheme)
      (progn 
        (setq org-numbering-level-scheme my/org-numbering-chinese-scheme)
        (message "Switched to Chinese numbering scheme"))
    (setq org-numbering-level-scheme my/org-numbering-academic-scheme)
    (message "Switched to Academic numbering scheme")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n t") #'my/org-numbering-toggle-scheme))
```


### Add Your Own Numbering Scheme

You can create your own numbering scheme by registering it with `org-numbering-register-scheme`. Each scheme needs four functions:

1. `increment-fn`: Generate the next number in sequence
2. `format-fn`: Format the number for display
3. `parse-fn`: Parse a number from string
4. `validate-fn`: Validate a number

Here's a complete example of adding a custom scheme that uses emoji numbers (1️⃣, 2️⃣, 3️⃣...):

```elisp
;; Define the emoji mapping
(defconst my-emoji-numbers
  '((1 . "1️⃣") (2 . "2️⃣") (3 . "3️⃣") (4 . "4️⃣") (5 . "5️⃣")
    (6 . "6️⃣") (7 . "7️⃣") (8 . "8️⃣") (9 . "9️⃣") (10 . "🔟"))
  "Mapping between numbers and emoji numbers.")

;; Increment function: get next number
(defun my-emoji-increment (current)
  "Get next emoji number after CURRENT.
If CURRENT is nil, start from 1️⃣."
  (let* ((num (if current
                  (car (rassoc current my-emoji-numbers))
                0))
         (next-num (1+ num)))
    (if (> next-num 10)
        "1️⃣"  ; Wrap around after 🔟
      (cdr (assq next-num my-emoji-numbers)))))

;; Format function: just return the emoji
(defun my-emoji-format (emoji)
  "Format emoji number EMOJI."
  emoji)

;; Parse function: convert string to emoji
(defun my-emoji-parse (str)
  "Parse emoji number from STR."
  (when (string-match "^[1-9️⃣🔟]$" str)
    str))

;; Validate function: check if it's a valid emoji number
(defun my-emoji-validate (emoji)
  "Validate emoji number EMOJI."
  (and (stringp emoji)
       (rassoc emoji my-emoji-numbers)))

;; Register the new scheme
(org-numbering-register-scheme 'emoji
                              :increment-fn #'my-emoji-increment
                              :format-fn #'my-emoji-format
                              :parse-fn #'my-emoji-parse
                              :validate-fn #'my-emoji-validate)

;; Add format style for the new scheme
(add-to-list 'org-numbering-format-style
             '(emoji . "%s"))  ; No extra formatting needed

;; Use it in your configuration
(setq org-numbering-level-scheme
      '((1 . ((scheme . emoji)        ; 1️⃣
              (combine . nil)))))
```

Each function serves a specific purpose:

- `increment-fn`: Takes the current number and returns the next one in sequence
  - Input: Current number (or nil for first number)
  - Output: Next number in sequence

- `format-fn`: Formats the number for display
  - Input: Number to format
  - Output: String to display

- `parse-fn`: Converts a string back to a number
  - Input: String to parse
  - Output: Number if valid, nil if invalid

- `validate-fn`: Checks if a number is valid for this scheme
  - Input: Number to validate
  - Output: t if valid, nil if invalid

After registering your scheme, you can use it just like any built-in scheme in your `org-numbering-level-scheme` configuration.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Author

Yibie (yibie@outlook.com) 