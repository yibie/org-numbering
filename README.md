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

2. German Style:
```elisp
(setq org-numbering-level-scheme
      '((1 . ((scheme . decimal)      ; 1.
              (combine . nil)))
        (2 . ((scheme . upper-alpha)  ; A.
              (combine . nil)))
        (3 . ((scheme . alpha)        ; a)
              (combine . nil)))
        (4 . ((scheme . greek)        ; α)
              (combine . nil)))
        (5 . ((scheme . dash)         ; -
              (combine . nil)))))
```

3. Chinese Academic Style:
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

4. Japanese Document Style:
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

## Customization

### Separator for Combined Numbers

You can customize the separator used in combined numbers:
```elisp
(setq org-numbering-separator ".")  ; Default is "."
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Author

Yibie (yibie@outlook.com) 