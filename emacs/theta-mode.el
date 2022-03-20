(defconst theta-syntax-table
  (let ((table (make-syntax-table)))

    ;; _ is a word character in Theta
    (modify-syntax-entry ?_ "w" table)

    ;; Brackets
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Comments
    (modify-syntax-entry ?\/ ". 124b" table) ;; // starts comments
    (modify-syntax-entry ?\n "> b" table) ;; \n ends comments
    (modify-syntax-entry ?* ". 23a" table) ;; * starts/ends comments in /* and */

    table))

                                        ; Syntax Highlighting
(defconst theta-version-keywords
  '("language-version" "avro-version")
  "Keywords that specify version constraints in Theta module headers.")

(defconst theta-definition-keywords
  '("type" "alias" "enum")
  "Theta keywords that define new type names.")

(defconst theta-primitive-types
  '("Bool"
    "Bytes"
    "Int"
    "Long"
    "Float"
    "Double"
    "String"
    "Date"
    "Datetime"
    "UUID")

  "Types that are built into Theta, indexed by the version of
  Theta they were introduced in.")

(defconst theta-doc-line-comment
  '(: "///" (0+ not-newline) line-end)
  "`rx' expression for Theta line documentation comments ('///
...')")

(defconst theta-doc-block-comment
  '(: "/**" (0+ (| (not "*") (: "*" (not "/")))) (| "*/" string-end))
  "`rx' expression for Theta block documentation comments ('/**
... */').")

(defconst theta-font-lock-version-constraints
  (list
   (rx-to-string
    `(: (group-n 1 (or ,@theta-version-keywords))
        (0+ space)
        ":"
        (0+ space)
        (group-n 2 (: (1+ digit) "." (1+ digit) "." (1+ digit)))))
   '(1 font-lock-keyword-face)
   '(2 font-lock-constant-face)))

(defconst theta-font-lock-definitions
  (list
   (rx-to-string
    `(: (group-n 1 (or ,@theta-definition-keywords))
        (1+ space)
        (group-n 2 (1+ word))))
   '(1 font-lock-keyword-face)
   '(2 font-lock-type-face)))

(defconst theta-font-lock-field-definitions
  (list
   (rx-to-string
    `(: (group-n 1 (1+ word))
        (0+ space)
        ":"))
   '(1 font-lock-function-name-face)))

(defconst theta-font-lock-imports
  (list
   (rx-to-string
    `(: (group-n 1 "import")
        (1+ space)
        (group-n 2 (: (0+ (: (1+ word) ".")) (1+ word)))))
   '(1 font-lock-keyword-face)
   '(2 font-lock-reference-face)))

(defconst theta-font-lock-builtin
  (list
   (rx-to-string
    `(: word-start (or ,@theta-primitive-types) word-end))
   '(0 font-lock-builtin-face)))

(defconst theta-font-lock-doc
  (list
   (rx-to-string
    `(| ,theta-doc-line-comment ,theta-doc-block-comment))
   '(0 font-lock-doc-face t)))

(defconst theta-font-lock-keywords
  (list theta-font-lock-version-constraints
        theta-font-lock-definitions
        theta-font-lock-field-definitions
        theta-font-lock-imports
        theta-font-lock-builtin
        theta-font-lock-doc))

                                        ; Indentation
(defcustom theta-indent-level 4
  "The indentation level for Theta expressions.")

(require 'smie)

;; Simple grammar for Theta modules
(defconst theta-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)

      (version (version-keyword ":" version-number))
      (version-keyword ("language-version")
                       ("avro-version"))
      (version-separator (version "---" statement))
      (version-number)

      (statement
       ("import" id)
       ("type" type-definition)
       ("enum" type-definition)
       ("alias" alias-definition))

      (type-definition (id "=" type-body))
      (alias-definition (id "=" id))

      (type-body (type-body "|" type-body)
                 ("{" fields "}"))

      (fields (fields "," fields)
              (id ":" id)))

    '((assoc ":"))
    '((assoc ","))
    '((assoc "|"))
    '((assoc "=")))))

(defun theta-indentation-rules (kind token)
  (pcase (cons kind token)
    ;; The "---" token is used as a bit of a hack to force no
    ;; indentation on the line.
    (`(:elem . empty-line-token)
     (cond
      ((theta-point-in-record-p) ",")
      ((theta-point-in-variant-p) "|")
      (t "---")))

    ;; The module header should not be indented at all.
    (`(:before . "avro-version") '(column . 0))
    (`(:before . "language-version") '(column . 0))
    (`(:before . "---") '(column . 0))

    ;; Top-level keywords never need to be indented (declarations like
    ;; `type Foo = ...` always start at the beginning of the line)
    (`(:before . "type") '(column . 0))
    (`(:before . "enum") '(column . 0))
    (`(:before . "alias") '(column . 0))
    (`(:before . "import") '(column . 0))

    ;; Introduce exactly one extra level of indentation after the =
    ;; in `type Foo = ...` and `alias Foo = ...`
    (`(:after . "=") theta-indent-level)
    (`(:before . "=")
     (if (smie-rule-hanging-p) 0 theta-indent-level))

    ;; On lines like `type Foo = {`, don't introduce an extra level of
    ;; indentation on the next line. (One level is introduced by the =
    ;; already.)
    (`(:before . "{")
     (if (smie-rule-hanging-p) (smie-rule-parent)))

    ;; Align fields and variant cases to each other.
    (`(:before . "|") (smie-rule-separator kind))
    (`(:before . ",") (smie-rule-separator kind))))

(defun theta-point-in-record-p ()
    "Returns whether the point is currently inside a record
    body (ie the field definitions between { and }).

    This includes both records and the record bodies of variant
    cases."

    (condition-case nil
        (save-excursion
          (re-search-backward "[{}]")
          (eq (char-after) ?{))
      (error nil)))

(defun theta-previous-definition ()
  "Returns the character location of the definition (type/alias
  Foo = ...) that comes before the current point. If there is no
  definition before the point, returns 0."
  (condition-case nil
      (save-excursion
        (search-backward "=")
        (point))
    (error 0)))

(defun theta-previous-variant-definition ()
  "Returns the character location of the variant definition (type
  Foo = Bar {...}) that comes before the current point. If there
  is no definition before the point, returns 0."
  (condition-case nil
      (save-excursion
        (re-search-backward "=[[:space:]\n]*\\w+[[:space:]\n]*{")
        (point))
    (error 0)))

(defun theta-point-in-variant-p ()
    "Returns whether the point is currently either between or
    right after the case definitions in a variant."
    (let ((previous-variant (theta-previous-variant-definition)))
      (and (not (eq previous-variant 0))
           (not (theta-point-in-record-p))
           (eq (theta-previous-definition) previous-variant))))

                                        ; Mode Definition
(define-derived-mode theta-mode prog-mode "Θ"
  "A mode for working with Theta schema defintions."
  :syntax-table theta-syntax-table

  (set (make-local-variable 'font-lock-defaults) '(theta-font-lock-keywords))
  (set (make-local-variable 'comment-start) "//")

  ;; Hack to fix "(void-variable smie--parent)" errors SMIE was
  ;; generating on calling `smie-rule-parent-p`-style functions.
  (set (make-local-variable 'smie--parent) nil)
  (set (make-local-variable 'smie--after) nil)
  (set (make-local-variable 'smie--token) nil)

  (font-lock-fontify-buffer)

  (smie-setup theta-grammar 'theta-indentation-rules))

(provide 'theta-mode)
