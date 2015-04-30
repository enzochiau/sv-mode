;; ;;; sv.el --- editing sv source files under emacs  -*- lexical-binding: t; -*-

;; ;; Copyright (C) 1997, 2001-2015 Free Software Foundation, Inc.

;; ;; Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
;; ;;	   John Eaton <jwe@sv.org>
;; ;; Maintainer: emacs-devel@gnu.org
;; ;; Keywords: languages

;; ;; This file is part of GNU Emacs.

;; ;; GNU Emacs is free software: you can redistribute it and/or modify
;; ;; it under the terms of the GNU General Public License as published by
;; ;; the Free Software Foundation, either version 3 of the License, or
;; ;; (at your option) any later version.

;; ;; GNU Emacs is distributed in the hope that it will be useful,
;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; ;; GNU General Public License for more details.

;; ;; You should have received a copy of the GNU General Public License
;; ;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; ;;; Commentary:

;; ;; This package provides Emacs support for Sv.  It defines a major
;; ;; mode for editing Sv code and contains code for interacting with
;; ;; an inferior Sv process using comint.

;; ;; See the documentation of `sv-mode' and `run-sv' for further
;; ;; information on usage and customization.

;; ;;; Code:
;; (require 'comint)

;; ;;; For emacs < 24.3.
;; (require 'newcomment)
;; (eval-and-compile
;;   (unless (fboundp 'user-error)
;;     (defalias 'user-error 'error))
;;   (unless (fboundp 'delete-consecutive-dups)
;;     (defalias 'delete-consecutive-dups 'delete-dups))
;;   (unless (fboundp 'completion-table-with-cache)
;;     (defun completion-table-with-cache (fun &optional ignore-case)
;;       ;; See eg bug#11906.
;;       (let* (last-arg last-result
;;              (new-fun
;;               (lambda (arg)
;;                 (if (and last-arg (string-prefix-p last-arg arg ignore-case))
;;                     last-result
;;                   (prog1
;;                       (setq last-result (funcall fun arg))
;;                     (setq last-arg arg))))))
;;         (completion-table-dynamic new-fun)))))
;; (eval-when-compile
;;   (unless (fboundp 'setq-local)
;;     (defmacro setq-local (var val)
;;       "Set variable VAR to value VAL in current buffer."
;;       (list 'set (list 'make-local-variable (list 'quote var)) val))))

;; (defgroup sv nil
;;   "Editing Sv code."
;;   :link '(custom-manual "(sv-mode)Top")
;;   :link '(url-link "http://www.gnu.org/s/sv")
;;   :link '(custom-group-link :tag "Font Lock Face7s group" font-lock-faces)
;;   :group 'languages)

;; (define-obsolete-function-alias 'sv-submit-bug-report
;;   'report-emacs-bug "24.4")

;; (define-abbrev-table 'sv-abbrev-table nil
;;   "Abbrev table for Sv's reserved words.
;; Used in `sv-mode' and `inferior-sv-mode' buffers.")

(defvar sv-comment-char ?#
  "Character to start an Sv comment.")

(defvar sv-comment-start (char-to-string sv-comment-char)
  "Sv-specific `comment-start' (which see).")

(defvar sv-comment-start-skip "\\(^\\|\\S<\\)\\(?:%!\\|\\s<+\\)\\s-*"
  "Sv-specific `comment-start-skip' (which see).")

;; (defvar sv-begin-keywords
;;   '("classdef" "do" "enumeration" "events" "for" "function" "if" "methods"
;;     "parfor" "properties" "switch" "try" "unwind_protect" "while"))

;; (defvar sv-else-keywords
;;   '("case" "catch" "else" "elseif" "otherwise" "unwind_protect_cleanup"))

;; (defvar sv-end-keywords
;;   '("endclassdef" "endenumeration" "endevents" "endfor" "endfunction" "endif"
;;     "endmethods" "endparfor" "endproperties" "endswitch" "end_try_catch"
;;     "end_unwind_protect" "endwhile" "until" "end"))

;; (defvar sv-reserved-words
;;   (append sv-begin-keywords
;; 	  sv-else-keywords
;; 	  sv-end-keywords
;; 	  '("break" "continue" "global" "persistent" "return"))
;;   "Reserved words in Sv.")

;; (defvar sv-function-header-regexp
;;   (concat "^\\s-*\\_<\\(function\\)\\_>"
;; 	  "\\([^=;(\n]*=[ \t]*\\|[ \t]*\\)\\(\\(?:\\w\\|\\s_\\)+\\)\\_>")
;;   "Regexp to match an Sv function header.
;; The string `function' and its name are given by the first and third
;; parenthetical grouping.")

;; 
;; (defvar sv-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\M-."     'sv-find-definition)
;;     (define-key map "\M-\C-j"  'sv-indent-new-comment-line)
;;     (define-key map "\C-c\C-p" 'sv-previous-code-line)
;;     (define-key map "\C-c\C-n" 'sv-next-code-line)
;;     (define-key map "\C-c\C-a" 'sv-beginning-of-line)
;;     (define-key map "\C-c\C-e" 'sv-end-of-line)
;;     (define-key map [remap down-list] 'smie-down-list)
;;     (define-key map "\C-c\M-\C-h" 'sv-mark-block)
;;     (define-key map "\C-c]" 'smie-close-block)
;;     (define-key map "\C-c/" 'smie-close-block)
;;     (define-key map "\C-c;" 'sv-update-function-file-comment)
;;     (define-key map "\C-hd" 'sv-help)
;;     (define-key map "\C-ha" 'sv-lookfor)
;;     (define-key map "\C-c\C-l" 'sv-source-file)
;;     (define-key map "\C-c\C-f" 'sv-insert-defun)
;;     (define-key map "\C-c\C-il" 'sv-send-line)
;;     (define-key map "\C-c\C-ib" 'sv-send-block)
;;     (define-key map "\C-c\C-if" 'sv-send-defun)
;;     (define-key map "\C-c\C-ir" 'sv-send-region)
;;     (define-key map "\C-c\C-ia" 'sv-send-buffer)
;;     (define-key map "\C-c\C-is" 'sv-show-process-buffer)
;;     (define-key map "\C-c\C-iq" 'sv-hide-process-buffer)
;;     (define-key map "\C-c\C-ik" 'sv-kill-process)
;;     (define-key map "\C-c\C-i\C-l" 'sv-send-line)
;;     (define-key map "\C-c\C-i\C-b" 'sv-send-block)
;;     (define-key map "\C-c\C-i\C-f" 'sv-send-defun)
;;     (define-key map "\C-c\C-i\C-r" 'sv-send-region)
;;     (define-key map "\C-c\C-i\C-a" 'sv-send-buffer)
;;     (define-key map "\C-c\C-i\C-s" 'sv-show-process-buffer)
;;     (define-key map "\C-c\C-i\C-q" 'sv-hide-process-buffer)
;;     (define-key map "\C-c\C-i\C-k" 'sv-kill-process)
;;     map)
;;   "Keymap used in Sv mode.")



;; (easy-menu-define sv-mode-menu sv-mode-map
;;   "Menu for Sv mode."
;;   '("Sv"
;;     ["Split Line at Point"          sv-indent-new-comment-line t]
;;     ["Previous Code Line"           sv-previous-code-line t]
;;     ["Next Code Line"               sv-next-code-line t]
;;     ["Begin of Line"                sv-beginning-of-line t]
;;     ["End of Line"                  sv-end-of-line t]
;;     ["Mark Block"                   sv-mark-block t]
;;     ["Close Block"                  smie-close-block t]
;;     "---"
;;     ["Start Sv Process"         run-sv t]
;;     ["Documentation Lookup"         info-lookup-symbol t]
;;     ["Help on Function"             sv-help t]
;;     ["Search help"                  sv-lookfor t]
;;     ["Find Function Definition"     sv-find-definition t]
;;     ["Insert Function"              sv-insert-defun t]
;;     ["Update Function File Comment" sv-update-function-file-comment t]
;;     "---"
;;     ["Function Syntax Hints" (eldoc-mode 'toggle)
;;      :style toggle :selected (bound-and-true-p eldoc-mode)
;;      :help "Display function signatures after typing `SPC' or `('"]
;;     ["Delimiter Matching"           show-paren-mode
;;      :style toggle :selected show-paren-mode
;;      :help "Highlight matched pairs such as `if ... end'"
;;      :visible (fboundp 'smie--matching-block-data)]
;;     ["Auto Fill"                    auto-fill-mode
;;      :style toggle :selected auto-fill-function
;;      :help "Automatic line breaking"]
;;     ["Electric Layout"              electric-layout-mode
;;      :style toggle :selected electric-layout-mode
;;      :help "Automatically insert newlines around some chars"]
;;     "---"
;;     ("Debug"
;;      ["Send Current Line"       sv-send-line t]
;;      ["Send Current Block"      sv-send-block t]
;;      ["Send Current Function"   sv-send-defun t]
;;      ["Send Region"             sv-send-region t]
;;      ["Send Buffer"             sv-send-buffer t]
;;      ["Source Current File"     sv-source-file t]
;;      ["Show Process Buffer"     sv-show-process-buffer t]
;;      ["Hide Process Buffer"     sv-hide-process-buffer t]
;;      ["Kill Process"            sv-kill-process t])
;;     "---"
;;     ["Sv Mode Manual"       (info "(sv-mode)Top") t]
;;     ["Customize Sv"         (customize-group 'sv) t]
;;     ["Submit Bug Report"        report-emacs-bug t]))

;; (defvar sv-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     (modify-syntax-entry ?\r " "  table)
;;     (modify-syntax-entry ?+ "."   table)
;;     (modify-syntax-entry ?- "."   table)
;;     (modify-syntax-entry ?= "."   table)
;;     (modify-syntax-entry ?* "."   table)
;;     (modify-syntax-entry ?/ "."   table)
;;     (modify-syntax-entry ?> "."   table)
;;     (modify-syntax-entry ?< "."   table)
;;     (modify-syntax-entry ?& "."   table)
;;     (modify-syntax-entry ?| "."   table)
;;     (modify-syntax-entry ?! "."   table)
;;     (modify-syntax-entry ?\\ "."  table)
;;     (modify-syntax-entry ?\' "."  table)
;;     (modify-syntax-entry ?\` "."  table)
;;     (modify-syntax-entry ?. "."   table)
;;     (modify-syntax-entry ?\" "\"" table)
;;     (modify-syntax-entry ?_ "_"   table)
;;     ;; The "b" flag only applies to the second letter of the comstart
;;     ;; and the first letter of the comend, i.e. the "4b" below is ineffective.
;;     ;; If we try to put `b' on the single-line comments, we get a similar
;;     ;; problem where the % and # chars appear as first chars of the 2-char
;;     ;; comend, so the multi-line ender is also turned into style-b.
;;     ;; So we need the new "c" comment style.
;;     (modify-syntax-entry ?\% "< 13"  table)
;;     (modify-syntax-entry ?\# "< 13"  table)
;;     (modify-syntax-entry ?\{ "(} 2c"  table)
;;     (modify-syntax-entry ?\} "){ 4c"  table)
;;     (modify-syntax-entry ?\n ">"  table)
;;     table)
;;   "Syntax table in use in `sv-mode' buffers.")

;; (defcustom sv-font-lock-texinfo-comment t
;;   "Control whether to highlight the texinfo comment block."
;;   :type 'boolean
;;   :group 'sv
;;   :version "24.4")

;; (defcustom sv-blink-matching-block t
;;   "Control the blinking of matching Sv block keywords.
;; Non-nil means show matching begin of block when inserting a space,
;; newline or semicolon after an else or end keyword."
;;   :type 'boolean
;;   :group 'sv)

(defcustom sv-block-offset 2
  "Extra indentation applied to statements in Sv block structures."
  :type 'integer
  :group 'sv)

;; (defvar sv-block-comment-start
;;   (concat (make-string 2 sv-comment-char) " ")
;;   "String to insert to start a new Sv comment on an empty line.")

;; (defcustom sv-continuation-offset 4
;;   "Extra indentation applied to Sv continuation lines."
;;   :type 'integer
;;   :group 'sv)

(eval-and-compile
  (defconst sv-continuation-marker-regexp "\\\\\\|\\.\\.\\."))

;; (defvar sv-continuation-regexp
;;   (concat "[^#%\n]*\\(" sv-continuation-marker-regexp
;;           "\\)\\s-*\\(\\s<.*\\)?$"))

;; ;; Char \ is considered a bad decision for continuing a line.
;; (defconst sv-continuation-string "..."
;;   "Character string used for Sv continuation lines.")

;; (defvar sv-mode-imenu-generic-expression
;;   (list
;;    ;; Functions
;;    (list nil sv-function-header-regexp 3))
;;   "Imenu expression for Sv mode.  See `imenu-generic-expression'.")

;; (defcustom sv-mode-hook nil
;;   "Hook to be run when Sv mode is started."
;;   :type 'hook
;;   :group 'sv)

;; (defcustom sv-send-show-buffer t
;;   "Non-nil means display `inferior-sv-buffer' after sending to it."
;;   :type 'boolean
;;   :group 'sv)

;; (defcustom sv-send-line-auto-forward t
;;   "Control auto-forward after sending to the inferior Sv process.
;; Non-nil means always go to the next Sv code line after sending."
;;   :type 'boolean
;;   :group 'sv)

;; (defcustom sv-send-echo-input t
;;   "Non-nil means echo input sent to the inferior Sv process."
;;   :type 'boolean
;;   :group 'sv)


;;; SMIE indentation

(require 'smie)

;; Use '__operators__' in Sv REPL to get a full list.
(defconst sv-operator-table
  '((assoc ";" "\n") (assoc ",") ; The doc claims they have equal precedence!?
    ;; (right "=" "+=" "-=" "*=" "/=")
    ;; (assoc "&&") (assoc "||") ; The doc claims they have equal precedence!?
    ;; (assoc "&") (assoc "|")   ; The doc claims they have equal precedence!?
    ;; (nonassoc "<" "<=" "==" ">=" ">" "!=" "~=")
    ;; (nonassoc ":")                      ;No idea what this is.
    ;; (assoc "+" "-")
    ;; (assoc "*" "/" "\\" ".\\" ".*" "./")
    ;; (nonassoc "'" ".'")
    ;; (nonassoc "++" "--" "!" "~")        ;And unary "+" and "-".
    ;; (right "^" "**" ".^" ".**")
    ;; ;; It's not really an operator, but for indentation purposes it
    ;; ;; could be convenient to treat it as one.
    ;; (assoc "...")
    ))

(defconst sv-smie-bnf-table
  '((atom)
    ;; We can't distinguish the first element in a sequence with
    ;; precedence grammars, so we can't distinguish the condition
    ;; if the `if' from the subsequent body, for example.
    ;; This has to be done later in the indentation rules.
    (exp (exp "\n" exp)
         ;; We need to mention at least one of the operators in this part
         ;; of the grammar: if the BNF and the operator table have
         ;; no overlap, SMIE can't know how they relate.
         (exp ";" exp)
         ("class" exp "endcalss")
         ;; ("try" exp "catch" exp "end_try_catch")
         ;; ("try" exp "catch" exp "end")
         ;; ("unwind_protect" exp
         ;;  "unwind_protect_cleanup" exp "end_unwind_protect")
         ;; ("unwind_protect" exp "unwind_protect_cleanup" exp "end")
         ;; ("for" exp "endfor")
         ;; ("for" exp "end")
         ;; ("parfor" exp "endparfor")
         ;; ("parfor" exp "end")
         ;; ("do" exp "until" atom)
         ("begin" exp "end")
         ("module" exp "endmodule")
         ("while" exp "endwhile")
         ;; ("while" exp "end")
         ;; ("if" exp "endif")
         ;; ("if" exp "else" exp "endif")
         ;; ("if" exp "elseif" exp "else" exp "endif")
         ;; ("if" exp "elseif" exp "elseif" exp "else" exp "endif")
         ;; ("if" exp "elseif" exp "elseif" exp "else" exp "end")
         ;; ("switch" exp "case" exp "endswitch")
         ;; ("switch" exp "case" exp "otherwise" exp "endswitch")
         ;; ("switch" exp "case" exp "case" exp "otherwise" exp "endswitch")
         ;; ("switch" exp "case" exp "case" exp "otherwise" exp "end")
         ("function" exp "endfunction")
         ;; ("function" exp "end")
         ;; ("enumeration" exp "endenumeration")
         ;; ("enumeration" exp "end")
         ;; ("events" exp "endevents")
         ;; ("events" exp "end")
         ("methods" exp "endmethods")
         ;; ("methods" exp "end")
         ;; ("properties" exp "endproperties")
         ;; ("properties" exp "end")
         ;; ("classdef" exp "endclassdef")
         ;; ("classdef" exp "end")
         )
    ;; (fundesc (atom "=" atom))
    ))

(defconst sv-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2 sv-smie-bnf-table
                     '((assoc "\n" ";")))

    (smie-precs->prec2 sv-operator-table))))

;; Tokenizing needs to be refined so that ";;" is treated as two
;; tokens and also so as to recognize the \n separator (and
;; corresponding continuation lines).

(defconst sv-operator-regexp
  (regexp-opt (remove "\n" (apply 'append
                                  (mapcar 'cdr sv-operator-table)))))

(defun sv-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (not (eq (char-before) ?\;)) ;Coalesce ";" and "\n".
           (> pos (line-end-position))
           (if (looking-back sv-continuation-marker-regexp (- (point) 3))
               (progn
                 (goto-char (match-beginning 0))
                 (forward-comment (- (point)))
                 nil)
             t)
           ;; Ignore it if it's within parentheses.
           (let ((ppss (syntax-ppss)))
             (not (and (nth 1 ppss)
                       (eq ?\( (char-after (nth 1 ppss)))))))
      (skip-chars-forward " \t")
      ;; Why bother distinguishing \n and ;?
      ";") ;;"\n"
     ((and (looking-back sv-operator-regexp (- (point) 3) 'greedy)
           ;; Don't mistake a string quote for a transpose.
           (not (looking-back "\\s\"" (1- (point)))))
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     (t
      (smie-default-backward-token)))))

(defun sv-smie-forward-token ()
  (skip-chars-forward " \t")
  (when (looking-at (eval-when-compile
                      (concat "\\(" sv-continuation-marker-regexp
                              "\\)[ \t]*\\($\\|[%#]\\)")))
    (goto-char (match-end 1))
    (forward-comment 1))
  (cond
   ((and (looking-at "[%#\n]")
         (not (or (save-excursion (skip-chars-backward " \t")
                                  ;; Only add implicit ; when needed.
                                  (or (bolp) (eq (char-before) ?\;)))
                  ;; Ignore it if it's within parentheses.
                  (let ((ppss (syntax-ppss)))
                    (and (nth 1 ppss)
                         (eq ?\( (char-after (nth 1 ppss))))))))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ;; Why bother distinguishing \n and ;?
    ";") ;;"\n"
   ((progn (forward-comment (point-max)) nil))
   ((looking-at ";[ \t]*\\($\\|[%#]\\)")
    ;; Combine the ; with the subsequent \n.
    (goto-char (match-beginning 1))
    (forward-comment 1)
    ";")
   ((and (looking-at sv-operator-regexp)
         ;; Don't mistake a string quote for a transpose.
         (not (looking-at "\\s\"")))
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t
    (smie-default-forward-token))))

(defun sv-smie-rules (kind token)
  (pcase (cons kind token)
    ;; We could set smie-indent-basic instead, but that would have two
    ;; disadvantages:
    ;; - changes to sv-block-offset wouldn't take effect immediately.
    ;; - edebug wouldn't show the use of this variable.
    (`(:elem . basic) sv-block-offset)
    ;; Since "case" is in the same BNF rules as switch..end, SMIE by default
    ;; aligns it with "switch".
    (`(:before . "case") (if (not (smie-rule-sibling-p)) sv-block-offset))
    (`(:before . "begin") (if (not (smie-rule-sibling-p)) sv-block-offset))
    (`(:after . ";")
     (cond
      ((smie-rule-parent-p "module" "classdef" "events" "enumeration" "function" "if"
                           "while" "else" "elseif" "for" "parfor" "begin"
                           "properties" "methods" "otherwise" "case"
                           "try" "catch" "unwind_protect"
                           "unwind_protect_cleanup")
       (smie-rule-parent sv-block-offset))
      ((smie-rule-parent-p "class")
       (smie-rule-parent (1+ sv-block-offset)))
      (t nil))
       ;; For (invalid) code between switch and case.
       ;; (if (smie-rule-parent-p "switch") 4)
     ;; nil)
     )))

;; (defun sv-indent-comment ()
;;   "A function for `smie-indent-functions' (which see)."
;;   (save-excursion
;;     (back-to-indentation)
;;     (cond
;;      ((sv-in-string-or-comment-p) nil)
;;      ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}")
;;       0)
;;      ;; Exclude %{, %} and %!.
;;      ((and (looking-at-p "\\s<\\(?:[^{}!]\\|$\\)")
;;            (not (looking-at-p "\\(\\s<\\)\\1")))
;;       (comment-choose-indent)))))

;; 
;; (defvar sv-font-lock-keywords
;;   (list
;;    ;; Fontify all builtin keywords.
;;    (cons (concat "\\_<\\("
;;                  (regexp-opt sv-reserved-words)
;;                  "\\)\\_>")
;;          'font-lock-keyword-face)
;;    ;; Note: 'end' also serves as the last index in an indexing expression.
;;    ;; Ref: http://www.mathworks.com/help/matlab/ref/end.html
;;    (list (lambda (limit)
;;            (while (re-search-forward "\\_<end\\_>" limit 'move)
;;              (let ((beg (match-beginning 0))
;;                    (end (match-end 0)))
;;                (unless (sv-in-string-or-comment-p)
;;                  (condition-case nil
;;                      (progn
;;                        (goto-char beg)
;;                        (backward-up-list)
;;                        (when (memq (char-after) '(?\( ?\[ ?\{))
;;                          (put-text-property beg end 'face nil))
;;                        (goto-char end))
;;                    (error (goto-char end))))))
;;            nil))
;;    ;; Fontify all operators.
;;    (cons sv-operator-regexp 'font-lock-builtin-face)
;;    ;; Fontify all function declarations.
;;    (list sv-function-header-regexp
;;          '(1 font-lock-keyword-face)
;;          '(3 font-lock-function-name-face nil t)))
;;   "Additional Sv expressions to highlight.")

;; (defun sv-syntax-propertize-function (start end)
;;   (goto-char start)
;;   (sv-syntax-propertize-sqs end)
;;   (funcall (syntax-propertize-rules
;;             ("\\\\" (0 (when (eq (nth 3 (save-excursion
;;                                           (syntax-ppss (match-beginning 0))))
;;                                  ?\")
;;                          (string-to-syntax "\\"))))
;;             ;; Try to distinguish the string-quotes from the transpose-quotes.
;;             ("\\(?:^\\|[[({,; ]\\)\\('\\)"
;;              (1 (prog1 "\"'" (sv-syntax-propertize-sqs end)))))
;;            (point) end))

;; (defun sv-syntax-propertize-sqs (end)
;;   "Propertize the content/end of single-quote strings."
;;   (when (eq (nth 3 (syntax-ppss)) ?\')
;;     ;; A '..' string.
;;     (when (re-search-forward
;;            "\\(?:\\=\\|[^']\\)\\(?:''\\)*\\('\\)\\($\\|[^']\\)" end 'move)
;;       (goto-char (match-beginning 2))
;;       (when (eq (char-before (match-beginning 1)) ?\\)
;;         ;; Backslash cannot escape a single quote.
;;         (put-text-property (1- (match-beginning 1)) (match-beginning 1)
;;                            'syntax-table (string-to-syntax ".")))
;;       (put-text-property (match-beginning 1) (match-end 1)
;;                          'syntax-table (string-to-syntax "\"'")))))

;; (defvar electric-layout-rules)

;;;###autoload
(define-derived-mode sv-mode prog-mode "Sv"
  "Major mode for editing Sv code.

Sv is a high-level language, primarily intended for numerical
computations.  It provides a convenient command line interface
for solving linear and nonlinear problems numerically.  Function
definitions can also be stored in files and used in batch mode.

See Info node `(sv-mode) Using Sv Mode' for more details.

Key bindings:
\\{sv-mode-map}"
  ;; :abbrev-table sv-abbrev-table
  :group 'sv

  (smie-setup sv-smie-grammar #'sv-smie-rules
              :forward-token  #'sv-smie-forward-token
              :backward-token #'sv-smie-backward-token)
  (setq-local smie-indent-basic 'sv-block-offset)
  ;; (add-hook 'smie-indent-functions #'sv-indent-comment nil t)

  ;; (setq-local smie-blink-matching-triggers
  ;;             (cons ?\; smie-blink-matching-triggers))
  ;; (unless sv-blink-matching-block
  ;;   (remove-hook 'post-self-insert-hook #'smie-blink-matching-open 'local))

  ;; (setq-local electric-indent-chars
  ;;             (cons ?\; electric-indent-chars))
  ;; ;; IIUC matlab-mode takes the opposite approach: it makes RET insert
  ;; ;; a ";" at those places where it's correct (i.e. outside of parens).
  ;; (setq-local electric-layout-rules '((?\; . after)))

  (setq-local comment-use-syntax t)
  (setq-local comment-start sv-comment-start)
  (setq-local comment-end "")
  (setq-local comment-start-skip sv-comment-start-skip)
  (setq-local comment-add 1)

  ;; (setq-local parse-sexp-ignore-comments t)
  ;; (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
  ;; (setq-local paragraph-separate paragraph-start)
  ;; (setq-local paragraph-ignore-fill-prefix t)
  ;; (setq-local fill-paragraph-function 'sv-fill-paragraph)

  ;; (setq-local fill-nobreak-predicate
  ;;             (lambda () (eq (sv-in-string-p) ?')))
  ;; (with-no-warnings
  ;;   (if (fboundp 'add-function)         ; new in 24.4
  ;;       (add-function :around (local 'comment-line-break-function)
  ;;                     #'sv--indent-new-comment-line)
  ;;     (setq-local comment-line-break-function
  ;;                 (apply-partially #'sv--indent-new-comment-line
  ;;                                  #'comment-indent-new-line))))

  ;; (setq font-lock-defaults '(sv-font-lock-keywords))

  ;; (setq-local syntax-propertize-function #'sv-syntax-propertize-function)

  ;; (setq-local imenu-generic-expression sv-mode-imenu-generic-expression)
  ;; (setq-local imenu-case-fold-search nil)

  ;; (setq-local add-log-current-defun-function #'sv-add-log-current-defun)

  ;; (add-hook 'completion-at-point-functions 'sv-completion-at-point nil t)
  ;; (add-hook 'before-save-hook 'sv-sync-function-file-names nil t)
  ;; (setq-local beginning-of-defun-function 'sv-beginning-of-defun)
  ;; (and sv-font-lock-texinfo-comment (sv-font-lock-texinfo-comment))
  ;; (add-function :before-until (local 'eldoc-documentation-function)
  ;;               'sv-eldoc-function)

  ;;(easy-menu-add sv-mode-menu)
  )

;; 
;; (defcustom inferior-sv-program "sv"
;;   "Program invoked by `inferior-sv'."
;;   :type 'string
;;   :group 'sv)

;; (defcustom inferior-sv-buffer "*Inferior Sv*"
;;   "Name of buffer for running an inferior Sv process."
;;   :type 'string
;;   :group 'sv)

;; (defcustom inferior-sv-prompt
;;   ;; For Sv >= 3.8, default is always 'sv', see
;;   ;; http://hg.savannah.gnu.org/hgweb/sv/rev/708173343c50
;;   "\\(?:^sv\\(?:.bin\\|.exe\\)?\\(?:-[.0-9]+\\)?\\(?::[0-9]+\\)?\\|^debug\\|^\\)>+ "
;;   "Regexp to match prompts for the inferior Sv process."
;;   :type 'regexp
;;   :group 'sv)

;; (defcustom inferior-sv-prompt-read-only comint-prompt-read-only
;;   "If non-nil, the Sv prompt is read only.
;; See `comint-prompt-read-only' for details."
;;   :type 'boolean
;;   :group 'sv
;;   :version "24.4")

;; (defcustom inferior-sv-startup-file
;;   (let ((n (file-name-nondirectory inferior-sv-program)))
;;     (locate-user-emacs-file (format "init_%s.m" n) (format ".emacs-%s" n)))
;;   "Name of the inferior Sv startup file.
;; The contents of this file are sent to the inferior Sv process on
;; startup."
;;   :type '(choice (const :tag "None" nil) file)
;;   :group 'sv
;;   :version "24.4")

;; (defcustom inferior-sv-startup-args '("-i" "--no-line-editing")
;;   "List of command line arguments for the inferior Sv process.
;; For example, for suppressing the startup message and using `traditional'
;; mode, include \"-q\" and \"--traditional\"."
;;   :type '(repeat string)
;;   :group 'sv
;;   :version "24.4")

;; (defcustom inferior-sv-mode-hook nil
;;   "Hook to be run when Inferior Sv mode is started."
;;   :type 'hook
;;   :group 'sv)

;; (defcustom inferior-sv-error-regexp-alist
;;   '(("error:\\s-*\\(.*?\\) at line \\([0-9]+\\), column \\([0-9]+\\)"
;;      1 2 3 2 1)
;;     ("warning:\\s-*\\([^:\n]+\\):.*at line \\([0-9]+\\), column \\([0-9]+\\)"
;;      1 2 3 1 1))
;;   "Value for `compilation-error-regexp-alist' in inferior sv."
;;   :version "24.4"
;;   :type '(repeat (choice (symbol :tag "Predefined symbol")
;;                          (sexp :tag "Error specification")))
;;   :group 'sv)

;; (defvar inferior-sv-compilation-font-lock-keywords
;;   '(("\\_<PASS\\_>" . compilation-info-face)
;;     ("\\_<FAIL\\_>" . compilation-error-face)
;;     ("\\_<\\(warning\\):" 1 compilation-warning-face)
;;     ("\\_<\\(error\\):" 1 compilation-error-face)
;;     ("^\\s-*!!!!!.*\\|^.*failed$" . compilation-error-face))
;;   "Value for `compilation-mode-font-lock-keywords' in inferior sv.")

;; (defvar inferior-sv-process nil)

;; (defvar inferior-sv-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map comint-mode-map)
;;     (define-key map "\M-." 'sv-find-definition)
;;     (define-key map "\t" 'completion-at-point)
;;     (define-key map "\C-hd" 'sv-help)
;;     (define-key map "\C-ha" 'sv-lookfor)
;;     ;; Same as in `shell-mode'.
;;     (define-key map "\M-?" 'comint-dynamic-list-filename-completions)
;;     (define-key map "\C-c\C-l" 'inferior-sv-dynamic-list-input-ring)
;;     (define-key map [menu-bar inout list-history]
;;       '("List Input History" . inferior-sv-dynamic-list-input-ring))
;;     map)
;;   "Keymap used in Inferior Sv mode.")

;; (defvar inferior-sv-mode-syntax-table
;;   (let ((table (make-syntax-table sv-mode-syntax-table)))
;;     table)
;;   "Syntax table in use in `inferior-sv-mode' buffers.")

;; (defvar inferior-sv-font-lock-keywords
;;   (list
;;    (cons inferior-sv-prompt 'font-lock-type-face))
;;   ;; Could certainly do more font locking in inferior Sv ...
;;   "Additional expressions to highlight in Inferior Sv mode.")

;; (defvar inferior-sv-output-list nil)
;; (defvar inferior-sv-output-string nil)
;; (defvar inferior-sv-receive-in-progress nil)

;; (define-obsolete-variable-alias 'inferior-sv-startup-hook
;;   'inferior-sv-mode-hook "24.4")

;; (defvar inferior-sv-dynamic-complete-functions
;;   '(inferior-sv-completion-at-point comint-filename-completion)
;;   "List of functions called to perform completion for inferior Sv.
;; This variable is used to initialize `comint-dynamic-complete-functions'
;; in the Inferior Sv buffer.")

;; (defvar info-lookup-mode)
;; (defvar compilation-error-regexp-alist)
;; (defvar compilation-mode-font-lock-keywords)

;; (declare-function compilation-forget-errors "compile" ())

;; (defun inferior-sv-process-live-p ()
;;   (process-live-p inferior-sv-process))

;; (define-derived-mode inferior-sv-mode comint-mode "Inferior Sv"
;;   "Major mode for interacting with an inferior Sv process.

;; See Info node `(sv-mode) Running Sv from Within Emacs' for more
;; details.

;; Key bindings:
;; \\{inferior-sv-mode-map}"
;;   :abbrev-table sv-abbrev-table
;;   :group 'sv

;;   (setq comint-prompt-regexp inferior-sv-prompt)

;;   (setq-local comment-use-syntax t)
;;   (setq-local comment-start sv-comment-start)
;;   (setq-local comment-end "")
;;   (setq comment-column 32)
;;   (setq-local comment-start-skip sv-comment-start-skip)

;;   (setq font-lock-defaults '(inferior-sv-font-lock-keywords nil nil))

;;   (setq-local info-lookup-mode 'sv-mode)
;;   (setq-local eldoc-documentation-function 'sv-eldoc-function)

;;   (setq-local comint-input-ring-file-name
;;               (or (getenv "SV_HISTFILE") "~/.sv_hist"))
;;   (setq-local comint-input-ring-size
;;               (string-to-number (or (getenv "SV_HISTSIZE") "1024")))
;;   (comint-read-input-ring t)
;;   (setq-local comint-dynamic-complete-functions
;;               inferior-sv-dynamic-complete-functions)
;;   (setq-local comint-prompt-read-only inferior-sv-prompt-read-only)
;;   (add-hook 'comint-input-filter-functions
;;             'inferior-sv-directory-tracker nil t)
;;   ;; http://thread.gmane.org/gmane.comp.gnu.sv.general/48572
;;   (add-hook 'window-configuration-change-hook
;;             'inferior-sv-track-window-width-change nil t)
;;   (setq-local compilation-error-regexp-alist inferior-sv-error-regexp-alist)
;;   (setq-local compilation-mode-font-lock-keywords
;;               inferior-sv-compilation-font-lock-keywords)
;;   (compilation-shell-minor-mode 1)
;;   (compilation-forget-errors))

;; ;;;###autoload
;; (defun inferior-sv (&optional arg)
;;   "Run an inferior Sv process, I/O via `inferior-sv-buffer'.
;; This buffer is put in Inferior Sv mode.  See `inferior-sv-mode'.

;; Unless ARG is non-nil, switches to this buffer.

;; The elements of the list `inferior-sv-startup-args' are sent as
;; command line arguments to the inferior Sv process on startup.

;; Additional commands to be executed on startup can be provided either in
;; the file specified by `inferior-sv-startup-file' or by the default
;; startup file, `~/.emacs-sv'."
;;   (interactive "P")
;;   (let ((buffer (get-buffer-create inferior-sv-buffer)))
;;     (unless arg
;;       (pop-to-buffer buffer))
;;     (unless (comint-check-proc buffer)
;;       (with-current-buffer buffer
;;         (inferior-sv-startup)
;;         (inferior-sv-mode)))
;;     buffer))

;; ;;;###autoload
;; (defalias 'run-sv 'inferior-sv)

;; (defun inferior-sv-startup ()
;;   "Start an inferior Sv process."
;;   (let ((proc (comint-exec-1
;;                (substring inferior-sv-buffer 1 -1)
;;                inferior-sv-buffer
;;                inferior-sv-program
;;                (append
;;                 inferior-sv-startup-args
;;                 ;; --no-gui is introduced in Sv > 3.7
;;                 (and (not (member "--no-gui" inferior-sv-startup-args))
;;                      (zerop (process-file inferior-sv-program
;;                                           nil nil nil "--no-gui" "--help"))
;;                      '("--no-gui"))))))
;;     (set-process-filter proc 'inferior-sv-output-digest)
;;     (setq inferior-sv-process proc
;;           inferior-sv-output-list nil
;;           inferior-sv-output-string nil
;;           inferior-sv-receive-in-progress t)

;;     ;; This may look complicated ... However, we need to make sure that
;;     ;; we additional startup code only AFTER Sv is ready (otherwise,
;;     ;; output may be mixed up).  Hence, we need to digest the Sv
;;     ;; output to see when it issues a prompt.
;;     (while inferior-sv-receive-in-progress
;;       (unless (inferior-sv-process-live-p)
;;         ;; Spit out the error messages.
;;         (when inferior-sv-output-list
;;           (princ (concat (mapconcat 'identity inferior-sv-output-list "\n")
;;                          "\n")
;;                  (process-mark inferior-sv-process)))
;;         (error "Process `%s' died" inferior-sv-process))
;;       (accept-process-output inferior-sv-process))
;;     (goto-char (point-max))
;;     (set-marker (process-mark proc) (point))
;;     (insert-before-markers
;;      (concat
;;       (if (not (bobp)) "\n")
;;       (if inferior-sv-output-list
;;           (concat (mapconcat
;;                    'identity inferior-sv-output-list "\n")
;;                   "\n"))))

;;     ;; An empty secondary prompt, as e.g. obtained by '--braindead',
;;     ;; means trouble.
;;     (inferior-sv-send-list-and-digest (list "PS2\n"))
;;     (when (string-match "\\(PS2\\|ans\\) = *$"
;;                         (car inferior-sv-output-list))
;;       (inferior-sv-send-list-and-digest (list "PS2 ('> ');\n")))

;;     (inferior-sv-send-list-and-digest
;;      (list "disp (getenv ('SV_SRCDIR'))\n"))
;;     (process-put proc 'sv-srcdir
;;                  (unless (equal (car inferior-sv-output-list) "")
;;                    (car inferior-sv-output-list)))

;;     ;; O.K., now we are ready for the Inferior Sv startup commands.
;;     (inferior-sv-send-list-and-digest
;;      (list "more off;\n"
;;            (unless (equal inferior-sv-output-string ">> ")
;;              ;; See http://hg.savannah.gnu.org/hgweb/sv/rev/708173343c50
;;              "PS1 ('sv> ');\n")
;;            (when (and inferior-sv-startup-file
;;                       (file-exists-p inferior-sv-startup-file))
;;              (format "source ('%s');\n" inferior-sv-startup-file))))
;;     (when inferior-sv-output-list
;;       (insert-before-markers
;;        (mapconcat 'identity inferior-sv-output-list "\n")))

;;     ;; And finally, everything is back to normal.
;;     (set-process-filter proc 'comint-output-filter)
;;     ;; Just in case, to be sure a cd in the startup file won't have
;;     ;; detrimental effects.
;;     (with-demoted-errors (inferior-sv-resync-dirs))
;;     ;; Generate a proper prompt, which is critical to
;;     ;; `comint-history-isearch-backward-regexp'.  Bug#14433.
;;     (comint-send-string proc "\n")))

;; (defun inferior-sv-completion-table ()
;;   (completion-table-with-cache
;;    (lambda (command)
;;      (inferior-sv-send-list-and-digest
;;       (list (format "completion_matches ('%s');\n" command)))
;;      (delete-consecutive-dups
;;       (sort inferior-sv-output-list 'string-lessp)))))

;; (defun inferior-sv-completion-at-point ()
;;   "Return the data to complete the Sv symbol at point."
;;   ;; http://debbugs.gnu.org/14300
;;   (unless (string-match-p "/" (or (comint--match-partial-filename) ""))
;;     (let ((beg (save-excursion
;;                  (skip-syntax-backward "w_" (comint-line-beginning-position))
;;                  (point)))
;;           (end (point)))
;;       (when (and beg (> end beg))
;;         (list beg end (completion-table-in-turn
;;                        (inferior-sv-completion-table)
;;                        'comint-completion-file-name-table))))))

;; (define-obsolete-function-alias 'inferior-sv-complete
;;   'completion-at-point "24.1")

;; (defun inferior-sv-dynamic-list-input-ring ()
;;   "List the buffer's input history in a help buffer."
;;   ;; We cannot use `comint-dynamic-list-input-ring', because it replaces
;;   ;; "completion" by "history reference" ...
;;   (interactive)
;;   (if (or (not (ring-p comint-input-ring))
;;           (ring-empty-p comint-input-ring))
;;       (message "No history")
;;     (let ((history nil)
;;           (history-buffer " *Input History*")
;;           (index (1- (ring-length comint-input-ring)))
;;           (conf (current-window-configuration)))
;;       ;; We have to build up a list ourselves from the ring vector.
;;       (while (>= index 0)
;;         (setq history (cons (ring-ref comint-input-ring index) history)
;;               index (1- index)))
;;       ;; Change "completion" to "history reference"
;;       ;; to make the display accurate.
;;       (with-output-to-temp-buffer history-buffer
;;         (display-completion-list history)
;;         (set-buffer history-buffer))
;;       (message "Hit space to flush")
;;       (let ((ch (read-event)))
;;         (if (eq ch ?\ )
;;             (set-window-configuration conf)
;;           (setq unread-command-events (list ch)))))))

;; (defun inferior-sv-output-digest (_proc string)
;;   "Special output filter for the inferior Sv process.
;; Save all output between newlines into `inferior-sv-output-list', and
;; the rest to `inferior-sv-output-string'."
;;   (setq string (concat inferior-sv-output-string string))
;;   (while (string-match "\n" string)
;;     (setq inferior-sv-output-list
;; 	  (append inferior-sv-output-list
;; 		  (list (substring string 0 (match-beginning 0))))
;; 	  string (substring string (match-end 0))))
;;   (if (string-match inferior-sv-prompt string)
;;       (setq inferior-sv-receive-in-progress nil))
;;   (setq inferior-sv-output-string string))

;; (defun inferior-sv-check-process ()
;;   (or (inferior-sv-process-live-p)
;;       (error (substitute-command-keys
;;               "No inferior sv process running. Type \\[run-sv]"))))

;; (defun inferior-sv-send-list-and-digest (list)
;;   "Send LIST to the inferior Sv process and digest the output.
;; The elements of LIST have to be strings and are sent one by one.  All
;; output is passed to the filter `inferior-sv-output-digest'."
;;   (inferior-sv-check-process)
;;   (let* ((proc inferior-sv-process)
;; 	 (filter (process-filter proc))
;; 	 string)
;;     (set-process-filter proc 'inferior-sv-output-digest)
;;     (setq inferior-sv-output-list nil)
;;     (unwind-protect
;; 	(while (setq string (car list))
;; 	  (setq inferior-sv-output-string nil
;; 		inferior-sv-receive-in-progress t)
;; 	  (comint-send-string proc string)
;; 	  (while inferior-sv-receive-in-progress
;; 	    (accept-process-output proc))
;; 	  (setq list (cdr list)))
;;       (set-process-filter proc filter))))

;; (defvar inferior-sv-directory-tracker-resync nil)
;; (make-variable-buffer-local 'inferior-sv-directory-tracker-resync)

;; (defun inferior-sv-directory-tracker (string)
;;   "Tracks `cd' commands issued to the inferior Sv process.
;; Use \\[inferior-sv-resync-dirs] to resync if Emacs gets confused."
;;   (when inferior-sv-directory-tracker-resync
;;     (or (inferior-sv-resync-dirs 'noerror)
;;         (setq inferior-sv-directory-tracker-resync nil)))
;;   (cond
;;    ((string-match "^[ \t]*cd[ \t;]*$" string)
;;     (cd "~"))
;;    ((string-match "^[ \t]*cd[ \t]+\\([^ \t\n;]*\\)[ \t\n;]*" string)
;;     (condition-case err
;;         (cd (match-string 1 string))
;;       (error (setq inferior-sv-directory-tracker-resync t)
;;              (message "%s: `%s'"
;;                       (error-message-string err)
;;                       (match-string 1 string)))))))

;; (defun inferior-sv-resync-dirs (&optional noerror)
;;   "Resync the buffer's idea of the current directory.
;; This command queries the inferior Sv process about its current
;; directory and makes this the current buffer's default directory."
;;   (interactive)
;;   (inferior-sv-send-list-and-digest '("disp (pwd ())\n"))
;;   (condition-case err
;;       (progn
;;         (cd (car inferior-sv-output-list))
;;         t)
;;     (error (unless noerror (signal (car err) (cdr err))))))

;; (defcustom inferior-sv-minimal-columns 80
;;   "The minimal column width for the inferior Sv process."
;;   :type 'integer
;;   :group 'sv
;;   :version "24.4")

;; (defvar inferior-sv-last-column-width nil)

;; (defun inferior-sv-track-window-width-change ()
;;   ;; http://thread.gmane.org/gmane.comp.gnu.sv.general/48572
;;   (let ((width (max inferior-sv-minimal-columns (window-width))))
;;     (unless (eq inferior-sv-last-column-width width)
;;       (setq-local inferior-sv-last-column-width width)
;;       (when (inferior-sv-process-live-p)
;;         (inferior-sv-send-list-and-digest
;;          (list (format "putenv ('COLUMNS', '%s');\n" width)))))))

;; 
;; ;;; Miscellaneous useful functions

;; (defun sv-in-comment-p ()
;;   "Return non-nil if point is inside an Sv comment."
;;   (nth 4 (syntax-ppss)))

;; (defun sv-in-string-p ()
;;   "Return non-nil if point is inside an Sv string."
;;   (nth 3 (syntax-ppss)))

;; (defun sv-in-string-or-comment-p ()
;;   "Return non-nil if point is inside an Sv string or comment."
;;   (nth 8 (syntax-ppss)))

;; (defun sv-looking-at-kw (regexp)
;;   "Like `looking-at', but sets `case-fold-search' nil."
;;   (let ((case-fold-search nil))
;;     (looking-at regexp)))

;; (defun sv-maybe-insert-continuation-string ()
;;   (if (or (sv-in-comment-p)
;; 	  (save-excursion
;; 	    (beginning-of-line)
;; 	    (looking-at sv-continuation-regexp)))
;;       nil
;;     (delete-horizontal-space)
;;     (insert (concat " " sv-continuation-string))))

;; (defun sv-completing-read ()
;;   (let ((def (or (thing-at-point 'symbol)
;;                  (save-excursion
;;                    (skip-syntax-backward "-(")
;;                    (thing-at-point 'symbol)))))
;;     (completing-read
;;      (format (if def "Function (default %s): "
;;                "Function: ") def)
;;      (inferior-sv-completion-table)
;;      nil nil nil nil def)))

;; (defun sv-goto-function-definition (fn)
;;   "Go to the function definition of FN in current buffer."
;;   (let ((search
;;          (lambda (re sub)
;;            (let ((orig (point)) found)
;;              (goto-char (point-min))
;;              (while (and (not found) (re-search-forward re nil t))
;;                (when (and (equal (match-string sub) fn)
;;                           (not (nth 8 (syntax-ppss))))
;;                  (setq found t)))
;;              (unless found (goto-char orig))
;;              found))))
;;     (pcase (and buffer-file-name (file-name-extension buffer-file-name))
;;       (`"cc" (funcall search
;;                       "\\_<DEFUN\\(?:_DLD\\)?\\s-*(\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)" 1))
;;       (t (funcall search sv-function-header-regexp 3)))))

;; (defun sv-function-file-p ()
;;   "Return non-nil if the first token is \"function\".
;; The value is (START END NAME-START NAME-END) of the function."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (equal (funcall smie-forward-token-function) "function")
;;       (forward-word -1)
;;       (let* ((start (point))
;;              (end (progn (forward-sexp 1) (point)))
;;              (name (when (progn
;;                            (goto-char start)
;;                            (re-search-forward sv-function-header-regexp
;;                                               end t))
;;                      (list (match-beginning 3) (match-end 3)))))
;;         (cons start (cons end name))))))

;; ;; Like forward-comment but stop at non-comment blank
;; (defun sv-skip-comment-forward (limit)
;;   (let ((ppss (syntax-ppss)))
;;     (if (nth 4 ppss)
;;         (goto-char (nth 8 ppss))
;;       (goto-char (or (comment-search-forward limit t) (point)))))
;;   (while (and (< (point) limit) (looking-at-p "\\s<"))
;;     (forward-comment 1)))

;; ;;; First non-copyright comment block
;; (defun sv-function-file-comment ()
;;   "Beginning and end positions of the function file comment."
;;   (save-excursion
;;     (goto-char (point-min))
;;     ;; Copyright block: sv/libinterp/parse-tree/lex.ll around line 1634
;;     (while (save-excursion
;;              (when (comment-search-forward (point-max) t)
;;                (when (eq (char-after) ?\{) ; case of block comment
;;                  (forward-char 1))
;;                (skip-syntax-forward "-")
;;                (let ((case-fold-search t))
;;                  (looking-at-p "\\(?:copyright\\|author\\)\\_>"))))
;;       (sv-skip-comment-forward (point-max)))
;;     (let ((beg (comment-search-forward (point-max) t)))
;;       (when beg
;;         (goto-char beg)
;;         (sv-skip-comment-forward (point-max))
;;         (list beg (point))))))

;; (defun sv-sync-function-file-names ()
;;   "Ensure function name agree with function file name.
;; See Info node `(sv)Function Files'."
;;   (interactive)
;;   (when buffer-file-name
;;     (pcase-let ((`(,start ,_end ,name-start ,name-end)
;;                  (sv-function-file-p)))
;;       (when (and start name-start)
;;         (let* ((func (buffer-substring name-start name-end))
;;                (file (file-name-sans-extension
;;                       (file-name-nondirectory buffer-file-name)))
;;                (help-form (format "\
;; a: Use function name `%s'
;; b: Use file name `%s'
;; q: Don't fix\n" func file))
;;                (c (unless (equal file func)
;;                     (save-window-excursion
;;                       (help-form-show)
;;                       (read-char-choice
;;                        "Which name to use? (a/b/q) " '(?a ?b ?q))))))
;;           (pcase c
;;             (`?a (let ((newname (expand-file-name
;;                                  (concat func (file-name-extension
;;                                                buffer-file-name t)))))
;;                    (when (or (not (file-exists-p newname))
;;                              (yes-or-no-p
;;                               (format "Target file %s exists; proceed? " newname)))
;;                      (when (file-exists-p buffer-file-name)
;;                        (rename-file buffer-file-name newname t))
;;                      (set-visited-file-name newname))))
;;             (`?b (save-excursion
;;                    (goto-char name-start)
;;                    (delete-region name-start name-end)
;;                    (insert file)))))))))

;; (defun sv-update-function-file-comment (beg end)
;;   "Query replace function names in function file comment."
;;   (interactive
;;    (progn
;;      (barf-if-buffer-read-only)
;;      (if (use-region-p)
;;          (list (region-beginning) (region-end))
;;        (or (sv-function-file-comment)
;;            (error "No function file comment found")))))
;;   (save-excursion
;;     (let* ((bounds (or (sv-function-file-p)
;;                        (error "Not in a function file buffer")))
;;            (func (if (cddr bounds)
;;                      (apply #'buffer-substring (cddr bounds))
;;                    (error "Function name not found")))
;;            (old-func (progn
;;                        (goto-char beg)
;;                        (when (re-search-forward
;;                               "[=}]\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
;;                               (min (line-end-position 4) end)
;;                               t)
;;                          (match-string 1))))
;;            (old-func (read-string (format (if old-func
;;                                               "Name to replace (default %s): "
;;                                             "Name to replace: ")
;;                                           old-func)
;;                                   nil nil old-func)))
;;       (if (and func old-func (not (equal func old-func)))
;;           (perform-replace old-func func 'query
;;                            nil 'delimited nil nil beg end)
;;         (message "Function names match")))))

;; (defface sv-function-comment-block
;;   '((t (:inherit font-lock-doc-face)))
;;   "Face used to highlight function comment block."
;;   :group 'sv)

;; (eval-when-compile (require 'texinfo))

;; (defun sv-font-lock-texinfo-comment ()
;;   (let ((kws
;;          (eval-when-compile
;;            (delq nil (mapcar
;;                       (lambda (kw)
;;                         (if (numberp (nth 1 kw))
;;                             `(,(nth 0 kw) ,(nth 1 kw) ,(nth 2 kw) prepend)
;;                           (message "Ignoring Texinfo highlight: %S" kw)))
;;                       texinfo-font-lock-keywords)))))
;;     (font-lock-add-keywords
;;      nil
;;      `((,(lambda (limit)
;;            (while (and (< (point) limit)
;;                        (search-forward "-*- texinfo -*-" limit t)
;;                        (sv-in-comment-p))
;;              (let ((beg (nth 8 (syntax-ppss)))
;;                    (end (progn
;;                           (sv-skip-comment-forward (point-max))
;;                           (point))))
;;                (put-text-property beg end 'font-lock-multiline t)
;;                (font-lock-prepend-text-property
;;                 beg end 'face 'sv-function-comment-block)
;;                (dolist (kw kws)
;;                  (goto-char beg)
;;                  (while (re-search-forward (car kw) end 'move)
;;                    (font-lock-apply-highlight (cdr kw))))))
;;            nil)))
;;      'append)))

;; 
;; ;;; Indentation

;; (defun sv-indent-new-comment-line (&optional soft)
;;   "Break Sv line at point, continuing comment if within one.
;; Insert `sv-continuation-string' before breaking the line
;; unless inside a list.  Signal an error if within a single-quoted
;; string."
;;   (interactive)
;;   (funcall comment-line-break-function soft))

;; (defun sv--indent-new-comment-line (orig &rest args)
;;   (cond
;;    ((sv-in-comment-p) nil)
;;    ((eq (sv-in-string-p) ?')
;;     (error "Cannot split a single-quoted string"))
;;    ((eq (sv-in-string-p) ?\")
;;     (insert sv-continuation-string))
;;    (t
;;     (delete-horizontal-space)
;;     (unless (and (cadr (syntax-ppss))
;;                  (eq (char-after (cadr (syntax-ppss))) ?\())
;;       (insert " " sv-continuation-string))))
;;   (apply orig args)
;;   (indent-according-to-mode))

;; (define-obsolete-function-alias
;;   'sv-indent-defun 'prog-indent-sexp "24.4")

;; 
;; ;;; Motion
;; (defun sv-next-code-line (&optional arg)
;;   "Move ARG lines of Sv code forward (backward if ARG is negative).
;; Skips past all empty and comment lines.  Default for ARG is 1.

;; On success, return 0.  Otherwise, go as far as possible and return -1."
;;   (interactive "p")
;;   (or arg (setq arg 1))
;;   (beginning-of-line)
;;   (let ((n 0)
;; 	(inc (if (> arg 0) 1 -1)))
;;     (while (and (/= arg 0) (= n 0))
;;       (setq n (forward-line inc))
;;       (while (and (= n 0)
;; 		  (looking-at "\\s-*\\($\\|\\s<\\)"))
;; 	(setq n (forward-line inc)))
;;       (setq arg (- arg inc)))
;;     n))

;; (defun sv-previous-code-line (&optional arg)
;;   "Move ARG lines of Sv code backward (forward if ARG is negative).
;; Skips past all empty and comment lines.  Default for ARG is 1.

;; On success, return 0.  Otherwise, go as far as possible and return -1."
;;   (interactive "p")
;;   (or arg (setq arg 1))
;;   (sv-next-code-line (- arg)))

;; (defun sv-beginning-of-line ()
;;   "Move point to beginning of current Sv line.
;; If on an empty or comment line, go to the beginning of that line.
;; Otherwise, move backward to the beginning of the first Sv code line
;; which is not inside a continuation statement, i.e., which does not
;; follow a code line ending with `...' or is inside an open
;; parenthesis list."
;;   (interactive)
;;   (beginning-of-line)
;;   (unless (looking-at "\\s-*\\($\\|\\s<\\)")
;;     (while (or (when (cadr (syntax-ppss))
;;                  (goto-char (cadr (syntax-ppss)))
;;                  (beginning-of-line)
;;                  t)
;;                (and (or (looking-at "\\s-*\\($\\|\\s<\\)")
;;                         (save-excursion
;;                           (if (zerop (sv-previous-code-line))
;;                               (looking-at sv-continuation-regexp))))
;;                     (zerop (forward-line -1)))))))

;; (defun sv-end-of-line ()
;;   "Move point to end of current Sv line.
;; If on an empty or comment line, go to the end of that line.
;; Otherwise, move forward to the end of the first Sv code line which
;; does not end with `...' or is inside an open parenthesis list."
;;   (interactive)
;;   (end-of-line)
;;   (unless (save-excursion
;;             (beginning-of-line)
;;             (looking-at "\\s-*\\($\\|\\s<\\)"))
;;     (while (or (when (cadr (syntax-ppss))
;;                  (condition-case nil
;;                      (progn
;;                        (up-list 1)
;;                        (end-of-line)
;;                        t)
;;                    (error nil)))
;;                (and (save-excursion
;;                       (beginning-of-line)
;;                       (or (looking-at "\\s-*\\($\\|\\s<\\)")
;;                           (looking-at sv-continuation-regexp)))
;;                     (zerop (forward-line 1)))))
;;     (end-of-line)))

;; (defun sv-mark-block ()
;;   "Put point at the beginning of this Sv block, mark at the end.
;; The block marked is the one that contains point or follows point."
;;   (interactive)
;;   (if (and (looking-at "\\sw\\|\\s_")
;;            (looking-back "\\sw\\|\\s_" (1- (point))))
;;       (skip-syntax-forward "w_"))
;;   (unless (or (looking-at "\\s(")
;;               (save-excursion
;;                 (let* ((token (funcall smie-forward-token-function))
;;                        (level (assoc token smie-grammar)))
;;                   (and level (not (numberp (cadr level)))))))
;;     (backward-up-list 1))
;;   (mark-sexp))

;; (defun sv-beginning-of-defun (&optional arg)
;;   "Sv-specific `beginning-of-defun-function' (which see)."
;;   (or arg (setq arg 1))
;;   ;; Move out of strings or comments.
;;   (when (sv-in-string-or-comment-p)
;;     (goto-char (sv-in-string-or-comment-p)))
;;   (letrec ((orig (point))
;;            (toplevel (lambda (pos)
;;                        (condition-case nil
;;                            (progn
;;                              (backward-up-list 1)
;;                              (funcall toplevel (point)))
;;                          (scan-error pos)))))
;;     (goto-char (funcall toplevel (point)))
;;     (when (and (> arg 0) (/= orig (point)))
;;       (setq arg (1- arg)))
;;     (forward-sexp (- arg))
;;     (and (< arg 0) (forward-sexp -1))
;;     (/= orig (point))))

;; (defun sv-fill-paragraph (&optional _arg)
;;   "Fill paragraph of Sv code, handling Sv comments."
;;   ;; FIXME: difference with generic fill-paragraph:
;;   ;; - code lines are only split, never joined.
;;   ;; - \n that end comments are never removed.
;;   ;; - insert continuation marker when splitting code lines.
;;   (interactive "P")
;;   (save-excursion
;;     (let ((end (progn (forward-paragraph) (copy-marker (point) t)))
;;           (beg (progn
;;                  (forward-paragraph -1)
;;                  (skip-chars-forward " \t\n")
;;                  (beginning-of-line)
;;                  (point)))
;;           (cfc (current-fill-column))
;;           comment-prefix)
;;       (goto-char beg)
;;       (while (< (point) end)
;;         (condition-case nil
;;             (indent-according-to-mode)
;;           (error nil))
;;         (move-to-column cfc)
;;         ;; First check whether we need to combine non-empty comment lines
;;         (if (and (< (current-column) cfc)
;;                  (sv-in-comment-p)
;;                  (not (save-excursion
;;                         (beginning-of-line)
;;                         (looking-at "^\\s-*\\s<+\\s-*$"))))
;;             ;; This is a nonempty comment line which does not extend
;;             ;; past the fill column.  If it is followed by a nonempty
;;             ;; comment line with the same comment prefix, try to
;;             ;; combine them, and repeat this until either we reach the
;;             ;; fill-column or there is nothing more to combine.
;;             (progn
;;               ;; Get the comment prefix
;;               (save-excursion
;;                 (beginning-of-line)
;;                 (while (and (re-search-forward "\\s<+")
;;                             (not (sv-in-comment-p))))
;;                 (setq comment-prefix (match-string 0)))
;;               ;; And keep combining ...
;;               (while (and (< (current-column) cfc)
;;                           (save-excursion
;;                             (forward-line 1)
;;                             (and (looking-at
;;                                   (concat "^\\s-*"
;;                                           comment-prefix
;;                                           "\\S<"))
;;                                  (not (looking-at
;;                                        (concat "^\\s-*"
;;                                                comment-prefix
;;                                                "\\s-*$"))))))
;;                 (delete-char 1)
;;                 (re-search-forward comment-prefix)
;;                 (delete-region (match-beginning 0) (match-end 0))
;;                 (fixup-whitespace)
;;                 (move-to-column cfc))))
;;         ;; We might also try to combine continued code lines>  Perhaps
;;         ;; some other time ...
;;         (skip-chars-forward "^ \t\n")
;;         (delete-horizontal-space)
;;         (if (or (< (current-column) cfc)
;;                 (and (= (current-column) cfc) (eolp)))
;;             (forward-line 1)
;;           (if (not (eolp)) (insert " "))
;;           (or (funcall normal-auto-fill-function)
;;               (forward-line 1))))
;;       t)))

;; (defun sv-completion-at-point ()
;;   "Find the text to complete and the corresponding table."
;;   (let* ((beg (save-excursion (skip-syntax-backward "w_") (point)))
;;          (end (point)))
;;     (if (< beg (point))
;;         ;; Extend region past point, if applicable.
;;         (save-excursion (skip-syntax-forward "w_")
;;                         (setq end (point))))
;;     (when (> end beg)
;;       (list beg end (or (and (inferior-sv-process-live-p)
;;                              (inferior-sv-completion-table))
;;                         sv-reserved-words)))))

;; (define-obsolete-function-alias 'sv-complete-symbol
;;   'completion-at-point "24.1")

;; (defun sv-add-log-current-defun ()
;;   "A function for `add-log-current-defun-function' (which see)."
;;   (save-excursion
;;     (end-of-line)
;;     (and (beginning-of-defun)
;;          (re-search-forward sv-function-header-regexp
;;                             (line-end-position) t)
;;          (match-string 3))))

;; 
;; ;;; Electric characters && friends
;; (define-skeleton sv-insert-defun
;;   "Insert an Sv function skeleton.
;; Prompt for the function's name, arguments and return values (to be
;; entered without parens)."
;;   (let* ((defname (file-name-sans-extension (buffer-name)))
;;          (name (read-string (format "Function name (default %s): " defname)
;;                             nil nil defname))
;;          (args (read-string "Arguments: "))
;;          (vals (read-string "Return values: ")))
;;     (format "%s%s (%s)"
;;             (cond
;;              ((string-equal vals "") vals)
;;              ((string-match "[ ,]" vals) (concat "[" vals "] = "))
;;              (t (concat vals " = ")))
;;             name
;;             args))
;;   \n sv-block-comment-start "usage: " str \n
;;   sv-block-comment-start '(delete-horizontal-space) \n
;;   sv-block-comment-start '(delete-horizontal-space) \n
;;   "function " > str \n
;;   _ \n
;;   "endfunction" > \n)

;; ;;; Communication with the inferior Sv process
;; (defun sv-kill-process ()
;;   "Kill inferior Sv process and its buffer."
;;   (interactive)
;;   (when (and (buffer-live-p (get-buffer inferior-sv-buffer))
;;              (or (yes-or-no-p (format "Kill %S and its buffer? "
;;                                       inferior-sv-process))
;;                  (user-error "Aborted")))
;;     (when (inferior-sv-process-live-p)
;;       (set-process-query-on-exit-flag inferior-sv-process nil)
;;       (process-send-string inferior-sv-process "quit;\n")
;;       (accept-process-output inferior-sv-process))
;;     (kill-buffer inferior-sv-buffer)))

;; (defun sv-show-process-buffer ()
;;   "Make sure that `inferior-sv-buffer' is displayed."
;;   (interactive)
;;   (if (get-buffer inferior-sv-buffer)
;;       (display-buffer inferior-sv-buffer)
;;     (message "No buffer named %s" inferior-sv-buffer)))

;; (defun sv-hide-process-buffer ()
;;   "Delete all windows that display `inferior-sv-buffer'."
;;   (interactive)
;;   (if (get-buffer inferior-sv-buffer)
;;       (delete-windows-on inferior-sv-buffer)
;;     (message "No buffer named %s" inferior-sv-buffer)))

;; (defun sv-source-file (file)
;;   "Execute FILE in the inferior Sv process.
;; This is done using Sv's source function.  FILE defaults to
;; current buffer file unless called with a prefix arg \\[universal-argument]."
;;   (interactive (list (or (and (not current-prefix-arg) buffer-file-name)
;;                          (read-file-name "File: " nil nil t))))
;;   (or (stringp file)
;;       (signal 'wrong-type-argument (list 'stringp file)))
;;   (inferior-sv t)
;;   (with-current-buffer inferior-sv-buffer
;;     (comint-send-string inferior-sv-process
;;                         (format "source '%s'\n" file))))

;; (defun sv-send-region (beg end)
;;   "Send current region to the inferior Sv process."
;;   (interactive "r")
;;   (inferior-sv t)
;;   (let ((proc inferior-sv-process)
;;         (string (buffer-substring-no-properties beg end))
;;         line)
;;     (with-current-buffer inferior-sv-buffer
;;       ;; http://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00095.html
;;       (compilation-forget-errors)
;;       (setq inferior-sv-output-list nil)
;;       (while (not (string-equal string ""))
;;         (if (string-match "\n" string)
;;             (setq line (substring string 0 (match-beginning 0))
;;                   string (substring string (match-end 0)))
;;           (setq line string string ""))
;;         (setq inferior-sv-receive-in-progress t)
;;         (inferior-sv-send-list-and-digest (list (concat line "\n")))
;;         (while inferior-sv-receive-in-progress
;;           (accept-process-output proc))
;;         (insert-before-markers
;;          (mapconcat 'identity
;;                     (append
;;                      (if sv-send-echo-input (list line) (list ""))
;;                      inferior-sv-output-list
;;                      (list inferior-sv-output-string))
;;                     "\n")))))
;;   (if sv-send-show-buffer
;;       (display-buffer inferior-sv-buffer)))

;; (defun sv-send-buffer ()
;;   "Send current buffer to the inferior Sv process."
;;   (interactive)
;;   (sv-send-region (point-min) (point-max)))

;; (defun sv-send-block ()
;;   "Send current Sv block to the inferior Sv process."
;;   (interactive)
;;   (save-excursion
;;     (sv-mark-block)
;;     (sv-send-region (point) (mark))))

;; (defun sv-send-defun ()
;;   "Send current Sv function to the inferior Sv process."
;;   (interactive)
;;   (save-excursion
;;     (mark-defun)
;;     (sv-send-region (point) (mark))))

;; (defun sv-send-line (&optional arg)
;;   "Send current Sv code line to the inferior Sv process.
;; With positive prefix ARG, send that many lines.
;; If `sv-send-line-auto-forward' is non-nil, go to the next unsent
;; code line."
;;   (interactive "P")
;;   (or arg (setq arg 1))
;;   (if (> arg 0)
;;       (let (beg end)
;; 	(beginning-of-line)
;; 	(setq beg (point))
;; 	(sv-next-code-line (- arg 1))
;; 	(end-of-line)
;; 	(setq end (point))
;; 	(if sv-send-line-auto-forward
;; 	    (sv-next-code-line 1))
;; 	(sv-send-region beg end))))

;; (defun sv-eval-print-last-sexp ()
;;   "Evaluate Sv sexp before point and print value into current buffer."
;;   (interactive)
;;   (inferior-sv t)
;;   (let ((standard-output (current-buffer))
;; 	(print-escape-newlines nil)
;; 	(opoint (point)))
;;     (terpri)
;;     (prin1
;;      (save-excursion
;;        (forward-sexp -1)
;;        (inferior-sv-send-list-and-digest
;; 	(list (concat (buffer-substring-no-properties (point) opoint)
;; 		      "\n")))
;;        (mapconcat 'identity inferior-sv-output-list "\n")))
;;     (terpri)))

;; 

;; (defcustom sv-eldoc-message-style 'auto
;;   "Sv eldoc message style: auto, oneline, multiline."
;;   :type '(choice (const :tag "Automatic" auto)
;;                  (const :tag "One Line" oneline)
;;                  (const :tag "Multi Line" multiline))
;;   :group 'sv
;;   :version "24.4")

;; ;; (FN SIGNATURE1 SIGNATURE2 ...)
;; (defvar sv-eldoc-cache nil)

;; (defun sv-eldoc-function-signatures (fn)
;;   (unless (equal fn (car sv-eldoc-cache))
;;     (inferior-sv-send-list-and-digest
;;      (list (format "print_usage ('%s');\n" fn)))
;;     (let (result)
;;       (dolist (line inferior-sv-output-list)
;;         (when (string-match
;;                "\\s-*\\(?:--[^:]+\\|usage\\):\\s-*\\(.*\\)$"
;;                line)
;;           (push (match-string 1 line) result)))
;;       (setq sv-eldoc-cache
;;             (cons (substring-no-properties fn)
;;                   (nreverse result)))))
;;   (cdr sv-eldoc-cache))

;; (defun sv-eldoc-function ()
;;   "A function for `eldoc-documentation-function' (which see)."
;;   (when (inferior-sv-process-live-p)
;;     (let* ((ppss (syntax-ppss))
;;            (paren-pos (cadr ppss))
;;            (fn (save-excursion
;;                  (if (and paren-pos
;;                           ;; PAREN-POS must be after the prompt
;;                           (>= paren-pos
;;                               (if (eq (get-buffer-process (current-buffer))
;;                                       inferior-sv-process)
;;                                   (process-mark inferior-sv-process)
;;                                 (point-min)))
;;                           (or (not (eq (get-buffer-process (current-buffer))
;;                                        inferior-sv-process))
;;                               (< (process-mark inferior-sv-process)
;;                                  paren-pos))
;;                           (eq (char-after paren-pos) ?\())
;;                      (goto-char paren-pos)
;;                    (setq paren-pos nil))
;;                  (when (or (< (skip-syntax-backward "-") 0) paren-pos)
;;                    (thing-at-point 'symbol))))
;;            (sigs (and fn (sv-eldoc-function-signatures fn)))
;;            (oneline (mapconcat 'identity sigs
;;                                (propertize " | " 'face 'warning)))
;;            (multiline (mapconcat (lambda (s) (concat "-- " s)) sigs "\n")))
;;       ;;
;;       ;; Return the value according to style.
;;       (pcase sv-eldoc-message-style
;;         (`auto (if (< (length oneline) (window-width (minibuffer-window)))
;;                    oneline
;;                  multiline))
;;         (`oneline oneline)
;;         (`multiline multiline)))))

;; (defcustom sv-help-buffer "*Sv Help*"
;;   "Buffer name for `sv-help'."
;;   :type 'string
;;   :group 'sv
;;   :version "24.4")

;; ;; Used in a mode derived from help-mode.
;; (declare-function help-button-action "help-mode" (button))

;; (define-button-type 'sv-help-file
;;   'follow-link t
;;   'action #'help-button-action
;;   'help-function 'sv-find-definition)

;; (define-button-type 'sv-help-function
;;   'follow-link t
;;   'action (lambda (b)
;;             (sv-help
;;              (buffer-substring (button-start b) (button-end b)))))

;; (defvar sv-help-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\M-."  'sv-find-definition)
;;     (define-key map "\C-hd" 'sv-help)
;;     (define-key map "\C-ha" 'sv-lookfor)
;;     map))

;; (define-derived-mode sv-help-mode help-mode "OctHelp"
;;   "Major mode for displaying Sv documentation."
;;   :abbrev-table nil
;;   :syntax-table sv-mode-syntax-table
;;   (eval-and-compile (require 'help-mode))
;;   ;; Don't highlight `EXAMPLE' as elisp symbols by using a regexp that
;;   ;; can never match.
;;   (setq-local help-xref-symbol-regexp "x\\`"))

;; (defun sv-help (fn)
;;   "Display the documentation of FN."
;;   (interactive (list (sv-completing-read)))
;;   (inferior-sv-send-list-and-digest
;;    (list (format "help ('%s');\n" fn)))
;;   (let ((lines inferior-sv-output-list)
;;         (inhibit-read-only t))
;;     (when (string-match "error: \\(.*\\)$" (car lines))
;;       (error "%s" (match-string 1 (car lines))))
;;     (with-help-window sv-help-buffer
;;       (princ (mapconcat 'identity lines "\n"))
;;       (with-current-buffer sv-help-buffer
;;         ;; Bound to t so that `help-buffer' returns current buffer for
;;         ;; `help-setup-xref'.
;;         (let ((help-xref-following t))
;;           (help-setup-xref (list 'sv-help fn)
;;                            (called-interactively-p 'interactive)))
;;         ;; Note: can be turned off by suppress_verbose_help_message.
;;         ;;
;;         ;; Remove boring trailing text: Additional help for built-in functions
;;         ;; and operators ...
;;         (goto-char (point-max))
;;         (when (search-backward "\n\n\n" nil t)
;;           (goto-char (match-beginning 0))
;;           (delete-region (point) (point-max)))
;;         ;; File name highlight
;;         (goto-char (point-min))
;;         (when (re-search-forward "from the file \\(.*\\)$"
;;                                  (line-end-position)
;;                                  t)
;;           (let* ((file (match-string 1))
;;                  (dir (file-name-directory
;;                        (directory-file-name (file-name-directory file)))))
;;             (replace-match "" nil nil nil 1)
;;             (insert "`")
;;             ;; Include the parent directory which may be regarded as
;;             ;; the category for the FN.
;;             (help-insert-xref-button (file-relative-name file dir)
;;                                      'sv-help-file fn)
;;             (insert "'")))
;;         ;; Make 'See also' clickable.
;;         (with-syntax-table sv-mode-syntax-table
;;           (when (re-search-forward "^\\s-*See also:" nil t)
;;             (let ((end (save-excursion (re-search-forward "^\\s-*$" nil t))))
;;               (while (re-search-forward
;;                       "\\s-*\\([^,\n]+?\\)\\s-*\\(?:[,]\\|[.]?$\\)" end t)
;;                 (make-text-button (match-beginning 1) (match-end 1)
;;                                   :type 'sv-help-function)))))
;;         (sv-help-mode)))))

;; (defun sv-lookfor (str &optional all)
;;   "Search for the string STR in all function help strings.
;; If ALL is non-nil search the entire help string else only search the first
;; sentence."
;;   (interactive "sSearch for: \nP")
;;   (inferior-sv-send-list-and-digest
;;    (list (format "lookfor (%s'%s');\n"
;;                  (if all "'-all', " "")
;;                  str)))
;;   (let ((lines inferior-sv-output-list))
;;     (when (and (stringp (car lines))
;;                (string-match "error: \\(.*\\)$" (car lines)))
;;       (error "%s" (match-string 1 (car lines))))
;;     (with-help-window sv-help-buffer
;;       (with-current-buffer sv-help-buffer
;;         (if lines
;;             (insert (mapconcat 'identity lines "\n"))
;;           (insert (format "Nothing found for \"%s\".\n" str)))
;;         ;; Bound to t so that `help-buffer' returns current buffer for
;;         ;; `help-setup-xref'.
;;         (let ((help-xref-following t))
;;           (help-setup-xref (list 'sv-lookfor str all)
;;                            (called-interactively-p 'interactive)))
;;         (goto-char (point-min))
;;         (when lines
;;           (while (re-search-forward "^\\([^[:blank:]]+\\) " nil 'noerror)
;;             (make-text-button (match-beginning 1) (match-end 1)
;;                               :type 'sv-help-function)))
;;         (unless all
;;           (goto-char (point-max))
;;           (insert "\nRetry with ")
;;           (insert-text-button "'-all'"
;;                               'follow-link t
;;                               'action #'(lambda (_b)
;;                                           (sv-lookfor str '-all)))
;;           (insert ".\n"))
;;         (sv-help-mode)))))

;; (defcustom sv-source-directories nil
;;   "A list of directories for Sv sources.
;; If the environment variable SV_SRCDIR is set, it is searched first."
;;   :type '(repeat directory)
;;   :group 'sv
;;   :version "24.4")

;; (defun sv-source-directories ()
;;   (let ((srcdir (or (and inferior-sv-process
;;                          (process-get inferior-sv-process 'sv-srcdir))
;;                     (getenv "SV_SRCDIR"))))
;;     (if srcdir
;;         (cons srcdir sv-source-directories)
;;       sv-source-directories)))

;; (defvar sv-find-definition-filename-function
;;   #'sv-find-definition-default-filename)

;; (defun sv-find-definition-default-filename (name)
;;   "Default value for `sv-find-definition-filename-function'."
;;   (pcase (file-name-extension name)
;;     (`"oct"
;;      (sv-find-definition-default-filename
;;       (concat "libinterp/dldfcn/"
;;               (file-name-sans-extension (file-name-nondirectory name))
;;               ".cc")))
;;     (`"cc"
;;      (let ((file (or (locate-file name (sv-source-directories))
;;                      (locate-file (file-name-nondirectory name)
;;                                   (sv-source-directories)))))
;;        (or (and file (file-exists-p file))
;;            (error "File `%s' not found" name))
;;        file))
;;     (`"mex"
;;      (if (yes-or-no-p (format "File `%s' may be binary; open? "
;;                               (file-name-nondirectory name)))
;;          name
;;        (user-error "Aborted")))
;;     (t name)))

;; (defvar find-tag-marker-ring)

;; (defun sv-find-definition (fn)
;;   "Find the definition of FN.
;; Functions implemented in C++ can be found if
;; variable `sv-source-directories' is set correctly."
;;   (interactive (list (sv-completing-read)))
;;   (require 'etags)
;;   (let ((orig (point)))
;;     (if (and (derived-mode-p 'sv-mode)
;;              (sv-goto-function-definition fn))
;;         (ring-insert find-tag-marker-ring (copy-marker orig))
;;       (inferior-sv-send-list-and-digest
;;        ;; help NAME is more verbose
;;        (list (format "\
;; if iskeyword('%s') disp('`%s'' is a keyword') else which('%s') endif\n"
;;                      fn fn fn)))
;;       (let (line file)
;;         ;; Skip garbage lines such as
;;         ;;     warning: fmincg.m: possible Matlab-style ....
;;         (while (and (not file) (consp inferior-sv-output-list))
;;           (setq line (pop inferior-sv-output-list))
;;           (when (string-match "from the file \\(.*\\)$" line)
;;             (setq file (match-string 1 line))))
;;         (if (not file)
;;             (user-error "%s" (or line (format "`%s' not found" fn)))
;;           (ring-insert find-tag-marker-ring (point-marker))
;;           (setq file (funcall sv-find-definition-filename-function file))
;;           (when file
;;             (find-file file)
;;             (sv-goto-function-definition fn)))))))

;; (provide 'sv)
;; ;;; sv.el ends here
