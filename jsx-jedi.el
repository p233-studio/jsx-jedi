;;; jsx-jedi.el --- Enlightened JS/TS/JSX editing powers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Peiwen Lu

;; Author: Peiwen Lu <hi@peiwen.lu>
;; Version: 0.0.1
;; Created: 20 May 2024
;; Keywords: languages convenience tools tree-sitter javascript typescript jsx react
;; URL: https://github.com/p233-studio/jsx-jedi
;; Compatibility: emacs-version >= 29.1
;; Package-Requires: ((emacs "29.1") (avy "0.5"))

;;; This file is NOT part of GNU Emacs

;;; License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; JSX-Jedi brings enlightened editing powers to your JavaScript, TypeScript
;; and JSX development experience in Emacs.

;; For detailed documentation and usage instructions, visit:
;; https://github.com/p233-studio/jsx-jedi#readme

;;; Code:

(require 'avy)

(defvar jsx-jedi-kill-nodes      '("expression_statement"
                                   "function_declaration"
                                   "import_statement"
                                   "interface_declaration"
                                   "jsx_element"
                                   "jsx_expression"
                                   "jsx_self_closing_element"
                                   "lexical_declaration"
                                   "object"
                                   "pair"
                                   "return_statement"
                                   "type_alias_declaration"))

(defvar jsx-jedi-empty-nodes     '("arguments"
                                   "array"
                                   "array_pattern"
                                   "formal_parameters"
                                   "jsx_element"
                                   "jsx_expression"
                                   "named_imports"
                                   "object"
                                   "object_pattern"
                                   "statement_block"
                                   "string"
                                   "template_string"))

(defvar jsx-jedi-zap-nodes       '("arguments"
                                   "array"
                                   "array_pattern"
                                   "formal_parameters"
                                   "jsx_expression"
                                   "jsx_opening_element"
                                   "jsx_self_closing_element"
                                   "named_imports"
                                   "object_pattern"
                                   "string"
                                   "template_string"))

(defvar jsx-jedi-copy-nodes      '("expression_statement"
                                   "function_declaration"
                                   "interface_declaration"
                                   "jsx_element"
                                   "jsx_self_closing_element"
                                   "lexical_declaration"
                                   "pair"
                                   "string"
                                   "template_string"
                                   "type_alias_declaration"))

(defvar jsx-jedi-duplicate-nodes '("function_declaration"
                                   "jsx_element"
                                   "jsx_self_closing_element"
                                   "lexical_declaration"
                                   "pair"))

(defvar jsx-jedi-mark-nodes      '("expression_statement"
                                   "function_declaration"
                                   "interface_declaration"
                                   "jsx_element"
                                   "jsx_self_closing_element"
                                   "lexical_declaration"
                                   "pair"
                                   "return_statement"
                                   "statement_block"
                                   "type_alias_declaration"))

(defvar jsx-jedi-comment-nodes   '("expression_statement"
                                   "import_statement"
                                   "interface_declaration"
                                   "jsx_element"
                                   "jsx_self_closing_element"
                                   "lexical_declaration"
                                   "pair"
                                   "return_statement"
                                   "function_declaration"
                                   "type_alias_declaration"))

(defvar jsx-jedi-avy-nodes       '("expression_statement"
                                   "function_declaration"
                                   "import_statement"
                                   "interface_declaration"
                                   "jsx_element"
                                   "jsx_self_closing_element"
                                   "lexical_declaration"
                                   "return_statement"
                                   "statement_block"
                                   "string"
                                   "template_string"
                                   "type_alias_declaration"))

(defun jsx-jedi--kill-region-and-goto-start (start end)
  "Kill the region between START and END, and move point to START."
  (kill-region start end)
  (goto-char start))

(defun jsx-jedi--find-comment-block-bounds (node)
  "Get the bounds of the comment block containing NODE.
Return a cons cell (START . END) representing the bounds."
  (let ((start-node node)
        (end-node node))
    (while (string= (treesit-node-type (treesit-node-prev-sibling start-node)) "comment")
      (setq start-node (treesit-node-prev-sibling start-node)))
    (while (and end-node (string= (treesit-node-type (treesit-node-next-sibling end-node)) "comment"))
      (setq end-node (treesit-node-next-sibling end-node)))
    (cons (treesit-node-start start-node) (treesit-node-end end-node))))

(defun jsx-jedi--find-containing-node (node position)
  "Find the first ancestor node (including NODE itself) that contains POSITION."
  (when node
    (let ((start (treesit-node-start node))
          (end (treesit-node-end node)))
      (if (and (<= start position) (<= position end))
          node
        (jsx-jedi--find-containing-node (treesit-node-parent node) position)))))

(defun jsx-jedi--node-at ()
  "Find the nearest node that contains point."
  (let ((node (treesit-node-at (point))))
    (jsx-jedi--find-containing-node node (point))))

(defun jsx-jedi-kill ()
  "Kill the suitable syntax node at point."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (bounds (if (string= (treesit-node-type node) "comment")
                          (jsx-jedi--find-comment-block-bounds node)
                        (when-let ((parent (treesit-parent-until node (lambda (n)
                                                                        (let ((node-type (treesit-node-type n)))
                                                                          (if (string= node-type "jsx_expression")
                                                                              (not (treesit-parent-until n (lambda (m)
                                                                                                             (string= (treesit-node-type m) "jsx_attribute"))))
                                                                            (member node-type jsx-jedi-kill-nodes)))))))
                          (let ((kill-comma (and (member (treesit-node-type parent) '("object" "pair"))
                                                 (string= (treesit-node-text (treesit-node-next-sibling parent) t) ","))))
                            (cons (treesit-node-start parent) (if kill-comma
                                                                  (1+ (treesit-node-end parent))
                                                                (treesit-node-end parent))))))))
    (kill-region (car bounds) (cdr bounds))
    (when (save-excursion
            (beginning-of-line)
            (looking-at-p "^[[:space:]]*$"))
      (delete-blank-lines)
      (indent-for-tab-command))))


(defun jsx-jedi-empty ()
  "Empty the content of the JSX element or other suitable syntax node at point.

The function intentionally skips JSX attribute nodes, as the
jsx-jedi-kill-attribute-value function is specifically designed for emptying
attribute values, providing a clearer separation of concerns."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (element (treesit-parent-until node (lambda (n)
                                                    (let ((node-type (treesit-node-type n)))
                                                      (if (string= node-type "jsx_expression")
                                                          (not (treesit-parent-until n (lambda (m)
                                                                                         (string= (treesit-node-type m) "jsx_attribute"))))
                                                        (member node-type jsx-jedi-empty-nodes))))))
              (opening-node (treesit-node-child element 0))
              (closing-node (treesit-node-child element -1))
              (start (treesit-node-end opening-node))
              (end (treesit-node-start closing-node)))
    (jsx-jedi--kill-region-and-goto-start start end)))


(defun jsx-jedi-zap ()
  "Zap the suitable syntax node at point to the end."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (parent (treesit-parent-until node (lambda (n)
                                                   (member (treesit-node-type n) jsx-jedi-zap-nodes))))
              (end (1- (treesit-node-end parent))))
    (delete-region (point) end)))


(defun jsx-jedi-copy ()
  "Copy the suitable syntax node at point to the kill ring."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (parent (treesit-parent-until node (lambda (n)
                                                   (member (treesit-node-type n) jsx-jedi-copy-nodes))))
              (start (treesit-node-start parent))
              (end (treesit-node-end parent)))
    (kill-ring-save start end)
    (pulse-momentary-highlight-region start end)))


(defun jsx-jedi-duplicate ()
  "Duplicate the suitable syntax node at point."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (element (treesit-parent-until node (lambda (n)
                                                    (member (treesit-node-type n) jsx-jedi-duplicate-nodes))))
              (element-text (treesit-node-text element t))
              (end (treesit-node-end element)))
    (goto-char end)
    (newline)
    (insert element-text)
    (indent-region end (point))
    (let ((highlight-start (save-excursion
                             (goto-char end)
                             (skip-chars-forward " \t\n")
                             (point))))
      (pulse-momentary-highlight-region highlight-start (point)))))

(defun jsx-jedi-mark ()
  "Select the suitable syntax node at point."
  (interactive)
  (let ((node (jsx-jedi--node-at)))
    (if (string= (treesit-node-type node) "comment")
        (when-let* ((bounds (jsx-jedi--find-comment-block-bounds node))
                    (start (car bounds))
                    (end (cdr bounds)))
          (goto-char start)
          (set-mark end)
          (activate-mark))
      (when-let* ((parent (treesit-parent-until node (lambda (n)
                                                       (member (treesit-node-type n) jsx-jedi-mark-nodes))))
                  (start (treesit-node-start parent))
                  (end (treesit-node-end parent)))
        (goto-char start)
        (set-mark end)
        (activate-mark)))))


(defun jsx-jedi-comment-uncomment ()
  "Comment or uncomment the suitable syntax node at point."
  (interactive)
  (let* ((node (jsx-jedi--node-at))
         (node-comment-p (string= (treesit-node-type node) "comment"))
         (parent-jsx-expression-p (string= (treesit-node-type (treesit-node-parent node)) "jsx_expression"))
         (normal-comment-p (and node-comment-p
                                (not parent-jsx-expression-p)))
         (jsx-comment-p (and parent-jsx-expression-p
                             (or node-comment-p
                                 (string= (treesit-node-type (treesit-node-prev-sibling node)) "comment")
                                 (string= (treesit-node-type (treesit-node-next-sibling node)) "comment")))))
    (cond
     (normal-comment-p
      (let* ((bounds (jsx-jedi--find-comment-block-bounds node))
             (start (car bounds))
             (end (cdr bounds)))
        (uncomment-region start end)))
     (jsx-comment-p
      (let* ((comment (treesit-parent-until node (lambda (n)
                                                   (string= (treesit-node-type n) "jsx_expression")) t))
             (comment-text (treesit-node-text comment t))
             (uncomment-text (replace-regexp-in-string "{/\\*[[:space:]]*" "" (replace-regexp-in-string "[[:space:]]*\\*/}" "" comment-text)))
             (start (treesit-node-start comment))
             (end (treesit-node-end comment)))
        (delete-region start end)
        (insert uncomment-text)
        (indent-region start end)
        (goto-char start)))
     ((not (or normal-comment-p jsx-comment-p))
      (when-let* ((parent (treesit-parent-until node (lambda (n)
                                                       (member (treesit-node-type n) jsx-jedi-comment-nodes))))
                  (start (treesit-node-start parent))
                  (end (treesit-node-end parent)))
        (if (member (treesit-node-type parent) '("jsx_element" "jsx_self_closing_element"))
            (let ((comment-text (concat "{/* " (treesit-node-text parent t) " */}"))
                  (start (treesit-node-start parent))
                  (end (treesit-node-end parent)))
              (kill-region start end)
              (insert comment-text)
              (indent-region start end))
          (comment-region start end)))))))


(defun jsx-jedi-avy-word ()
  "Jump to a word within the nearest suitable parent node at point using Avy."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (parent (treesit-parent-until node (lambda (n)
                                                   (member (treesit-node-type n) jsx-jedi-avy-nodes))))
              (start (treesit-node-start parent))
              (end (treesit-node-end parent)))
    (avy-goto-word-0 t start end)))


(defun jsx-jedi-raise-element ()
  "Raise the JSX element at point."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (element (treesit-parent-until node (lambda (n)
                                                    (member (treesit-node-type n)
                                                            '("jsx_element"
                                                              "jsx_expression"
                                                              "jsx_self_closing_element")))))
              (element-text (treesit-node-text element t))
              (element-parent (treesit-parent-until element (lambda (n)
                                                              (string= (treesit-node-type n) "jsx_element"))))
              (start (treesit-node-start element-parent))
              (end (treesit-node-end element-parent)))
    (delete-region start end)
    (insert element-text)
    (indent-region start (point))))


(defun jsx-jedi-move-to-opening-tag ()
  "Move point to the opening tag of the JSX element at point."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (element (treesit-parent-until node (lambda (n)
                                                    (string= (treesit-node-type n) "jsx_element"))))
              (opening-node (treesit-node-child element 0))
              (position (1- (treesit-node-end opening-node))))
    (goto-char position)))


(defun jsx-jedi-move-to-closing-tag ()
  "Move point to the closing tag of the JSX element at point."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (element (treesit-parent-until node (lambda (n)
                                                    (string= (treesit-node-type n) "jsx_element"))))
              (closing-node (treesit-node-child element -1))
              (position (+ 2 (treesit-node-start closing-node))))
    (goto-char position)))


(defun jsx-jedi-kill-attribute ()
  "Kill the JSX attribute at point."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (attribute (treesit-parent-until node (lambda (n)
                                                      (string= (treesit-node-type n) "jsx_attribute"))))
              (start (treesit-node-start attribute))
              (end (treesit-node-end attribute)))
    (if (string= (buffer-substring-no-properties (1- start) start) " ")
        (kill-region (1- start) end)
      (kill-region start end))))


(defun jsx-jedi-copy-attribute ()
  "Copy the JSX attribute at point to the kill ring."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (attribute (treesit-parent-until node (lambda (n)
                                                      (string= (treesit-node-type n) "jsx_attribute"))))
              (start (treesit-node-start attribute))
              (end (treesit-node-end attribute)))
    (kill-ring-save start end)
    (pulse-momentary-highlight-region start end)))


(defun jsx-jedi-kill-attribute-value ()
  "Kill the value of the JSX attribute at point."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (attribute (treesit-parent-until node (lambda (n)
                                                      (string= (treesit-node-type n) "jsx_attribute"))))
              (value (treesit-node-child attribute -1)))
    (let ((start (1+ (treesit-node-start value)))
          (end (1- (treesit-node-end value))))
      (jsx-jedi--kill-region-and-goto-start start end))))


(defun jsx-jedi-move-to-prev-attribute ()
  "Move point to the previous JSX attribute."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (element (if (member (treesit-node-text node t) '(">" "/>"))
                           node
                         (treesit-parent-until node (lambda (n)
                                                      (string= (treesit-node-type n) "jsx_attribute")))))
              (prev-element (treesit-node-prev-sibling element))
              (attribute-p (string= (treesit-node-type prev-element) "jsx_attribute")))
    (goto-char (treesit-node-start prev-element))))


(defun jsx-jedi-move-to-next-attribute ()
  "Move point to the next JSX attribute."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (element (if (string= (treesit-node-type node) "identifier")
                           node
                         (treesit-parent-until node (lambda (n)
                                                      (string= (treesit-node-type n) "jsx_attribute")))))
              (next-element (treesit-node-next-sibling element))
              (attribute-p (string= (treesit-node-type next-element) "jsx_attribute")))
    (goto-char (treesit-node-start next-element))))


(defun jsx-jedi-declaration-to-if-statement ()
  "Convert the variable declaration at point to an if statement."
  (interactive)
  (when-let* ((node (jsx-jedi--node-at))
              (parent (treesit-parent-until node (lambda (n)
                                                   (string= (treesit-node-type n) "lexical_declaration"))))
              (value (treesit-search-subtree parent (lambda (n)
                                                      (string= (treesit-node-type n) "call_expression"))))
              (value-text (treesit-node-text value t))
              (start (treesit-node-start parent))
              (end (treesit-node-end parent)))
    (delete-region start end)
    (insert (format "if (%s) {\n\n}" value-text))
    (indent-region start (point))
    (forward-line -1)
    (indent-for-tab-command)))


;;;###autoload
(define-minor-mode jsx-jedi-mode
  "Minor mode for JSX-related editing commands, specifically designed for
tsx-ts-mode and typescript-ts-mode."
  :lighter " JSX Jedi"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-k")     'jsx-jedi-kill)
            (define-key map (kbd "C-c C-e")     'jsx-jedi-empty)
            (define-key map (kbd "C-c C-z")     'jsx-jedi-zap)
            (define-key map (kbd "C-c C-w")     'jsx-jedi-copy)
            (define-key map (kbd "C-c C-x")     'jsx-jedi-duplicate)
            (define-key map (kbd "C-c C-SPC")   'jsx-jedi-mark)
            (define-key map (kbd "C-c C-;")     'jsx-jedi-comment-uncomment)
            (define-key map (kbd "C-c C-j")     'jsx-jedi-avy-word)
            (define-key map (kbd "C-c C-t C-r") 'jsx-jedi-raise-element)
            (define-key map (kbd "C-c C-t C-,") 'jsx-jedi-move-to-opening-tag)
            (define-key map (kbd "C-c C-t C-.") 'jsx-jedi-move-to-closing-tag)
            (define-key map (kbd "C-c C-a C-k") 'jsx-jedi-kill-attribute)
            (define-key map (kbd "C-c C-a C-w") 'jsx-jedi-copy-attribute)
            (define-key map (kbd "C-c C-a C-v") 'jsx-jedi-kill-attribute-value)
            (define-key map (kbd "C-c C-a C-p") 'jsx-jedi-move-to-prev-attribute)
            (define-key map (kbd "C-c C-a C-n") 'jsx-jedi-move-to-next-attribute)
            map))


;;;###autoload
(add-hook 'js-ts-mode-hook #'jsx-jedi-mode)

;;;###autoload
(add-hook 'tsx-ts-mode-hook #'jsx-jedi-mode)

;;;###autoload
(add-hook 'typescript-ts-mode-hook #'jsx-jedi-mode)


(provide 'jsx-jedi)

;;; jsx-jedi.el ends here
