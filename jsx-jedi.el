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


(defvar jsx-jedi-kill-node-types      '("comment"
                                        "expression_statement"
                                        "function_declaration"
                                        "import_statement"
                                        "interface_declaration"
                                        "jsx_attribute"
                                        "jsx_element"
                                        "jsx_expression"
                                        "jsx_self_closing_element"
                                        "lexical_declaration"
                                        "object"
                                        "pair"
                                        "return_statement"
                                        "type_alias_declaration"))

(defvar jsx-jedi-empty-node-types     '("arguments"
                                        "array"
                                        "array_pattern"
                                        "formal_parameters"
                                        "jsx_attribute"
                                        "jsx_element"
                                        "jsx_expression"
                                        "named_imports"
                                        "object"
                                        "object_pattern"
                                        "pair"
                                        "statement_block"
                                        "string"
                                        "template_string"))

(defvar jsx-jedi-zap-node-types       '("arguments"
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

(defvar jsx-jedi-copy-node-types      '("expression_statement"
                                        "function_declaration"
                                        "interface_declaration"
                                        "jsx_attribute"
                                        "jsx_element"
                                        "jsx_self_closing_element"
                                        "lexical_declaration"
                                        "pair"
                                        "string"
                                        "template_string"
                                        "type_alias_declaration"))

(defvar jsx-jedi-duplicate-node-types '("function_declaration"
                                        "jsx_element"
                                        "jsx_self_closing_element"
                                        "lexical_declaration"
                                        "pair"))

(defvar jsx-jedi-mark-node-types      '("comment"
                                        "expression_statement"
                                        "function_declaration"
                                        "interface_declaration"
                                        "jsx_element"
                                        "jsx_self_closing_element"
                                        "lexical_declaration"
                                        "pair"
                                        "return_statement"
                                        "statement_block"
                                        "type_alias_declaration"))

(defvar jsx-jedi-comment-node-types   '("expression_statement"
                                        "import_statement"
                                        "interface_declaration"
                                        "jsx_element"
                                        "jsx_self_closing_element"
                                        "lexical_declaration"
                                        "pair"
                                        "return_statement"
                                        "function_declaration"
                                        "type_alias_declaration")
  "This list is used to find nodes that can be commented. No need to include `comment' here.")

(defvar jsx-jedi-avy-node-types       '("expression_statement"
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

(defvar jsx-jedi-raise-node-types     '("jsx_element"
                                        "jsx_expression"
                                        "jsx_self_closing_element"))

(defvar jsx-jedi-tag-node-types       '("jsx_element"
                                        "jsx_self_closing_element"))


(defun jsx-jedi--kill-region-and-goto-start (start end)
  "Kill the region between START and END, and move point to START."
  (kill-region start end)
  (goto-char start))


(defun jsx-jedi--delete-blank-line ()
  "Delete the blank line at point if it exists, and indent the line."
  (when (save-excursion
          (beginning-of-line)
          (looking-at-p "^[[:space:]]*$"))
    (delete-blank-lines)
    (indent-for-tab-command)))


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


(defun jsx-jedi--find-node-at-point (node position)
  "Find the first ancestor node (including NODE itself) that contains POSITION."
  (when node
    (let ((start (treesit-node-start node))
          (end (treesit-node-end node)))
      (if (and (<= start position) (<= position end))
          node
        (let ((parent (treesit-node-parent node)))
          (when (and parent
                     (or (not (= start (treesit-node-start parent)))
                         (not (= end (treesit-node-end parent)))))
            (jsx-jedi--find-node-at-point parent position)))))))


(defun jsx-jedi--find-node-info (valid-types)
  "Find node at point matching one of VALID-TYPES.
VALID-TYPES is a list of node types to consider.
Returns a list (TYPE START END NODE) with node type, bounds and the node itself,
or nil if no matching node is found."
  (let ((node (treesit-node-at (point))))
    (if (and (string= (treesit-node-type node) "comment")
             (member "comment" valid-types))
        (let ((bounds (jsx-jedi--find-comment-block-bounds node)))
          (list "comment" (car bounds) (cdr bounds) node))
      (when-let* ((node-at-point (jsx-jedi--find-node-at-point node (point)))
                  (found-node (treesit-parent-until node-at-point (lambda (n)
                                                                    (member (treesit-node-type n) valid-types)) t)))
        (list (treesit-node-type found-node)
              (treesit-node-start found-node)
              (treesit-node-end found-node)
              found-node)))))


(defun jsx-jedi-kill ()
  "Kill the suitable syntax node at point."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-kill-node-types))
              (type (nth 0 node-info))
              (start (nth 1 node-info))
              (end (nth 2 node-info))
              (node (nth 3 node-info)))
    (let ((kill-end end))
      (when (and (member type '("object" "pair"))
                 (string= (treesit-node-text (treesit-node-next-sibling node) t) ","))
        (setq kill-end (1+ end)))
      (kill-region start kill-end)
      (jsx-jedi--delete-blank-line))))


(defun jsx-jedi-empty ()
  "Empty the content of the JSX element or other suitable syntax node at point."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-empty-node-types))
              (type (nth 0 node-info))
              (node (nth 3 node-info)))
    (pcase type
      ("jsx_attribute"
       (when-let ((value-node (treesit-node-child node -1)))
         (jsx-jedi--kill-region-and-goto-start
          (1+ (treesit-node-start value-node))
          (1- (treesit-node-end value-node)))))
      ("pair"
       (when-let ((value-node (treesit-node-child node -1)))
         (jsx-jedi--kill-region-and-goto-start
          (treesit-node-start value-node)
          (treesit-node-end value-node))))
      (_
       (when-let ((opening-node (treesit-node-child node 0))
                  (closing-node (treesit-node-child node -1)))
         (jsx-jedi--kill-region-and-goto-start
          (treesit-node-end opening-node)
          (treesit-node-start closing-node)))))))


(defun jsx-jedi-zap ()
  "Zap the suitable syntax node at point to the end."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-zap-node-types))
              (end (1- (nth 2 node-info))))
    (delete-region (point) end)))


(defun jsx-jedi-copy ()
  "Copy the suitable syntax node at point to the kill ring."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-copy-node-types))
              (start (nth 1 node-info))
              (end (nth 2 node-info)))
    (kill-ring-save start end)
    (pulse-momentary-highlight-region start end)))


(defun jsx-jedi-duplicate ()
  "Duplicate the suitable syntax node at point."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-duplicate-node-types))
              (end (nth 2 node-info))
              (node (nth 3 node-info))
              (text (treesit-node-text node t)))
    (goto-char end)
    (newline)
    (insert text)
    (indent-region end (point))
    (let ((highlight-start (save-excursion
                             (goto-char end)
                             (skip-chars-forward " \t\n")
                             (point))))
      (pulse-momentary-highlight-region highlight-start (point)))))


(defun jsx-jedi-mark ()
  "Select the suitable syntax node at point."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-mark-node-types))
              (start (nth 1 node-info))
              (end (nth 2 node-info)))
    (goto-char start)
    (set-mark end)
    (activate-mark)))


(defun jsx-jedi-comment-uncomment ()
  "Comment or uncomment the suitable syntax node at point."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (comment-p (string= (treesit-node-type node) "comment"))
         (in-jsx-expression-p (string= (treesit-node-type (treesit-node-parent node)) "jsx_expression"))
         (js-comment-p (and comment-p
                            (not in-jsx-expression-p)))
         (jsx-comment-p (and in-jsx-expression-p
                             (or comment-p
                                 (string= (treesit-node-type (treesit-node-prev-sibling node)) "comment")
                                 (string= (treesit-node-type (treesit-node-next-sibling node)) "comment")))))
    (cond
     (js-comment-p
      (let* ((bounds (jsx-jedi--find-comment-block-bounds node))
             (start (car bounds))
             (end (cdr bounds)))
        (uncomment-region start end)))
     (jsx-comment-p
      (let* ((comment (treesit-node-parent node))
             (comment-text (treesit-node-text comment t))
             (uncomment-text (replace-regexp-in-string "{/\\*[[:space:]]*" "" (replace-regexp-in-string "[[:space:]]*\\*/}" "" comment-text)))
             (start (treesit-node-start comment))
             (end (treesit-node-end comment)))
        (delete-region start end)
        (insert uncomment-text)))
     (t
      (when-let* ((element (treesit-parent-until node (lambda (n)
                                                        (member (treesit-node-type n) jsx-jedi-comment-node-types)) t))
                  (start (treesit-node-start element))
                  (end (treesit-node-end element)))
        (if (member (treesit-node-type element) jsx-jedi-tag-node-types)
            (let ((comment-text (concat "{/* " (treesit-node-text element t) " */}")))
              (kill-region start end)
              (insert comment-text))
          (if (eq (char-after end) ?,)
              (comment-region start (1+ end))
            (comment-region start end))))))))


(defun jsx-jedi-avy-word ()
  "Jump to a word within the nearest suitable parent node at point using Avy."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-avy-node-types))
              (start (nth 1 node-info))
              (end (nth 2 node-info)))
    (avy-goto-word-0 t start end)))


(defun jsx-jedi-raise ()
  "Raise the JSX element at point."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-raise-node-types))
              (node (nth 3 node-info))
              (text (treesit-node-text node t))
              (parent (treesit-parent-until node (lambda (n)
                                                   (string= (treesit-node-type n) "jsx_element")) t))
              (start (treesit-node-start parent))
              (end (treesit-node-end parent)))
    (delete-region start end)
    (insert text)
    (indent-region start (point))))


(defun jsx-jedi-wrap-tap ()
  "Wrap the JSX element at point with user inputted tag name."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-tag-node-types))
              (start (nth 1 node-info))
              (end (nth 2 node-info))
              (node (nth 3 node-info))
              (text (treesit-node-text node t))
              (tag-name (read-string "Enter wrapping tag name: "))
              (wrapped-text (concat "<" tag-name ">\n" text "\n</" tag-name ">")))
    (delete-region start end)
    (insert wrapped-text)
    (indent-region start (point))))


(defun jsx-jedi-move-to-opening-tag ()
  "Move point to the opening tag of the JSX element at point."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-tag-node-types))
              (node (nth 3 node-info))
              (opening-node (treesit-node-child node 0))
              (start (treesit-node-start opening-node)))
    (goto-char start)))


(defun jsx-jedi-move-to-closing-tag ()
  "Move point to the closing tag of the JSX element at point."
  (interactive)
  (when-let* ((node-info (jsx-jedi--find-node-info jsx-jedi-tag-node-types))
              (type (nth 0 node-info))
              (node (nth 3 node-info))
              (closing-node (treesit-node-child node -1)))
    (if (string= type "jsx_self_closing_element")
        (goto-char (1+ (treesit-node-start closing-node)))
      (goto-char (1- (treesit-node-end closing-node))))))


;;;###autoload
(define-minor-mode jsx-jedi-mode
  "Minor mode for JSX-related editing commands, specifically designed for
tsx-ts-mode and typescript-ts-mode."
  :lighter " JSX Jedi"
  :keymap (make-sparse-keymap))

;;;###autoload
(add-hook 'js-ts-mode-hook #'jsx-jedi-mode)

;;;###autoload
(add-hook 'tsx-ts-mode-hook #'jsx-jedi-mode)

;;;###autoload
(add-hook 'typescript-ts-mode-hook #'jsx-jedi-mode)



(provide 'jsx-jedi)

;;; jsx-jedi.el ends here
