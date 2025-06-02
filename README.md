# jsx-jedi-mode

**Working in progress.**

JSX Jedi streamlines editing by providing context-aware operations for JavaScript, TypeScript, and JSX/TSX code. Leveraging tree-sitter's precise syntax analysis, it intelligently identifies relevant code structures based on cursor position—eliminating the need for exact cursor placement or manual selection of complete statements.

Note: JSX Jedi is specifically designed for the [tree-sitter-typescript](https://github.com/tree-sitter/tree-sitter-typescript) parser and only works with the built-in `js-ts-mode`, `typescript-ts-mode`, and `tsx-ts-mode` in Emacs 29 and later versions.

## Installation

```elisp
(use-package avy)

(use-package jsx-jedi
  :straight (:type git :host github :repo "p233-studio/jsx-jedi"))
```

## How it works

### Smart Node Selection

When you execute a command like `jsx-jedi-kill` or `jsx-jedi-empty`, JSX Jedi:

1. Examines your current cursor position
2. Identifies the syntax node at that position
3. Traverses up the tree-sitter syntax tree, searching for a matching node type from a predefined list (e.g., `jsx-jedi-kill-node-types`)
4. Applies the requested operation to the most appropriate node

This intelligent selection means you can place your cursor *anywhere* within a structure, whether inside a JSX element, attribute, expression, or tag name, and JSX Jedi will act on the most logical enclosing Node. No more precise cursor positioning or manual selection required. Simply place the cursor somewhere in or near the target and execute the command, JSX Jedi handles the rest.

### Operation-Specific Node Types

Each operation in JSX Jedi has its own list of node types that it considers valid targets:

```elisp
(defvar jsx-jedi-kill-node-types '("comment"
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
                                   "type_alias_declaration")
    "Nodes that can be killed by jsx-jedi-kill.")
```

These node type lists serve as *action filters* that determine which syntax elements each command can operate on. You can customize these lists to adapt JSX Jedi's behavior to your specific workflow and preferences, adding or removing node types to make commands more selective or more inclusive.

## Functions

TODO

## Key Bindings

JSX Jedi provides an empty `jsx-jedi-mode-map` keymap, allowing you to create custom keybindings tailored to your workflow. Here's an example to get you started:

``` elisp
(use-package jsx-jedi
  :straight (:type git :host github :repo "p233-studio/jsx-jedi")
  :bind (:map jsx-jedi-mode-map
              ("C-c C-k"     . jsx-jedi-kill)
              ("C-c C-e"     . jsx-jedi-empty)
              ("C-c C-z"     . jsx-jedi-zap)
              ("C-c C-w"     . jsx-jedi-copy)
              ("C-c C-x"     . jsx-jedi-duplicate)
              ("C-c C-SPC"   . jsx-jedi-mark)
              ("C-c C-;"     . jsx-jedi-comment-uncomment)
              ("C-c C-j"     . jsx-jedi-avy-word)
              ("C-c C-t C-r" . jsx-jedi-raise)
              ("C-c C-t C-w" . jsx-jedi-wrap-tag)
              ("C-c C-t C-," . jsx-jedi-move-to-opening-tag)
              ("C-c C-t C-." . jsx-jedi-move-to-closing-tag)))
```

