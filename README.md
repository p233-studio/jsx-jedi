# jsx-jedi-mode

This project is working in progress.

## Installation

```elisp
(use-package avy)

(use-package jsx-jedi
  :straight (:type git :host github :repo "p233-studio/jsx-jedi"))
```

## Functions

All functions are specific to the [tree-sitter-typescript](https://github.com/tree-sitter/tree-sitter-typescript) parser and only work in the Emacs 29 built-in `tsx-ts-mode` and `typescript-ts-mode`.

### `jsx/kill`

Kill the appropriate syntax node at the current point. This function works with the following tree-sitter node types:

- `expression_statement`
- `function_declaration`
- `import_statement`
- `interface_declaration`
- `jsx_element`
- `jsx_expression`
- `jsx_self_closing_element`
- `lexical_declaration`
- `object`
- `pair`
- `return_statement`
- `type_alias_declaration`

_Note: This function intentionally skips `jsx_attribute` nodes in the `jsx_expression` format._

### `jsx/empty`

Empty the content of the JSX element or other suitable syntax node at point.

_This function intentionally skips JSX attribute nodes, as the `jsx/kill-attribute-value` function is specifically designed for emptying attribute values, providing a clearer separation of concerns._

### `jsx/zap`

Zap the suitable syntax node at point to the end.

### `jsx/copy`

Copy the suitable syntax node at point to the kill ring.

### `jsx/duplicate`

Duplicate the suitable syntax node at point.

### `jsx/mark`

Select the suitable syntax node at point.

### `jsx/comment-uncomment`

Comment or uncomment the suitable syntax node at point.

### `jsx/avy-word`

Jump to a word within the nearest suitable parent node at point using Avy.

### `jsx/raise-element`

Raise the JSX element at point.

### `jsx/move-to-opening-tag`

Move point to the opening tag of the JSX element at point.

### `jsx/move-to-closing-tag`

Move point to the closing tag of the JSX element at point.

### `jsx/kill-attribute`

Kill the JSX attribute at point.

### `jsx/copy-attribute`

Copy the JSX attribute at point to the kill ring.

### `jsx/kill-attribute-value`

Kill the value of the JSX attribute at point.
 
### `jsx/move-to-preview-attribute`

Move point to the previous JSX attribute.

### `jsx/move-to-next-attribute`

Move point to the next JSX attribute.

### `jsx/declaration-to-if-statement`

Convert the variable declaration at point to an if statement.

## Key bindings

General:

- `C-c C-k`: `jsx/kill`
- `C-c C-e`: `jsx/empty`
- `C-c C-z`: `jsx/zap`
- `C-c C-w`: `jsx/copy`
- `C-c C-x`: `jsx/duplicate`
- `C-c C-SPC`: `jsx/mark`
- `C-c C-;`: `jsx/comment-uncomment`
- `C-c C-j`: `jsx/avy-word`

Tag manipulation:

- `C-c C-t C-r`: `jsx/raise-element`
- `C-c C-t C-,`: `jsx/move-to-opening-tag`
- `C-c C-t C-.`: `jsx/move-to-closing-tag`

Attribute manipulation:

- `C-c C-a C-k`: `jsx/kill-attribute`
- `C-c C-a C-w`: `jsx/copy-attribute`
- `C-c C-a C-v`: `jsx/kill-attribute-value`
- `C-c C-a C-p`: `jsx/move-to-prev-attribute`
- `C-c C-a C-n`: `jsx/move-to-next-attribute`

Note: There is no default key binding for function `jsx/declaration-to-if-statement`. You can bind it to a key of your choice using `define-key` or `global-set-key`.

You can customize these keybindings by modifying the `jsx-jedi-mode-map` variable or by using `define-key` in your Emacs configuration.

## Tree-sitter nodes and actions

### `arguments`

```tsx
func(arguments)
```

The `(arguments)` in `call_expression`

### `array_pattern`

```tsx
const [a, b] = func();
```

The `[a, b]` in `lexical_declaration`

### `array`

```tsx
[1, 2, 3]
```

### `comment`

```tsx
// inline comment
/* block comment */
{
  /* block comment */
}
```

### `expression_statement`

```tsx
func();
obj.mothod();
```

### `formal_parameters`

```tsx
function func(parameters) {}
```

The `(parameters)` in `function_declaration`

### `function_declaration`

```tsx
function func() {}
```

### `import_statement`

```tsx
import X from "x";
import { X } from "x";
```

### `interface_declaration`

```tsx
interface XXX {}
```

### `jsx_attribute`

```tsx
<tag attribute=XXX></tag>
```

The `attribute=XXX` in `jsx_opening_element` or `jsx_self_closing_element`

### `jsx_element`

```tsx
<tag></tag>
```

### `jsx_expression`

All `{}` in jsx syntax

### `jsx_opening_element`

```tsx
<tag></tag>
```

The opening `<tag>` in `jsx_element`

### `jsx_self_closing_element`

```tsx
<tag />
```

### `lexical_declaration`

```tsx
var a = 1;
let b = 1;
const c = 1;
```

### `named_imports`

```tsx
import { X } from "x";
```

The `{}` in `import_statement`

### `object_pattern`

```tsx
const { a, b } = func();
```

The `{ a, b }` in `lexical_declaration`

### `object`

```tsx
{}
```

### `pair`

`property: value` in Object

### `return_statement`

```tsx
return ...;
```

### `statement_block`

```tsx
function func() {}
if {} else {}
```

The `{}` in `function_declaration` or `if_statement`

### `string`

```tsx
'...'
"..."
```

### `template_string`

```tsx
`...`
```

### `type_alias_declaration`

```tsx
type Type = {};
```
