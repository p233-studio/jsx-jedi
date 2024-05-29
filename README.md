# jsx-jedi-mode

Working in progress.

## Installation

## Functions

All functions are specific to the [tree-sitter-typescript](https://github.com/tree-sitter/tree-sitter-typescript) parser and only work in the Emacs 29 built-in `tsx-ts-mode` and `typescript-ts-mode`.

- `jsx/kill`

  Kill the suitable syntax node at point.

  The function intentionally skips JSX attribute nodes in `jsx_expression` format.

- `jsx/empty`

  Empty the content of the JSX element or other suitable syntax node at point.

  The function intentionally skips JSX attribute nodes, as the `jsx/kill-attribute-value` function is specifically designed for emptying attribute values, providing a clearer separation of concerns.

- `jsx/zap`

  Zap the suitable syntax node at point to the end.

- `jsx/copy`

  Copy the suitable syntax node at point to the kill ring.

- `jsx/duplicate`

  Duplicate the suitable syntax node at point.

- `jsx/mark`

  Select the suitable syntax node at point.

- `jsx/comment-uncomment`

  Comment or uncomment the suitable syntax node at point.

- `jsx/avy-word`

  Jump to a word within the nearest suitable parent node at point using Avy.

- `jsx/raise-element`

  Raise the JSX element at point.

- `jsx/move-to-opening-tag`

  Move point to the opening tag of the JSX element at point.

- `jsx/move-to-closing-tag`

  Move point to the closing tag of the JSX element at point.

- `jsx/kill-attribute`

  Kill the JSX attribute at point.

- `jsx/copy-attribute`

  Copy the JSX attribute at point to the kill ring.

- `jsx/kill-attribute-value`

  Kill the value of the JSX attribute at point.
 
- `jsx/move-to-preview-attribute`

  Move point to the previous JSX attribute.

- `jsx/move-to-next-attribute`

  Move point to the next JSX attribute.

- `jsx/declaration-to-if-statement`

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

#### Actions:

- `jsx/empty`
- `jsx/zap`

### `array_pattern`

```tsx
const [a, b] = func();
```

The `[a, b]` in `lexical_declaration`

#### Actions:

- `jsx/empty`
- `jsx/zap`

### `array`

```tsx
[1, 2, 3]
```

#### Actions:

- `jsx/empty`
- `jsx/zap`

### `comment`

```tsx
// inline comment
/* block comment */
{
  /* block comment */
}
```

#### Actions:

- `jsx/kill`
- `jsx/mark`
- `jsx/comment-uncomment`

### `expression_statement`

```tsx
func();
obj.mothod();
```

#### Actions:

- `jsx/kill`
- `jsx/copy`
- `jsx/mark`
- `jsx/comment-uncomment`
- `jsx/avy-word`

### `formal_parameters`

```tsx
function func(parameters) {}
```

The `(parameters)` in `function_declaration`

#### Actions:

- `jsx/empty`
- `jsx/zap`

### `function_declaration`

```tsx
function func() {}
```

#### Actions:

- `jsx/kill`
- `jsx/copy`
- `jsx/duplicate`
- `jsx/mark`
- `jsx/comment-uncomment`
- `jsx/avy-word`

### `import_statement`

```tsx
import X from "x";
import { X } from "x";
```

#### Actions:

- `jsx/kill`
- `jsx/comment-uncomment`
- `jsx/avy-word`

### `interface_declaration`

```tsx
interface XXX {}
```

#### Actions:

- `jsx/kill`
- `jsx/copy`
- `jsx/mark`
- `jsx/comment-uncomment`
- `jsx/avy-word`

### `jsx_attribute`

```tsx
<tag attribute=XXX></tag>
```

The `attribute=XXX` in `jsx_opening_element` or `jsx_self_closing_element`

#### Actions:

- `jsx/kill-attribute`
- `jsx/copy-attribute`
- `jsx/kill-attribute-value`
- `jsx/move-to-prev-attribute`
- `jsx/move-to-next-attribute`

### `jsx_element`

```tsx
<tag></tag>
```

#### Actions:

- `jsx/kill`
- `jsx/empty`
- `jsx/copy`
- `jsx/duplicate`
- `jsx/mark`
- `jsx/comment-uncomment`
- `jsx/avy-word`
- `jsx/raise-element`
- `jsx/move-to-opening-tag`
- `jsx/move-to-closing-tag`

### `jsx_expression`

All `{}` in jsx syntax

#### Actions:

- `jsx/kill`
- `jsx/empty`
- `jsx/zap`
- `jsx/raise-element`

### `jsx_opening_element`

```tsx
<tag></tag>
```

The opening `<tag>` in `jsx_element`

#### Actions:

- `jsx/zap`

### `jsx_self_closing_element`

```tsx
<tag />
```

#### Actions:

- `jsx/kill`
- `jsx/zap`
- `jsx/copy`
- `jsx/duplicate`
- `jsx/mark`
- `jsx/comment-uncomment`
- `jsx/avy-word`
- `jsx/raise-element`

### `lexical_declaration`

```tsx
var a = 1;
let b = 1;
const c = 1;
```

#### Actions:

- `jsx/kill`
- `jsx/copy`
- `jsx/duplicate`
- `jsx/mark`
- `jsx/comment-uncomment`
- `jsx/avy-word`
- `jsx/declaration-to-if-statement`

### `named_imports`

```tsx
import { X } from "x";
```

The `{}` in `import_statement`

#### Actions:

- `jsx/empty`
- `jsx/zap`

### `object_pattern`

```tsx
const { a, b } = func();
```

The `{ a, b }` in `lexical_declaration`

#### Actions:

- `jsx/empty`
- `jsx/zap`

### `object`

```tsx
{}
```

#### Actions:

- `jsx/kill`
- `jsx/empty`

### `pair`

`property: value` in Object

#### Actions:

- `jsx/kill`
- `jsx/copy`
- `jsx/duplicate`
- `jsx/mark`
- `jsx/comment-uncomment`

### `return_statement`

```tsx
return ...;
```

#### Actions:

- `jsx/kill`
- `jsx/mark`
- `jsx/avy-word`

### `statement_block`

```tsx
function func() {}
if {} else {}
```

The `{}` in `function_declaration` or `if_statement`

#### Actions:

- `jsx/empty`
- `jsx/mark`
- `jsx/avy-word`

### `string`

```tsx
'...'
"..."
```

#### Actions:

- `jsx/empty`
- `jsx/zap`
- `jsx/copy`
- `jsx/avy-word`

### `template_string`

```tsx
`...`
```

#### Actions:

- `jsx/empty`
- `jsx/zap`
- `jsx/copy`
- `jsx/avy-word`

### `type_alias_declaration`

```tsx
type Type = {};
```

- `jsx/kill`
- `jsx/copy`
- `jsx/mark`
- `jsx/comment-uncomment`
- `jsx/avy-word`
