# polyglot

**polyglot** is a filter to convert XHTML into polyglot HTML5. It's a
  command and a MirageOS-compatible OCaml library.

To use **polyglot** from the command line, just

```
$ polyglot < input.xhtml > output.html
```

and `input.xhtml` will be converted to polyglot HTML5 with the result sent
to `output.html`.

## Modifications

`polyglot` will convert non-void HTML elements into empty rather than
self-closing tags.

`polyglot` will introduce an HTML5 DTD if one is not already present.
