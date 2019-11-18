# mdfmt

## Synopsis

This is a Markdown formatting tool that takes valid markdown files and outputs
well formatted Markdown files. It uses the
[cmark-hs](https://hackage.haskell.org/package/cmark) parsing library to create
an AST from your markdown file, then formats the contents as necessary. This
program is a Haskell rewrite of my original Rust prototype.
