# mdfmt

## Synopsis

This is a Markdown formatting tool that takes valid markdown files and outputs
well formatted Markdown files. It uses the
[cmark-gfm-hs](https://hackage.haskell.org/package/cmark-gfm) parsing library
to create an AST from your markdown file, then formats the contents as
necessary. This program is a Haskell rewrite of my original Rust prototype.

## Usage

```txt
mdfmt - a markdown formatter

Usage: mdfmt-exe INPUT [-i|--in-place] [-c|--config CONFIG] [-w|--width WIDTH]
                 [-o|--out OUT]
  Format markdown files

Available options:
  INPUT                    The input filename
  -i,--in-place            Whether to modify the file in-place (this is a
                           destructive operation) and can be used concurrently
                           with `--out OUT`
  -c,--config CONFIG       The filename of a config file. If this is not
                           supplied then the program will use the default
                           configuration values.
  -w,--width WIDTH         The desired column width of the formatted document.
                           If this is not supplied, the default text width will
                           be set to 80.
  -o,--out OUT             The path write the formatted output to. This can be
                           used concurrently with the `-i` flag.
  -h,--help                Show this help text

This is fairly basic and only allows you to configure the text width of the
document, which defaults to 80.
```

The config file option has not been implemented yet, mostly because the only
configurable option is the text width.

## Example

Given the above portion of the readme, let's format it with this tool with
`--width 80`:

    # mdfmt

    ## Synopsis

    This is a Markdown formatting tool that takes valid markdown files and outputs
    well formatted Markdown files. It uses the
    [cmark-gfm-hs](https://hackage.haskell.org/package/cmark-gfm) parsing library to
    create an AST from your markdown file, then formats the contents as necessary.
    This program is a Haskell rewrite of my original Rust prototype.

    ## Usage

    mdfmt - a markdown formatter

    Usage: mdfmt-exe INPUT [-i|--in-place] [-c|--config CONFIG] [-w|--width WIDTH]
                     [-o|--out OUT]
      Format markdown files

    Available options:
      INPUT                    The input filename
      -i,--in-place            Whether to modify the file in-place (this is a
                               destructive operation) and can be used concurrently
                               with `--out OUT`
      -c,--config CONFIG       The filename of a config file. If this is not
                               supplied then the program will use the default
                               configuration values.
      -w,--width WIDTH         The desired column width of the formatted document.
                               If this is not supplied, the default text width will
                               be set to 80.
      -o,--out OUT             The path write the formatted output to. This can be
                               used concurrently with the `-i` flag.
      -h,--help                Show this help text

    This is fairly basic and only allows you to configure the text width of the
    document, which defaults to 80.
    ```

    The config file option has not been implemented yet, mostly because the only
    configurable option is the text width.

With `--width 100`

    # mdfmt

    ## Synopsis

    This is a Markdown formatting tool that takes valid markdown files and outputs well formatted
    Markdown files. It uses the [cmark-gfm-hs](https://hackage.haskell.org/package/cmark-gfm) parsing
    library to create an AST from your markdown file, then formats the contents as necessary. This
    program is a Haskell rewrite of my original Rust prototype.

    ## Usage

    ``` txt
    mdfmt - a markdown formatter

    Usage: mdfmt-exe INPUT [-i|--in-place] [-c|--config CONFIG] [-w|--width WIDTH]
                     [-o|--out OUT]
      Format markdown files

    Available options:
      INPUT                    The input filename
      -i,--in-place            Whether to modify the file in-place (this is a
                               destructive operation) and can be used concurrently
                               with `--out OUT`
      -c,--config CONFIG       The filename of a config file. If this is not
                               supplied then the program will use the default
                               configuration values.
      -w,--width WIDTH         The desired column width of the formatted document.
                               If this is not supplied, the default text width will
                               be set to 80.
      -o,--out OUT             The path write the formatted output to. This can be
                               used concurrently with the `-i` flag.
      -h,--help                Show this help text

    This is fairly basic and only allows you to configure the text width of the
    document, which defaults to 80.
    ```

    The config file option has not been implemented yet, mostly because the only configurable option is
    the text width.

