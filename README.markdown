Manning Book
============

This is a haskell package intended to provide the tools to write a book for Manning Publications. Some of the utilities in this package rely on downloading dependencies from Manning Publications. This will be performed when a dependency is required. If it is already downloaded, it will not be downloaded again.

Included in this package are six executables:

1. `manning-book-pdf`

Generates the PDF output file for your book using Manning's `AAMakePDF` tool.

2. `manning-book-validate`

Validates your book using Manning's `AAValidator` tool.

3. `manning-book-conf`

Prints out the current configuration unless passed the `--set-config` argument, in which case, the default configuration is written to the configuration file. This file is located at `~/.manning-book`. *WARNING* If you pass `--set-config`, this will overwrite any existing configuration file, but since you keep that file in revision control (right?), you'll be fine.

4. `manning-book-spellcheck`

Uses `aspell` to spell-check your book. If you pass the `--non-interactive` argument, you will not be prompted to correct any errors as is usual.

5. `manning-book-init`

6. `manning-book-clean`
