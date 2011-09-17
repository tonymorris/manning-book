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

Downloads all dependencies and sets up files for spell-checking
  * The file to specify elements which should be not be checked for spelling errors. For example, `<programlisting>` elements probably shouldn't be spell-checked because they contain source code.
  * The file to specify words to ignore during spell-checking. You are likely to be writing about a specialised domain, which includes words that may not appear in a standard dictionary. Running `manning-book-spellcheck` will add these words when you do interactive spell-checking.

Note that none of these operations are essential, since these spell-checking files will be set up at first spell-check and dependencies will be downloaded when required anyway.

6. `manning-book-clean`

Deletes all bilt artifacts of the book, which is essentially only the PDF. Note that the entire directory will be deleted. If you pass the `--all` argument, the dependencies will also be deleted and so wil be downloaded again when required.

