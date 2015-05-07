# pandoc-csv2table-filter

A Pandoc filter that replaces image links having *.csv extension with
[Pandoc Table Markdown][1].
In your markdown, include the csv file as shown below.

> \!\[This text will become the table caption\](table.csv)

You can use Pandoc Markdown in the CSV file.
It will be parsed by the Pandoc Markdown Reader.

## Specifying Options

You can specify type of table, whether to treat first line in CSV file as
header or not, and column alignments by specifying a code block right after
the caption.

Consider the following example:

> \!\[Another table. `m-y-lrcd`\](table.csv)

This will be rendered as a multiline table with a header with first column
left-aligned, second right-aligned, third center-aligned and fourth will have
default alignment.
The code block will be removed from the final caption.

The options in the code block have the form.

`<table-type>-<header>-<alignments>`

Possible values for `<table-type>` are:

-   **`s`** for **s**imple table
-   **`m`** for **m**ultiline table
-   **`g`** for **g**rid table
-   **`p`** for **p**ipe table

Possible values for `<header>` are:

-   **`y`** (from **y**es) when you want the first row of CSV file to be the
    header.
-   **`n`** (from **n**o) when you want to omit the header.

Possible values for `<alignments>` are:

-   **`l`** for **l**eft alignment of the column
-   **`r`** for **r**ight alignment of the column
-   **`c`** for **c**center alignment of the column
-   **`d`** for **d**efault alignment of the column

You can specify alignments for more than one column by specifying these letters
in series.
In case the number of letters exceed the number of columns in the CSV file, the
extra letters at the end will be ignored.






[1]: http://pandoc.org/README.html#tables