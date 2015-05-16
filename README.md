# pandoc-csv2table-filter

A Pandoc filter that replaces image links having *.csv extension with
[Pandoc Table Markdown][1].

![A CSV file rendered to Markdown and PDF][png]

## Usage

In your markdown, include the csv file as shown below.

> \!\[This text will become the table caption\](table.csv)

You can use Pandoc Markdown in the CSV file.
It will be parsed by the Pandoc Markdown Reader before insertion into the
document.

See [example.md][example-md] and the rendered [pdf][example-pdf] version in
the Examples folder for more details on usage.

## Configuration String

A configuration string lets you specify

-   Type of the table
-   Column alignments
-   Whether to treat the first line of the CSV file as header or not

It is included right before the closing square bracket **without any space in
between**, as shown in the example below.

> \!\[Another table. mylrcd](table.csv)

`mylrcd` is the configuration string.
This will be rendered as a **m**ultiline table with a header with first column
**l**eft-aligned, second **r**ight-aligned, third **c**enter-aligned, and the
fourth one having **d**efault alignment.
***The config string will be removed from the caption after being processed.***

The config string can contain following letters:

-   **`s`** for **s**imple table
-   **`m`** for **m**ultiline table
-   **`g`** for **g**rid table
-   **`p`** for **p**ipe table
-   **`y`** (from **y**es) when you want the first row of CSV file to be the
    header.
-   **`n`** (from **n**o) when you want to omit the header.
-   **`l`** for **l**eft alignment of the column
-   **`r`** for **r**ight alignment of the column
-   **`c`** for **c**center alignment of the column
-   **`d`** for **d**efault alignment of the column

You can specify `l` `r` `c` `d` for each column in a series.
The extra letters will be ignored if they exceed the number of columns in the
CSV file.

## License

Copyright &copy; 2015 [Wasif Hasan Baig](https://twitter.com/_wbaig)

Source code is released under the Terms and Conditions of [MIT License](http://opensource.org/licenses/MIT).

Please refer to the [License file][license] in the project's root directory.

[png]: https://raw.githubusercontent.com/baig/pandoc-csv2table-filter/master/Examples/demo.png
[license]: https://raw.githubusercontent.com/baig/pandoc-csv2table-filter/master/LICENSE
[example-md]: https://raw.githubusercontent.com/baig/pandoc-csv2table-filter/master/Examples/example.md
[example-pdf]: https://github.com/baig/pandoc-csv2table-filter/blob/master/Examples/example.pdf
[1]: http://pandoc.org/README.html#tables