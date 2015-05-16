# Pandoc csv2table Filter

A Pandoc filter that replaces image links having *.csv extension with
[Pandoc Table Markdown][1].

![A CSV file rendered to Markdown and PDF][png]

## Usage

There are three ways to include CSV in your markdown.

1.  **Referencing CSV file in Image Links:**

    ```
    ![This text will become the table caption](table.csv)
    ```
    
2.  **Referencing CSV file in Fenced Code Blocks:**
    
    ```
    ~~~{.table caption="This is the **caption**" source="table.csv"}
    ~~~
    ```
    
    Only code blocks having `.table` class will be processed.
    
3.  **Including CSV content inside Fenced Code Blocks:**

    ```
    ~~~{.table caption="This is the **caption**"}
    Fruit, Quantity, Price
    apples, 15, 3.24
    oranges, 12, 2.22
    ~~~
    ```

    Only code blocks having `.table` class will be processed.

CSV file or content can contain pandoc markdown. It will be parsed by the Pandoc Markdown
Reader before being inserted as a table into the document.

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