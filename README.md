# Pandoc csv2table Filter

A Pandoc filter that replaces CSV content (either inside fenced code blocks or referenced CSV files) with
[Pandoc Table Markdown][tables].

![A CSV file rendered to Markdown and PDF][png]

[png]: Examples/demo.png

## Installation

You can use this filter in two ways.

1.  **Install using Cabal:** After installing Haskell platform, run:

    > ```
    > cabal install pandoc-csv2table
    > ```
    
2.  **Use the single file filter:** There is a gist hosting a single file
    version of this filter [here][gist]. Clone it and make it an executable script by
    running:
    
    > ```
    > chmod +x pandoc-csv2table.hs
    > ```
    
[gist]: https://gist.github.com/baig/b69e3146251bd90d12e7

You can also use this filter in your Pandoc application by calling
`tablifyCsvlinks`.

## Usage

### Referencing or including CSV

There are three ways to include CSV in your markdown.

1.  **Referencing CSV file in Image Links:**

    ```
    ![This text will become the table caption](table.csv)
    ```
    
2.  **Referencing CSV file in Fenced Code Blocks:**
    
        ```{.table caption="This is the **caption**" source="table.csv"}  
        ```
    
    Only code blocks having `.table` class will be processed.
    
3.  **Including CSV content inside Fenced Code Blocks:**

        ```{.table aligns="LCR" caption="This is the **caption**" header="yes"}  
        Fruit, Quantity, Price  
        apples, 15, 3.24  
        oranges, 12, 2.22  
        ```

    Only code blocks having `.table` class will be processed.

CSV file or content can contain pandoc markdown. It will be parsed by the Pandoc Markdown
Reader before being inserted as a table into the document.

### Running the filter

Given the filename `test.md`, run the filter using command

    pandoc --filter pandoc-csv2table -o test.html test.md

or

    pandoc -t json test.md | pandoc-csv2table | pandoc -f json -o test.html

## Examples

See [example.md][md] and the rendered [pdf][] version in the Examples folder
for more details on usage.

[md]: Examples/example.md
[pdf]: Examples/example.pdf

## Options

You can specify a [configuration string][cfg] for image links and
[attributes][atr] for fenced code blocks. There are valid options for specifying

-   Type of the table
-   Column alignments
-   Whether to treat the first line of the CSV file as header or not

[cfg]: README.md#configuration-string
[atr]: README.md#attributes

### Configuration String

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

### Attributes

You can specify header attributes in fecnced code blocks like this:

    ```{.table type="pipe" aligns="LCR" caption="A **caption**" header="yes"}  
    Fruit, Quantity, Price  
    apples, 15, 3.24  
    oranges, 12, 2.22  
    ```

***Note: `.table` must be included if the fenced code block is intended to be
processed by this filter.***

Valid attributes that you can specify in code blocks include:

-   **type** of the table can be **`simple`**, **`multiline`**,
    **`grid`** (default), or **`pipe`**.
-   **header** can be `yes` (default) or `no`
-   **caption** is a string which can contain markdown.
-   **source** is the path to a valid CSV file to be rendered as pandoc table.
    If present, the contents inside the fenced code blocks are ignored.
-   **aligns** specify alignment for each column. Use **`L`** for left, **`R`**
    for right, **`C`** for center, and **`D`** for default.

## License

Copyright &copy; 2015-2016 [Wasif Hasan Baig](https://twitter.com/_wbaig)

Source code is released under the Terms and Conditions of [MIT License](http://opensource.org/licenses/MIT).

Please refer to the [License file][license] in the project's root directory.

[license]: LICENSE
[tables]: http://pandoc.org/README.html#tables
