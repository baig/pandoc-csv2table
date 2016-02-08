#!/usr/bin/env runhaskell

{-
The MIT License (MIT)

Copyright (c) 2015 Wasif Hasan Baig <pr.wasif@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

{-|
  A Pandoc filter that replaces image or fenced code blocks and replaces them
  with rendered pandoc markdown tables.
  
  Image Links should have a "csv" extension.
  Include the csv file in markdown as
  
  > ![This is the table caption.](table.csv)
  
  Instead of image links, you can use fenced code blocks to reference an
  external CSV file using the "source" attribute.
  
  > ```{.table caption="This is the **caption**" source="table.csv"}
  > ```

  You can also omit the source attribute and include the contents of the CSV
  inside the code block directly.
  
  > ```{.table caption="This is the **caption**"}
  > Fruit, Quantity, Price
  > apples, 15, 3.24
  > oranges, 12, 2.22
  > ```
  
  You can include Pandoc Markdown in the CSV file. It will be parsed
  by the Pandoc Markdown Reader when the table is inserted in the document.
  
  For a detailed explanation of usage and options, see <https://github.com/baig/pandoc-csv2table-filter/blob/master/README.md README>
  at project's source directory.
-}

import Text.CSV         (parseCSV, parseCSVFromFile)
import Data.List        (isSuffixOf)
import Text.Pandoc.JSON (Block(Para, CodeBlock), Inline(Image), toJSONFilter)
-- Local imports
import Text.Table.Tablify

main :: IO ()
main = toJSONFilter tablifyCsvLinks
