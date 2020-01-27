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

{- |
   Module      : Text.Table.Tablify
   Copyright   : Copyright (C) 2015 Wasif Hasan Baig
   License     : MIT

   Maintainer  : Venkateswara Rao Mandela <venkat.mandela@gmail.com>
   Stability   : alpha

Function for operating on Pandoc fenced code blocks or image blocks
with the ".table" attribute and converting them into Pandoc tables.
-}

module Text.Table.Tablify (
    tablifyCsvLinks,
    tablifyCsvLinksPure
  ) where

import Text.CSV         (parseCSV, parseCSVFromFile)
import Text.Pandoc.JSON (Block(Para, CodeBlock), Inline(Image), toJSONFilter)
import Data.List        (isSuffixOf)
-- Local imports
import Text.Table.Helper

tablifyCsvLinks :: Block -> IO [Block]
-- variant 1: Referencing CSV file in Image Links
#if MIN_VERSION_pandoc(1,16,0)
-- Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
tablifyCsvLinks (Para [(Image _ l (f, _))]) | "csv" `isSuffixOf` (getString f) = do
#else
-- Image [Inline] Target -- ^ Image:  alt text (list of inlines), target
tablifyCsvLinks (Para [(Image l (f, _))]) | "csv" `isSuffixOf` f = do
#endif
    csv <- parseCSVFromFile $ getString f
    case csv of
        (Left _)    -> return []
        (Right xss) -> return .
                       toBlocks .
                       tableFromImageInline l $
                       xss
-- variant 2 and 3: Fenced Code Blocks
tablifyCsvLinks b@(CodeBlock (_, cs, as) s) | "table" `elem` (map getString cs) = do
    let file = getAtr "source" as1
    case file of
      -- variant 2: Referencing CSV file in Fenced Code Blocks
      "" -> case s1 of
              "" -> return [b]
              _  -> case (parseCSV "" s1) of
                      (Left _)    -> return []
                      (Right xss) -> return .
                                     toBlocks .
                                     tableFromCodeBlock as1 $
                                     xss
      -- variant 3:Including CSV content inside Fenced Code Blocks
      _  -> do
              csv <- parseCSVFromFile file
              case csv of
                (Left _)    -> return []
                (Right xss) -> return .
                               toBlocks .
                               tableFromCodeBlock as1 $
                               xss
    where s1 = getString s
          as1 = (map (applyToTuple getString) as )
-- Return input unchanged in case of no match
tablifyCsvLinks x = return [x]

tablifyCsvLinksPure :: Block -> [Block]
tablifyCsvLinksPure b@(CodeBlock (_, cs, as) s) | "table" `elem` (map getString cs) = do
  case s1 of
    "" -> return b
    _  -> case (parseCSV "" s1) of
            (Left _)    -> return b
            (Right xss) -> toBlocks .
                           tableFromCodeBlock as1 $
                           xss
  where s1 = getString s
        as1 = (map (applyToTuple getString) as )
tablifyCsvLinksPure x = return x
