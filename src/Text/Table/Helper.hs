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
   Module      : Text.Table.Helper
   Copyright   : Copyright (C) 2015 Wasif Hasan Baig
   License     : MIT

   Maintainer  : Wasif Hasan Baig <pr.wasif@gmail.com>
   Stability   : alpha

This helper module exports functions extract values from Pandoc AST and 
build Pandoc Document from CSV.
-}

module Text.Table.Helper (
      addInlineLabel
    , getTableType
    , toTableType
    , getAligns
    , isHeaderPresent
    , isHeaderPresent1
    , removeConfigString
    , toBlocks
    , getAtr
    , toAlign
    , tableFromImageInline
    , tableFromCodeBlock
) where

import Text.CSV (CSV)
import Data.List (isInfixOf)
import Text.Pandoc (readMarkdown, def)
import qualified Text.Pandoc.JSON as J
-- Local imports
import Text.Table.Definition
import Text.Table.Builder

-- Helper functions to manipulate the Pandoc Document and parse the 
-- Configuration String.

-- | Add Inline from Image into Table as the caption
addInlineLabel :: [J.Inline] -> J.Pandoc -> J.Pandoc
addInlineLabel i (J.Pandoc m [(J.Table _ as ds ts tss)]) = J.Pandoc m [(J.Table i as ds ts tss)]
addInlineLabel _ x = x

-- | Extracts Blocks from Pandoc Document
toBlocks :: J.Pandoc -> [J.Block]
toBlocks (J.Pandoc _ bs) = bs

toTableType1 :: String -> TableType
toTableType1 (x:ys) = case x of
                       's' -> Simple
                       'm' -> Multiline
                       'p' -> Pipe
                       _   -> Grid
toTableType1 []     = Grid

toTableType :: String -> TableType
toTableType s = case s of
                  "simple"    -> Simple
                  "multiline" -> Multiline
                  "pipe"      -> Pipe
                  _           -> Grid

getTableType :: [J.Inline] -> TableType
getTableType ((J.Str s):[]) = toTableType1 s
getTableType (_:is)         = getTableType is
getTableType []             = Grid

-- | Whether to treat first line of CSV as a header or not.
isHeaderPresent :: [J.Inline] -> Bool
isHeaderPresent ((J.Str s):[]) = not $ "n" `isInfixOf` s
isHeaderPresent (_:is)         = isHeaderPresent is
isHeaderPresent []             = True

isHeaderPresent1 :: String -> Bool
isHeaderPresent1 ('n':'o':[]) = False
isHeaderPresent1 _            = True

toAlign :: String -> [Align]
toAlign (x:ys) = case x of
                   'l' -> LeftAlign    : toAlign ys
                   'L' -> LeftAlign    : toAlign ys
                   'r' -> RightAlign   : toAlign ys
                   'R' -> RightAlign   : toAlign ys
                   'c' -> CenterAlign  : toAlign ys
                   'C' -> CenterAlign  : toAlign ys
                   'd' -> DefaultAlign : toAlign ys
                   'D' -> DefaultAlign : toAlign ys
                   _   -> []          ++ toAlign ys
toAlign []     = []

-- | Parse Config String for alignment information
getAligns :: [J.Inline] -> [Align]
getAligns ((J.Str s):[]) = toAlign s
getAligns (_:is)         = getAligns is
getAligns []             = []

-- | Remove Str Inline from caption
removeConfigString :: [J.Inline] -> [J.Inline]
removeConfigString (_:[]) = []
removeConfigString (x:ys) = x : removeConfigString ys
removeConfigString []     = []

-- | Get value of attribute
getAtr :: AtrName -> Atrs -> AtrValue
getAtr a ((at,v):_) | a == at = v
getAtr a (_:xs)               = getAtr a xs
getAtr a []                   = ""

-- | Make Pandoc Table from Image Inline 
tableFromImageInline :: [J.Inline] -> CSV -> J.Pandoc
tableFromImageInline l = addInlineLabel (removeConfigString l) .
                         readMarkdown def .
                         toMarkdown (getTableType l) AfterTable .
                         mkTable "" (getAligns l) (isHeaderPresent l)

-- | Make Pandoc Table from Code Block 
tableFromCodeBlock :: Atrs -> CSV -> J.Pandoc
tableFromCodeBlock as = readMarkdown def .
                        toMarkdown (toTableType $ getAtr "type" as) AfterTable .
                        mkTable (getAtr "caption" as)
                                (toAlign $ getAtr "aligns" as)
                                (isHeaderPresent1 $ getAtr "header" as)
