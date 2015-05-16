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
   Module      : Text.Table.Definition
   Copyright   : Copyright (C) 2015 Wasif Hasan Baig
   License     : MIT

   Maintainer  : Wasif Hasan Baig <pr.wasif@gmail.com>
   Stability   : alpha

Definition of 'Table' data structure for internal representation.
-}

module Text.Table.Definition (
      TableType (..)
    , CaptionPos (..)
    , Align (..)
    , Cell (..)
    , Row (..)
    , Column (..)
    , Header (..)
    , Table (..)
    , Span
    , Width
    , Gutter
    , Lines
    , Caption
    , AtrName
    , AtrValue
    , Atrs
) where

-- Type synonyms
type Span      = Int
type Width     = Int
type Gutter    = Int
type Lines     = [String]
type Caption   = String
type AtrName   = String
type AtrValue  = String
type Atrs      = [(AtrName, AtrValue)]

-- Data Definitions

-- | Type of the 'Table'.
data TableType = Simple    -- Simple Table
               | Multiline -- Multiline Table
               | Grid      -- Grid Table
               | Pipe      -- Pipe Table
               deriving (Eq, Show)

-- | Position of the caption.
data CaptionPos = BeforeTable -- Insert caption before table markdown.
                | AfterTable  -- Insert caption after table markdown.
                deriving (Show)

-- | Alignment of a Column in the Table.
--   Not all TableTypes support column alignments.
data Align = LeftAlign    -- Left Align
           | RightAlign   -- Right Align
           | CenterAlign  -- Center Align
           | DefaultAlign -- Default Align
           deriving (Show)

-- | A cell in a table has column span, cell width, cell alignment and the
--   number of lines.
--   
--     * __Span:__  Number of lines spanned by the cell.
--     * __Width:__ Width of the column this cell is contained inside
--     * __Align:__ Alignment of the content inside the cells
--     * __Lines:__ A list of strings where each string represents a line
data Cell = Cell Span Width Align Lines
            deriving (Show)
            
-- | A Row contains a list of Cells.
data Row = Row [Cell]
           deriving (Show)

-- | A Column contain information about its width and alignment.
--   
--     * __Width:__  Character length of the widest 'Cell' in a 'Column'.
--     * __Align:__  Alignment of the cells inside this column
data Column = Column Width Align
              deriving (Show)

-- | A Header contains a Row if present, otherwise NoHeader.
data Header = Header Row
            | NoHeader
            deriving (Show)

-- | A Table has a caption, information about each column's width and
--   alignment, either a header with a row or no header, and a series of rows.
data Table = Table Caption [Column] Header [Row]
             deriving (Show)
