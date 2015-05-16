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
   Module      : Text.Table.Builder
   Copyright   : Copyright (C) 2015 Wasif Hasan Baig
   License     : MIT

   Maintainer  : Wasif Hasan Baig <pr.wasif@gmail.com>
   Stability   : alpha

Functions for building Tables and converting them to markdown.
-}

module Text.Table.Builder (
      mkTable
    , toMarkdown
    , CaptionPos (..)
    , Atrs
) where

import Data.List
import Data.Text (pack, unpack, justifyLeft, justifyRight, center)

-- Local import
import Text.Table.Definition

-- | Table Builder Functions
--   Helper functions to create a Table from the parsed CSV file (a list of
--   list of Strings).

-- | Converts Lines to Cell.
mkCell :: Align -> Lines -> [Cell]
mkCell a xs = map (Cell (span xs) 0 a) liness
            where
              span   = maximum . map (length . lines)
              liness = map lines xs

-- | Converts a list of Lines to a list of Cells.
mkCells :: [Align] -> [Lines] -> [[Cell]]
mkCells as xss = map (addCellAligns as .
                      addCellWidths columnWidths .
                      mkCell DefaultAlign) xss
               where
                 lines1       = map . map $ lines
                 calcWidths   = map . map . map $ (+2) . length
                 columnWidths = map maximum .
                                map concat  .
                                transpose   .
                                calcWidths  .
                                lines1      $
                                xss

addCellAligns :: [Align] -> [Cell] -> [Cell]
addCellAligns (a:as) (c:cs) = updateCellAlign a c  : addCellAligns as cs
addCellAligns []     (c:cs) = c : addCellAligns [] cs
addCellAligns (a:as) []     = []
addCellAligns []     []     = []

updateCellAlign :: Align -> Cell -> Cell
updateCellAlign a (Cell s w _ xs) = Cell s w a xs

addCellWidths :: [Width] -> [Cell] -> [Cell]
addCellWidths (w:ws) (c:cs) = updateCellWidth w c : addCellWidths ws cs
addCellWidths []     (c:cs) = c : addCellWidths [] cs
addCellWidths (w:ws) []     = []
addCellWidths []     []     = []

updateCellWidth :: Width -> Cell -> Cell
updateCellWidth w (Cell s _ a xs) = Cell s w a xs

mkRows :: [Align] -> [Lines] -> [Row]
mkRows as = map Row . mkCells as

mkColumns :: [Align] -> Row -> [Column]
mkColumns as (Row cs) = columnify as cs
                      where
                        columnify (a:as) ((Cell _ w _ _):cs) = Column w a  : columnify as cs
                        columnify []     ((Cell _ w _ _):cs) = Column w DefaultAlign : columnify [] cs
                        columnify (a:as) []                  = []
                        columnify []     []                  = []

mkTable :: Caption -> [Align] -> Bool -> [Lines] -> Table
mkTable c as h xss = case h of
                       True  -> Table c columns mkHeader $ tail $ mkRows as csv
                       False -> Table c columns NoHeader $ mkRows as csv
                   where
                     csv      = filter (/=[""]) xss
                     columns  = mkColumns as . head . mkRows as $ csv
                     mkHeader = Header $ head $ mkRows as csv

insertRowSeparator :: TableType -> [Column] -> Lines -> Lines
insertRowSeparator (Multiline) cs xs = intersperse "\n" xs
insertRowSeparator (Grid)      cs xs = intersperse (separator cs) xs
                                     where
                                       separator ((Column w _):cs) = "+" ++ replicate (w+2) '-' ++ separator cs
                                       separator []     = "+\n"
insertRowSeparator _           _  _  = []
                                                 
mkTableBorder :: TableType -> [Column] -> String
mkTableBorder (Multiline) ((Column w _):[]) = replicate w '-' ++ "\n"
mkTableBorder (Multiline) ((Column w _):cs) = replicate (w+1) '-' ++ mkTableBorder Multiline cs
mkTableBorder (Grid)      []                = "+\n"
mkTableBorder (Grid)      ((Column w _):cs) = "+" ++ replicate (w+2) '-' ++ mkTableBorder Grid cs
mkTableBorder _           _                 = ""

mkHeaderRowSeparator :: TableType -> [Column] -> String
mkHeaderRowSeparator (Grid) ((Column w _):cs) = "+" ++ replicate (w+2) '=' ++
                                                mkHeaderRowSeparator Grid cs
mkHeaderRowSeparator (Pipe) ((Column w a):cs) = (let sep = "|" ++ replicate (w+2) '-' 
                                                 in case a of
                                                     LeftAlign   -> "|:" ++ (drop 2 sep)
                                                     RightAlign  -> reverse $ ":" ++ (drop 1 . reverse $ sep)
                                                     CenterAlign -> ("|:" ++) $ drop 2 $ reverse $ ":" ++ (drop 1 . reverse $ sep)
                                                     _           -> reverse . drop 1. reverse $ sep ) ++
                                                mkHeaderRowSeparator Pipe cs
mkHeaderRowSeparator (Grid) []                = "+\n"
mkHeaderRowSeparator (Pipe) []                = "|\n"
mkHeaderRowSeparator t      ((Column w _):[]) = replicate w '-' ++ "\n"
mkHeaderRowSeparator t      ((Column w _):cs) = replicate w '-' ++ " " ++
                                                mkHeaderRowSeparator t cs

-- | Add members to pad cell content based on span
padCell :: Cell -> Cell
padCell (Cell s w a xs) = Cell s w a (xs ++ padList)
                        where
                          padList  = replicate padByNum (take w $ repeat ' ')
                          padByNum = s - length xs

-- | Expects padded cells
alignCellStrings :: TableType -> Cell -> Lines
alignCellStrings (Grid) (Cell _ w a cs) = map (alignText w LeftAlign) cs
alignCellStrings _      (Cell _ w a cs) = map (alignText w a) cs

cellToLines :: TableType -> Cell -> Lines
cellToLines t = alignCellStrings t . padCell

flatten :: [Lines] -> String
flatten = concatMap (++"\n") . map concat

addGutter :: Gutter -> Lines -> Lines
addGutter g (x:xs) = x : map ((replicate g ' ')++) xs

alignText :: Width -> Align -> String -> String
alignText w (LeftAlign)    = unpack . justifyLeft   w ' ' . pack
alignText w (RightAlign)   = unpack . justifyRight  w ' ' . pack
alignText w (CenterAlign)  = unpack . center        w ' ' . pack
alignText w (DefaultAlign) = unpack . justifyLeft   w ' ' . pack

row2Md :: TableType -> Row -> String
row2Md (Grid) (Row cs) = flatten $ transpose $ appendPipes $ map (cellToLines Grid) cs
row2Md (Pipe) (Row cs) = flatten $ transpose $ appendPipes $ map (cellToLines Pipe) cs
row2Md t      (Row cs) = flatten $ map (addGutter 1) $ transpose $ map (cellToLines t) cs

appendPipes :: [Lines] -> [Lines]
appendPipes (xs:[])  = [map (++" |") xs]
appendPipes (xs:xss) = map (("| "++) . (++" | ")) xs : (map (map (++" | ")) xss)

addCaption :: CaptionPos -> Caption -> String -> String
addCaption _             [] s = s
addCaption (BeforeTable) c  s = "Table: " ++ c ++ "\n\n" ++ s
addCaption (AfterTable)  c  s = s ++ "\nTable: " ++ c

toMarkdown :: TableType -> CaptionPos -> Table -> String
toMarkdown (Simple)    = toSimpleMd
toMarkdown (Multiline) = toMultilineMd
toMarkdown (Grid)      = toGridMd
toMarkdown (Pipe)      = toPipeMd

toMultilineMd :: CaptionPos -> Table -> String
toMultilineMd l (Table c cs (Header h) rs) = addCaption l c $
                                             mkTableBorder Multiline cs ++
                                             row2Md Multiline h ++
                                             mkHeaderRowSeparator Multiline cs ++
                                             (concatMap (++"") $
                                              insertRowSeparator Multiline cs $
                                              map (row2Md Multiline) rs) ++
                                             mkTableBorder Multiline cs
toMultilineMd l (Table c cs (NoHeader) rs) = addCaption l c $
                                             mkHeaderRowSeparator Multiline cs ++
                                             (concatMap (++"") $
                                              insertRowSeparator Multiline cs $
                                              map (row2Md Multiline) rs) ++
                                             mkHeaderRowSeparator Multiline cs

toGridMd :: CaptionPos -> Table -> String
toGridMd l (Table c cs (Header h) rs) = addCaption l c $
                                        mkTableBorder Grid cs ++
                                        row2Md Grid h ++
                                        mkHeaderRowSeparator Grid cs ++
                                        (concatMap (++"") $
                                         insertRowSeparator Grid cs $
                                         map (row2Md Grid) rs) ++
                                        mkTableBorder Grid cs
toGridMd l (Table c cs (NoHeader) rs) = addCaption l c $
                                        mkTableBorder Grid cs ++
                                        (concatMap (++"") $
                                         insertRowSeparator Grid cs $
                                         map (row2Md Grid) rs) ++
                                        mkTableBorder Grid cs

toPipeMd :: CaptionPos -> Table -> String
toPipeMd l (Table c cs (Header h) rs) = addCaption l c $
                                        row2Md Pipe h ++
                                        mkHeaderRowSeparator Pipe cs ++
                                        (concatMap (++"") $
                                         map (row2Md Pipe) rs)
toPipeMd l (Table c cs (NoHeader) rs) = addCaption l c $
                                        mkHeaderRowSeparator Pipe cs ++
                                        (concatMap (++"") $
                                         map (row2Md Pipe) rs)

toSimpleMd :: CaptionPos -> Table -> String
toSimpleMd l (Table c cs (Header h) rs) = addCaption l c $
                                          row2Md Simple h ++
                                          mkHeaderRowSeparator Simple cs ++
                                          (concatMap (++"") $
                                           map (row2Md Simple) rs) ++
                                          mkHeaderRowSeparator Simple cs
toSimpleMd l (Table c cs (NoHeader) rs) = addCaption l c $
                                          mkHeaderRowSeparator Simple cs ++
                                          (concatMap (++"") $
                                           map (row2Md Simple) rs) ++
                                          mkHeaderRowSeparator Simple cs
