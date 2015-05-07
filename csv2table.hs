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
  A Pandoc filter that replaces Image Links having *.csv extension with
  Pandoc Table Markdown. In your markdown, include the csv file as shown below.

  >  ![This is the table caption.](table.csv)

  You can include Pandoc Markdown in the CSV file. It will be parsed
  by the Pandoc Markdown Reader when the table is inserted in the document.
-}

import Text.CSV
import Data.List
import qualified Text.Pandoc as P
import qualified Text.Pandoc.JSON as J
import qualified Data.Text as T

--------------------------------------------------------------------------------

main :: IO ()
main = J.toJSONFilter tablifyCsvLinks

tablifyCsvLinks (J.Para [(J.Image l (f, _))]) | "csv" `isSuffixOf` f = do
    csv <- parseCSVFromFile f
    case csv of
        (Left _)    -> return []
        (Right xss) -> return $
                        pandocToBlocks $
                        addInlineLabel (cleanLabel l) .
                        P.readMarkdown P.def $
                        toMarkdown (getTableType l) AfterTable $
                        mkTable "" (getAligns l) (isHeaderPresent l) $
                        xss
tablifyCsvLinks x = return [x]

--------------------------------------------------------------------------------

type Span    = Int
type Width   = Int
type Gutter  = Int
type Lines   = [String]
type Caption = String

-- | Alignment of a column in the Table.
--   Not all 'TableTypes' support column alignments.
data Align = LeftAlign    -- Left Align
           | RightAlign   -- Right Align
           | CenterAlign  -- Center Align
           | DefaultAlign -- Default Align
           deriving (Show)

-- | Type of Pandoc Table to render.
data TableType = Simple    -- Simple Table
               | Multiline -- Multiline Table
               | Grid      -- Grid Table
               | Pipe      -- Pipe Table
               deriving (Eq, Show)

-- | Position of the Table caption.
data CaptionPos = BeforeTable | AfterTable
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

-- | A Column contain information about its width and alignment.
data Column = Column Width Align
            deriving (Show)
              -- Alignment of the cells inside this column
              -- Width is the character length of the widest cell 

-- | A Row contains a series of cells.
data Row = Row [Cell]
         deriving (Show)

-- | A Header, if present, contains a Row.
data Header = Header Row | NoHeader
            deriving (Show)

-- | A Table has a caption, information about each column's width and
--   alignment, either a header with a row or no header, and a series of rows.
data Table = Table Caption [Column] Header [Row]
           deriving (Show)

mkCells :: [Align] -> [Lines] -> [[Cell]]
mkCells as xss = map (addCellAligns as .
                      addCellWidths columnWidths .
                      mkCell DefaultAlign) xss
               where
                 lines1       = map . map $ lines
                 calcWidths   = map . map . map $ length
                 columnWidths = map maximum .
                                map concat  .
                                transpose   .
                                calcWidths  .
                                lines1      $
                                xss

updateCellWidth :: Width -> Cell -> Cell
updateCellWidth w (Cell s _ a xs) = Cell s w a xs

updateCellAlign :: Align -> Cell -> Cell
updateCellAlign a (Cell s w _ xs) = Cell s w a xs

addCellAligns :: [Align] -> [Cell] -> [Cell]
addCellAligns (a:as) (c:cs) = updateCellAlign a c  : addCellAligns as cs
addCellAligns []     (c:cs) = c : addCellAligns [] cs
addCellAligns (a:as) []     = []
addCellAligns []     []     = []

addCellWidths :: [Width] -> [Cell] -> [Cell]
addCellWidths (w:ws) (c:cs) = updateCellWidth w c : addCellWidths ws cs
addCellWidths []     (c:cs) = c : addCellWidths [] cs
addCellWidths (w:ws) []     = []
addCellWidths []     []     = []

mkCell :: Align -> Lines -> [Cell]
mkCell a xs = map (Cell (span xs) 0 a) liness
            where
              span   = maximum . map (length . lines)
              liness = map lines xs

mkTable :: Caption -> [Align] -> Bool -> [Lines] -> Table
mkTable c as h xss = case h of
                       True  -> Table c columns mkHeader $ mkRows as (tail csv)
                       False -> Table c columns NoHeader $ mkRows as csv
                   where
                     csv      = filter (/=[""]) xss
                     columns  = mkColumns as . head . mkRows as $ csv
                     mkHeader = Header $ head $ mkRows as csv

mkRows :: [Align] -> [Lines] -> [Row]
mkRows as = map Row . mkCells as

mkColumns :: [Align] -> Row -> [Column]
mkColumns as (Row cs) = columnify as cs
                      where
                        columnify (a:as) ((Cell _ w _ _):cs) = Column w a  : columnify as cs
                        columnify []     ((Cell _ w _ _):cs) = Column w DefaultAlign : columnify [] cs
                        columnify (a:as) []                  = []
                        columnify []     []                  = []

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
                                                     _  -> sep ) ++
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
alignCellStrings :: Cell -> Lines
alignCellStrings (Cell _ w a cs) = map (alignText w 1 a) cs

cellToLines :: Cell -> Lines
cellToLines = alignCellStrings . padCell

flatten :: [Lines] -> String
flatten = concatMap (++"\n") . map concat

addGutter :: Gutter -> Lines -> Lines
addGutter g (x:xs) = x : map ((replicate g ' ')++) xs

alignText :: Width -> Gutter -> Align -> String -> String
alignText w g (LeftAlign)    = T.unpack . T.justifyLeft   w ' ' . T.pack
alignText w g (RightAlign)   = T.unpack . T.justifyRight  w ' ' . T.pack
alignText w g (CenterAlign)  = T.unpack . T.center        w ' ' . T.pack
alignText w g (DefaultAlign) = T.unpack . T.justifyLeft   w ' ' . T.pack

row2Md :: TableType -> Row -> String
row2Md t (Row cs) | (t == Grid || t== Pipe) = flatten $
                       transpose $
                       appendPipes $
                       map cellToLines $
                       cs
row2Md t (Row cs) | (t == Simple || t == Multiline) = flatten $
                       map (addGutter 1) $
                       transpose $
                       map cellToLines $
                       cs

appendPipes :: [Lines] -> [Lines]
appendPipes (xs:xss) = map (("| "++) . (++" | ")) xs : (map (map (++" |")) xss)
appendPipes [] = []

addCaption :: CaptionPos -> Caption -> String -> String
addCaption _             [] s = s
addCaption (BeforeTable) c  s = "Table: " ++ c ++ "\n\n" ++ s
addCaption (AfterTable)  c  s = s ++ "\nTable: " ++ c

toMarkdown :: TableType -> CaptionPos -> Table -> String
toMarkdown (Simple) = toSimpleMd
toMarkdown (Multiline) = toMultilineMd
toMarkdown (Grid) = toGridMd
toMarkdown (Pipe) = toPipeMd

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

-- | Adds Pandoc's [Inline] block inside table as caption
addInlineLabel :: [J.Inline] -> J.Pandoc -> J.Pandoc
addInlineLabel i (J.Pandoc m [(P.Table _ as ds ts tss)]) = J.Pandoc m [(P.Table i as ds ts tss)]
addInlineLabel _ x = x

-- | Extracts [Block] from J.Pandoc
pandocToBlocks :: J.Pandoc -> [J.Block]
pandocToBlocks (J.Pandoc _ bs) = bs

getTableType :: [J.Inline] -> TableType
getTableType ((J.Code _ s):_) = case (take 1 s) of
                                  "s" -> Simple
                                  "m" -> Multiline
                                  "g" -> Grid
                                  "p" -> Pipe
getTableType (_:is)           = getTableType is
getTableType []               = Grid

-- | Tells if header is present or not from code Inline
isHeaderPresent :: [J.Inline] -> Bool
isHeaderPresent ((J.Code _ s):_) = case (take 1 . drop 2 $ s) of
                                     "y" -> True
                                     "n" -> False
isHeaderPresent (_:is)           = isHeaderPresent is
isHeaderPresent []               = True

toAlign :: String -> [Align]
toAlign (x:ys) = case x of
                   'l' -> LeftAlign    : toAlign ys
                   'r' -> RightAlign   : toAlign ys
                   'c' -> CenterAlign  : toAlign ys
                   'd' -> DefaultAlign : toAlign ys
toAlign []     = []

-- | Extracts alignment information from Code Inline
getAligns :: [J.Inline] -> [Align]
getAligns ((J.Code _ s):_) = toAlign (drop 4 s)
getAligns (_:is)           = getAligns is
getAligns []               = []

-- | Remove Code Inline used for specifying alignment and header for the table
cleanLabel :: [J.Inline] -> [J.Inline]
cleanLabel ((J.Code _ s):ys) = cleanLabel ys
cleanLabel (x:ys)            = x : cleanLabel ys
cleanLabel []                = []