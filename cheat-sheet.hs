{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}
{-# LANGUAGE TupleSections #-}
module CC where

import Data.Char (toUpper)
import Prelude hiding (mapM,sequence,Num(..),(/))
import MarXup
import MarXup.Latex
import MarXup.Tex hiding (label)
import Control.Applicative
import Data.Monoid
import Control.Monad (unless)
import Graphics.Diagrams
import MarXup.Diagram
import MarXup.Latex.Math (ensureMath)
import Control.Lens (set)
-- import Data.String
import Data.Traversable
import Data.List (zip4,zipWith4,isSuffixOf,isPrefixOf)
import Algebra.Classes
preamble body = do
  documentClass "article" ["10pt"]
  usepackage "fontspec" []
  cmd "setsansfont" (tex "DejaVu Sans")
  cmd "setmainfont" (tex "DejaVu Serif")
  usepackage "tikz" []
  usepackage "graphicx" []
  usepackage "amssymb" []
  usepackage "varwidth" []
  usepackage "geometry" ["margin=1cm","paper=a4paper","landscape"]
  env "document" body


leftHandK = [["q","w","f","p","g"]
            ,["a","r","s","t","d"]
            ,["z","x","c","v","b"]]

rightHandK = [["j","l","u","y",";"," "]
             ,["h","n","e","i","o","'"]
             ,["k","m",",",".","/"," "]]

upKey c = case [c] of
  "'" -> head "\""
  "," -> '<'
  "." -> '>'
  "/" -> '?'
  _ -> toUpper c


data Argument = Bin Argument Argument | None | Char | SearchObject | TextRegion | Prefix | Enclosure | Reserved

argColor :: Argument -> String
argColor a = case a of
  None -> "gray"
  Char -> "red"
  SearchObject -> "orange"
  Prefix -> "yellow"
  TextRegion -> "blue"
  Bin _ _ -> "purple"
  _ -> "white"

reserved :: (TeX,Argument)
reserved = (italic "reserved",Reserved)

leftHandM = [["Quote", "backWard", "Forward", "Pursue", "Gather"]
           ,["Around", "Replace", "Splice", "Take", "Displace"]
           ,["", "eXtended", "Command", "⋎ (insert mark)", "Bank"]
           ]

leftHandL = [[("escape",Char), ("search backward",SearchObject), ("search forward",SearchObject), ("helm-occur",None), ("helm-...",Prefix)]
            ,[("enclose",Bin Enclosure TextRegion), ("kill+insert", TextRegion), ("yank", None), ("kill", TextRegion), ("replace char", Char)]
            ,[reserved, ("C-x",Prefix), ("C-c C-...",Prefix), ("insert mode",None), ("yank register",Char)]
            ]

leftHandR = [[("quotes (string)",None), ("word",None), ("word",None), ("paragraph",None), reserved]
            ,[("enclosure",TextRegion), ("whole-line",None), ("symbol",None), ("whitespace+",TextRegion), ("document",None), ("previous-region",None)]
            ,[("inclosure",TextRegion), ("s-expr",None), ("s-expr contents",None), reserved, reserved]
            ]


leftHandU = [[reserved, ("re-search backward",None), ("re-search forward",None), ("Play-macro",None), reserved]
            ,[reserved, ("Record macro", None), ("pop-yank", None), ("copy", TextRegion), reserved]
            ,[reserved, reserved, reserved, ("open line",None), ("copy register",Char)]
            ]

moveC :: String -> (TeX,Argument)
moveC "" = ("",Reserved)
moveC x | "avy" `isPrefixOf` x = (textual x,Char)
moveC x | "region" `isSuffixOf` x = (textual x,TextRegion)
moveC x = (textual x,None)
movesC = map (map moveC)
rightHandL = movesC
             [["jump-to-def", "begin-of-line", "previous-line", "next-line", "end-of-line"]
            ,["avy-jump", "smarter-left", "backward-char", "forward-char", "smarter-right", "toggle mark-active"]
            ,["pop-mark", "", "begin-of-expr", "end-of-expr", ""]
            ]

rightHandU = movesC
             [["reserved", "", "previous-paragraph", "next-paragraph", ""]
             ,["avy-jump-char", "", "smarter-up", "smarter-down", "", ""]
             ,["pop-mark-quick", "", "begin-of-buffer", "end-of-buffer", ""]
            ]

sm = cmd0 "shortmid"
ma = ensureMath
rightHandM = [["Jump", "⇤", "↑", "↓", "⇥"]
            ,["Hop", "⇠" , "←", "→", "⇢" , "'"]
            ,["bacK to marK", "", "↜", "↝", ""]
            ]

varwidth x body = env "varwidth" ((braces $ tex x) >> cmd0 "centering" >> body)

zzip = zipWith zip
zzipWith = zipWith . zipWith

zzip4 f = zipWith4 (zipWith4 f)
zzip3 f = zipWith3 (zipWith3 f)

keySize :: Constant
keySize = 65

fontsize :: String -> String -> TeX -> TeX
fontsize x y body = braces $ cmdn_ "fontsize" [tex x, tex y] <> body

-- keyHalf :: String -> (TeX,Argument) -> TexDiagram Anchorage
keyHalf k (act,arg) = do
  up <- using (fill (argColor arg)) $ draw $ box "keyHalf"
  upK <- label "k" $  textSize Huge $ sans $ textual $ k
  upT <- label "scr" $ varwidth "45pt" $ sans $ textSize ScriptSize $ act
  up # E .=. upT # E
  up # W .=. upK # W
  width up === constant keySize
  height up === constant (keySize / 2)
  return up


keyDiagram k mnem l u = do
  b <- using (set lineWidth thick) $ draw $ box "keyB"
  up <- keyHalf (map upKey k) u
  down <- keyHalf k l
  m <- (label "mnem" $ sans $ textSize Tiny $ mnem)
  m # SW .=. b # SW
  up # S .=. down # N
  b # NW .=. up # NW
  b # NE .=. up # NE
  b # SE .=. down # SE
  return b

keyFull :: Constant -> String -> TeX -> Argument -> TexDiagram Object
keyFull w k act arg = do
  b <- using (set lineWidth thick . fill (argColor arg)) $ draw $ box "kf"
  width b === constant w
  height b === constant keySize
  k' <- label "huge" $ textSize Huge $ textual $ k
  act' <- label "scr2" $ varwidth "55pt" $ sans $ textSize ScriptSize $ act
  k' # NW .=. b # NW
  act' # S .=. b # S
  return b

keyDist :: Constant
keyDist = 5

argDescs =
  [(Bin Enclosure TextRegion, "First an enclosure, then a region")
  ,(None,"No Argument")
  ,(Char,"A character")
  ,(SearchObject,"A search space")
  ,(TextRegion,"A region")
  ,(Prefix,"(Prefix map)")
  -- ,(Enclosure,"An enclosure")
  ,(Reserved,"(Reserved key)")]

legend :: TexDiagram ()
legend = do
  txt <- label "legleg" (textSize Tiny $ "Color corresponds to the type of expected argument:")
  ds <- forM argDescs $
    \ (arg,desc) -> do
        b <- using (set lineWidth thick . fill (argColor arg)) $ draw $ box "legend"
        width b === constant 15
        height b === constant 15
        l <- label "desc" desc
        b # E .=. l # W
        return b
  let ds' = ds++[txt]
  align xpart (map (# W) ds')
  spread vdist (constant 5) ds'

matrixDiag :: [[TexDiagram Object]] -> TexDiagram [[Object]]
matrixDiag matrix = do
  keys <- mapM sequence $ reverse $ matrix
  spread hdist (constant keyDist) $ (keys !! 1)
  spread vdist (constant keyDist) $ map head keys
  alignMatrix $ map (map (# Center)) keys
  return $ reverse keys

keyBDiag = do
  keys <- matrixDiag (zzip4 keyDiagram (leftHandK +++ rightHandK) (leftHandM +++ rightHandM) (leftHandL +++ rightHandL) (leftHandU +++ rightHandU))
  esc <- keyFull keySize "esc" "back to normal mode" None
  esc # SW .=. (keys !! 0 !! 0) # NW + (Point zero (constant keyDist))
  bar <- keyFull (keySize * 6 + keyDist * 5) "space" "select region" TextRegion
  keys !! 2 !! 2 # SW .=. bar # NW  + (Point zero (constant keyDist))
  return ()

regDiag = do
  txt <- label "lhtrs" "Left-hand text region specifiers:"
  keys <- matrixDiag (zzipWith keyHalf leftHandK leftHandR)
  spread vdist (constant 7) [keys!!0!!0,txt]
  return ()
  
(+++) :: [[a]] -> [[a]] -> [[a]]
x +++ y = zipWith (++) x y


main = renderTex Plain "cheat-sheet" docu

docu :: TeX
docu = preamble «
BOON cheat sheet. It is recommended to read the TUTORIAL to make sense of this. The color of a key indicates the type of argument it expects.

Command mode bindings.
@keyBDiag

@vspace"1em"

@regDiag
@hfill
@legend
»

