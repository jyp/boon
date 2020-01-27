{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -pgmF marxup -F #-}
{-# LANGUAGE TupleSections, RecordWildCards, RecursiveDo, OverloadedStrings #-}
module Main (main) where

import Data.Char (toUpper,chr)
import Prelude hiding (mapM,sequence,Num(..),(/))
import MarXup
import MarXup.Latex
import MarXup.Tex hiding (label)
import Graphics.Diagrams
import MarXup.Diagram
import Control.Lens (set)
import Data.Traversable
import Data.List (isSuffixOf,isPrefixOf)
import Algebra.Classes
import qualified Layout
import System.Environment
import Mcolemak
import Mqwerty
import Mcolemak
import Mqwertz
import Mworkman

preamble body = do
  documentClass "article" ["10pt"]
  usepackage "fontspec" []
  cmd "setsansfont" (tex "DejaVu Sans")
  -- cmd "setmainfont" (tex "DejaVu Serif")
  usepackage "tikz" []
  usepackage "graphicx" []
  usepackage "amssymb" []
  usepackage "varwidth" []
  usepackage "geometry" ["margin=1cm","paper=a4paper","landscape"]
  env "document" body

data CheatSheet = CS
  { leftHandK, rightHandK :: [[String]] -- keycap glyphs
  , commandsInfo, selectorsInfo :: [(String, (TeX,Argument,TeX))]
  }

commandArgument :: String -> Argument
commandArgument x | "region" `isSuffixOf` x = TextRegion
                  | "character" `isSuffixOf` x = Char
                  | "map" `isSuffixOf` x = Prefix
                  | "avy" `isPrefixOf` x = Char
commandArgument "boon-enclose" = Bin Enclosure TextRegion
commandArgument "selectContent" = TextRegion
commandArgument "" = Reserved
commandArgument _  = None



upKey :: Char -> Char
upKey c = case [c] of
  "'" -> head "\""
  ";" -> ':'
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
  TextRegion -> "cyan"
  Bin _ _ -> "purple"
  _ -> "white"

varwidth :: forall a. String -> Tex a -> Tex a
varwidth x body = env "varwidth" ((braces $ tex x) >> cmd0 "centering" >> body)

keySize :: Constant
keySize = 65

massageInfo :: (Integer, (String,String)) -> (String, (TeX,Argument,TeX))
massageInfo (c,(mnemonic,command)) = ([chr (fromIntegral c)],(cmdTex,arg,textual mnemonic))
  where (cmdTex,arg) | command == "nil" = (italic "reserved",Reserved)
                     | otherwise = (textual (shortenCommand command),commandArgument command)

shortenCommand :: String -> String
shortenCommand c | "boon-" `isPrefixOf` c = shortenCommand (drop 5 c)
shortenCommand c | "-map" `isSuffixOf` c = shortenCommand (dropR 4 c)
shortenCommand c = c

dropR :: forall a. Int -> [a] -> [a]
dropR n = reverse . drop n . reverse

keyHalf :: [(String, (TeX, Argument, TeX))]
           -> String -> TexDiagram Object
keyHalf kmInfo k = do
  let (act,arg,mnem) = case lookup k kmInfo of
                         Nothing -> (mempty,Reserved,mempty)
                         Just (act,arg,mnem) -> (act,arg,mnem)
  up <- using (fill (argColor arg)) $ draw $ box "keyHalf"
  upK <- label "k" $  textSize Huge $ sans $ textual $ k
  upT <- label "scr" $ varwidth "45pt" $ sans $ textSize ScriptSize $ act
  m <- (label "mnem" $ sans $ textSize Tiny $ mnem)
  m # SW .=. up # SW
  up # E .=. upT # E
  up # W .=. upK # W
  width up === constant keySize
  height up === constant (keySize / 2)
  return up


keyDiagram :: [(String, (TeX, Argument, TeX))] -> String -> Diagram TeX Tex Object
keyDiagram kmInfo k = do
  b <- using (set lineWidth thick) $ draw $ box "keyB"
  up <- keyHalf kmInfo (map upKey k)
  down <- keyHalf kmInfo k
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

argDescs :: [(Argument, TeX)]
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

keyBDiag :: CheatSheet -> TexDiagram ()
keyBDiag CS {..} = do
  keys <- matrixDiag (map (map (keyDiagram commandsInfo)) (leftHandK +++ rightHandK))
  esc <- keyFull keySize "esc" "back to normal mode" None
  esc # SW .=. (keys !! 0 !! 0) # NW + (Point zero (constant keyDist))
  bar <- keyFull (keySize * 6 + keyDist * 5) "space" "select region" TextRegion
  keys !! 2 !! 2 # SW .=. bar # NW  + (Point zero (constant keyDist))
  return ()

regDiag :: CheatSheet -> TexDiagram ()
regDiag CS {..} = do
  txt <- label "lhtrs" "Left-hand text region specifiers:"
  keys <- matrixDiag (map (map (keyHalf selectorsInfo)) leftHandK)
  spread vdist (constant 7) [keys!!0!!0,txt]
  return ()

(+++) :: [[a]] -> [[a]] -> [[a]]
x +++ y = zipWith (++) x y

mkCS (commandMap,movesMap,selectMap) =
  CS {leftHandK = [], rightHandK = []
     ,commandsInfo = ("",(mempty,Reserved,mempty)):
                     map massageInfo (Layout.commandMap ++ Layout.movesMap)
     ,selectorsInfo = map massageInfo Layout.selectMap
     }

main :: IO ()
main = do
  [flavor] <- getArgs
  let cs' = case flavor of
         "colemak" -> (mkCS colemak) {
           leftHandK = [["q","w","f","p","g"]
                       ,["a","r","s","t","d"]
                       ,["z","x","c","v","b"]]
           ,rightHandK = [["j","l","u","y",";",""]
                         ,["h","n","e","i","o","'"]
                         ,["k","m",",",".","/",""]]}
         "qwerty" -> (mkCS qwerty)
               {leftHandK = [["q","w","e","r","t"]
                   ,["a","s","d","f","g"]
                   ,["z","x","c","v","b"]]

               ,rightHandK = [["y","u","i","o","p",""]
                    ,["h","j","k","l",";","'"]
                    ,["n","m",",",".","/",""]]}
         "qwertz" -> (mkCS qwertz)
               {leftHandK = [["q","w","e","r","t"]
                            ,["a","s","d","f","g"]
                            ,["y","x","c","v","b"]]

               ,rightHandK = [["z","u","i","o","p",""]
                             ,["h","j","k","l","ö","ä"]
                             ,["n","m",";",":","-",""]]}
         "workman" -> (mkCS workman)
               {leftHandK = [["q","d","r","w","b"]
                            ,["a","s","h","t","g"]
                            ,["z","x","m","c","v"]]

               ,rightHandK = [["j","f","u","p","k",";"]
                             ,["y","n","e","o","i","'"]
                             ,["k","l",",",".","/",""]]}
  renderTex Plain flavor (docu cs')

docu :: CheatSheet -> TeX
docu csData = preamble «
BOON cheat sheet. It is recommended to read the TUTORIAL to make sense of this. The color of a key indicates the type of argument it expects.

Command mode bindings.
@keyBDiag(csData)

@vspace"1em"

@regDiag(csData)
@hfill
@legend
»

