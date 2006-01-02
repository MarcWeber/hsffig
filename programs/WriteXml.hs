-- Output contents of a XML file to standard output

module WriteXml where

import Text.XML.HXT.Parser
import Text.XML.HXT.DOM
import System.Exit
import Control.Monad
import qualified Data.Map as Map
import C_BNF
import HsfUtils
import WriteHsc

-- The main idea is to be able to reconstruct a header from the XML output.
-- It it assumed that some kind of a filter may process the XML in between.

writeXml (tus, tymap) fn = do
  let d = buildXml tus tymap fn
  putXmlDocument [(a_output_file, "-"),
                  (a_show_tree,"0"),
                  (a_indent,"1")] "" d
  putStrLn ""

buildXml tus tymap fn = 
  mkRootTree [] [
    mkXTagTree "hsffig" [justxmlattr "filename" fn] [mkstructs tymap fn, mktypes tymap fn]
  ]
 
xmlattr an av = mkXAttrTree an [mkXTextTree av]

justxmlattr an (Just av) = xmlattr an av
justxmlattr an Nothing = xmlattr an ""

mklistitem s = mkXTagTree "li" [] [mkXTextTree s]

mktypes tymap fn = 
  let typedefs = Map.toList $ Map.filterWithKey tdefonly tymap
      tdefonly _ (DictDecl _) = True
      tdefonly _ _ = False
  in  mkXTagTree "types" [] $ map (tdefaliasxml tymap) typedefs

tdefaliasxml tymap (tal,DictDecl (Declaration dss [id] _)) =
  let target = (ts2ts .
                (monadify "IO") .
                mapc2hs .
                state2ts .
                dcl2ts .
                (connectta tymap)) (simplifydecl dss id)
      polish ('@':s) = s
      polish z = z
  in  mkXTagTree "typedef" [
        xmlattr "alias" ("T_" ++ tal),
        xmlattr "original" (polish target)
      ] []

mkstructs tymap fn = 
  let structs = Map.filterWithKey structonly tymap
      structonly _ (DictStruct _ _) = True
      structonly _ _ = False
      strpairs = Map.toList structs
  in  mkXTagTree "structs" [] $ map (onestructxml tymap fn) strpairs

onestructxml tymap fn (strname, strdecl) = 
  let (strinfo, fldinfos) = mkStructsInfo tymap strname strdecl
      fldinfos' = filter keepfld fldinfos
      keepfld fi = case (isDynamic fi, isVariadic fi||isDirect fi) of
                  (False, _)    -> True
                  (True, False) -> True
                  (True, True)  -> False
  in  mkXTagTree "struct" [xmlattr "type" (convName strinfo)] (map onememberxml fldinfos') where

onememberxml fi = 
  mkXTagTree "member" [
    xmlattr "name" (fldName fi),
    xmlattr "type" (instType fi)
  ] []

