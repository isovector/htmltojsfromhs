{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Lib where

import Data.List
import Data.Foldable
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup
import Control.Monad.State
import qualified Data.Map as M

makeBaseFunctor ''TagTree

-- TODO(sandy): bullshit. make these better later
data Element = Element { getElementName  :: String }
             | Text    { getTextContents :: String }

type DoStuff = StateT OurState IO

data OurState = OurState
  { uniqueVarNum   :: Int
  , knownNamespaces :: M.Map String String
  }


createElement :: String -> DoStuff Element
createElement tagname = do
  el_name <- makeName "elem"
  lift $ putStrLn $
    concat
      [ "let "
      , el_name
      , " = document.createElement(\""
      , tagname
      , "\");"
      ]
  return $ Element el_name

data PkgNamespace = Global | Package String

instantiateComponent
    :: PkgNamespace  -- pkg name
    -> String        -- component name
    -> Maybe String  -- uniqueid
    -> DoStuff Element
instantiateComponent pkg comp uniq = do
  tmp_name <- makeName "tmp"
  el_name <- makeName "elem"

  lift $ putStrLn $
    concat
      [ "let "
      , tmp_name
      , " = new "
      , getQualifiedPackage pkg comp
      , "();"
      ]

  lift $ putStrLn $
    concat
      [ "let "
      , el_name
      , " = "
      , tmp_name
      , ".getRootElement();"
      ]

  case uniq of
    Nothing -> pure ()
    Just u -> do
      lift $ putStrLn $
        concat
          [ "this._"
          , u
          , " = "
          , tmp_name
          , ";"
          ]

  return $ Element el_name

getQualifiedPackage :: PkgNamespace -> String -> String
getQualifiedPackage Global s = s
getQualifiedPackage (Package ns) s = ns ++ "." ++ s


addClass :: Element -> String -> DoStuff ()
addClass el = callMethod el "addClass" . pure . show

callMethod :: Element -> String -> [String] -> DoStuff ()
callMethod el method args =
  lift $ putStrLn $ concat
    [ getElementName el
    , "."
    , method
    , "("
    , intercalate ", " args
    , ");"
    ]

appendChild :: Element -> Element -> DoStuff ()
appendChild parent = callMethod parent "appendChild"
                   . pure
                   . getElementName

setAttribute :: Element -> String -> String -> DoStuff ()
setAttribute el attr_name attr_val =
  callMethod el "setAttribute" [show attr_name, show attr_val]

setTextContent :: Element -> String -> DoStuff ()
setTextContent el contents =
  lift $ putStrLn $ concat
    [ getElementName el
    , ".textContent = "
    , show contents
    , ";"
    ]


test :: DoStuff ()
test = do
  root  <- createElement "div"
  elem0 <- createElement "div"
  addClass elem0 "hello-world"
  setAttribute elem0 "id" "hello"
  appendChild root elem0


makeName :: String -> DoStuff String
makeName str = do
  OurState i ns <- get
  put $ OurState (i + 1) ns
  return $ str ++ show i


makeHtml :: TagTree String -> DoStuff Element
makeHtml = cataA $ \case
  TagBranchF tagname attribs children -> do
    comptag <- parseComponentTag tagname
    my_el <-
      case comptag of
        Just (pkg, comp) ->
          instantiateComponent pkg comp $ lookup "uniqueId" attribs
        Nothing -> createElement tagname

    case lookup "class" attribs of
      Just the_class -> addClass my_el the_class
      Nothing        -> pure ()

    for_ (filter (not . shouldIgnoreAttr . fst) attribs)
         $ uncurry
         $ setAttribute my_el

    for_ children $ \js_child -> do
      child <- js_child
      case child of
        Text text -> setTextContent my_el text
        Element var_name -> callMethod my_el "appendChild" [var_name]

    return my_el

  TagLeafF (TagText text) -> do
    return $ Text text


shouldIgnoreAttr :: String -> Bool
shouldIgnoreAttr "class"    = True
shouldIgnoreAttr "uniqueId" = True
shouldIgnoreAttr _          = False


parseComponentTag :: String -> DoStuff (Maybe (PkgNamespace, String))
parseComponentTag str = do
  case break (== ':') str of
    (_, "") -> pure $ Nothing
    (pkg, _ : comp) -> do
      pkgns <- parsePkgNamespace pkg
      return $ Just (pkgns, comp)


parsePkgNamespace :: String -> DoStuff PkgNamespace
parsePkgNamespace "component" = return Global
parsePkgNamespace ns = do
  OurState _ nsmap <- get
  case M.lookup ns nsmap of
    Just ns' -> pure $ Package ns'
    Nothing -> error $ "you used a bad namespace! " ++ ns


parseXmlns :: String -> Maybe String
parseXmlns str =
  case break (== ':') str of
    ("xmlns", _ : ns@(_:_)) -> Just ns
    _                       -> Nothing


findXmlnses :: [(String, String)] -> M.Map String String
findXmlnses attrs = M.fromList $ do
  (name, val) <- attrs
  case parseXmlns name of
    Just ns -> return (ns, val)
    Nothing -> []


getTheStateBro :: [TagTree String] -> OurState
getTheStateBro [TagBranch _ attrs _] = OurState 0 $ findXmlnses attrs
getTheStateBro _ = error "you fucked the shit dawg"


main :: IO ()
main = do
  let tree = parseTree "<html xmlns:sandy=\"bigdog\"><sandy:maguire id=\"travis\" uniqueId=\"haskellCool\">hi travis</sandy:maguire></html>"

  flip evalStateT (getTheStateBro tree)
     $ traverse_ makeHtml
     $ tree

