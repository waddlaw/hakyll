module Hakyll.Web.Html.ContentHash
  ( contentHashUrls
  , contentHashCompiler
  , contentHashRoute
  ) where

import qualified Data.ByteString.Lazy as LB (readFile, ByteString)
import Data.Digest.Pure.MD5 (md5)
import Hakyll.Core.Compiler (Compiler, makeItem, unsafeCompiler)
import Hakyll.Core.Identifier (Identifier, toFilePath)
import Hakyll.Core.Item (Item, itemBody)
import Hakyll.Core.Routes (Routes, customRouteIO)
import Hakyll.Web.Html (withTagsM, isExternal)
import Text.HTML.TagSoup (Tag(TagOpen))

-- |
--
-- img の src リンクのみ hash パスに変更
--
contentHashUrls :: Item String -> Compiler (Item String)
contentHashUrls item = (withTagsM tag . itemBody $ item) >>= makeItem
  where
    tag (TagOpen "img" a) = fmap (TagOpen "img") $ mapM attr a
    tag x = return x
    attr (k, v) = do
      hashItem <-
        if (k `elem` ["src"]) && (not $ isExternal v)
          then contentHashCompiler v
          else makeItem v
      return (k, itemBody hashItem)

-- |
--
-- 与えられたパスのハッシュを計算する
-- 常に引数は1つ
--
contentHashCompiler :: FilePath -> Compiler (Item String)
contentHashCompiler path = do
  content <- unsafeCompiler . LB.readFile $ path
  makeItem $ renderHashPath path content

-- |
--
-- コンテンツハッシュルート
--
contentHashRoute :: Routes
contentHashRoute = customRouteIO toHash

-- |
--
-- ファイルの md5sum を基にファイルパスを生成
--
toHash :: Identifier -> IO FilePath
toHash id' = do
  let path = toFilePath id'
  content <- LB.readFile path
  return $ renderHashPath path content

renderHashPath :: FilePath -> LB.ByteString -> String
renderHashPath fp = render . show . md5
  where
    render hv = mconcat ["images/", hv, ".", ext]
    ext = getExt fp

getExt :: String -> String
getExt = reverse . takeWhile (/= '.') . reverse
