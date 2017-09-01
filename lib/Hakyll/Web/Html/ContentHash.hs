module Hakyll.Web.Html.ContentHash
  ( contentHashUrls
  , contentHashCompiler
  ) where

import qualified Data.ByteString.Lazy as LB (readFile)
import Data.Digest.Pure.MD5 (md5)
import Hakyll.Core.Compiler (Compiler, makeItem, unsafeCompiler)
import Hakyll.Core.Item (Item, itemBody)
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
          then contentHashCompiler [v]
          else makeItem v
      return (k, itemBody hashItem)

-- |
--
-- 与えられたパスのハッシュを計算する
-- 常に引数は1つ
--
contentHashCompiler :: [FilePath] -> Compiler (Item String)
contentHashCompiler path = do
  let ext = getExt $ head path
  content <- unsafeCompiler $ LB.readFile $ head path
  makeItem $ (\fn -> mconcat ["/images/", fn, ".", ext]) $ show $ md5 content
  where
    getExt = reverse . takeWhile (/= '.') . reverse
