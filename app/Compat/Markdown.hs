{-# LANGUAGE OverloadedStrings #-}

module Compat.Markdown where

import Brick qualified as B
import Brick.Widgets.Core qualified as W
import CMark qualified as Md
import Data.Char (isSpace)
import Data.List (unsnoc)
import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro ((^.))
import Types

data InlineFragment = InlineFragment [B.AttrName] Text

data StyledChar = StyledChar [B.AttrName] Char

parseMarkdownText :: Text -> B.Widget (MName St)
parseMarkdownText = go . Md.commonmarkToNode []
 where
  go (Md.Node _ typ nodes) = case typ of
    Md.DOCUMENT -> blocks nodes
    Md.THEMATIC_BREAK -> W.str "---"
    Md.PARAGRAPH -> W.padBottom (W.Pad 1) $ inlines nodes
    Md.BLOCK_QUOTE -> W.withAttr (B.attrName "mkQuote") $ inlines nodes
    Md.HTML_BLOCK _ -> W.emptyWidget
    Md.CUSTOM_BLOCK _ _ -> blocks nodes
    Md.CODE_BLOCK _ source -> W.txtWrap source
    Md.HEADING _ -> W.padBottom (W.Pad 1) . W.withAttr (B.attrName "mkHeader") $ inlines nodes
    Md.LIST attributes -> renderList attributes nodes
    Md.ITEM -> blocks nodes
    Md.TEXT content -> W.txtWrap content
    Md.SOFTBREAK -> W.str " "
    Md.LINEBREAK -> W.str " "
    Md.HTML_INLINE _ -> W.emptyWidget
    Md.CUSTOM_INLINE _ _ -> inlines nodes
    Md.CODE content -> W.txtWrap content
    Md.EMPH -> inlines nodes
    Md.STRONG -> W.withAttr (B.attrName "mkStrong") $ inlines nodes
    Md.LINK _ _ -> inlines nodes
    Md.IMAGE _ _ -> inlines nodes

  blocks = W.vBox . map go
  inlines = inlineFlow . inlineFragments

  renderList attributes =
    W.vBox . zipWith renderItem [Md.listStart attributes ..]
   where
    renderItem index (Md.Node _ Md.ITEM itemNodes) =
      W.hBox [W.str $ itemPrefix index, blocks itemNodes]
    renderItem _ node = go node

    itemPrefix index =
      case Md.listType attributes of
        Md.BULLET_LIST -> "- "
        Md.ORDERED_LIST -> show index <> ". "

inlineFragments :: [Md.Node] -> [InlineFragment]
inlineFragments = concatMap (collectInline [])

collectInline :: [B.AttrName] -> Md.Node -> [InlineFragment]
collectInline attrs (Md.Node _ inlineType children) =
  case inlineType of
    Md.TEXT content -> [InlineFragment attrs content]
    Md.SOFTBREAK -> [InlineFragment attrs " "]
    Md.LINEBREAK -> [InlineFragment attrs " "]
    Md.CODE content -> [InlineFragment (B.attrName "mkQuote" : attrs) content]
    Md.HTML_INLINE _ -> []
    Md.CUSTOM_INLINE _ _ -> descend attrs
    Md.EMPH -> descend attrs
    Md.STRONG -> descend (B.attrName "mkStrong" : attrs)
    Md.LINK _ _ -> descend attrs
    Md.IMAGE _ _ -> descend attrs
    _ -> descend attrs
 where
  descend styles = concatMap (collectInline styles) children

inlineFlow :: [InlineFragment] -> B.Widget (MName St)
inlineFlow fragments =
  B.Widget B.Greedy B.Fixed $ do
    context <- B.getContext
    let width = max 1 $ context ^. B.availWidthL
    B.render . W.vBox $ renderInlineLine <$> wrapInline width (fragmentChars =<< fragments)

fragmentChars :: InlineFragment -> [StyledChar]
fragmentChars (InlineFragment attrs content) = StyledChar attrs <$> Text.unpack content

wrapInline :: Int -> [StyledChar] -> [[StyledChar]]
wrapInline width = foldl addWord [] . inlineWords
 where
  addWord [] word = [word]
  addWord rows word =
    case unsnoc rows of
      Just (finished, current)
        | length current + 1 + length word <= width ->
            finished <> [current <> [spaceAfter current] <> word]
      _ -> rows <> [word]

  spaceAfter (StyledChar attrs _ : _) = StyledChar attrs ' '
  spaceAfter [] = StyledChar [] ' '

inlineWords :: [StyledChar] -> [[StyledChar]]
inlineWords chars =
  case dropWhile whitespace chars of
    [] -> []
    remaining ->
      let (word, rest) = break whitespace remaining
       in word : inlineWords rest
 where
  whitespace (StyledChar _ char) = isSpace char

renderInlineLine :: [StyledChar] -> B.Widget (MName St)
renderInlineLine = W.hBox . fmap renderRun . styledRuns
 where
  styledRuns [] = []
  styledRuns (first : rest) =
    let (sameStyle, remaining) = span ((== attrs first) . attrs) rest
     in (attrs first, char first : (char <$> sameStyle)) : styledRuns remaining

  renderRun (styles, content) = foldr W.withAttr (W.str content) styles

  attrs (StyledChar styles _) = styles
  char (StyledChar _ content) = content
