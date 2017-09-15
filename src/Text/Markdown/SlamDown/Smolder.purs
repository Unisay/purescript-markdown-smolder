module Text.Markdown.SlamDown.Smolder where

import Text.Markdown.SlamDown as S
import Control.Monad.Free (liftF)
import Data.List (List(..), intercalate, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Text.Smolder.HTML (a, blockquote, br, code, em, hr, img, li, ol, p, pre, strong, ul)
import Text.Smolder.HTML.Attributes (alt, className, href, src)
import Text.Smolder.Markup (Markup, MarkupM(..), parent, text, (!))
import Text.Smolder.Renderer.String (render)
import Prelude hiding (div)

empty :: ∀ e. Markup e
empty = liftF $ Empty unit

fromSlamDown :: S.SlamDownP ~> Markup
fromSlamDown (S.SlamDown blocks) = traverse_ translateBlock blocks

translateBlock :: S.Block ~> Markup
translateBlock S.Rule = hr
translateBlock (S.LinkReference label dest) = a ! href dest $ text label
translateBlock (S.CodeBlock (S.Fenced _ "") ss) = pre $ code $ text $ intercalate "\n" ss
translateBlock (S.CodeBlock (S.Fenced _ lang) ss) = pre $ code ! className ("language-" <> lang) $ text $ intercalate "\n" ss
translateBlock (S.CodeBlock S.Indented ss) = pre $ code $ text $ intercalate "\n" ss
translateBlock (S.Lst (S.Ordered _) items) = ol $ traverse_ (traverse_ (replaceParagraph li)) items
translateBlock (S.Lst (S.Bullet _) items) = ul $ traverse_ (traverse_ (replaceParagraph li)) items
translateBlock (S.Blockquote blocks) = blockquote $ traverse_ translateBlock blocks
translateBlock (S.Header n inlines) = parent ("h" <> show n) $ traverse_ translateInline inlines
translateBlock (S.Paragraph inlines) = p $ traverse_ translateInline inlines

replaceParagraph :: (∀ e. Markup e -> Markup e) -> S.Block ~> Markup
replaceParagraph container (S.Paragraph (inline : Nil)) = container $ translateInline inline
replaceParagraph _ b = translateBlock b

translateInline :: S.Inline ~> Markup
translateInline (S.FormField _ _ _) = empty-- TODO
translateInline (S.Image inlines s) = img ! src s ! alt (render $ traverse_ translateInline inlines)
translateInline (S.Link _ (S.ReferenceLink Nothing)) = empty-- TODO
translateInline (S.Link _ (S.ReferenceLink (Just _))) = empty-- TODO
translateInline (S.Link _ (S.InlineLink _)) = empty-- TODO
translateInline (S.Code b s) = code $ text s
translateInline (S.Strong inlines) = strong $ traverse_ translateInline inlines
translateInline (S.Emph inlines) = em $ traverse_ translateInline inlines
translateInline S.LineBreak = br
translateInline S.SoftBreak = text "\n"
translateInline S.Space = text " "
translateInline (S.Entity _) = empty -- TODO
translateInline (S.Str s) = text s
