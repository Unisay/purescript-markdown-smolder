module Text.Markdown.SlamDown.Smolder where

import Control.Monad.Free (liftF)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Prelude hiding (div)
import Text.Markdown.SlamDown (SlamDownP(..))
import Text.Markdown.SlamDown.Syntax (Block(..), CodeBlockType(..), Inline(..), LinkTarget(..), ListType(..), SlamDownP)
import Text.Smolder.HTML (blockquote, br, hr, li, ol, p, ul)
import Text.Smolder.Markup (Markup, MarkupM(..), parent, text)

empty :: ∀ e. Markup e
empty = liftF $ Empty unit

fromSlamDown :: SlamDownP ~> Markup
fromSlamDown (SlamDown blocks) = traverse_ translateBlock blocks

translateBlock :: Block ~> Markup
translateBlock Rule = hr
translateBlock (LinkReference s1 s2) = empty
translateBlock (CodeBlock (Fenced b s) ss) = empty
translateBlock (CodeBlock Indented ss) = empty
translateBlock (Lst (Ordered s) lists) = ol $ traverse_ (traverse_ (replaceParagraph li)) lists
translateBlock (Lst (Bullet s) lists) = ul $ traverse_ (traverse_ (replaceParagraph li)) lists
translateBlock (Blockquote blocks) = blockquote $ traverse_ translateBlock blocks
translateBlock (Header n inlines) = parent ("h" <> show n) $ traverse_ translateInline inlines
translateBlock par@(Paragraph _) = replaceParagraph p par

replaceParagraph :: (∀ e. Markup e -> Markup e) -> Block ~> Markup
replaceParagraph container (Paragraph inlines) = container $ traverse_ translateInline inlines
replaceParagraph _ b = translateBlock b

translateInline :: Inline ~> Markup
translateInline (FormField _ _ _) = empty
translateInline (Image _ _) = empty
translateInline (Link _ (ReferenceLink Nothing)) = empty
translateInline (Link _ (ReferenceLink (Just _))) = empty
translateInline (Link _ (InlineLink _)) = empty
translateInline (Code _ _) = empty
translateInline (Strong _) = empty
translateInline (Emph _) = empty
translateInline LineBreak = br
translateInline SoftBreak = empty
translateInline Space = text " "
translateInline (Entity _) = empty
translateInline (Str s) = text s
