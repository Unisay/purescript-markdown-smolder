module Text.Markdown.SlamDown.Smolder where

import Data.Traversable (traverse_)
import Control.Monad.Free (liftF)
import Data.Maybe (Maybe(..))
import Text.Markdown.SlamDown (SlamDownP(..))
import Text.Markdown.SlamDown.Syntax (Block(..), CodeBlockType(..), Inline(..), LinkTarget(..), ListType(..), SlamDownP)
import Text.Smolder.HTML (br, p)
import Text.Smolder.Markup (Markup, MarkupM(..), parent, text)
import Prelude hiding (div)

empty :: ∀ e. Markup e
empty = liftF $ Empty unit

fromSlamDown :: SlamDownP ~> Markup
fromSlamDown (SlamDown blocks) = traverse_ translateBlock blocks

translateBlock :: Block ~> Markup
translateBlock Rule = empty
translateBlock (LinkReference s1 s2) = empty
translateBlock (CodeBlock (Fenced b s) ss) = empty
translateBlock (CodeBlock Indented ss) = empty
translateBlock (Lst (Ordered s) _) = empty
translateBlock (Lst (Bullet s) _) = empty
translateBlock (Blockquote _) = empty
translateBlock (Header n inlines) = parent ("h" <> show n) $ traverse_ translateInline inlines
translateBlock (Paragraph inlines) = p $ traverse_ translateInline inlines

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
