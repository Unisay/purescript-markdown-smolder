module Text.Markdown.SlamDown.Smolder where

import Control.Monad.Free (liftF)
import Data.List (List(..), intercalate, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Text.Markdown.SlamDown (SlamDownP(..))
import Text.Markdown.SlamDown.Syntax (Block(..), CodeBlockType(..), Inline(..), LinkTarget(..), ListType(..), SlamDownP)
import Text.Smolder.HTML (blockquote, br, code, hr, li, ol, a, p, pre, ul)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup (Markup, MarkupM(..), parent, text, (!))
import Prelude hiding (div)

empty :: ∀ e. Markup e
empty = liftF $ Empty unit

fromSlamDown :: SlamDownP ~> Markup
fromSlamDown (SlamDown blocks) = traverse_ translateBlock blocks

translateBlock :: Block ~> Markup
translateBlock Rule = hr
translateBlock (LinkReference label dest) = a ! href dest $ text label
translateBlock (CodeBlock (Fenced b s) ss) = p $ code $ text $ intercalate "\n" ss
translateBlock (CodeBlock Indented ss) = pre $ code $ text $ intercalate "\n" ss
translateBlock (Lst (Ordered _) items) = ol $ traverse_ (traverse_ (replaceParagraph li)) items
translateBlock (Lst (Bullet _) items) = ul $ traverse_ (traverse_ (replaceParagraph li)) items
translateBlock (Blockquote blocks) = blockquote $ traverse_ translateBlock blocks
translateBlock (Header n inlines) = parent ("h" <> show n) $ traverse_ translateInline inlines
translateBlock par@(Paragraph _) = replaceParagraph p par

replaceParagraph :: (∀ e. Markup e -> Markup e) -> Block ~> Markup
replaceParagraph container (Paragraph (inline : Nil)) = container $ translateInline inline
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
