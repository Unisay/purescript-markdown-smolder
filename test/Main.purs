module Test.Main where

import Prelude
import Test.Unit.Assert as Assert
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Test.Unit (Test, suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Markdown.SlamDown (SlamDownP)
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder (fromSlamDown)
import Text.Smolder.HTML (a, blockquote, br, code, em, h1, h2, h3, hr, li, ol, p, pre, strong, ul)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (Markup, parent, text, (!))
import Text.Smolder.Renderer.String (render)

main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 | e
                 ) Unit
main = runTest $
  suite "SlamDown ~> Markup" do
    test "horizontal line" $
      matchMd "---" hr
    test "link reference" $
      matchMd "[foo]: /url" (a ! href "/url" $ text "foo")
    test "paragraph with string" $
      matchMd "foo" (p $ text "foo")
    test "header (1) with string" $
      matchMd "# foo" (h1 $ text "foo")
    test "header (2) with string" $
      matchMd "## foo" (h2 $ text "foo")
    test "header (3) with string" $
      matchMd "### foo" (h3 $ text "foo")
    test "header (4) with string" $
      matchMd "#### foo" (parent "h4" $ text "foo")
    test "header (5) with string" $
      matchMd "##### foo" (parent "h5" $ text "foo")
    test "header (6) with string" $
      matchMd "###### foo" (parent "h6" $ text "foo")
    test "blockquote" $
      matchMd "> foo" (blockquote $ p $ text "foo")
    test "ordered list" $
      matchMd "1. foo\n1. bar" (ol $ (li $ text "foo") *> (li $ text "bar"))
    test "unordered list" $
      matchMd "- foo\n- bar" (ul $ (li $ text "foo") *> (li $ text "bar"))
    test "indented code block" $
      matchMd "    indented" (pre $ code $ text "indented")
    test "fenced code block" $
      matchMd "```haskell\nfenced\n```" (pre $ code ! className "language-haskell" $ text "fenced")
    test "code span" $
      matchMd "``\nfoo\n``" (p $ code $ text "foo")
    test "emphasised text" $
      matchMd "_foo_" (p $ em $ text "foo")
    test "strong text" $
      matchMd "__foo__" (p $ strong $ text "foo")
    test "hard line break" $
      matchMd "foo  \nbaz" (p $ (text "foo") *> br *> (text "baz"))
    test "soft line break" $
      matchMd "foo\nbaz" (p $ text "foo\nbaz")

type Markdown = String

matchMd :: ∀ e a. Markdown -> Markup a -> Test (console :: CONSOLE | e)
matchMd md mp = do
  expected <- pure mp
  markdown <- parseMarkdown md
  -- logShow markdown
  let actual = fromSlamDown markdown
  Assert.equal (render expected) (render actual)

parseMarkdown :: ∀ e. Markdown -> Aff e (SlamDownP String)
parseMarkdown s = either (throwError <<< error) pure (parseMd s)
