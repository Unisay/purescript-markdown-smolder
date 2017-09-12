module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Test.Unit (Test, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Markdown.SlamDown (SlamDownP)
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder (fromSlamDown)
import Text.Smolder.HTML (blockquote, h1, h2, h3, hr, li, ol, p, ul)
import Text.Smolder.Markup (Markup, parent, text)
import Text.Smolder.Renderer.String (render)

main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 | e
                 ) Unit
main = runTest $
  suite "SlamDown ~> Markup" do
    test "translate horizontal line" $
      matchMd "---" hr
    test "translate paragraph with string" $
      matchMd "foo" (p $ text "foo")
    test "translate header (1) with string" $
      matchMd "# foo" (h1 $ text "foo")
    test "translate header (2) with string" $
      matchMd "## foo" (h2 $ text "foo")
    test "translate header (3) with string" $
      matchMd "### foo" (h3 $ text "foo")
    test "translate header (4) with string" $
      matchMd "#### foo" (parent "h4" $ text "foo")
    test "translate header (5) with string" $
      matchMd "##### foo" (parent "h5" $ text "foo")
    test "translate header (6) with string" $
      matchMd "###### foo" (parent "h6" $ text "foo")
    test "translate blockquote" $
      matchMd "> foo" (blockquote $ p $ text "foo")
    test "translate ordered list" $
      matchMd """1. foo
1. bar""" (ol $ (li $ text "foo") *> (li $ text "bar"))
    test "translate unordered list" $
      matchMd """- foo
- bar""" (ul $ (li $ text "foo") *> (li $ text "bar"))

type Markdown = String

matchMd :: ∀ e a. Markdown -> Markup a -> Test (console :: CONSOLE | e)
matchMd md mp = do
  expected <- pure mp
  markdown <- parseMarkdown md
  let actual = fromSlamDown markdown
  Assert.equal (render expected) (render actual)

parseMarkdown :: ∀ e. Markdown -> Aff e (SlamDownP String)
parseMarkdown s = either (throwError <<< error) pure (parseMd s)
