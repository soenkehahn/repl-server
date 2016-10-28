
module HandleSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec

import           Handle

spec :: Spec
spec = around_ silence $ do
  describe "readUntil" $ do
    it "reads until the specified string" $ do
      (readH, writeH) <- createPipe
      hPutStr writeH "foo bar huhu" >> hFlush writeH
      readUntil "bar" readH `shouldReturn` "foo "

    it "blocks until it encounters the given string" $ do
      (readH, writeH) <- createPipe
      readResultA <- async $ do
        readUntil "bar" readH
      threadDelay 100000
      hPutStr writeH "foo bar huhu" >> hFlush writeH
      wait readResultA `shouldReturn` "foo "

    it "can be invoked twice" $ do
      (readH, writeH) <- createPipe
      hPutStr writeH "foo bar huhu bar" >> hFlush writeH
      _ <- readUntil "bar" readH
      readUntil "bar" readH `shouldReturn` " huhu "

    it "can handle prefixes of the given string correctly" $ do
      (readH, writeH) <- createPipe
      hPutStr writeH "foo ba bar" >> hFlush writeH
      readUntil "bar" readH `shouldReturn` "foo ba "

  describe "captureWhile" $ do
    it "allows to read from a handle while executing another action" $ do
      (readH, writeH) <- createPipe
      (captured, ()) <- captureWhile readH $ do
        hPutStrLn writeH "foo" >> hFlush writeH
      captured `shouldBe` "foo\n"
