import Test.DocTest (doctest)


main :: IO ()
main = doctest [
  "-XOverloadedStrings",
  "-XConstraintKinds",
  "-XDeriveGeneric",
  "-XFlexibleContexts",
  "-XGeneralizedNewtypeDeriving",
  "-XScopedTypeVariables",
  "-XTupleSections",
  "-isrc", "src"
 ]
