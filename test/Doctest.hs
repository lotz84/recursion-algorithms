import Test.DocTest

main :: IO ()
main = doctest ["-XBlockArguments", "-XLambdaCase", "docs"]
