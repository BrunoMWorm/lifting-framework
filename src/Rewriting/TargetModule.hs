module Rewriting.TargetModule where

fooOld :: String -> IO ()
fooOld = print

data FooArg = Foo | Bar

fooNew :: FooArg -> IO ()
fooNew Foo = print "Foo"
fooNew Bar = print "Bar"

baz, bar, quux :: IO ()
baz = fooOld "foo"
 
bar = fooOld "bar"

quux = fooOld "quux"
