# First Impressions

What is a cabal?

What is the canonical way to do local dev?
```
cabal build
ghc --make
runhaskell
runghc
```

Case analysis—OMG AWESOME

Arrays are hard. This can not be the data structure I want to use.

So many ways to do one thing:
```
putStrLn (show (1 + 1))
putStrLn (show $ 1 + 1)
putStrLn $ show (1 + 1)
putStrLn $ show $ 1 + 1
(putStrLn . show) (1 + 1)
putStrLn . show $ 1 + 1
putStrLn . show . (+1) $ 1
(putStrLn . show . (+1)) 1
```

Creating random numbers was a (pleasantly difficult) chore. Neat that forcing
state is hard.

Debugging is hard when you just want to print.

Output is messy but am happy and had fun making this.
