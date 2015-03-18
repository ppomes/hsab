# hsab
Apache ab-like utility in haskell

Small sample to start learning haskell: argv parsing, threading, http calls

Compilation: 
ghc -threaded --make ab.hs

Usage: ab [OPTION...]
  -n num  --numbers=num      number of requests per thread
  -c num  --concurrency=num  number of threads
  -u str  --URL=str          URL
