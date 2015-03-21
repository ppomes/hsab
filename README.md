# hsab
Apache ab-like utility in haskell

Small sample to start learning haskell: argv parsing, threading, http calls

Compilation: 
ghc -threaded --make ab.hs

Usage: ab [-n|--numbers ARG] [-c|--concurrency ARG] [-u|--URL ARG]
  An ab-like utility

Available options:
  -h,--help                Show this help text
  -n,--numbers ARG         Number of requests per thread
  -c,--concurrency ARG     Number of threads
  -u,--URL ARG             Queried URL
