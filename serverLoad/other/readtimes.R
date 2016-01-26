require(microbenchmark)
require(readr)

infile <- '/home/nathan/sol-eng/presentations/201601-shinydev/randomWalkApp/data/randomWalk.txt'
microbenchmark(
  a = read.csv(infile,as.is=T,col.names=c('dte','Load')),
  b = scan(infile,list(dte='',Load=0),sep=','),
  c = read_csv(infile,c('dte','Load')),
  times = 10
)

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# a 372.7099 380.5551 399.5621 384.6479 394.3263 522.6826    10
# b 352.0259 358.3997 365.8868 362.9517 377.0505 386.1611    10
# c 126.6125 129.4901 134.3283 133.7247 139.0507 141.8382    10
