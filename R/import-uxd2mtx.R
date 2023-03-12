#' Convert UXD file containing a single range to R matrix object
#'
#' Assumes the UXD file contains two columns.
#'
#' @param uxdfile path to UXD file
#'
#' @return matrix with two columns
#' @export
uxd2mtx <- function(uxdfile) {

   # regexpr matching the comment characters used in Bruker's UXD
   cchar <- "[;_]"
   # regexpr matching one character of any digit
   cdata <- "[0-9]"

   # A new file (datafile) containing only data will be created,
   # extension ".data" appended to uxdfile
   # datafile <- paste(uxdfile,".data",sep="")

   ufile <- file(uxdfile, "r")
   # n=-1 reads *all* lines from UXD file
   f <- readLines(ufile, n = -1)
   close(ufile)

   # This way we identify data rows by looking for numeric characters.
   # wh <- regexpr("[0-9]", f)
   # This way we identify header rows
   # We assume that all other rows are data
   wh <- regexpr(cchar, f)

   # this gives us the corresponding index vector
   mh <- wh[1:length(wh)]
   # the value of each element corresponds to the position of the regexp match.
   # value = 1 means the first character of the row is cchar (row is header)
   # value =-1 means no cchar occur on the row (row is data)

   i <- seq(1, length(mh) - 1, 1)
   j <- seq(2, length(mh), 1)

   starts <- which(mh[i] == 1 & mh[j] != 1) + 1
   ends   <- length(mh)
   f      <- f[starts:ends]

   zz <- textConnection(f, "r")
   ff <- matrix(scan(zz, what = numeric()), ncol = 2, byrow = TRUE)
   close(zz)

   # zz <- file(datafile, "w") # open connection to datafile
   # write.table(ff, file = datafile, row.names = F, sep = ",")
   # close(zz)

   # return matrix
   ff
}
