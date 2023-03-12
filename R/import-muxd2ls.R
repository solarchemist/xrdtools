#' Convert UXD file to R list object
#'
#' This function reads an UXD file which contains several ranges (created in
#' a programmed run, for example).
#'
#' @param uxdfile path to UXD file
#'
#' @return list of matrices, as many as there were ranges in the UXD file
#'
#' @export
muxd2ls <- function(uxdfile) {

   cchar <- "[;_]" #comment characters used in Bruker's UXD
   cdata <- "[0-9]"

   ufile <- file(uxdfile, "r")
   f <- readLines(ufile, n=-1)
   close(ufile)

   wh <- regexpr(cchar, f)
   mh <- wh[1:length(wh)]

   i <- seq(1, length(mh) - 1, 1)
   j <- seq(2, length(mh), 1)
   starts <- which(mh[i] == 1 & mh[j] != 1) + 1
   ends   <- which(mh[i] != 1 & mh[j] == 1)
   ends   <- c(ends, length(mh))

   ff <- list()
   for (s in 1:length(starts)) {
      zz <- textConnection(f[starts[s]:ends[s]], "r")
      ms <- matrix(scan(zz, what = numeric()), ncol = 2, byrow = T)
      close(zz)
      ff[[s]] <- ms
   }
   # Return list of matrices
   return(ff)
}
