#' Import data and metadata from UXD files converted using the XCH tool
#'
#' Read UXD files from Bruker diffractometer with one or multiple ranges
#' (converted from Bruker's native format using XCH v1.0).
#' It automatically calculates CPS (counts per second) if COUNTS are present,
#' and vice versa (note that this depends on specific strings in the UXD format).
#' Old UXD-files (probably from the Siemens diffractometers) contain the following line:
#' "; (Data for Range number 1)"
#' Converted UXD files (from the Bruker D8) drop the parentheses and "number":
#' "; Data for range 1"
#' This function is for the **former** case.
#'
#' @param uxdfile path to UXD file
#'
#' @return dataframe with the following columns:
#'   $ sampleid        : chr
#'   $ substrateid     : chr
#'   $ thth            : num
#'   $ counts (or cps) : num
#'   $ steptime        : num
#'   $ stepsize        : num
#'   $ theta           : num
#'   $ khi             : num
#'   $ phi             : num
#'   $ x               : num
#'   $ y               : num
#'   $ z               : num
#'   $ divergence      : num
#'   $ antiscatter     : num
#'   $ cps (or counts) : num
#'
#' @export
muxd2df <- function(uxdfile) {

   range.header.start.rexp <- "^; \\(Data for Range"
   range.header.end.rexp <- "^_2THETA[^=]"

   # Read the input multirange file
   ufile <- file(uxdfile, "r")
   # Note that readLines apparently completely skips empty lines.
   # In that case line numbers do not match between source and f.
   f <- readLines(ufile, n=-1) #read _all_ lines from UXD file
   close(ufile)

   # Fetch a sampleid for the current job
   sampleid <- common::ProvideSampleId(uxdfile)
   substrateid <- common::ProvideSampleId(uxdfile, "dirname")

   # Look for header start rows
   range.header.start.rows <- which(regexpr(range.header.start.rexp, f) == 1)
   # Look for header end rows
   range.header.end.rows <- which(regexpr(range.header.end.rexp, f) == 1)

   # Calculate number of ranges
   ranges.total <- ifelse(
      length(range.header.start.rows) == length(range.header.end.rows),
      length(range.header.start.rows),
      NA) #why would they not be equal?
   if (is.na(ranges.total)) {
      # Obviously something bad happened.
      # Do something about it. echo an error message perhaps.
      # But why would they not be equal?
   }

   # Determine whether we have COUNTS or COUNTS PER SECOND in current UXD-file
   # Assuming it is the same for all ranges in this job (a safe assumption).
   if (f[range.header.end.rows][1] == "_2THETACOUNTS") {
      # we got counts
      counts.flag <- TRUE
      cps.flag <- FALSE
   }
   if (f[range.header.end.rows][1] == "_2THETACPS") {
      # we got counts per second
      counts.flag <-FALSE
      cps.flag <- TRUE
   }

   # Extract headers (as-is) and put them in a list (by range)
   headers.raw <- list()
   for (range in 1:ranges.total) {
      headers.raw[[range]] <- f[range.header.start.rows[range]:range.header.end.rows[range]]
   }

   # Data always start on the row after header end
   range.data.start.rows <- range.header.end.rows + 1
   # Data end rows precedes header with one row, except for the first range
   # But only if data contained more than one range, obviously. Let's make the code check for that
   if (ranges.total > 1) {
      range.data.end.rows <- c(range.header.start.rows[2:length(range.header.start.rows)] - 1, length(f))
   } else {
      # Data in fact only contains one range
      range.data.end.rows <- length(f)
   }

   # Extract data (as-is) and put it an list (by range)
   data.raw <- list()
   for (range in 1:ranges.total) {
      data.raw[[range]] <- f[range.data.start.rows[range]:range.data.end.rows[range]]
   }

   # Specify header parameters to include in dataframe
   header.param.rexp <- c(
      rotspeed = "^_ROTATION_SPEED=",
      steptime = "^_STEPTIME=",
      stepsize = "^_STEPSIZE=",
      theta = "^_THETA=",
      khi = "^_KHI=",
      phi = "^_PHI=",
      x = "^_X=",
      y = "^_Y=",
      z = "^_Z=",
      divergence = "^_DIVERGENCE=",
      antiscatter = "^_ANTISCATTER=")

   # Collect data and header parameters in dataframes, by range in a list
   data <- list()
   for (range in 1:ranges.total) {
      zz <- textConnection(data.raw[[range]], "r")
      data[[range]] <- structure(
         data.frame(
            sampleid,
            substrateid,
            common::int2padstr(range, "0", 3),
            matrix(scan(zz, what = numeric()), ncol = 2, byrow = T)),
         names = c(
            "sampleid",
            "substrateid",
            "range",
            "thth",
            ifelse(counts.flag, "counts", "cps")))
      close(zz)
      tmp.names <- names(data[[range]])
      # Collect header parameters
      for (param in 1:length(header.param.rexp)) {
         # if current param does not exist, save empty string
         if (length(which(regexpr(unname(header.param.rexp[param]), headers.raw[[range]]) == 1)) == 0) {
            data[[range]] <- cbind(data[[range]], "")
         } else {
            data[[range]] <- cbind(
               data[[range]],
               as.numeric(strsplit(
                  x = headers.raw[[range]][which(regexpr(unname(header.param.rexp[param]), headers.raw[[range]]) == 1)],
                  split = "=")[[1]][2]))
         }
      }
      names(data[[range]]) <- c(tmp.names, names(header.param.rexp))
   }

   # Calculate the other of the pair counts <-> cps
   if (counts.flag) {
      for (range in 1:ranges.total) {
         data[[range]] <- cbind(data[[range]], cps = data[[range]]$counts / data[[range]]$steptime)
      }
   }
   if (cps.flag) {
      for (range in 1:ranges.total) {
         data[[range]] <- cbind(data[[range]], counts = data[[range]]$cps * data[[range]]$steptime)
      }
   }

   # Return a unified dataframe
   data.df <- data[[1]]
   if (ranges.total > 1) {
      for (range in 2:ranges.total) {
         data.df <- rbind(data.df, data[[range]])
      }
   }

   return(data.df)
}
