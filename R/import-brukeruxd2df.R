#' Import data and metadata from UXD files
#'
#' Read UXD files from Bruker D8 with one or multiple ranges (converted from
#' Bruker's native  format using their FileExchange tool).
#' This function was tested for UXD v3.
#' It automatically calculates CPS (counts per second) if COUNTS are present,
#' and vice versa (note that this depends on specific strings in the UXD format).
#' Old UXD-files (probably from the Siemens diffractometers) contain the following line:
#' "; (Data for Range number 1)".
#' Converted UXD files (from the Bruker D8) drop the parentheses and "number":
#' "; Data for range 1"
#' This function was tested for the latter case.
#'
#' @param uxdfile path to UXD file
#'
#' @return dataframe with the following columns
#'   $ sampleid          : chr
#'   $ substrateid       : chr
#'   $ range             : chr
#'   $ counts (or cps)   : num
#'   $ steptime          : num
#'   $ stepsize          : chr
#'   $ stepmode          : chr
#'   $ start2th          : chr
#'   $ rotationspeed     : chr
#'   $ voltage           : chr
#'   $ current           : chr
#'   $ angle             : num
#'   $ cps (or counts)   : num
#'   $ goniometer.radius : num
#'   $ fixed.divslit     : num
#'   $ fixed.antislit    : num
#'   $ datemeasured      : num
#'   $ anode             : num
#'
#' @export
brukeruxd2df <- function(uxdfile) {

   range.header.start.rexp <- "^; Data for range"
   range.header.end.rexp <- "^; Cnt1"

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
      NA) # why would they not be equal?
   if (is.na(ranges.total)) {
      # Obviously something bad happened.
      # Do something about it. echo an error message perhaps.
      # But why would they not be equal?
   }

   counts.flag <- rep(FALSE, ranges.total)
   cps.flag <- rep(FALSE, ranges.total)
   headers.raw <- list()
   # Determine whether we have COUNTS or COUNTS PER SECOND for each range
   for (i in 1:length(ranges.total)) {
      current.range.header.text <- f[range.header.start.rows[i]:range.header.end.rows[i]]
      # Save each header as a nw object in the headers.raw list
      headers.raw[[i]] <- current.range.header.text
      # If we find the line "_COUNTS = 1", that means the values are counts
      if(length(which(regexpr("^_COUNTS = 1", current.range.header.text) == 1)) != 0) {
         # counts
         counts.flag[i] <- TRUE
         cps.flag[i] <- FALSE
      } else {
         # CPS
         cps.flag[i] <- TRUE
         counts.flag[i] <- FALSE
      }
   }

   # Data always start on the row after header end
   range.data.start.rows <- range.header.end.rows + 1
   # Data end rows precedes header with one row, except for the first range
   # But only if data contained more than one range, obviously. Let's make the code check for that
   if (ranges.total > 1) {
      range.data.end.rows <-
         c(range.header.start.rows[2:length(range.header.start.rows)] - 1, length(f))
   } else {
      # Data in fact only contains one range
      range.data.end.rows <- length(f)
   }

   # Extract data (as-is) and put it an list (by range)
   data.raw <- list()
   for (range in 1:ranges.total) {
      data.raw[[range]] <- f[range.data.start.rows[range]:range.data.end.rows[range]]
   }

   # Specify header parameters in ranges to include in dataframe
   range.header.param.rexp <- c(
      steptime = "^_STEPTIME = ",
      stepsize = "^_STEPSIZE = ",
      stepmode = "^_STEPMODE = ",
      start2th = "^_2THETA = ",
      rotationspeed  = "^_ROTATION_SPEED = ",
      voltage = "_KV = ",
      current = "_MA = ")
      # possible extras: _datemeasured, _anode, _v4_inf_sampleid, _FIXED_DIVSLIT, _FIXED_ANTISLIT

   # Collect data and header parameters in dataframes, by range in a list
   data <- list()
   for (range in 1:ranges.total) {
      zz <- textConnection(data.raw[[range]], "r")
      data[[range]] <- data.frame(
         sampleid,
         substrateid,
         common::int2padstr(range, "0", 3),
         matrix(scan(zz, what = numeric()), ncol = 1, byrow = T))
      close(zz)
      # Collect range header parameters
      for (param in 1:length(range.header.param.rexp)) {
         data[[range]] <- cbind(
            data[[range]],
            strsplit(headers.raw[[range]][which(regexpr(unname(range.header.param.rexp[param]),
               headers.raw[[range]]) == 1)], " = ")[[1]][2])
      }
      names(data[[range]]) <- c(
         "sampleid",
         "substrateid",
         "range",
         ifelse(counts.flag, "counts", "cps"),
         names(range.header.param.rexp))
      # create thth values
      data[[range]]$angle <- seq(
         from = as.numeric(unique(data[[range]]$start2th)),
         by = as.numeric(unique(data[[range]]$stepsize)),
         length.out = dim(data[[range]])[1])
      # Recalculate steptime to seconds (divide by 192)
      data[[range]]$steptime <- as.numeric(data[[range]]$steptime) / 192
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

   # Build a unified dataframe
   data.df <- data[[1]]
   if (ranges.total > 1) {
      for (range in 2:ranges.total) {
         data.df <- rbind(data.df, data[[range]])
      }
   }
   # save the column names of data.df to temp vector
   names.df <- names(data.df)

   # Specify header parameters in ranges to include in dataframe
   header.param.rexp <- c(
      goniometer.radius = "^_GONIOMETER_RADIUS",
      fixed.divslit = "^_FIXED_DIVSLIT",
      fixed.antislit = "^_FIXED_ANTISLIT",
      datemeasured = "^_DATEMEASURED",
      anode = "^_ANODE")

   main.header.raw <- f[1:range.header.start.rows[1]]

   # Collect header parameters
   for (param in 1:length(header.param.rexp)) {
      data.df <- cbind(
         data.df,
         strsplit(main.header.raw[which(regexpr(unname(header.param.rexp[param]),
               main.header.raw) == 1)], " = ")[[1]][2])
   }
   # update names of cols of data.df
   names(data.df) <- c(names.df, names(header.param.rexp))

   return(data.df)
}
