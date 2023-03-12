
#' Import data and metadata from Bruker BRML files
#'
#' The BRML archive is readable with standard zip/unzip tools.
#' I have found that the BRML archive contains the following files/folders:
#' + Experiment0/
#' + Experiment0/ActiveContentContainer.xml
#' + Experiment0/DataContainer.xml
#' + Experiment0/EvaluationContainer.xml
#' + Experiment0/InstructionContainer.xml
#' + Experiment0/MeasurementContainer.xml
#' + Experiment0/TemplateContainer.xml
#' + experimentCollection.xml
#' and depending on the number of measurements, the folder Experiment0 will
#' contain the same number of RawData#.xml files, numbered starting from zero
#' + Experiment0/RawData0.xml
#' + Experiment0/RawData1.xml
#' + ...
#'
#' @param brml.file complete path with filename to BRML file
#'
#' @return dataframe
#' @export
brml2df <- function(brml.file) {
   options(stringsAsFactors = FALSE)

   parameter.filelist <- c(
      #CreatingVersion
      "experimentCollection.xml",
      #MachineName
      "Experiment0/ActiveContentContainer.xml",
      #SamplePosition="1A01"
      "Experiment0/DataContainer.xml",
      #RotationSpeed
      "Experiment0/MeasurementContainer.xml")

   # to know how many RawData files the BRML archive contains, we need to read it
   brml.filelist <- utils::unzip(zipfile = brml.file, list = TRUE)
   rawdata.filelist <- sort(
      brml.filelist$Name[grep(pattern = "RawData", x = brml.filelist$Name)])
   brml.index <- data.frame(
      path = c(parameter.filelist, rawdata.filelist),
      data = c(rep(FALSE, length(parameter.filelist)), rep(TRUE, length(rawdata.filelist))))
   brml.index$name <- basename(brml.index$path)
   brml.index$label <- gsub(
      pattern = "\\.xml$",
      replacement = "",
      x = brml.index$name)

   # Read the contents of the XML files in brml.index
   brml.list <- list()
   brml.tree <- list()
   brml <- list()
   for (i in 1:dim(brml.index)[1]) {
      con <- unz(description = brml.file, filename = brml.index$path[i])
      # use scan() instead of readLines(), the latter misses the last line of each XML file...
      brml.list[[i]] <- scan(con, what = "character")
      close(con)
      brml.tree[[brml.index$label[i]]] <- XML::xmlInternalTreeParse(brml.list[[i]])
      brml[[brml.index$label[i]]] <- XML::xmlRoot(brml.tree[[i]])
   }

   #####################################################################
   #### Check <CreatingVersion> to deduce structure of data in xml files
   # It appears that Bruker has made significant changes under the hood sometime between
   # Oct and Dec 2013. In particular, the actual data has been moved under a new
   # subheader "DataRoutes".
   # To work around this, we set a flag
   creatingversion <- XML::xmlValue(brml[["experimentCollection"]][["CreatingVersion"]])
   if (length(grep(pattern="^3", x=creatingversion)) > 0) {
      # if major version == 3
      dataroutes <- TRUE
   } else {
      dataroutes <- FALSE
   }

   xrd.data <- NULL
   # extract the experimental data from the rawdata XML file
   for (i in 1:length(which(brml.index$data))) {
      label.ii <- brml.index$label[which(brml.index$data)][i]

      if (dataroutes) {
         # version 3
         Datum <- unname(
            XML::xmlSApply(brml[[label.ii]][["DataRoutes"]][["DataRoute"]], XML::xmlValue)[which(XML::xmlSApply(brml[[label.ii]][["DataRoutes"]][["DataRoute"]], XML::xmlName) == "Datum")])
      } else {
         # version 2
         Datum <- unname(
            XML::xmlSApply(brml[[label.ii]], XML::xmlValue)[which(XML::xmlSApply(brml[[label.ii]], XML::xmlName) == "Datum")])
      }

      zz <- textConnection(paste(Datum, collapse = ","), "r")
      xrd.ii <- structure(
         data.frame(
            sampleid = common::ProvideSampleId(brml.file, "filename"),
            substrateid = common::ProvideSampleId(brml.file,"dirname"),
            uid = paste(
               common::ProvideSampleId(brml.file, "filename"),
               common::int2padstr(ii=i, pchr="0", w=3),
               sep = "-"),
            range = common::int2padstr(ii=i, pchr="0", w=3),
            matrix(
               data = scan(file = zz, sep = ",", what = numeric()),
               ncol = 5,
               byrow = TRUE),
            MeasurementPoints = ifelse(
               dataroutes,
               as.numeric(XML::xmlValue(brml[[label.ii]][["DataRoutes"]][["DataRoute"]][["ScanInformation"]][["MeasurementPoints"]])),
               as.numeric(XML::xmlValue(brml[[label.ii]][["ScanInformation"]][["MeasurementPoints"]]))),
            TimePerStep = ifelse(
               dataroutes,
               as.numeric(XML::xmlValue(brml[[label.ii]][["DataRoutes"]][["DataRoute"]][["ScanInformation"]][["TimePerStep"]])),
               as.numeric(XML::xmlValue(brml[[label.ii]][["ScanInformation"]][["TimePerStep"]]))),
            ThThStart = ifelse(
                  dataroutes,
                  as.numeric(XML::xmlValue(brml[[label.ii]][["DataRoutes"]][["DataRoute"]][["ScanInformation"]][["ScanAxes"]][[1]][["Start"]])),
                  as.numeric(XML::xmlValue(brml[[label.ii]][["ScanInformation"]][["ScanAxes"]][[1]][["Start"]]))),
            ThThStop = ifelse(
               dataroutes,
               as.numeric(XML::xmlValue(brml[[label.ii]][["DataRoutes"]][["DataRoute"]][["ScanInformation"]][["ScanAxes"]][[1]][["Stop"]])),
               as.numeric(XML::xmlValue(brml[[label.ii]][["ScanInformation"]][["ScanAxes"]][[1]][["Stop"]]))),
            ThThIncrement = ifelse(
               dataroutes,
               as.numeric(XML::xmlValue(brml[[label.ii]][["DataRoutes"]][["DataRoute"]][["ScanInformation"]][["ScanAxes"]][[1]][["Increment"]])),
               as.numeric(XML::xmlValue(brml[[label.ii]][["ScanInformation"]][["ScanAxes"]][[1]][["Increment"]]))),
            TimeStampStarted = XML::xmlValue(brml[[label.ii]][["TimeStampStarted"]]),
            TimeStampFinished = XML::xmlValue(brml[[label.ii]][["TimeStampFinished"]])),
         names = c(
            "sampleid",
            "substrateid",
            "uid",
            "range",
            "TimePerStepEffective",
            "IC", # unknown quantity
            "ThTh", # 2th
            "Th", # th
            "Counts",
            "MeasurementPoints",
            "TimePerStep",
            "ThThStart",
            "ThThStop",
            "ThThIncrement",
            "TimeStampStarted",
            "TimeStampFinished"))
      close(zz)
      xrd.data <- rbind(xrd.data, xrd.ii)
   }

   # Fetch the parameters that do not depend on the actual measurement...
   # MachineName
   xrd.data$MachineName <- ifelse(
      dataroutes,
      "",
      unname(XML::xmlAttrs(brml[["ActiveContentContainer"]][["Identifier"]])["MachineName"]))

   ### THE FOLLOWING PARAMETERS REALLY SHOULD DEPEND ON ACTUAL MEASUREMENT!
   ### But they are not written to the RawData# file, but a joint file. How does Bruker's software keep track??
   ### FRAGILE IMPLEMENTATION!
   # SamplePosition
   xrd.data$SamplePosition <- ifelse(
      dataroutes,
      "",
      unname(XML::xmlAttrs(brml[["DataContainer"]][["MeasurementInfo"]])["SamplePosition"]))
   # BsmlFileName
   xrd.data$BsmlFileName <- ifelse(
      dataroutes,
      "",
      unname(XML::xmlAttrs(brml[["DataContainer"]][["MeasurementInfo"]])["BsmlFileName"]))
   # RotationSpeed # per minute
   xrd.data$RotationSpeed <- ifelse(
      dataroutes,
      "",
      unname(XML::xmlAttrs(brml[["MeasurementContainer"]][["BaseMethods"]][["Method"]][["LogicData"]][["DataEntityContainer"]][["RotationSpeed"]])["Value"]))

   # ThTh values are not evenly spaced (which breaks diffractometry::baselinefit),
   # so we fix it by using linear interpolation
   xrd.data$ThTh.interp <- stats::approx(
      x = xrd.data$ThTh,
      y = xrd.data$Counts,
      method = "linear",
      n = length(xrd.data$ThTh))$x
   xrd.data$Counts.interp <- stats::approx(
      x = xrd.data$ThTh,
      y = xrd.data$Counts,
      method = "linear",
      n = length(xrd.data$ThTh))$y

   return(xrd.data)
}
