#' Read data and metadata from XRDML files
#'
#' @param xrdml_source_path path to XRDML file
#'
#' @return list with three dataframes:
#'   xrd.data:
#'     $ angle
#'     $ counts
#'     $ att.factors
#'     $ secperstep
#'     $ cps
#'     $ cpspermA
#'     $ sampleid
#'     $ substrateid
#'     $ comment
#'     $ usedWavelength
#'     $ IB.radius
#'     $ IB.xraytube
#'     $ IB.xray.anode
#'     $ IB.xray.tension
#'     $ IB.xray.current
#'     $ IB.focus.type
#'     $ IB.focus.length
#'     $ IB.focus.width
#'     $ IB.takeoff.angle
#'     $ IB.xray.mirror
#'     $ IB.soller.slit
#'     $ IB.beam.attenuator
#'     $ sample.offset
#'     $ DB.radius
#'     $ DB.parallel.plate.collimator
#'     $ DB.detector
#'     $ scan.mode
#'     $ scan.axis
#'     $ start.time
#'     $ end.time
#'     $ application.software
#'     $ control.software
#'     $ intensities.unit
#'     $ common.counting.time
#'   axis.data
#'     $ sampleid
#'     $ axis
#'     $ unit
#'     $ startPosition
#'     $ endPosition
#'     $ commonPosition
#'   xrdml.params
#'     $ sampleid
#'     $ substrateid
#'     $ comment
#'     $ usedWavelength
#'     $ IB.radius
#'     $ IB.xraytube
#'     $ IB.xray.anode
#'     $ IB.xray.tension
#'     $ IB.xray.current
#'     $ IB.focus.type
#'     $ IB.focus.length
#'     $ IB.focus.width
#'     $ IB.takeoff.angle
#'     $ IB.xray.mirror
#'     $ IB.soller.slit
#'     $ IB.beam.attenuator
#'     $ sample.offset
#'     $ DB.radius
#'     $ DB.parallel.plate.collimator
#'     $ DB.detector
#'     $ scan.mode
#'     $ scan.axis
#'     $ start.time
#'     $ end.time
#'     $ application.software
#'     $ control.software
#'     $ intensities.unit
#'     $ common.counting.time
#'
#' @export
xrdml2df <- function(xrdml_source_path) {

   # lubridate parser and stamp functions now (since version 1.3.0) handle ISO8601 date format
   # (e.g., 2013-01-24 19:39:07.880-06:00, 2013-01-24 19:39:07.880Z)
   # # It appears the regular function ymd_hms() now handles also ISO8601 dates.
   # lubridate::ymd_hms_o() not yet in the stable branch
   # we use it to convert the date-time strings into R date-time objects
   # Note: XML file uses ISO 8601 formatted date-time strings.
   # see http://www.w3.org/TR/NOTE-datetime
   # The regular lubridate::ymd_hms() cannot parse 8601,
   # but ymd_hms_o() specifically handles ISO 8601.

   xrdml.tree <- XML::xmlInternalTreeParse(xrdml_source_path)
   xrdml <- XML::xmlRoot(xrdml.tree)

   sampleid <- gsub("\\.xrdml", "", basename(xrdml_source_path))

   xrdml.params <-
   data.frame(substrateid =
                 # if full sampleid given ("7B0407-001") strip the numbers to leave only substrateid...
                 sub(pattern = "-\\d{3}$", replacement = "", x = XML::xmlValue(xrdml["sample"]$sample)),
              sampleid = sampleid,
              comment =
                 XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["comment"]$comment),
              usedWavelength =
                 paste(as.character(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["usedWavelength"]$usedWavelength)), ": ",
                       XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["usedWavelength"]$
                          usedWavelength["kAlpha1"]$kAlpha1), " ",
                       as.character(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["usedWavelength"]$
                          usedWavelength["kAlpha1"]$kAlpha1)),
                       sep = ""),
              IB.radius =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["radius"]$radius),
                       as.character(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["radius"]$radius))),
              IB.xraytube =
                 paste(XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube, name = "name"),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube, name = "id")),
              IB.xray.anode =
                 XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["anodeMaterial"]$anodeMaterial),
              IB.xray.tension =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["tension"]$tension),
                       unname(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                              xrdMeasurement["incidentBeamPath"]$
                              incidentBeamPath["xRayTube"]$
                              xRayTube["tension"]$tension))),
              IB.xray.current =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["current"]$current),
                       unname(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                              xrdMeasurement["incidentBeamPath"]$
                              incidentBeamPath["xRayTube"]$
                              xRayTube["current"]$current))),
              IB.focus.type =
                 as.character(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                    xrdMeasurement["incidentBeamPath"]$
                    incidentBeamPath["xRayTube"]$xRayTube["focus"]$focus)),
              IB.focus.length =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["length"]$length),
                       as.character(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["length"]$length))),
              IB.focus.width =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["width"]$width),
                       as.character(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["width"]$width))),
              IB.takeoff.angle =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["takeOffAngle"]$takeOffAngle),
                       as.character(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["takeOffAngle"]$takeOffAngle))),
              IB.xray.mirror =
                 paste(XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror, name = "name"),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror["crystal"]$
                          crystal, name = "type"),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror["crystal"]$
                          crystal, name = "shape"),
                       XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror["crystal"]$
                          crystal),
                       XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["acceptanceAngle"]$acceptanceAngle),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["acceptanceAngle"]$acceptanceAngle,
                          name = "unit"),
                       XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["length"]$length),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["length"]$length,
                                  name = "unit")),
              IB.soller.slit =
                 XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                            incidentBeamPath["sollerSlit"]$sollerSlit, name = "name"),
              IB.beam.attenuator =
                 paste(XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["beamAttenuator"]$beamAttenuator, name = "name"),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["beamAttenuator"]$beamAttenuator, name = "id")),
              sample.offsets =
                 paste(XML::xmlSApply(xrdml["xrdMeasurement"]$
                          xrdMeasurement["sampleOffset"]$
                          sampleOffset, XML::xmlGetAttr, "axis"),
                       XML::xmlSApply(xrdml["xrdMeasurement"]$
                          xrdMeasurement["sampleOffset"]$
                          sampleOffset, XML::xmlValue, "axis"),
                       XML::xmlSApply(xrdml["xrdMeasurement"]$
                          xrdMeasurement["sampleOffset"]$
                          sampleOffset, XML::xmlGetAttr, "unit"),
                       collapse = ", "),
              DB.radius =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["radius"]$radius),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["radius"]$radius, name = "unit")),
              DB.parallel.plate.collimator =
                 XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["diffractedBeamPath"]$
                            diffractedBeamPath["parallelPlateCollimator"]$
                            parallelPlateCollimator, name = "name"),
              DB.detector =
                 paste(XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["detector"]$
                          detector, name = "name"),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["detector"]$
                          detector, name = "xsi:type")),
              scan.mode = XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                                    scan, name = "mode"),
              scan.axis = XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                                    scan, name = "scanAxis"),
              start.time =
                 lubridate::ymd_hms(XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                           scan["header"]$header["startTimeStamp"]$startTimeStamp)),
              end.time =
                 lubridate::ymd_hms(XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                           scan["header"]$header["endTimeStamp"]$endTimeStamp)),
              application.software =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["applicationSoftware"]$applicationSoftware),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["applicationSoftware"]$applicationSoftware,
                          name = "version")),
              control.software =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["instrumentControlSoftware"]$
                          instrumentControlSoftware),
                       XML::xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["instrumentControlSoftware"]$
                          instrumentControlSoftware, name = "version")),
              intensities.unit =
                 unname(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                                 xrdMeasurement["scan"]$scan["dataPoints"]$
                                 dataPoints["intensities"]$intensities)),
              common.counting.time =
                 paste(XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["dataPoints"]$dataPoints["commonCountingTime"]$
                          commonCountingTime),
                       unname(XML::xmlAttrs(xrdml["xrdMeasurement"]$
                              xrdMeasurement["scan"]$scan["dataPoints"]$
                              dataPoints["commonCountingTime"]$commonCountingTime))))

   ## Read settings for all axes (2theta, omega, X, Y, Z, etc.)
   # number of axes (use as counter)
   axes.number <- length(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$scan["dataPoints"]$dataPoints["positions"])
   # initialise
   axis.value <- list()
   axis.unit <- list()
   for (i in 1:axes.number) {
      # Fetch all values from the current axis (as named char array)
      # note the transpose operation
      axis.value[[i]] <- data.frame(t(
         XML::xmlSApply(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
         scan["dataPoints"]$dataPoints["positions"][i]$positions,
         XML::xmlValue, "axis")))
      # Fetch the axis and unit from the current axis (as named char array)
      axis.unit[[i]] <- data.frame(t(
         XML::xmlAttrs(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
         scan["dataPoints"]$dataPoints["positions"][i]$positions)))
   }

   # Try to condense axis.value into one dataframe
   # Ok, another approach. Concatetate all names of axis.value and axis.unit
   # (creates an empty dataframe)
   axis.all.names <- c(unique(names(unlist(axis.unit))), unique(names(unlist(axis.value))))
   axis.data <- data.frame(matrix(data = "", nrow = length(axis.unit), ncol = length(axis.all.names)))
   names(axis.data) <- axis.all.names

   # Loop over axis.value and axis.unit and move their contents into axis.data
   for (row in 1:length(axis.unit)) {
      # now loop over each column
      for (k in 1:dim(axis.unit[[row]])[2]) {
         axis.data[row, which(names(axis.data) == names(axis.unit[[row]])[k])] <-
            axis.unit[[row]][, k]
      }
      for (k in 1:dim(axis.value[[row]])[2]) {
         axis.data[row, which(names(axis.data) == names(axis.value[[row]])[k])] <-
            axis.value[[row]][, k]
      }
   }

   # Add sampleid column to axis.data
   axis.data$sampleid <- sampleid

   # Read the intensities data (one long text string, use textConnection)
   zz <- textConnection(
      XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$scan["dataPoints"]$dataPoints["intensities"]$intensities), "r")
   intensities <- scan(zz, what = numeric())
   close(zz)

   # Read the beam attenuation factors (one long text string, use textConnection)
   zz <- textConnection(
      XML::xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$scan["dataPoints"]$dataPoints["beamAttenuationFactors"]$beamAttenuationFactors), "r")
   beam.attenuation.factors <- scan(zz, what = numeric())
   close(zz)

   ## Use the identified scanAxis (in xrdml.params) to identify the current start and stop values
   # scan.axis can have a few different values:
   # 2Theta-Omega, 2Theta, and Omega2-Theta are all scans along 2T
   # Note that a correct GI is achieved by selecting scanAxis:2Theta and setting other gonio angle to "Omega"
   start.currentaxis <- ifelse(
      xrdml.params$scan.axis %in% c("2Theta-Omega", "2Theta", "Omega2-Theta"),
      as.numeric(axis.data[which(axis.data$axis == "2Theta"), "startPosition"]),
      as.numeric(axis.data[which(axis.data$axis == xrdml.params$scan.axis), "startPosition"]))
   #
   end.currentaxis <- ifelse(
      xrdml.params$scan.axis %in% c("2Theta-Omega", "2Theta", "Omega2-Theta"),
      as.numeric(axis.data[which(axis.data$axis == "2Theta"), "endPosition"]),
      as.numeric(axis.data[which(axis.data$axis == xrdml.params$scan.axis), "endPosition"]))

   exp.data <- data.frame(
      angle = seq(start.currentaxis, end.currentaxis, length.out = length(intensities)),
      counts = intensities,
      att.factors = beam.attenuation.factors)

   # attenuation factors not used for anything, currently
   # exp.data$counts <- exp.data$intensities * exp.data$att.factors
   exp.data$secperstep <- as.numeric(unlist(strsplit(xrdml.params$common.counting.time, " "))[1])
   exp.data$cps <- exp.data$counts / as.numeric(unlist(strsplit(xrdml.params$common.counting.time, " "))[1])
   exp.data$cpspermA <- exp.data$cps / as.numeric(unlist(strsplit(xrdml.params$IB.xray.current, " "))[1])

   # add axis data to xrdml.params dataframe
   for (i in 1:dim(axis.data)[1]) {
      if (axis.data$commonPosition[i] == "") {
         # save both start and stop of scan
         xrdml.params[, paste0(axis.data$axis[i], ".start")] <- as.numeric(axis.data$startPosition[i])
         xrdml.params[, paste0(axis.data$axis[i], ".end")] <- as.numeric(axis.data$endPosition[i])
      } else {
         # constant value
         xrdml.params[, axis.data$axis[i]] <- as.numeric(axis.data$commonPosition[i])
      }
   }

   xrd.data <- cbind(exp.data, xrdml.params)

   # Return xrd.data, axis.data and xrdml.params in a list
   xrd.list <- list(
      xrd.data = xrd.data,
      axis.data = axis.data,
      xrdml.params = xrdml.params)

   return(xrd.list)
}
