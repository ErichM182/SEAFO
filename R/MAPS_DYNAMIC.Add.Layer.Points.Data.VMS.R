#? ### ### ### ### ### ### ###
#' @title Compile the SEAFO Dynamic BaseMap (Leaflet)
#' @name maps_dynamic.add.layer.points.data.vms
#' @family MAPS Functions (SEAFO)
#'
#'
#' @description
#' A Helper Function that compiles an interactive `leaflet` BaseMap of the South East Atlantic
#' Fisheries Organisation (SEAFO) Convention Area (CA) and its surrounding geographic context.
#' The BaseMap combines a selected ESRI tile provider with the SEAFO spatial layers that ship
#' with this package (i.e. the SEAFO Convention Area, the SEAFO Exclusive Economic Zones and the
#' SEAFO VME Closures) and equips the resulting map widget with a measuring tool, a map scale bar
#' and a non-collapsed Layer Control box. This function was created to support the routine
#' data-analysis work conducted by the Scientific Committee (SC) of the SEAFO, but may also be
#' used as a quick "situational awareness" BaseMap in other (3rd Party) R libraries and custom R
#' functions - as long as the `SEAFO` library is linked (i.e. previously installed and/or
#' specified as a library dependency).
#'
#'
#' @param snMapSRC ([numeric]) a Numeric (i.e. [numeric]) value that specifies the
#'                BaseMap tile source <selector>. A value of `1` selects the "ESRI Ocean
#'                BaseMap" provider; any other value (i.e. the default `2`) selects the "ESRI
#'                NatGeo World Map" provider.
#' @param ssHeaderWCL ([character]) a String or Text (i.e. [character]) value that
#'                    specifies the caption (i.e. header title) prepended to the Layer Control
#'                    box (WCL) of the compiled BaseMap (default: "SEAFO Convention Area (CA)").
#'                    The caption is applied with an `htmlwidgets::onRender()` JavaScript
#'                    callback, so it is only visible in the rendered map.
#' @param snZoomMin ([numeric]) a Numeric (i.e. [numeric]) value that specifies the
#'                minimum zoom level allowed for the BaseMap tiles (default: `2`).
#' @param snZoomMax ([numeric]) a Numeric (i.e. [numeric]) value that specifies the
#'                maximum zoom level allowed for the BaseMap tiles (default: `10`).
#' @param sbShowEEZs ([logical]) a Boolean value that specifies whether the SEAFO Exclusive
#'                  Economic Zones (EEZs) layer must be added to the BaseMap (default: `TRUE`).
#'                  The layer is sourced from `./data/dfSEAFO_EEZv11.rda` (v11, 2019).
#' @param sbShowCA ([logical]) a Boolean value that specifies whether the SEAFO Convention Area
#'                (CA) layer must be added to the BaseMap (default: `TRUE`). The layer is
#'                sourced from `./data/dfSEAFO_CA.rda` and is a pre-requisite for both the SEAFO
#'                area labels (`sbShowLabels`) and the SEAFO Closed Areas (`sbShowClosedAreas`).
#' @param sbShowClosedAreas ([logical]) a Boolean value that specifies whether the SEAFO VME
#'                          Closures (VMECs) layer must be added to the BaseMap (default:
#'                          `TRUE`). The layer is sourced from
#'                          `./data/dfSEAFO_ClosedAreas.rda` and is only compiled when
#'                          `sbShowCA = TRUE`.
#' @param sbShowFishFootPrint ([logical]) a Boolean value that is reserved for the forthcoming
#'                            fishing-footprint (VMS) overlay. The argument is currently
#'                            internalized only and does NOT yet add a layer to the BaseMap
#'                            (default: `TRUE`).
#' @param sbShowLabels ([logical]) a Boolean value that specifies whether the SEAFO area labels
#'                    (i.e. "A", "A1", "B", "B1", "C", "C1", "D" and "D1") must be added to the
#'                    BaseMap (default: `TRUE`). The labels are only compiled when
#'                    `sbShowCA = TRUE`, and are always-on (i.e. `noHide = TRUE`, text only).
#' @param sbHideWCL ([logical]) a Boolean value that specifies whether the Layer Control box
#'                 (WCL) must be compiled for the BaseMap (default: `TRUE`). NB: when set to
#'                 `FALSE` the Layer Control box is omitted altogether, so the overlay layers
#'                 can no longer be toggled by the user.
#' @param sbNotifyRunTimeF ([logical]) a Boolean value that specifies whether the function must
#'                         notify the user of its run-time START and STOP with an audible cue
#'                         (i.e. `beepr::beep()`; default: `FALSE`).
#' @param ... ([list]) a List of R Objects function argument to catch all "Fall-Through" or
#'            "DotsArgs" values used by the nested functions within this main function.
#'            Applicable DotsArgs are collated in the [SEAFO::RENV_FSID] immutable list (i.e.
#'            all `F_ARGS_*` list items) and here pertain to the following nested R functions:
#'            <br>
#'               1. [SEAFO::info.post.func.self.id()] (i.e. `sbPrintPretty`, `sbRunSelfID` and
#'                  `ssFuncCallerID`), and <br>
#'               2. [SEAFO::info.post.note()] .
#'
#'
#' @returns
#' A `leaflet` `htmlwidget` object - i.e. the fully compiled and interactive SEAFO BaseMap. The
#' widget is handed back to the calling R function (or to the console), where it is auto-printed
#' when not assigned, so it may either be inspected on the spot or stored in an R object for
#' later use (e.g. `coMap <- maps_dynamic.compile.basemap.seafo()`).
#'
#'
#' @details
#' ### BaseMap source
#' The tile provider is selected with `snMapSRC`: a value of `1` maps to the "ESRI Ocean
#' BaseMap" provider, and any other value (i.e. the default `2`) maps to the "ESRI NatGeo World
#' Map" provider. Both provider definitions are pulled from the `leaflet.providers` package, and
#' the selected provider name is written to the console via [SEAFO::info.post.note()] as the
#' BaseMap is compiled.
#'
#' ### Spatial layers
#' The SEAFO spatial layers are read at run-time (i.e. with `base::load()`) from the package's
#' `./data/` folder, which is resolved with `here::here()`. The function therefore expects to be
#' run from the R Project (i.e. package source) root - i.e. the directory that holds the `data/`
#' folder. The layers, in the order they are added, are:
#'
#' * **EEZs** - the SEAFO Exclusive Economic Zones (`dfSEAFO_EEZv11.rda`), added when
#'   `sbShowEEZs = TRUE`;
#' * **SEAFO CA** - the SEAFO Convention Area (`dfSEAFO_CA.rda`), added when
#'   `sbShowCA = TRUE`;
#' * **SEAFO Labels** - the eight SEAFO area tags (`A`, `A1`, `B`, `B1`, `C`, `C1`, `D` and
#'   `D1`), added when both `sbShowCA = TRUE` and `sbShowLabels = TRUE`;
#' * **SEAFO Closed Areas** - the SEAFO VME Closures (`dfSEAFO_ClosedAreas.rda`), added when
#'   both `sbShowCA = TRUE` and `sbShowClosedAreas = TRUE`.
#'
#' ### Map controls
#' The BaseMap is compiled with a measuring tool (top-left), a map scale bar (bottom-left) and,
#' when `sbHideWCL = TRUE`, a non-collapsed Layer Control box (top-left) whose caption is
#' supplied via `ssHeaderWCL`. The measuring tool is localised with `localization = "en"`,
#' because the underlying `leaflet-measure` plugin falls back to English SILENTLY for an
#' unrecognised locale key (i.e. only valid keys such as `"fr"`, `"pt_PT"` or `"es"` are safe).
#' The initial map view is fitted to the SEAFO Convention Area bounding box (i.e. longitudes
#' `-25.25` to `31.25` and latitudes `2.75` to `-52.05`).
#'
#' ### Self-identification
#' The function also brackets its own execution with "Function Self-ID" console notifications
#' (i.e. via [SEAFO::info.post.func.self.id()]), reported against its START and STOP Code Editor
#' Line Numbers (CELN). These notifications are only printed when `sbRunSelfID = TRUE` is passed
#' through the `...` argument.
#'
#'
#' @examples
#' \dontrun{
#' ### Compile the DEFAULT SEAFO BaseMap (ESRI NatGeo World Map tiles) ...
#' library(SEAFO)   # <- Ensures the `SEAFO` library is <already> installed locally.
#'
#' coMap <- SEAFO::maps_dynamic.compile.basemap.seafo()
#' coMap            # <- Renders the interactive BaseMap in the RStudio Viewer pane !!!
#'
#'
#' ### Compile the ESRI Ocean BaseMap without the EEZ & Closed Areas layers ...
#' SEAFO::maps_dynamic.compile.basemap.seafo(
#'   snMapSRC          = 1,               # <- Select the ESRI Ocean BaseMap tiles ...
#'   sbShowEEZs        = FALSE,           # <- Skip the SEAFO EEZ layer ...
#'   sbShowClosedAreas = FALSE,           # <- Skip the SEAFO VME Closures layer ...
#'   ssHeaderWCL       = "SEAFO Layers",  # <- Custom Layer Control box caption ...
#'   sbNotifyRunTimeF  = TRUE             # <- Audible START & STOP notification ...
#' )
#'
#'
#' ### Compile a "clean" CA BaseMap (no overlays, no labels) with Self-ID notes ...
#' SEAFO::maps_dynamic.compile.basemap.seafo(
#'   sbShowEEZs     = FALSE,
#'   sbShowLabels   = FALSE,
#'   snZoomMin      = 3,
#'   snZoomMax      = 8,
#'   sbRunSelfID    = TRUE,                # <- <DotsArg> prints the Self-ID notifications ...
#'   ssFuncCallerID = "my.custom.r.func"   # <- <DotsArg> tags the calling function ...
#' )
#' }
#'
#'
#'
#' @export
#? ### ### ###
"maps_dynamic.add.layer.points.data.vms" <- function(
  lmMapObj=NULL, dfVMS=NULL, snSpeedSplit=1.15, ssVarShipCourse="CO",
  ssVarYear="VMS_Year", ssLabelVarYear="VMS Years", sbShowToggleYear=TRUE,
  ssVarMonth="VMS_Month", ssLabelVarMonth="VMS Month", sbShowToggleMonth=FALSE,
  ssVarShipIDs="Ship_Name_Anon", ssLabelVarShipIDs="Vessel ID", vsShipColors=NULL,
  ssVarShipSpeed="Speed_Corr_kt", ssLabelVarSpeed="Vessel State (speed in knots)",
  ssVarShipFlag="Flag_Anon", ssLabelVarShipFlag="Vessel Flag", sbShowShipFlag=FALSE,
  ssVarLATs="LAT_DeciDegree", ssVarLONs="LON_DeciDegree", sbPlotOnlyFishingVMS=FALSE,
  ssVarTime="VMS_Time", ssLabelVarTime="VMS Time", ssVarDate="VMS_Date", ssLabelVarDate="VMS Date"
) {

  # ========================================================================================================= #
  #   FUNCTION ARGUMENTS:                                                                                  ####
  #    - lmMapObj      : [leaflet] a Leaflet map object to which VMS data will be added as the top layer.      #
  #    - dfVMS_        : [data.frame] VMS data with coordinate columns: Lat_DD, Lon_DD.                        #
  #    - snSpeedSplit : [numeric] speed threshold (knots). Points >= cutoff are drawn as heading arrows;      #
  #                                points < cutoff are drawn as dots. Default 3.0.                                      #
  #    - sbPlotOnlyFishingVMS  : [logical] TRUE = plot only stationary records (speed < cutoff). FALSE = plot all records. Default TRUE.
  #                                Default TRUE.                                                                        #
  #    - ssVarYear     : [character] column used for the VMS Years control. Default 'Year_Parsed'.             #
  #    - ssVarShipSpeed   : [character] column used for Vessel State filtering. Default 'Speed_Num'.              #
  #    - ssVarShipIDs     : [character] column used for the Vessel ID control. Default 'Vessel_Name'.             #
  #    - ssLabelVarYear   : [character] header label for the VMS Years control. Default 'VMS Years'.              #
  #    - ssLabelVarSpeed  : [character] header label for the Vessel State control. Default 'Vessel State'.        #
  #    - ssLabelVarShipIDs: [character] header label for the Vessel ID control. Default 'Vessel ID'.              #
  #                                                                                                           #
  #   CONTROLS ADDED (native Leaflet look, does NOT overwrite existing layers control):                     ####
  #    - VMS Years (left side)  : check box toggles to show/hide individual vsLevelsYears_ (SHAPE via radius).          #
  #    - Vessel State (top right): check box toggles to show/hide steaming and stationary records.             #
  #    - Vessel ID (top right)  : check box toggles to show/hide individual vsLevelsShipIDs_ (COLOR).                   #
  #                                                                                                           #
  #   RENDERING: Uses a single <canvas> overlay (not individual Leaflet layers) for high performance.       ####
  # ========================================================================================================= #

  ####--- STEP 1 - Run Initialization Processes ---####
  ####--- 1.1 - Define the Identifier TAGs for Custom Function ---####
  TAG_FUNC_ID <- 'VMS Plot';
  TAG_FUNC_ID_FULL <- 'Plotting VMS Points on Leaflet Map';

  ####--- 1.2 - Capture Function START DateTime ---####
  startDateTime <- base::Sys.time();
  formatDateTime <- '%a, %d %b %Y @ %X';

  base::print(
    base::paste0(
      '  -->   CUSTOM FUNCTION [ ', TAG_FUNC_ID_FULL, ' ] '
      , 'STARTED on ', base::format(startDateTime, formatDateTime),'   <--  '
    )
  )

  # Turn off 3rd Party Warnings while this function executes !!
  base::suppressWarnings({

    ####--- Validate inputs ---####
    dfVMS_ <- NULL;
    if (base::is.null(lmMapObj)) {
      base::stop(base::paste0(' --> ', TAG_FUNC_ID, ' 53 | lmMapObj may not be NULL !!!'));
    }
    if (base::is.null(dfVMS)) {
      dfVMS_ <- utils::read.csv(
        file = "./data/dfSEAFO_GPS_VMS.csv"
      );
    } else {
      dfVMS_ <- dfVMS;
    }

    rcf_resolve.var.ids <- function(
      ssArg, fallbackNames, required = TRUE, label = 'control'
    ) {
      colNames <- base::names(dfVMS_)
      pick_col <- function(x) {
        if (base::is.null(x) || base::length(x) != 1 ||
            base::is.na(x) || !base::nzchar(base::as.character(x))) {
          return(NULL)
        }
        xChar <- base::as.character(x)
        if (base::grepl('^[0-9]+$', xChar)) {
          idx <- base::as.integer(xChar)
          if (idx >= 1 && idx <= base::ncol(dfVMS_)) return(base::names(dfVMS_)[idx])
        }
        exact <- base::which(colNames == xChar)
        if (base::length(exact) > 0) return(colNames[exact[1]])
        fuzzy <- base::which(base::tolower(colNames) == base::tolower(xChar))
        if (base::length(fuzzy) > 0) return(colNames[fuzzy[1]])
        NULL
      }
      found <- pick_col(ssArg)
      if (!base::is.null(found)) return(found)
      for (fb in fallbackNames) {
        found <- pick_col(fb)
        if (!base::is.null(found)) return(found)
      }
      if (required) {
        base::stop(base::paste0(
          ' --> ', TAG_FUNC_ID, ' 84 | Missing required column for ', label,
          '. Tried: ', base::paste(base::c(base::as.character(ssArg), fallbackNames), collapse = ', ')
        ))
      }
      NULL
    }

    ssVarLATs_ <- rcf_resolve.var.ids(
      ssVarLATs, base::c('Lat_DD', 'LATs', 'LAT', 'LATS', 'Latitudes'), TRUE, 'VMS Latitudes'
    );
    ssVarLONs_ <- rcf_resolve.var.ids(
      ssVarLONs, base::c('Lon_DD', 'LONs', 'LON', 'LONS', 'Longitudes'), TRUE, 'VMS Longitudes'
    );
    ssVarDate_ <- rcf_resolve.var.ids(
      ssVarDate, base::c('Date_Parsed', 'Date', 'DATE', 'DT'), TRUE, 'VMS Date'
    );
    ssVarTime_ <- rcf_resolve.var.ids(
      ssVarTime, base::c('Time_Parsed', 'Time', 'TIME', 'TM'), TRUE, 'VMS Time'
    );
    ssVarYear_ <- rcf_resolve.var.ids(
      ssVarYear, base::c('Year_Parsed', 'Year', 'YEAR'), TRUE, 'VMS Years'
    );
    ssVarShipIDs_ <- rcf_resolve.var.ids(
      ssVarShipIDs, base::c('Vessel_Name', 'Name', 'Registry_number_IR'), TRUE, 'Vessel ID'
    );
    ssVarShipSpeed_ <- rcf_resolve.var.ids(
      ssVarShipSpeed, base::c('Speed_Num', 'Speed_kt', 'Speed (kt)', 'Speed'), FALSE, 'Vessel State'
    );
    ssVarShipCourse_ <- rcf_resolve.var.ids(
      ssVarShipCourse, base::c('Ships_CRS', 'Ship_Crs', 'Course (degrees)'), FALSE, 'Ship Course'
    );

    vsReqCols_ <- base::c(ssVarLATs_, ssVarLONs_);
    vsMissCols_ <- base::setdiff(vsReqCols_, base::names(dfVMS_));   # <- Identify mising Vars !!!
    if (base::length(vsMissCols_) > 0) {
      base::stop(base::paste0(
        ' --> rcf_maps.points.plot.gps.data.vms | Missing required columns: ',
        base::paste(vsMissCols_, collapse = ', ')
      ))
    }

    # Tooltip columns (optional; fall back gracefully if missing)
    sbHasDate_   <- ssVarDate_ %in% base::names(dfVMS_);
    sbHasTime_   <- ssVarTime_ %in% base::names(dfVMS_);
    sbHasSpeed_  <- ssVarShipSpeed_ %in% base::names(dfVMS_);
    sbHasCourse_ <- ssVarShipCourse_ %in% base::names(dfVMS_);

    # Filter to records with valid coordinates ...
    dfVMS_ <- dfVMS_[!base::is.na(dfVMS_[[ssVarLATs_]]) & !base::is.na(dfVMS_[[ssVarLONs_]]), ]
    if (base::nrow(dfVMS_) == 0) {
      base::stop(base::paste0(' --> ', TAG_FUNC_ID, ' 112 | No valid coordinates in dfVMS_'))
    }

    ### ####--- 1.4 - Required Libraries ---####
    ### base::print(base::paste0(' --> ', TAG_FUNC_ID, ' 116 | ', 'Loading required R Libraries !!'))
    ### reqPacks <- base::c('leaflet', 'crayon', 'beepr', 'RColorBrewer', 'jsonlite')   # Update 'reqPacks' list accordingly.
    ### for (p in reqPacks) {   # Iterates through 'reqPacks' assessing list items one-by-one with the code below.
    ###   if (base::suppressMessages(base::suppressWarnings(!base::require(p, quietly=TRUE, character.only=TRUE)))) {
    ###     base::print(base::paste0(' --> ', TAG_FUNC_ID, ' 120 | ', "Installing missing R Library  ->  '", p, "' " ))
    ###     utils::install.packages(p)
    ###   }   # Checks if library is not already installed & executes a new install if required.
    ###   base::suppressMessages(base::suppressWarnings(base::library(p, quietly=TRUE, character.only=TRUE)))    # Loads each library after installation.
    ### }
    ### base::message(base::cat(crayon::blue(base::paste0(' --> ', TAG_FUNC_ID, ' 125 | ', 'Loaded ALL Required R Libraries.'))))

    ####--- Determine Unique Values and Color/Shape Palettes ---####
    vsLevelsYears_ <- base::sort(base::unique(dfVMS_[[ssVarYear_]]))
    vsLevelsYears_ <- vsLevelsYears_[!base::is.na(vsLevelsYears_)]
    vsLevelsShipIDs_ <- base::sort(base::unique(dfVMS_[[ssVarShipIDs_]]))
    vsLevelsShipIDs_ <- vsLevelsShipIDs_[!base::is.na(vsLevelsShipIDs_)]
    if (base::length(vsLevelsYears_) == 0) {
      base::stop(base::paste0(' --> ', TAG_FUNC_ID, ' 137 | No valid values in VMS Years column: ', ssVarYear_))
    }
    if (base::length(vsLevelsShipIDs_) == 0) {
      base::stop(base::paste0(' --> ', TAG_FUNC_ID, ' 140 | No valid values in Vessel ID column: ', ssVarShipIDs_))
    }

    tblCountsYears_ <- base::table(dfVMS_[[ssVarYear_]]);
    vsCountsTableYears_ <- base::as.numeric(tblCountsYears_[base::as.character(vsLevelsYears_)]);
    yearCounts <- base::prettyNum(vsCountsTableYears_, big.mark = ",", scientific = FALSE)
    vsTab <- base::table(dfVMS_[[ssVarShipIDs_]])
    vesselCountsRaw <- base::as.numeric(vsTab[base::as.character(vsLevelsShipIDs_)])
    vesselCounts <- base::prettyNum(vesselCountsRaw, big.mark = ",", scientific = FALSE)

    # Vessel color palette (categorical, handles any number of vsLevelsShipIDs_)
    vsShipColors_ <- NULL;
    if (base::is.null(vsShipColors)) {
      vsShipColors_ <- base::suppressWarnings(
        grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(9, "Set1")
        )(base::length(vsLevelsShipIDs_))
      );
    } else {
      vsShipColors_ <- vsShipColors;
    }
    base::names(vsShipColors_) <- vsLevelsShipIDs_;
    ### cfShipPAL_ <- leaflet::colorFactor(
    ###   palette = vsShipColors_, domain = vsLevelsShipIDs_
    ### );
    tpColorPTS_ <- vsShipColors_[base::as.character(dfVMS_[[ssVarShipIDs_]])];
    tpColorPTS_[base::is.na(tpColorPTS_)] <- "#333333";

    # Year shape: use different circle radii (older -> smaller, recent -> larger)
    yrNumeric <- base::as.numeric(vsLevelsYears_)
    if (base::all(base::is.na(yrNumeric))) {
      dfVMS_$ptRadius  <- 3.75
      dfVMS_$ptWeight  <- 1.50
      dfVMS_$ptOpacity <- 0.75
    } else {
      yrMin <- base::min(yrNumeric, na.rm = TRUE)
      yrMax <- base::max(yrNumeric, na.rm = TRUE)
      yrRange <- yrMax - yrMin
      if (yrRange == 0) yrRange <- 1
      dfVMS_$ptRadius  <- 3.75
      dfVMS_$ptWeight  <- 1.50
      dfVMS_$ptOpacity <- 0.75
    }

    ####--- Build VMS data array for JS Canvas rendering ---####

    speedRaw <- if (sbHasSpeed_) dfVMS_[[ssVarShipSpeed_]] else base::rep(NA_real_, base::nrow(dfVMS_));
    speedRawText <- base::as.character(speedRaw);
    speedNum <- base::suppressWarnings(base::as.numeric(speedRawText));
    stateText <- base::tolower(base::trimws(speedRawText));
    steamingStateValues <- base::c('steaming', 'steam', 'fast', 'moving', 'active', 's', 'yes', 'true', '1');
    stationaryStateValues <- base::c('stationary', 'stopped', 'stop', 'slow', 'static', 'idle', 'anchored', 'anchor', 'fishing', '0', 'no', 'false');
    stateCode <- base::ifelse(
      stateText %in% steamingStateValues, 'steaming',
      base::ifelse(stateText %in% stationaryStateValues, 'stationary', '')
    );
    rawStateNonEmpty <- !base::is.na(speedRawText) & base::trimws(speedRawText) != '' & speedRawText != 'NA'
    hasNonNumericState <- base::any(rawStateNonEmpty & base::is.na(speedNum))
    stateLooksExplicit <- base::grepl('state|status|condition', ssVarShipSpeed_, ignore.case = TRUE)
    hasStateText <- (hasNonNumericState || stateLooksExplicit) && base::any(stateCode != '')

    # Compute actual plot count based on sbPlotOnlyFishingVMS
    if (sbPlotOnlyFishingVMS) {
      if (hasStateText) {
        plotCount <- base::sum(stateCode == "stationary", na.rm = TRUE)
      } else if (sbHasSpeed_) {
        plotCount <- base::sum(speedNum < snSpeedSplit, na.rm = TRUE)
      } else {
        plotCount <- base::nrow(dfVMS_)
      }
    } else {
      plotCount <- base::nrow(dfVMS_)
    }
    base::message(base::paste0(
      ' --> ', TAG_FUNC_ID, ' 201 | Plotting ', plotCount, ' VMS points (records) ...'
    ));


    csVMS_JSON_ <- jsonlite::toJSON(
      base::data.frame(
        v   = base::as.character(dfVMS_[[ssVarShipIDs_]]),
        y   = base::as.character(dfVMS_[[ssVarYear_]]),
        lat = base::as.numeric(dfVMS_[[ssVarLATs_]]),
        lng = base::as.numeric(dfVMS_[[ssVarLONs_]]),
        t   = if (sbHasTime_) base::as.character(dfVMS_[[ssVarTime_]]) else "",
        d   = if (sbHasDate_) base::as.character(dfVMS_[[ssVarDate_]]) else base::as.character(dfVMS_[[ssVarYear_]]),
        s   = if (sbHasSpeed_) speedNum else base::rep(NA_real_, base::nrow(dfVMS_)),
        st  = if (hasStateText) stateCode else "",
        h   = if (sbHasCourse_) base::as.numeric(dfVMS_[[ssVarShipCourse_]]) else base::rep(NA_real_, base::nrow(dfVMS_)),
        r   = base::as.numeric(dfVMS_$ptRadius),
        w   = base::as.numeric(dfVMS_$ptWeight),
        o   = base::as.numeric(dfVMS_$ptOpacity),
        c   = tpColorPTS_,
        stringsAsFactors = FALSE
      ),
      dataframe = "rows",
      digits = NA
    );

    # No R-level marker creation -- Canvas rendering is done in JS via onRender
    m <- lmMapObj;

    ####--- Build checkbox HTML strings for JS injection ---####

    rfc_BuildItems_HTML <- function(items, colors = NULL, counts = NULL) {
      if (base::length(items) == 0) return('')

      # Master toggle check-box ...
      csMasterToggleHTML_ <- base::paste0(
        '<label style="display:flex;align-items:center;gap:5px;font-weight:bold;font-size:12px;',
        'margin-bottom:4px;cursor:pointer;">',
        '<input type="checkbox" class="vms-cb-master" checked> <b>Toggle ALL</b></label>'
      );

      csLinesHTML_ <- base::character(base::length(items));
      for (j in base::seq_along(items)) {
        item <- items[j]
        safeItem <- base::gsub("'", "\\'", base::as.character(item))
        dot <- if (!base::is.null(colors) && j <= base::length(colors) && !base::is.na(colors[j])) {
          base::paste0('<span style="color:', colors[j], ';">&#9679;</span>')
        } else ''
        countDisplay <- if (!base::is.null(counts) && j <= base::length(counts) && !base::is.na(counts[j])) {
          base::paste0(' <span class="vms-count" style="font-weight:normal;color:#888;">(n: ', counts[j], ')</span>')
        } else ''
        csLinesHTML_[j] <- base::paste0(
          '<label style="display:flex;align-items:center;gap:5px;white-space:nowrap;line-height:1.35;cursor:pointer;">',
          '<input type="checkbox" class="vms-cb" data-val="', safeItem, '" checked> ',
          dot, item, countDisplay, '</label>'
        )
      }
      base::paste0(csMasterToggleHTML_, base::paste(csLinesHTML_, collapse = ''));
    }

    csYearHTML_RAW_   <- rfc_BuildItems_HTML(vsLevelsYears_, NULL, yearCounts);
    csVesselsHTML_RAW_ <- rfc_BuildItems_HTML(vsLevelsShipIDs_, vsShipColors_, vesselCounts);

    # Escape characters for JS string embedding
    rcf_Escape_JS <- function(s) {
      s <- base::gsub("\\\\", "\\\\\\\\", s);
      s <- base::gsub('"', '\\\\"', s);
      s <- base::gsub("\n", "\\\\n", s);
      base::return(s);
    }

    rcf_Append_Control_Colon <- function(label) {
      label <- base::as.character(label);
      if (base::length(label) != 1 || base::is.na(label) || !base::nzchar(label)) label <- '';
      if (base::grepl('[:：]\\s*$', label)) base::return(label);
      base::paste0(label, ':');
    }

    rcf_Escape_JS_String <- function(s) {
      s <- base::gsub("\\\\", "\\\\\\\\", s);
      s <- base::gsub('"', '\\\\"', s);
      s <- base::gsub("'", "\\'", s);
      s <- base::gsub("\n", "\\\\n", s);
      base::return(s);
    }

    csLabelTime_  <- rcf_Escape_JS_String(rcf_Append_Control_Colon(ssLabelVarYear));
    csLabelVesselState_ <- rcf_Escape_JS_String(rcf_Append_Control_Colon(ssLabelVarSpeed));
    csLabelVesselID_  <- rcf_Escape_JS_String(rcf_Append_Control_Colon(ssLabelVarShipIDs));

    csYearHTML_CLEAN_    <- rcf_Escape_JS(csYearHTML_RAW_);
    csVesselsHTML_CLEAN_ <- rcf_Escape_JS(csVesselsHTML_RAW_);

    ####--- Inject JS that creates Canvas-based VMS renderer + native controls ---####
    jsCode <- base::paste0('
    function(el, x) {
      var map = this;

      // --- VMS data (embedded as JSON literal) ---
      var vmsData = ', csVMS_JSON_, ';

      // --- Speed-based rendering parameters ---
      var snSpeedSplit = ', snSpeedSplit, ';
      var initSteaming = ', base::tolower(base::as.character(!sbPlotOnlyFishingVMS)), ';  // initial Steaming checkbox state
      var sbPlotOnlyFishingVMS = ', base::tolower(base::as.character(sbPlotOnlyFishingVMS)), ';  // static filter: when TRUE, never draw steaming points

      function getIsSteaming(d) {
        if (d.st) return d.st === "steaming";
        if (d.s != null && !isNaN(d.s)) return d.s >= snSpeedSplit;
        return false;
      }

      // --- Canvas-based rendering layer for 208k+ points ---
      var VMSCanvas = L.Layer.extend({
        initialize: function(data) {
          this._data = data;
          this._filterVessels = null;
          this._filterYears = null;
          this._filterSteaming = true;
          this._filterStationary = true;
        },

        onAdd: function(map) {
          this._map = map;
          var container = L.DomUtil.create("div", "", map.getContainer());
          container.style.cssText = "position:absolute;left:0;top:0;width:100%;height:100%;z-index:950;pointer-events:none;overflow:hidden;";
          var canvas = L.DomUtil.create("canvas", "", container);
          this._container = container;
          this._canvas = canvas;
          this._resize();
          this._draw();
          map.on("moveend zoomend resize", this._onViewChange, this);
        },

        onRemove: function(map) {
          map.off("moveend zoomend resize", this._onViewChange, this);
          if (this._container.parentNode) this._container.parentNode.removeChild(this._container);
        },

        _onViewChange: function() {
          this._resize();
          this._draw();
        },

        _resize: function() {
          var size = this._map.getSize();
          this._canvas.width = size.x;
          this._canvas.height = size.y;
        },

        _draw: function() {
          var ctx = this._canvas.getContext("2d");
          var map = this._map;
          var data = this._data;
          var bounds = map.getBounds();
          var sw = bounds.getSouthWest(), ne = bounds.getNorthEast();

          ctx.clearRect(0, 0, this._canvas.width, this._canvas.height);

          var fV = this._filterVessels, fY = this._filterYears;
          for (var i = 0, n = data.length; i < n; i++) {
            var d = data[i];
            if (fV && fV.indexOf(d.v) < 0) continue;
            if (fY && fY.indexOf(d.y) < 0) continue;
            if (d.lat < sw.lat || d.lat > ne.lat || d.lng < sw.lng || d.lng > ne.lng) continue;
            // Vessel State filter: skip based on Steaming/Stationary checkboxes
            var isSteaming = getIsSteaming(d);
            // sbPlotOnlyFishingVMS: static filter, skips all steaming points regardless of checkbox
            if (sbPlotOnlyFishingVMS && isSteaming) continue;
            if (isSteaming && !this._filterSteaming) continue;
            if (!isSteaming && !this._filterStationary) continue;
            var pt = map.latLngToContainerPoint(L.latLng(d.lat, d.lng));
            ctx.fillStyle = d.c;
            ctx.globalAlpha = d.o;
            if (isSteaming) {
              // Arrow: triangle pointing in heading direction
              var hdg = d.h != null ? d.h : 0;
              var hr = hdg * Math.PI / 180;
              var fx = Math.sin(hr), fy = -Math.cos(hr);
              var px = Math.cos(hr), py = Math.sin(hr);
              var sz = d.r * 2.5;
              var tx = pt.x + fx * sz * 1.3;
              var ty = pt.y + fy * sz * 1.3;
              var lx = pt.x - fx * sz * 0.5 + px * sz * 0.65;
              var ly = pt.y - fy * sz * 0.5 + py * sz * 0.65;
              var rx = pt.x - fx * sz * 0.5 - px * sz * 0.65;
              var ry = pt.y - fy * sz * 0.5 - py * sz * 0.65;
              ctx.beginPath();
              ctx.moveTo(tx, ty);
              ctx.lineTo(lx, ly);
              ctx.lineTo(rx, ry);
              ctx.closePath();
              ctx.fill();
            } else {
              // Dot: circle
              ctx.beginPath();
              ctx.arc(pt.x, pt.y, d.r, 0, Math.PI * 2);
              ctx.fill();
            }
          }
        },

        setFilter: function(vsLevelsShipIDs_, vsLevelsYears_, steaming, stationary) {
          this._filterVessels = vsLevelsShipIDs_;
          this._filterYears = vsLevelsYears_;
          if (steaming !== undefined) this._filterSteaming = steaming;
          if (stationary !== undefined) this._filterStationary = stationary;
          this._draw();
        }
      });

      var vmsLayer = new VMSCanvas(vmsData);
      vmsLayer.addTo(map);

      function fmtNum(n) { return n.toLocaleString(); }

      // --- Control styling ---
      var controlStyle = "background:#fff;border-radius:5px;box-shadow:0 1px 5px rgba(0,0,0,0.65);padding:8px 10px;font:14px/1.5 \'Helvetica Neue\',Arial,Helvetica,sans-serif;color:#333;overflow-y:auto;box-sizing:border-box;";

      function makeControl(id, title, innerHTML, position) {
        var ctrl = L.control({position: position});
        ctrl.onAdd = function(map) {
          var div = L.DomUtil.create("div", "leaflet-control");
          div.id = id;
          div.style.cssText = controlStyle;
          div.innerHTML = "<b style=\\"display:block;font-size:15px;font-weight:bold;margin-bottom:4px;\\">" + title + "</b>" + innerHTML;
          return div;
        };
        ctrl.addTo(map);
      }

      var stateHtml =
                \'<div style="font-size:12px;color:#888;margin-top:-4px;margin-bottom:4px;padding-bottom:3px;">\' +
        \'Steaming &ge; \' + snSpeedSplit + \' kts &nbsp;|&nbsp; Stationary &lt; \' + snSpeedSplit + \' kts\' +
        \'</div>\' +
        \'<label style="display:flex;align-items:center;\' + (sbPlotOnlyFishingVMS ? \'gap:8px;\' : \'gap:5px;\') + \'white-space:nowrap;line-height:1.35;\' + (sbPlotOnlyFishingVMS ? \'opacity:0.55;cursor:default;\' : \'cursor:pointer;\') + \'">\' +
        \'<span style="position:relative;display:inline-flex;align-items:center;justify-content:center;">\' +
        \'<input type="checkbox" class="vms-cb-state" data-val="steaming" style="margin:0;vertical-align:baseline;" \' + (initSteaming ? \'checked\' : \'\') + (sbPlotOnlyFishingVMS ? \' disabled\' : \'\') + \'> \' +
        (sbPlotOnlyFishingVMS ? \'<span style="position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);font-size:13px;line-height:1;pointer-events:none;">\\u2717</span>\' : \'\') +
        \'</span>\' +
        \'Steaming <span class="vms-count" style="font-weight:normal;color:#888;"></span></label>\' +
        \'<label style="display:flex;align-items:center;gap:5px;white-space:nowrap;line-height:1.35;cursor:pointer;">\' +
        \'<span style="position:relative;display:inline-flex;align-items:center;justify-content:center;">\' +
        \'<input type="checkbox" class="vms-cb-state" data-val="stationary" style="margin:0;vertical-align:baseline;" checked> \' +
        \'</span>\' +
        \'Stationary <span class="vms-count" style="font-weight:normal;color:#888;"></span></label>\';
      makeControl("ctrl-vms-state", "', csLabelVesselState_, '", stateHtml, "topright");
      makeControl("ctrl-vms-time", "', csLabelTime_, '", "', csYearHTML_CLEAN_, '", "topleft");
      makeControl("ctrl-vms-vessel-id", "', csLabelVesselID_, '", "', csVesselsHTML_CLEAN_, '", "topright");

      // --- Apply filter: collect active values and redraw canvas ---
      function applyFilters() {
        var vList = [], yList = [];
        var vCbs = document.querySelectorAll("#ctrl-vms-vessel-id .vms-cb:checked");
        for (var i = 0; i < vCbs.length; i++) vList.push(vCbs[i].getAttribute("data-val"));
        var yCbs = document.querySelectorAll("#ctrl-vms-time .vms-cb:checked");
        for (var i = 0; i < yCbs.length; i++) yList.push(yCbs[i].getAttribute("data-val"));
        var vTotal = document.querySelectorAll("#ctrl-vms-vessel-id .vms-cb").length;
        var yTotal = document.querySelectorAll("#ctrl-vms-time .vms-cb").length;
        var steamingCb = document.querySelector("#ctrl-vms-state .vms-cb-state[data-val=steaming]");
        var stationaryCb = document.querySelector("#ctrl-vms-state .vms-cb-state[data-val=stationary]");
        vmsLayer.setFilter(
          vList.length === vTotal ? null : (vList.length > 0 ? vList : []),
          yList.length === yTotal ? null : (yList.length > 0 ? yList : []),
          steamingCb ? steamingCb.checked : true,
          stationaryCb ? stationaryCb.checked : true
        );
        syncMasters();

        // --- Update State-CL counts (filtered by Time + Vessel-ID) ---
        var vFilter = vList.length === vTotal ? null : (vList.length > 0 ? vList : []);
        var yFilter = yList.length === yTotal ? null : (yList.length > 0 ? yList : []);
        var steamCnt = 0, statCnt = 0;
        for (var j = 0; j < vmsData.length; j++) {
          var d = vmsData[j];
          if (yFilter && yFilter.indexOf(d.y) < 0) continue;
          if (vFilter && vFilter.indexOf(d.v) < 0) continue;
          var isSteaming = getIsSteaming(d);
          if (sbPlotOnlyFishingVMS && isSteaming) continue;
          if (isSteaming) steamCnt++; else statCnt++;
        }
        if (steamingCb) {
          var sl = steamingCb.closest("label");
          if (sl) { var ss = sl.querySelector(".vms-count"); if (ss) ss.textContent = "(n: " + fmtNum(steamCnt) + ")"; }
        }
        if (stationaryCb) {
          var sl2 = stationaryCb.closest("label");
          if (sl2) { var ss2 = sl2.querySelector(".vms-count"); if (ss2) ss2.textContent = "(n: " + fmtNum(statCnt) + ")"; }
        }

        // --- Update Vessel-ID-CL counts (filtered by Time + State) ---
        var steamingChecked = steamingCb ? steamingCb.checked : true;
        var stationaryChecked = stationaryCb ? stationaryCb.checked : true;
        var vesselCbsAll = document.querySelectorAll("#ctrl-vms-vessel-id .vms-cb");
        var vCounts = {};
        for (var i = 0; i < vesselCbsAll.length; i++) {
          vCounts[vesselCbsAll[i].getAttribute("data-val")] = 0;
        }
        for (var j = 0; j < vmsData.length; j++) {
          var d = vmsData[j];
          if (!vCounts.hasOwnProperty(d.v)) continue;
          if (yFilter && yFilter.indexOf(d.y) < 0) continue;
          var isSteaming = getIsSteaming(d);
          if (sbPlotOnlyFishingVMS && isSteaming) continue;
          if (isSteaming && !steamingChecked) continue;
          if (!isSteaming && !stationaryChecked) continue;
          vCounts[d.v]++;
        }
        for (var i = 0; i < vesselCbsAll.length; i++) {
          var cb = vesselCbsAll[i];
          var vesselVal = cb.getAttribute("data-val");
          var countSpan = cb.parentNode.querySelector(".vms-count");
          if (countSpan) countSpan.textContent = "(n: " + fmtNum(vCounts[vesselVal] || 0) + ")";
        }
      }

      // --- Synchronize master checkbox states after filtering ---
      function syncMasters() {
        ["ctrl-vms-state", "ctrl-vms-vessel-id", "ctrl-vms-time"].forEach(function(id) {
          var master = document.querySelector("#" + id + " .vms-cb-master");
          if (!master) return;
          var cbs = document.querySelectorAll("#" + id + " input[type=checkbox].vms-cb, " +
                                               "#" + id + " input[type=checkbox].vms-cb-state");
          var all = true, any = false;
          for (var i = 0; i < cbs.length; i++) {
            if (cbs[i].checked) any = true; else all = false;
          }
          master.checked = all;
          master.indeterminate = any && !all;
        });
      }

      // --- Event delegation: master toggle + individual checkboxes (click) ---
      var timer = null;
      map.getContainer().addEventListener("click", function(e) {
        var t = e.target;
        if (!t || t.tagName !== "INPUT" || t.type !== "checkbox") return;
        if (t.classList.contains("vms-cb-master")) {
          var parent = t.closest("[id^=\\"ctrl-vms-\\"]");
          if (parent) {
            var checked = t.checked;
            var children = parent.querySelectorAll("input[type=checkbox]:not(.vms-cb-master):not(:disabled)");
            for (var i = 0; i < children.length; i++) children[i].checked = checked;
          }
        }
        // TIME→Vessel-ID sync: when a TIME checkbox is toggled, update Vessel-ID checkboxes
        if (t.closest("#ctrl-vms-time")) {
          var selYears = [];
          var yChecked = document.querySelectorAll("#ctrl-vms-time .vms-cb:checked");
          for (var i = 0; i < yChecked.length; i++) selYears.push(yChecked[i].getAttribute("data-val"));
          var activeVessels = {};
          for (var j = 0; j < vmsData.length; j++) {
            if (selYears.indexOf(vmsData[j].y) >= 0) activeVessels[vmsData[j].v] = true;
          }
          var vCbsAll = document.querySelectorAll("#ctrl-vms-vessel-id .vms-cb");
          for (var i = 0; i < vCbsAll.length; i++) {
            vCbsAll[i].checked = !!activeVessels[vCbsAll[i].getAttribute("data-val")];
          }
        }
        if (t.classList.contains("vms-cb") || t.classList.contains("vms-cb-state") || t.classList.contains("vms-cb-master")) {
          if (timer) clearTimeout(timer);
          timer = setTimeout(applyFilters, 50);
        }
      });

      // Initial count population, fix widths, and apply height constraints
      setTimeout(function() {
        applyFilters();
        var mapHeight = map.getContainer().offsetHeight;
        var ctrlIds = ["ctrl-vms-time", "ctrl-vms-state", "ctrl-vms-vessel-id"];
        for (var i = 0; i < ctrlIds.length; i++) {
          var ctrl = document.getElementById(ctrlIds[i]);
          if (!ctrl) continue;
          // Measure and lock width (with scrollbar forced to account for its width)
          ctrl.style.overflowY = "scroll";
          ctrl.style.width = ctrl.offsetWidth + "px";
          ctrl.style.overflowY = "auto";
          // Apply height constraint
          if (ctrl.id === "ctrl-vms-state") {
            ctrl.style.maxHeight = "280px";
          } else {
            var ctrlRect = ctrl.getBoundingClientRect();
            var mapRect = map.getContainer().getBoundingClientRect();
            var topOffset = ctrlRect.top - mapRect.top;
            var available = mapHeight - topOffset - 15;
            ctrl.style.maxHeight = Math.max(available, 80) + "px";
          }
        }
      }, 0);

      // --- Spatial index for tooltip proximity lookup ---
      var cellSize = 0.05;
      var grid = {};
      for (var i = 0; i < vmsData.length; i++) {
        var d = vmsData[i];
        var cx = Math.floor(d.lng / cellSize);
        var cy = Math.floor(d.lat / cellSize);
        var key = cx + "," + cy;
        if (!grid[key]) grid[key] = [];
        grid[key].push(i);
      }

      // --- Custom DOM tooltip (avoids Leaflet popup pane z-index conflict) ---
      (function() {
        var tipDiv = null, tipMoveTime = 0, tipThreshPx = 15;
        var tipPane = map.getPane ? map.getPane("tooltipPane") : null;
        function hideTip() {
          if (tipDiv) { tipDiv.style.display = "none"; }
          if (tipPane) tipPane.style.display = "";
        }
        function removeTip() {
          if (tipDiv && tipDiv.parentNode) tipDiv.parentNode.removeChild(tipDiv);
          tipDiv = null;
          if (tipPane) tipPane.style.display = "";
        }

        map.getContainer().addEventListener("mousemove", function(e) {
          if (Date.now() - tipMoveTime < 50) return;
          tipMoveTime = Date.now();
          var cursorLatLng = map.mouseEventToLatLng(e);
          var cursorPoint = map.mouseEventToContainerPoint(e);
          var gx = Math.floor(cursorLatLng.lng / cellSize);
          var gy = Math.floor(cursorLatLng.lat / cellSize);
          var fV = vmsLayer._filterVessels, fY = vmsLayer._filterYears,
              fSt = vmsLayer._filterSteaming, fSg = vmsLayer._filterStationary;
          var bestIdx = -1, bestPx2 = tipThreshPx * tipThreshPx + 1;

          for (var dx = -1; dx <= 1; dx++) {
            for (var dy = -1; dy <= 1; dy++) {
              var cell = grid[(gx + dx) + "," + (gy + dy)];
              if (!cell) continue;
              for (var k = 0; k < cell.length; k++) {
                var idx = cell[k], d = vmsData[idx];
                if (fV && fV.indexOf(d.v) < 0) continue;
                if (fY && fY.indexOf(d.y) < 0) continue;
                var isSteaming = getIsSteaming(d);
                if (sbPlotOnlyFishingVMS && isSteaming) continue;
                if (isSteaming && !fSt) continue;
                if (!isSteaming && !fSg) continue;
                var pt = map.latLngToContainerPoint(L.latLng(d.lat, d.lng));
                var px2 = (pt.x - cursorPoint.x) * (pt.x - cursorPoint.x) +
                          (pt.y - cursorPoint.y) * (pt.y - cursorPoint.y);
                if (px2 < bestPx2) { bestPx2 = px2; bestIdx = idx; }
              }
            }
          }

          if (bestIdx >= 0) {
            var d = vmsData[bestIdx];
            var spd = d.s != null ? d.s.toFixed(1) : "N/A";
            var hdg = d.h != null ? d.h.toFixed(0) : "N/A";
            var dtStr = d.d + (d.t ? " " + d.t : "");
            var content = "<b>Vessel ID:</b> " + d.v + "<br>" +
                          "<b>Date-Time:</b> " + dtStr + "<br>" +
                          "<b>Speed:</b> " + spd + " kts &nbsp;|&nbsp; <b>Heading:</b> " + hdg + "\\u00B0";
            if (!tipDiv) {
              tipDiv = L.DomUtil.create("div", "", map.getContainer());
              tipDiv.style.cssText = "position:absolute;z-index:2000;pointer-events:none;white-space:nowrap;" +
                "background:#fff;border-radius:4px;padding:5px 11px;font:13px/1.5 Helvetica,Arial,sans-serif;" +
                "color:#333;box-shadow:0 1px 5px rgba(0,0,0,0.35);";
            }
            tipDiv.innerHTML = content;
            tipDiv.style.left = (cursorPoint.x + 14) + "px";
            tipDiv.style.top  = (cursorPoint.y - 12) + "px";
            tipDiv.style.display = "block";
            if (tipPane) tipPane.style.display = "none";
          } else {
            hideTip();
          }
        });

        map.getContainer().addEventListener("mouseleave", removeTip);
        map.getContainer().addEventListener("mousedown", removeTip);
      })();
    }');

    lmMap_ <- htmlwidgets::onRender(m, htmlwidgets::JS(jsCode))

    ####--- STEP 4 - FUNCTION OUTPUTS - Define any required function outputs here ---####
    ####--- Capture Function END DateTime   ####
    endDateTime <- base::Sys.time();
    funcExeDur <- base::difftime(endDateTime, startDateTime, units = 'secs');
    base::message(base::cat(crayon::yellow(base::paste0(' --> ', TAG_FUNC_ID, ' 671 | FUNCTION EXECUTION DURATION: ', base::round(funcExeDur, 3), ' seconds.'))))
    base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID, ' 672 | ', 'EXECUTION of CUSTOM FUNCTION [ ', TAG_FUNC_ID_FULL, ' ] ', ' STOPPED on ', base::format(endDateTime, formatDateTime),' .'))))

    beepr::beep(2)   # Notify user of FUNCTION CODE EXECUTION COMPLETION with an audio cue...
    base::return(lmMap_);

  })   # The 'suppressWarnings({})' closure bracket or brace !!
}


