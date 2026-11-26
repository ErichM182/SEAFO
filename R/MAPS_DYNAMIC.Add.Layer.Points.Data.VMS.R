#? ### ### ### ### ### ### ###
#' @title Add a VMS Points Layer to a SEAFO Leaflet BaseMap
#' @name maps_dynamic.add.layer.points.data.vms
#' @family MAPS Functions (SEAFO)
#'
#'
#' @description
#' A Helper Function that overlays the South East Atlantic Fisheries Organisation (SEAFO) Vessel
#' Monitoring System (VMS) records on an <already compiled> interactive `leaflet` BaseMap (i.e.
#' one returned by [SEAFO::maps_dynamic.compile.basemap()]). The position fixes are painted by a
#' single HTML `<canvas>` overlay rather than by one Leaflet layer per record, so very large
#' tracks (i.e. hundreds of thousands of fixes) render smoothly. Up to five purpose-built controls
#' are attached to the map so that the user can show/hide the points by Time (i.e. VMS Year), by
#' Month, by Vessel State (i.e. steaming versus stationary), by Vessel Flag and by Vessel ID, each
#' annotated with a live record count. The Time control is the MASTER CONTROL LAYER (MCL): the
#' toggle state and the counts of every other control are live-linked to the years that are
#' currently active. This function was created to support the routine data-analysis work conducted
#' by the Scientific Committee (SC) of the SEAFO, but may also be used as a quick "situational
#' awareness" overlay in other (3rd Party) R libraries and custom R functions - as long as the
#' `SEAFO` library is linked (i.e. previously installed and/or specified as a library dependency).
#'
#'
#' @param lmMapObj ([leaflet]) a `leaflet` map object (i.e. an `htmlwidget`) to which the VMS
#'                points layer must be added. The map is modified in place and handed back, so
#'                the argument may NOT be `NULL` (the function stops with an informative error
#'                when it is). Typically this is the widget returned by
#'                [SEAFO::maps_dynamic.compile.basemap()].
#' @param dfVMS ([data.frame]) a Data Frame holding the VMS records (i.e. one row per position
#'              fix). When `NULL` (the default) the demonstration data set that ships with the
#'              package is read from `./data/dfSEAFO_GPS_VMS.csv`. NB: that path is RELATIVE, so
#'              the function expects to be run from the R Project (i.e. package source) root -
#'              i.e. the directory that holds the `data/` folder. The frame must carry the
#'              coordinate, date-time, year and vessel-identifier columns, plus (optionally) the
#'              speed, the heading and an explicit steaming/stationary state (see the `ssVar*`
#'              arguments).
#' @param snSpeedSplit ([numeric]) a Numeric (i.e. [numeric]) value that specifies the speed
#'                threshold (in knots) that separates the two Vessel States (default: `1.15`).
#'                A record at or above the threshold is treated as "steaming" and is drawn as a
#'                triangle pointing along its heading; a record below it is treated as
#'                "stationary" and is drawn as a dot. The threshold is bypassed when the speed
#'                column carries an explicit state (i.e. text such as "steaming" or
#'                "stationary", or a column name containing "state", "status" or "condition").
#' @param ssVarShipCourse ([character]) a String or Text (i.e. [character]) value that specifies
#'                the VMS column holding the vessel heading (i.e. course over ground in degrees;
#'                default: `"CO"`). The heading orients the "steaming" triangles and is reported
#'                in the hover tooltip. Resolution falls back to `"Ships_CRS"`, `"Ship_Crs"` and
#'                `"Course (degrees)"`; the column is optional, so when it cannot be found every
#'                triangle simply points due North.
#' @param ssVarYear ([character]) a String or Text (i.e. [character]) value that specifies the
#'                VMS column used as the Time selector (i.e. the per-year check-boxes; default:
#'                `"VMS_Year"`). The column is REQUIRED. Resolution falls back to
#'                `"Year_Parsed"`, `"Year"` and `"YEAR"`, and the function stops when no such
#'                column exists. Each distinct (i.e. non-`NA`) value becomes one toggleable item
#'                in the Time control.
#' @param ssLabelVarYear ([character]) a String or Text (i.e. [character]) value that specifies
#'                the caption prepended to the Time control box (default: `"VMS Years"`). A
#'                trailing colon is appended automatically when the label does not already end
#'                in one.
#' @param sbShowToggleYear ([logical]) a Boolean value that specifies whether the Time control -
#'                i.e. the per-year check-boxes, which act as the MASTER CONTROL LAYER (MCL) -
#'                must be compiled (default: `TRUE`). When `TRUE` the control is the top-most
#'                control of the top-left corner and drives the toggle state of the Month, Flag
#'                and Vessel ID controls (see the *Controls* section of `@details`). When
#'                `FALSE` the control is omitted altogether, the Year filter is not applied and
#'                the remaining controls act independently of one another.
#' @param ssVarMonth ([character]) a String or Text (i.e. [character]) value that specifies the
#'                VMS column that drives the per-month selector (default: `"VMS_Month"`).
#'                Resolution falls back to `"Month_Parsed"`, `"Month"`, `"MONTH"`, `"MON"` and
#'                `"Mon"`; the column is optional. When it cannot be found no Month control is
#'                compiled, whatever the value of `sbShowToggleMonth`. Each distinct value
#'                becomes one toggleable item; a missing or blank cell is presented as the
#'                single item `"(none)"` so that those records stay reachable. Month names are
#'                ordered calendar-wise, numeric months numerically.
#' @param ssLabelVarMonth ([character]) a String or Text (i.e. [character]) value that specifies
#'                the caption prepended to the Month control box (default: `"VMS Month"`). A
#'                trailing colon is appended automatically when the label does not already end
#'                in one.
#' @param sbShowToggleMonth ([logical]) a Boolean value that specifies whether the Month
#'                control must be compiled (default: `FALSE`). The control is added to the
#'                top-left corner BELOW the Time control, and is only compiled when the month
#'                column can also be resolved (see `ssVarMonth`). Its toggle state is
#'                live-linked to the Time control (i.e. the MCL).
#' @param ssVarShipIDs ([character]) a String or Text (i.e. [character]) value that specifies
#'                the VMS column holding the vessel identifier (default: `"Ship_Name_Anon"`).
#'                The column is REQUIRED. Resolution falls back to `"Vessel_Name"`, `"Name"` and
#'                `"Registry_number_IR"`, and the function stops when no such column exists.
#'                Each distinct (i.e. non-`NA`) value becomes one toggleable item in the Vessel
#'                ID control, and the same values drive the per-vessel colour palette.
#' @param ssLabelVarShipIDs ([character]) a String or Text (i.e. [character]) value that
#'                specifies the caption prepended to the Vessel ID control box (default:
#'                `"Vessel ID"`). A trailing colon is appended automatically when the label does
#'                not already end in one.
#' @param vsShipColors ([character]) a Character (i.e. [character]) vector of R colour
#'                specifications used for the per-vessel palette. When `NULL` (the default) a
#'                palette is generated internally by interpolating
#'                `RColorBrewer::brewer.pal(9, "Set1")` over the number of distinct vessels.
#'                When supplied, the vector is used as-is and is matched POSITIONALLY to the
#'                sorted vessel identifiers, so it must be at least as long as the number of
#'                distinct vessels. Any record whose vessel cannot be matched is drawn in grey
#'                (i.e. `"#333333"`).
#' @param ssVarShipSpeed ([character]) a String or Text (i.e. [character]) value that specifies
#'                the VMS column carrying the speed over ground (default: `"Speed_Corr_kt"`).
#'                Resolution falls back to `"Speed_Num"`, `"Speed_kt"`, `"Speed (kt)"` and
#'                `"Speed"`; the column is optional. When it cannot be found (and no explicit
#'                state column exists) every record is treated as "stationary".
#' @param ssLabelVarSpeed ([character]) a String or Text (i.e. [character]) value that specifies
#'                the caption prepended to the Vessel State control box (default:
#'                `"Vessel State (speed in knots)"`). A trailing colon is appended automatically
#'                when the label does not already end in one.
#' @param ssVarShipFlag ([character]) a String or Text (i.e. [character]) value that specifies
#'                the VMS column holding the vessel flag or nationality (default: `"Flag_Anon"`).
#'                Resolution falls back to `"Flag"`, `"FLAG"`, `"Nationality"`, `"Country"` and
#'                `"Flag_State"`; the column is optional. When it cannot be found no Flag control
#'                is compiled, whatever the value of `sbShowShipFlag`. Each distinct value
#'                becomes one toggleable item; a missing or blank cell is presented as the
#'                single item `"(none)"` so that those records stay reachable.
#' @param ssLabelVarShipFlag ([character]) a String or Text (i.e. [character]) value that
#'                specifies the caption prepended to the Flag control box (default:
#'                `"Vessel Flag"`). A trailing colon is appended automatically when the label does
#'                not already end in one.
#' @param sbShowShipFlag ([logical]) a Boolean value that specifies whether the Vessel Flag
#'                control must be compiled (default: `FALSE`). The control is added to the
#'                top-right corner BETWEEN the Vessel State and the Vessel ID controls, and is
#'                only compiled when the flag column can also be resolved (see `ssVarShipFlag`).
#'                Its toggle state is live-linked to the Time control (i.e. the MCL).
#' @param ssVarLATs ([character]) a String or Text (i.e. [character]) value that specifies the
#'                VMS column holding the latitudes (default: `"LAT_DeciDegree"`). The column is
#'                REQUIRED. Resolution falls back to `"Lat_DD"`, `"LATs"`, `"LAT"`, `"LATS"` and
#'                `"Latitudes"`, and the function stops when no such column exists. Records with
#'                a missing latitude are discarded before plotting.
#' @param ssVarLONs ([character]) a String or Text (i.e. [character]) value that specifies the
#'                VMS column holding the longitudes (default: `"LON_DeciDegree"`). The column is
#'                REQUIRED. Resolution falls back to `"Lon_DD"`, `"LONs"`, `"LON"`, `"LONS"` and
#'                `"Longitudes"`, and the function stops when no such column exists. Records with
#'                a missing longitude are discarded before plotting.
#' @param sbPlotOnlyFishingVMS ([logical]) a Boolean value that specifies whether ONLY the
#'                stationary records must be drawn (default: `FALSE`). When `TRUE` the "steaming"
#'                records are excluded by a static (i.e. un-toggleable) filter: the Steaming
#'                check-box of the Vessel State control is rendered disabled, dimmed and struck
#'                through with a cross. When `FALSE` all records are drawn and the user may
#'                toggle either state.
#' @param ssVarTime ([character]) a String or Text (i.e. [character]) value that specifies the
#'                VMS column holding the time of each fix (default: `"VMS_Time"`). The column is
#'                REQUIRED. Resolution falls back to `"Time_Parsed"`, `"Time"`, `"TIME"` and
#'                `"TM"`, and the function stops when no such column exists. The value is only
#'                reported in the hover tooltip.
#' @param ssLabelVarTime ([character]) a String or Text (i.e. [character]) value that is RESERVED
#'                for a future time caption (default: `"VMS Time"`). The Time control box is
#'                currently captioned with `ssLabelVarYear`, so the argument is internalized only
#'                and does NOT alter the map.
#' @param ssVarDate ([character]) a String or Text (i.e. [character]) value that specifies the
#'                VMS column holding the date of each fix (default: `"VMS_Date"`). The column is
#'                REQUIRED. Resolution falls back to `"Date_Parsed"`, `"Date"`, `"DATE"` and
#'                `"DT"`, and the function stops when no such column exists. The value is only
#'                reported in the hover tooltip.
#' @param ssLabelVarDate ([character]) a String or Text (i.e. [character]) value that is RESERVED
#'                for a future date caption (default: `"VMS Date"`). The date is currently
#'                reported in the hover tooltip as a raw column value, so the argument is
#'                internalized only and does NOT alter the map.
#'
#'
#' @returns
#' A `leaflet` `htmlwidget` object - i.e. the supplied map (`lmMapObj`) with the VMS `<canvas>`
#' overlay and its controls attached through `htmlwidgets::onRender()`. Because the map is
#' handed back, the call may be chained onto the BaseMap builder (i.e.
#' `maps_dynamic.compile.basemap() |> maps_dynamic.add.layer.points.data.vms()`), or the result
#' may be stored in an R object for later use (e.g.
#' `coMap <- maps_dynamic.add.layer.points.data.vms(coMap)`).
#'
#'
#' @details
#' ### VMS data & column resolution
#' Every column is addressed by NAME. Each `ssVar*` argument is resolved in three passes: an exact
#' match, then a case-insensitive match, then a fixed list of common aliases. A 1-based column
#' index supplied as text (i.e. `"3"`) is also accepted. Latitude, longitude, date, time, year and
#' vessel identifier are mandatory; speed, heading, month and flag are optional and degrade
#' gracefully when absent. Rows with a missing latitude or longitude are discarded, and the
#' function stops with an informative error when no usable row remains.
#'
#' ### Rendering model
#' The fixes are NOT created as individual Leaflet layers. Instead the whole frame is serialised
#' to JSON (i.e. `jsonlite::toJSON()`) and embedded in a JavaScript closure that is attached to
#' the widget with `htmlwidgets::onRender()`. A single `<canvas>` element (i.e. a custom `L.Layer`
#' subclass) then paints every visible record, culled to the current viewport and redrawn on
#' `moveend`, `zoomend` and `resize`. This keeps the widget responsive for tracks of hundreds of
#' thousands of fixes.
#'
#' ### Vessel States
#' Each record is classified as either "steaming" or "stationary". An explicit state carried in
#' the speed column (i.e. text such as "steaming", "moving", "stationary" or "fishing") takes
#' precedence; otherwise the numeric speed is compared with `snSpeedSplit`. "Steaming" records are
#' drawn as triangles oriented along `ssVarShipCourse`, while "stationary" records are drawn as
#' filled circles.
#'
#' ### Controls
#' Up to five check-box controls are appended to the map, each with a live `(n: ...)` count. The
#' Time, Month, Flag and Vessel ID controls also carry a "Toggle ALL" master switch:
#'
#' * **Time** (i.e. `ssVarYear`) - top-left, top-most, a master switch plus one check-box per
#'   year. This is the **MASTER CONTROL LAYER (MCL)**; it is compiled when
#'   `sbShowToggleYear = TRUE`;
#' * **Month** (i.e. `ssVarMonth`) - top-left, directly UNDER the Time control, a master switch
#'   plus one check-box per month. It is compiled only when `sbShowToggleMonth = TRUE` and the
#'   month column can be resolved;
#' * **Vessel State** (i.e. `ssVarShipSpeed`) - top-right, one check-box for "Steaming" and one
#'   for "Stationary" (no master switch), together with the `snSpeedSplit` legend;
#' * **Vessel Flag** (i.e. `ssVarShipFlag`) - top-right, BETWEEN Vessel State and Vessel ID, a
#'   master switch plus one check-box per flag. It is compiled only when
#'   `sbShowShipFlag = TRUE` and the flag column can be resolved;
#' * **Vessel ID** (i.e. `ssVarShipIDs`) - top-right, bottom-most, a master switch plus one
#'   check-box per vessel, prefixed with that vessel's colour swatch.
#'
#' ### Master Control Layer live-linkage
#' The Time control is the MCL, and every other control is live-linked to it in two directions:
#'
#' * **Toggle state (master to slaves)** - whenever a year check-box (or the Time master switch)
#'   changes, the Month, Flag and Vessel ID check-boxes are re-ticked so that only the months,
#'   flags and vessels actually recorded in the selected years remain active;
#' * **Statistics (cross-filtering)** - every control reports each item's `(n: ...)` count against
#'   the current selection of all the OTHER controls, so switching a year re-computes the Month,
#'   Flag, State and Vessel ID counts at once. The single exception is the Time control itself,
#'   which ignores the Month, Flag and Vessel ID controls because those merely mirror its own year
#'   selection; counting through them would be circular and would drive an unticked year's count
#'   to zero. The Time counts therefore stay meaningful (i.e. they show what re-ticking that year
#'   would reveal), and they still respond to the Vessel State control.
#'
#' A control whose items are ALL checked imposes no constraint, and a control with NO item checked
#' hides every record. When `sbShowToggleYear = FALSE` the MCL is absent: the Year filter is not
#' applied and the remaining controls act independently of one another.
#'
#' The controls are rendered as native DOM elements and do NOT overwrite any existing Leaflet
#' layer control, so the map returned by [SEAFO::maps_dynamic.compile.basemap()] keeps its own
#' control box.
#'
#' ### Tooltip
#' Hovering near a record (i.e. within 15 pixels) pops up a custom DOM tooltip reporting the
#' Vessel ID, the Date-Time, the Speed (in knots) and the Heading (in degrees). A 0.05-degree
#' spatial grid index keeps the proximity search cheap, and Leaflet's own tooltip pane is hidden
#' while the custom tooltip is visible so that the two never overlap.
#'
#' ### Console feedback
#' The function prints a START banner, the number of records it is about to plot (i.e.
#' "Plotting N VMS points (records) ...") and a STOP banner carrying the elapsed run-time, then
#' plays an audible completion cue (i.e. `beepr::beep(2)`). The console banners are colourised
#' with `crayon`, and the per-vessel palette is generated with `RColorBrewer`; both packages are
#' used internally and must therefore be installed (i.e. they are not exposed as arguments).
#'
#'
#' @examples
#' \dontrun{
#' ### Add the demonstration VMS tracks to the DEFAULT SEAFO BaseMap ...
#' library(SEAFO)   # <- Ensures the `SEAFO` library is <already> installed locally.
#'
#' coMap <- SEAFO::maps_dynamic.compile.basemap()
#' coMap <- SEAFO::maps_dynamic.add.layer.points.data.vms(lmMapObj = coMap)
#' coMap            # <- Renders the BaseMap + the VMS overlay in the Viewer pane !!!
#'
#'
#' ### Compile & overlay in one go, with ALL five controls (Year drives the rest) ...
#' SEAFO::maps_dynamic.compile.basemap() |>
#'   SEAFO::maps_dynamic.add.layer.points.data.vms(
#'     sbShowToggleYear  = TRUE,    # <- MASTER CONTROL LAYER (top-left) ...
#'     sbShowToggleMonth = TRUE,    # <- Month CL, directly under the Year CL ...
#'     sbShowShipFlag    = TRUE     # <- Flag CL, between Vessel State and Vessel ID ...
#'   )
#'
#'
#' ### Compile & overlay in one go, showing ONLY the stationary (fishing) records ...
#' SEAFO::maps_dynamic.compile.basemap() |>
#'   SEAFO::maps_dynamic.add.layer.points.data.vms(sbPlotOnlyFishingVMS = TRUE)
#'
#'
#' ### Plot your own VMS frame, with custom vessel colours & column names ...
#' SEAFO::maps_dynamic.add.layer.points.data.vms(
#'   lmMapObj       = SEAFO::maps_dynamic.compile.basemap(snMapSRC = 1),
#'   dfVMS          = myVMS,                      # <- Your own VMS data frame ...
#'   ssVarLATs      = "Lat_DD",                   # <- Column holding the latitudes ...
#'   ssVarLONs      = "Lon_DD",                   # <- Column holding the longitudes ...
#'   ssVarShipIDs   = "Vessel_Name",              # <- Column holding the vessel IDs ...
#'   ssVarShipSpeed = "Speed_Num",                # <- Column holding the speed (knots) ...
#'   ssVarMonth     = "Month",                    # <- Column holding the months ...
#'   ssVarShipFlag  = "Flag",                     # <- Column holding the flags ...
#'   snSpeedSplit   = 3.0,                        # <- Fishing / steaming threshold ...
#'   vsShipColors   = c("#1B9E77", "#D95F02", "#7570B3")
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
    ssVarMonth_ <- rcf_resolve.var.ids(
      ssVarMonth, base::c('Month_Parsed', 'Month', 'MONTH', 'MON', 'Mon'), FALSE, 'VMS Month'
    );
    ssVarShipFlag_ <- rcf_resolve.var.ids(
      ssVarShipFlag, base::c('Flag', 'FLAG', 'Nationality', 'Country', 'Flag_State'), FALSE, 'Vessel Flag'
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

    # Month & Flag Control Layer columns (optional; the CL is skipped when absent)
    sbHasMonth_  <- !base::is.null(ssVarMonth_) && ssVarMonth_ %in% base::names(dfVMS_);
    sbHasFlag_   <- !base::is.null(ssVarShipFlag_) && ssVarShipFlag_ %in% base::names(dfVMS_);

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

    ####--- Month & Flag Control Layer (CL) levels and counts ---####
    # Source values for the two new CLs. Missing / blank cells collapse into a
    # single '(none)' item so that every record stays reachable by the toggles.
    rcf_Normalise_Level_Vals <- function(ssCol) {
      vsVals_ <- base::as.character(dfVMS_[[ssCol]]);
      vsVals_[base::is.na(vsVals_) | !base::nzchar(base::trimws(vsVals_))] <- '(none)';
      vsVals_;
    }

    # Order levels calendar-wise for month names, numerically for numbers and
    # alphabetically otherwise (so 'January' precedes 'February', not 'April').
    # A '(none)' level is always placed last.
    rcf_Sort_Levels <- function(vsVals_) {
      vsU_ <- base::sort(base::unique(vsVals_));
      if (base::length(vsU_) < 2) base::return(vsU_);
      sbNone_ <- vsU_ == '(none)';
      vsRest_ <- vsU_[!sbNone_];
      vsTail_ <- vsU_[sbNone_];
      if (base::length(vsRest_) > 0) {
        vsThree_ <- base::tolower(base::substr(base::trimws(vsRest_), 1, 3));
        vsMonKeys_ <- base::c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                              'jul', 'aug', 'sep', 'oct', 'nov', 'dec');
        vsMonIdx_ <- base::match(vsThree_, vsMonKeys_);
        if (!base::any(base::is.na(vsMonIdx_))) {
          base::return(base::c(vsRest_[base::order(vsMonIdx_)], vsTail_));
        }
        vnNum_ <- base::suppressWarnings(base::as.numeric(vsRest_));
        if (!base::any(base::is.na(vnNum_))) {
          base::return(base::c(vsRest_[base::order(vnNum_)], vsTail_));
        }
      }
      base::c(vsRest_, vsTail_);
    }

    vsMonthVals_ <- base::rep('(none)', base::nrow(dfVMS_));
    vsFlagVals_  <- base::rep('(none)', base::nrow(dfVMS_));
    if (sbHasMonth_) vsMonthVals_ <- rcf_Normalise_Level_Vals(ssVarMonth_);
    if (sbHasFlag_)  vsFlagVals_  <- rcf_Normalise_Level_Vals(ssVarShipFlag_);

    vsLevelsMonths_ <- rcf_Sort_Levels(vsMonthVals_);
    vsLevelsFlags_  <- rcf_Sort_Levels(vsFlagVals_);

    monthCounts <- base::prettyNum(
      base::as.numeric(base::table(vsMonthVals_)[vsLevelsMonths_]),
      big.mark = ",", scientific = FALSE
    )
    flagCounts <- base::prettyNum(
      base::as.numeric(base::table(vsFlagVals_)[vsLevelsFlags_]),
      big.mark = ",", scientific = FALSE
    )

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
        m   = vsMonthVals_,
        f   = vsFlagVals_,
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
        safeItem <- base::gsub('"', '&quot;', safeItem)   # <- keep the "data-val" attribute well-formed
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
    csMonthHTML_RAW_  <- rfc_BuildItems_HTML(vsLevelsMonths_, NULL, monthCounts);
    csFlagHTML_RAW_   <- rfc_BuildItems_HTML(vsLevelsFlags_, NULL, flagCounts);
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
    csLabelMonth_ <- rcf_Escape_JS_String(rcf_Append_Control_Colon(ssLabelVarMonth));
    csLabelVesselState_ <- rcf_Escape_JS_String(rcf_Append_Control_Colon(ssLabelVarSpeed));
    csLabelVesselFlag_  <- rcf_Escape_JS_String(rcf_Append_Control_Colon(ssLabelVarShipFlag));
    csLabelVesselID_  <- rcf_Escape_JS_String(rcf_Append_Control_Colon(ssLabelVarShipIDs));

    csYearHTML_CLEAN_    <- rcf_Escape_JS(csYearHTML_RAW_);
    csMonthHTML_CLEAN_   <- rcf_Escape_JS(csMonthHTML_RAW_);
    csFlagHTML_CLEAN_    <- rcf_Escape_JS(csFlagHTML_RAW_);
    csVesselsHTML_CLEAN_ <- rcf_Escape_JS(csVesselsHTML_RAW_);

    ####--- Assemble the Control Layer (CL) creation calls ---####
    # Placement order matters: Leaflet stacks controls of the same corner in the
    # order they are added, so the Month CL lands UNDER the Year CL (topleft) and
    # the Flag CL lands BETWEEN Vessel State and Vessel ID (topright).
    csControlCalls_ <- base::paste0(
      if (base::isTRUE(sbShowToggleYear)) {
        base::paste0(
          'makeControl("ctrl-vms-time", "', csLabelTime_, '", "',
          csYearHTML_CLEAN_, '", "topleft");\n'
        )
      } else '',
      if (base::isTRUE(sbShowToggleMonth) && sbHasMonth_) {
        base::paste0(
          'makeControl("ctrl-vms-month", "', csLabelMonth_, '", "',
          csMonthHTML_CLEAN_, '", "topleft");\n'
        )
      } else '',
      base::paste0(
        'makeControl("ctrl-vms-state", "', csLabelVesselState_, '", stateHtml, "topright");\n'
      ),
      if (base::isTRUE(sbShowShipFlag) && sbHasFlag_) {
        base::paste0(
          'makeControl("ctrl-vms-flag", "', csLabelVesselFlag_, '", "',
          csFlagHTML_CLEAN_, '", "topright");\n'
        )
      } else '',
      base::paste0(
        'makeControl("ctrl-vms-vessel-id", "', csLabelVesselID_, '", "',
        csVesselsHTML_CLEAN_, '", "topright");\n'
      )
    );

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

      // --- Control Layer (CL) registry ---
      // The YEAR CL is the MASTER CONTROL LAYER (MCL): its toggle state drives the
      // toggle state of every other CL, and every CL reports counts against the
      // current selection of all the OTHER CLs (live cross-filtering).
      var vmsCLs = [
        { id: "ctrl-vms-time",      key: "y",  sel: ".vms-cb",       master: true  },
        { id: "ctrl-vms-month",     key: "m",  sel: ".vms-cb",       derived: true },
        { id: "ctrl-vms-state",     key: "st", sel: ".vms-cb-state", master: false },
        { id: "ctrl-vms-flag",      key: "f",  sel: ".vms-cb",       derived: true },
        { id: "ctrl-vms-vessel-id", key: "v",  sel: ".vms-cb",       derived: true }
      ];

      // Value of record `d` in the dimension represented by the CL `cl`.
      function clValue(cl, d) {
        if (cl.id === "ctrl-vms-state") return getIsSteaming(d) ? "steaming" : "stationary";
        return d[cl.key];
      }

      // Read the checked items of one CL. Returns null when the CL is absent or
      // when every item is checked (i.e. "no constraint"); otherwise a hash.
      function readSelection(cl) {
        var root = document.getElementById(cl.id);
        if (!root) return null;
        var cbs = root.querySelectorAll(cl.sel);
        var sel = Object.create(null), n = 0, k = 0;
        for (var i = 0; i < cbs.length; i++) {
          n++;
          if (cbs[i].checked) { sel[cbs[i].getAttribute("data-val")] = true; k++; }
        }
        if (n === 0 || k === n) return null;
        return sel;
      }

      // Snapshot of every CL selection; `excludeId` is ignored (used when a CL
      // reports its own per-item counts against the other CLs only).
      function computeSelections(excludeId) {
        var out = Object.create(null);
        for (var i = 0; i < vmsCLs.length; i++) {
          out[vmsCLs[i].id] = (vmsCLs[i].id === excludeId) ? null : readSelection(vmsCLs[i]);
        }
        return out;
      }

      // Snapshot used when `cl` reports its OWN per-item counts. A CL never filters
      // its own counts, and the MASTER also ignores the CLs that it drives (Month,
      // Flag and Vessel ID mirror the master\'s own year selection, so including
      // them would make the master\'s counts circular - unticking a year would
      // drive that year\'s own count to zero).
      function selectionsFor(cl) {
        var out = Object.create(null);
        for (var i = 0; i < vmsCLs.length; i++) {
          var o = vmsCLs[i];
          if (o.id === cl.id || (cl.master && o.derived)) { out[o.id] = null; continue; }
          out[o.id] = readSelection(o);
        }
        return out;
      }

      // Does record `d` survive every CL in the snapshot `sels`?
      function passesLayers(d, sels) {
        for (var i = 0; i < vmsCLs.length; i++) {
          var cl = vmsCLs[i];
          // Static filter: sbPlotOnlyFishingVMS excludes steaming records outright.
          if (cl.id === "ctrl-vms-state" && sbPlotOnlyFishingVMS && getIsSteaming(d)) return false;
          var s = sels ? sels[cl.id] : null;
          if (s == null) continue;
          var v = clValue(cl, d);
          if (v == null || s[v] !== true) return false;
        }
        return true;
      }

      // Re-tick the slave CLs so that they mirror the year selection exactly:
      // only the months / flags / vessels actually recorded in the selected
      // years stay active. This is the MASTER -> SLAVE live-link.
      function relinkToMaster() {
        var root = document.getElementById("ctrl-vms-time");
        if (!root) return;
        var yCbs = root.querySelectorAll(".vms-cb");
        if (yCbs.length === 0) return;
        var selYears = Object.create(null), allYears = true;
        for (var i = 0; i < yCbs.length; i++) {
          if (yCbs[i].checked) selYears[yCbs[i].getAttribute("data-val")] = true;
          else allYears = false;
        }
        var live = { m: Object.create(null), f: Object.create(null), v: Object.create(null) };
        for (var j = 0; j < vmsData.length; j++) {
          var d = vmsData[j];
          if (!allYears && selYears[d.y] !== true) continue;
          if (d.m != null) live.m[d.m] = true;
          if (d.f != null) live.f[d.f] = true;
          if (d.v != null) live.v[d.v] = true;
        }
        var slaves = [
          { id: "ctrl-vms-month",      k: "m" },
          { id: "ctrl-vms-flag",       k: "f" },
          { id: "ctrl-vms-vessel-id",  k: "v" }
        ];
        for (var s = 0; s < slaves.length; s++) {
          var sRoot = document.getElementById(slaves[s].id);
          if (!sRoot) continue;
          var sCbs = sRoot.querySelectorAll(".vms-cb");
          for (var c = 0; c < sCbs.length; c++) {
            sCbs[c].checked = live[slaves[s].k][sCbs[c].getAttribute("data-val")] === true;
          }
        }
      }

      // --- Canvas-based rendering layer for 208k+ points ---
      var VMSCanvas = L.Layer.extend({
        initialize: function(data) {
          this._data = data;
          this._sel = null;              // <- per-Control-Layer selection map (null = no constraint)
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

          var sels = this._sel, fSt = this._filterSteaming, fSg = this._filterStationary;
          for (var i = 0, n = data.length; i < n; i++) {
            var d = data[i];
            if (d.lat < sw.lat || d.lat > ne.lat || d.lng < sw.lng || d.lng > ne.lng) continue;
            // Per-Control-Layer selection (Year = master, Month, State, Flag, Vessel ID)
            if (!passesLayers(d, sels)) continue;
            var isSteaming = getIsSteaming(d);
            if (isSteaming && !fSt) continue;
            if (!isSteaming && !fSg) continue;
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

        setFilter: function(sels, steaming, stationary) {
          this._sel = sels;
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
      ', csControlCalls_, '

      // --- Apply the current CL selection to the canvas and refresh every CL ---
      function applyFilters() {
        var steamCb = document.querySelector("#ctrl-vms-state .vms-cb-state[data-val=steaming]");
        var statCb  = document.querySelector("#ctrl-vms-state .vms-cb-state[data-val=stationary]");
        vmsLayer.setFilter(
          computeSelections(null),
          steamCb ? steamCb.checked : true,
          statCb  ? statCb.checked  : true
        );
        syncMasters();
        updateCounts();
      }

      // --- Live per-item counts: every CL reports against all the OTHER CLs ---
      function updateCounts() {
        for (var c = 0; c < vmsCLs.length; c++) {
          var cl = vmsCLs[c];
          var root = document.getElementById(cl.id);
          if (!root) continue;
          var cbs = root.querySelectorAll(cl.sel);
          if (cbs.length === 0) continue;
          var sels = selectionsFor(cl);
          var acc = Object.create(null);
          for (var i = 0; i < cbs.length; i++) acc[cbs[i].getAttribute("data-val")] = 0;
          for (var j = 0; j < vmsData.length; j++) {
            var d = vmsData[j];
            if (!passesLayers(d, sels)) continue;
            var v = clValue(cl, d);
            if (v == null || acc[v] === undefined) continue;
            acc[v]++;
          }
          for (var k = 0; k < cbs.length; k++) {
            // The Vessel State control nests its <input> inside a <span>, so the
            // count is located from the enclosing <label>, not the parent node.
            var host = cbs[k].closest("label") || cbs[k].parentNode;
            var span = host ? host.querySelector(".vms-count") : null;
            if (span) span.textContent = "(n: " + fmtNum(acc[cbs[k].getAttribute("data-val")] || 0) + ")";
          }
        }
      }

      // --- Synchronize the "Toggle ALL" master of every CL ---
      function syncMasters() {
        for (var c = 0; c < vmsCLs.length; c++) {
          var root = document.getElementById(vmsCLs[c].id);
          if (!root) continue;
          var master = root.querySelector(".vms-cb-master");
          if (!master) continue;
          var cbs = root.querySelectorAll("input[type=checkbox]:not(.vms-cb-master):not(:disabled)");
          var all = true, any = false;
          for (var i = 0; i < cbs.length; i++) {
            if (cbs[i].checked) any = true; else all = false;
          }
          master.checked = cbs.length > 0 && all;
          master.indeterminate = any && !all;
        }
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
        // MASTER LINK: any change in the Year CL re-ticks the Month / Flag / Vessel-ID
        // CLs so that only the values actually recorded in those years stay active.
        if (t.closest("#ctrl-vms-time")) relinkToMaster();
        if (t.classList.contains("vms-cb") || t.classList.contains("vms-cb-state") || t.classList.contains("vms-cb-master")) {
          if (timer) clearTimeout(timer);
          timer = setTimeout(applyFilters, 50);
        }
      });

      // Initial count population, fix widths, and apply height constraints
      setTimeout(function() {
        applyFilters();
        var mapHeight = map.getContainer().offsetHeight;
        var ctrlIds = ["ctrl-vms-time", "ctrl-vms-month", "ctrl-vms-state",
                       "ctrl-vms-flag", "ctrl-vms-vessel-id"];
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
          var sels = vmsLayer._sel,
              fSt = vmsLayer._filterSteaming, fSg = vmsLayer._filterStationary;
          var bestIdx = -1, bestPx2 = tipThreshPx * tipThreshPx + 1;

          for (var dx = -1; dx <= 1; dx++) {
            for (var dy = -1; dy <= 1; dy++) {
              var cell = grid[(gx + dx) + "," + (gy + dy)];
              if (!cell) continue;
              for (var k = 0; k < cell.length; k++) {
                var idx = cell[k], d = vmsData[idx];
                if (!passesLayers(d, sels)) continue;
                var isSteaming = getIsSteaming(d);
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

    ### beepr::beep(2)   # Notify user of FUNCTION CODE EXECUTION COMPLETION with an audio cue...
    base::return(lmMap_);

  })   # The 'suppressWarnings({})' closure bracket or brace !!
}


