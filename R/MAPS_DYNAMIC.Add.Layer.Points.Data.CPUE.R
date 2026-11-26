#? ### ### ### ### ### ### ###
#' @title Add a VMS Points Layer to a SEAFO Leaflet BaseMap
#' @name maps_dynamic.add.layer.points.cpue
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
#'              package is read from `./data/dfSEAFO_GPS_CPUE.csv`. NB: that path is RELATIVE, so
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
#' `maps_dynamic.compile.basemap() |> maps_dynamic.add.layer.points.cpue()`), or the result
#' may be stored in an R object for later use (e.g.
#' `coMap <- maps_dynamic.add.layer.points.cpue(coMap)`).
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
#' coMap <- SEAFO::maps_dynamic.add.layer.points.cpue(lmMapObj = coMap)
#' coMap            # <- Renders the BaseMap + the VMS overlay in the Viewer pane !!!
#'
#'
#' ### Compile & overlay in one go, with ALL five controls (Year drives the rest) ...
#' SEAFO::maps_dynamic.compile.basemap() |>
#'   SEAFO::maps_dynamic.add.layer.points.cpue(
#'     sbShowToggleYear  = TRUE,    # <- MASTER CONTROL LAYER (top-left) ...
#'     sbShowToggleMonth = TRUE,    # <- Month CL, directly under the Year CL ...
#'     sbShowShipFlag    = TRUE     # <- Flag CL, between Vessel State and Vessel ID ...
#'   )
#'
#'
#' ### Compile & overlay in one go, showing ONLY the stationary (fishing) records ...
#' SEAFO::maps_dynamic.compile.basemap() |>
#'   SEAFO::maps_dynamic.add.layer.points.cpue(sbPlotOnlyFishingVMS = TRUE)
#'
#'
#' ### Plot your own VMS frame, with custom vessel colours & column names ...
#' SEAFO::maps_dynamic.add.layer.points.cpue(
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
"maps_dynamic.add.layer.points.cpue" <- function(
  leafMap=NULL, sFileGPS=NULL, sPathData=NULL, sVarY='Total_Catch_Weight_kg', sVarLAT='Start_Set_Latitude',
  sVarLON='Start_Set_Longitude', sGroupBySelect='NONE', enModeHexAggr=enModeHexAGGR$SUM, bShowHexGrid=FALSE,
  enTypeGPS=enTypeGPS$CATCH, vsPlotSPP=lstCodeSPP$YHI, sGroupBy=lstGroupBY$YEAR, sTitleLyrCntrlr="SEAFO CA Map Layers",
  lstLocControls=lstMapLOC$TOPLEFT, bSaveMap=FALSE, bSingleMap=TRUE, sPlotRate=enShowRTE$NONE, bShowLabels=TRUE, sLabelsFull=FALSE,
  bShowLyrCntrlr=TRUE, bPrintInfoRF=FALSE, bPrintArgsRF=FALSE, bPrintStatusRF=FALSE
) {
  startEGLN_ <- 101;   # <- DO NOT DELETE OR MOVE !! <- Must ALWAYS BE on the very 1st line of the function body !!

  ###   Define the number of Editor lines (i.e. 'LineQTY') taken up by the Function Arguments ...   ###
  funcArgsLineQTY_ <- 4;   # <- DO NOT DELETE OR MOVE !!!



  ###   < --- --- --- --- --- --- ---  >   ####



  ####---   SECTION 1 - Run Function Init Processes   ---####
  ####---   1.1 - Identifier TAGs for Function   ---####
  base::gc();   # <- Clear any unused RAM before executing function ...
  TAG_FUNC_ID_SHORT_ <- 'SEAFO Maps Points';
  TAG_FUNC_ID_LONG_ <- 'SEAFO Maps Points Plotter ( SEAFO Map Points )';


  ####---   1.2 - Required Libraries   ---####
  ####   | - NOTES: Install or activate required libraries (packages)   ####
  if (FEET_SEAFO_MAP_PP_IS_INIT_RUN) {
    vsReqPacks <- c('crayon', 'beepr', 'RColorBrewer', 'leaflet', 'leafem', 'sfheaders', 'leaflethex');   # Update 'vsReqPacks' list accordingly.
    base::message(base::cat(crayon::blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', 123, ' | ', 'Loading required libraries { ', base::paste0(vsReqPacks, collapse = ", "), ' }'))))
    for (p in vsReqPacks) {   # Iterates through 'vsReqPacks' assessing list items one-by-one with the code below.
      if (base::suppressMessages(!base::require(p, character.only = TRUE))) {   # Checks if library is not already installed & executes a new install ...
        base::print(base::paste0(' > ', TAG_FUNC_ID_SHORT_, ' ', 126, ' -> ', 'Installing required library [ ', p, ' ] and dependencies !!'));
        if (p == 'leaflethex') {
          devtools::install_github("rpruim/leaflethex");
        } else {
          utils::install.packages(p);
        }
      }
      base::suppressPackageStartupMessages(base::suppressMessages(base::suppressWarnings(base::library(p, character.only = TRUE, quietly = TRUE))))    # Loads each library after installation - silently.
    }
    FEET_SEAFO_MAP_PP_IS_INIT_RUN <<- FALSE;
  }


  ####---   1.3 - Capture Function START DateTime   ---####
  #   Capture Function START DateTime   #
  sDateTimeSTART_ <- base::Sys.time();
  sDateTimeFormat_ <- '%a, %d %b %Y - %X';   # Datetime Format:

  # Announce the Function Execution - START - to the user ...
  if (!bPrintInfoRF && !bPrintStatusRF && !bPrintArgsRF) {   # <- Checks that ALL 3 Function Information Arguments are FALSE !!
    base::message((base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', 146, ' | ', 'CUSTOM FUNCTION [ ', TAG_FUNC_ID_LONG_, ' ] ', 'STARTED [ ', base::format(sDateTimeSTART_, sDateTimeFormat_),' ].')));
  }


  ####---   1.4 - Function Information ( detailed )   ---####
  maxInfoChars_ <- 107;   # <- DO NOT CHANGE, DELETE OR MOVE !!! ( the maximum number of characters in the 'FUNCTION INFORMATION' Section line of code ) ...
  infoLine3Spaces_ <- maxInfoChars_ - (11 + nchar(TAG_FUNC_ID_LONG_));
  funcInfo_ <- base::paste0('\n',
                            '###-####################################################################################################-###', '\n',
                            '# > FUNCTION INFORMATION:                                                                              #####', '\n',
                            '# ID:      ', TAG_FUNC_ID_LONG_, strrep(' ', (infoLine3Spaces_)), '#', '\n',
                            '# INFO:    This custom R function compiles a stand-alone base map for the South Eastern Atlantic           #', '\n',
                            '#          Fisheries Organisation (SEAFO) Convention Area (CA).                                            #', '\n',
                            '# NOTES:   < no important usage notes required for this custom R function >                                #', '\n',
                            '# SOURCES: - https://www.leafletjs.com/           ( general [JavaScript] info )                            #', '\n',
                            '#          - https://rstudio.github.io/leaflet/   ( R specific info )                                      #', '\n',
                            '#          - https://leaflet-extras.github.io/leaflet-providers/preview/   ( Leaflet Tile Providers List)  #', '\n',
                            '# OUTPUTS: This custom function outputs (returns) a list containing the following R data objects:          #', '\n',
                            '#          [ BaseMap ] : a static base map (image) of the SEAFO CA.                                        #', '\n',
                            '#          [ LeafMap ] : an interactive Leaflet map (SEAFO Base Map) for use with other custom functions.  #', '\n',
                            '# ######################################################################################################## #', '\n\n'
  );


  ####---   1.5 - Function Status ( explained )   ---####
  funcStatus_ <- base::paste0('\n',
                              '###-####################################################################################################-###', '\n',
                              '# > Status of (custom, main) function: [ Work-in-Progress ] ( function name ends in "_wip" ).          #####', '\n',
                              '#   - still need to implement the code logic for vsGroupVars[3] sorting and stats calculation.             #', '\n',
                              '#   - finally ... still need to ensure the Script Editor Gutter Line Numbering is correctly coded.         #', '\n',
                              '# ######################################################################################################## #', '\n\n'
  );


  ####---   1.6 - Function Arguments ( explained )   ---####
  funcArgs_ <- base::paste0('\n',
                            '# ======================================================================================================== #', '\n',
                            '# > FUNCTION ARGUMENTS: ( n = 31 )                                                                      ####', '\n',
                            '#    - sPathUtils     : [string] the absolute (directory) path to the "Map Utils" folder on the local      #', '\n',
                            '#                                computer where all the important base map (input) layer data sets are     #', '\n',
                            '#                                stored or located. [ default: NULL ]                                      #', '\n',
                            '#    - enMapSource    : [enum - MapSRC] the base layer (image) source as defined by the options of the     #', '\n',
                            '#                                       "MapSRC" enumerated data object. [ default: MapSRC$OCEAN ]         #', '\n',
                            '#    - enMapType      : [enum - MapTPE] the storage format for the static base map as defined by the       #', '\n',
                            '#                                        "MapTPE" enumerated data object. [ default: MapType$PNG ]         #', '\n',
                            '#    - bShowCA        : [boolean] a logical value that defines whether the SEAFO Convention Area (CA)      #', '\n',
                            '#                                 boundary should be plotted on the base map. [ default: TRUE ]            #', '\n',
                            '#    - bShowEEZs      : [boolean] a logical value that defines whether the African and SEAFO Economic      #', '\n',
                            '#                                 Exclusive Zones (EEZs) should be shown on the map. [ default: TRUE ]     #', '\n',
                            '#    - bShowCLAs      : [boolean] a logical value that defines whether the SEAFO Closed Areas should       #', '\n',
                            '#                                 be plotted on the base map. [ default: TRUE ]                            #', '\n',
                            '#    - sColrFillCA    : [string] the fill colour text value for the SEAFO Convention Area (CA) polygon     #', '\n',
                            '#                                layer. [ default: "transparent" ]                                         #', '\n',
                            '#    - sColrFillEEZs  : [string] fill colour text value for the Exclusive Economic Zones (EEZs) polygon    #', '\n',
                            '#                                layer. [ default: "white" ]                                               #', '\n',
                            '#    - sColrFillCLAs  : [string] fill colour text value for the SEAFO Closed Areas (CLAs) polygon layer.   #', '\n',
                            '#                                [ default: "red" ]                                                        #', '\n',
                            '#    - nFillAlphaCA   : [numeric] the transparency ( alpha ) decimal value for the SEAFO Convention        #', '\n',
                            '#                                 Area (CA) polygon layer. [ default: 1.00 ( i.e. "100%" ) ]               #', '\n',
                            '#    - nFillAlphaEEZs : [numeric] transparency ( alpha ) decimal value for the Exclusive Economic Zones    #', '\n',
                            '#                                 (EEZs) polygon layer. [ default: 0.28 ( i.e. "28%" ) ]                   #', '\n',
                            '#    - nFillAlphaCLAs : [numeric] transparency ( alpha ) decimal value for the SEAFO Closed Areas (CLAs)   #', '\n',
                            '#                                 polygon layer. [ default: 0.28 ( i.e. "28%" ) ]                          #', '\n',
                            '#    - sColrLineCA    : [string] the line colour text value for the SEAFO Convention Area (CA) polygon     #', '\n',
                            '#                                layer. [ default: "black" ]                                               #', '\n',
                            '#    - sColrLineEEZs  : [string] line colour text value for the Exclusive Economic Zones (EEZs) polygon    #', '\n',
                            '#                                layer. [ default: "white" ]                                               #', '\n',
                            '#    - sColrLineCLAs  : [string] line colour text value for the SEAFO Closed Areas (CLAs) polygon layer.   #', '\n',
                            '#                                [ default: "red" ]                                                        #', '\n',
                            '#    - enLineTypeCA   : [enum - LineTPE] the line type for the SEAFO Convention Area (CA) polygon layer    #', '\n',
                            '#                                        as defined by the "LineTPE" enum [ default: LineTPE$SOLID ]       #', '\n',
                            '#    - enLineTypeEEZs : [enum - LineTPE] line type for the Exclusive Economic Zones (EEZs) polygon layer   #', '\n',
                            '#                                        as defined by the "LineTPE" enum [ default: LineTPE$SOLID ]       #', '\n',
                            '#    - enLineTypeCLAs : [enum - LineTPE] line type for the SEAFO Closed Areas (CLAs) polygon layer as      #', '\n',
                            '#                                        defined by the "LineTPE" enum [ default: LineTPE$SOLID ]          #', '\n',
                            '#    - nLineWidthCA   : [numeric] line width decimal value for the SEAFO Convention Area (CA) polygon      #', '\n',
                            '#                                 layer outline or boundary (stroke). [ default: 1.00 ( i.e. "100%" ) ]    #', '\n',
                            '#    - nLineWidthEEZs : [numeric] line width decimal value for Exclusive Economic Zones (EEZs) polygon     #', '\n',
                            '#                                 layer outline or boundary (stroke). [ default: 1.00 ( i.e. "100%" ) ]    #', '\n',
                            '#    - nLineWidthCLAs : [numeric] line width decimal value for the SEAFO Closed Areas (CLAs) polygon       #', '\n',
                            '#                                 layer outline or boundary (stroke). [ default: 1.00 ( i.e. "100%" ) ]    #', '\n',
                            '#    - nLineAlphaCA   : [numeric] the transparency ( alpha ) decimal value for the SEAFO Convention        #', '\n',
                            '#                                 Area (CA) polygon layer outline. [ default: 1.00 ( i.e. "100%" ) ]       #', '\n',
                            '#    - nLineAlphaEEZs : [numeric] transparency ( alpha ) decimal value for the Exclusive Economic Zones    #', '\n',
                            '#                                 (EEZs) polygon layer outline. [ default: 1.00 ( i.e. "100%" ) ]          #', '\n',
                            '#    - nLineAlphaCLAs : [numeric] the transparency ( alpha ) decimal value for the SEAFO Closed Areas      #', '\n',
                            '#                                 (CLAs) polygon layer outline. [ default: 1.00 ( i.e. "100%" ) ]          #', '\n',
                            '#    - bShowLabels    : [boolean] a logical value that defines whether the SEAFO Convention Area labels    #', '\n',
                            '#                                 should be plotted on the base map or not. [ default: TRUE ]              #', '\n',
                            '#    - bLabelsFull    : [boolean] a logical value that defines whether the SEAFO Convention Area labels    #', '\n',
                            '#                                 should be abbreviated ( short ) or not ( full ). [ default: FALSE ]      #', '\n',
                            '#    - bShowControls  : [boolean] a logical value that defines whether the SEAFO Leaflet Base Map Layer    #', '\n',
                            '#                                 Controls should be activated or not. [ default: FALSE ]                  #', '\n',
                            '#    - lstLocControls : [list] placement of the SEAFO Leaflet Base Map Layer Controls as defined by the    #', '\n',
                            '#                              "MapLoc" list options [ default: MapLoc$TOPLEFT ]                           #', '\n',
                            '#    - bPrintInfoRF   : [boolean] a logical value that tells the function to print the full ( complete )   #', '\n',
                            '#                                 information for the custom R function. [ default: FALSE ]                #', '\n',
                            '#    - bPrintArgsRF   : [boolean] a logical value that tells the function to print the complete list of    #', '\n',
                            '#                                 input arguments ( only ) for the custom R function. [ default: FALSE ]   #', '\n',
                            '#    - bPrintStatusRF : [boolean] a logical value that tells the function to print the completion status   #', '\n',
                            '#                                 ( only ) of the custom R function. [ default: FALSE ]                    #', '\n',
                            '# ======================================================================================================== #', '\n\n'
  );


  ####---   1.7 - NB Internal Vectors, Variables & Constants    ---####
  # vsFuncMode_ <- c('SUM', 'COUNT');


  ####---   1.8 - Internal Helper Functions   ---####
  ###   Function that suppresses the default R (non-assignment) function 'output print' behaviour ...   ###
  'rf_suppress.output' <- function(varIN=NULL) {
    if (is.null(varIN)) {
      base::message(base::cat(crayon::red(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | ARGUMENT [ varIN ] CANNOT BE NULL !!!'))))
      base::stop(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(), ' | ', tagFull," script terminated [ reason: 'varIN' == NULL ]."))
    } else {
      sink(nullfile());   # suppress output behaviour ( 'nullfile()' should work on both Linux and Windows systems ) ...
      varIN;              # simply return the input variable to the function call ...
      sink();             # deactivate output suppression ...
    }
  }


  ###   Function that extracts the Editor Gutter Line Number - EGLN (at the line location from which the function was called) ...   ###
  'rf_get.egln' <- function (nScriptStartEGLN=startEGLN_) {
    x <- base::.traceback(x = 1);
    editorGutterLineNumber_ <- if (!is.null(srcref <- attr(x[[1]], 'srcref'))) {
      srcfile <- base::attr(srcref, 'srcfile');
      srcref[1];   # + nScriptStartEGLN - (funcArgsLineQTY_ + 1);
    }
    base::return(editorGutterLineNumber_);
  }


  ###   Function that easily & correctly sorts MONTH categorical data of full and abbreviated length types ...   ###
  # 'f.sort.months' <- function(vectMonths, dfMonths=NULL) {
  #   tagMonth <- data.frame(m1 = month.name, m2 = month.abb, n = 1:12)
  #   matchedMonths <- if(max(nchar(vectMonths)) == 3) match(vectMonths, tagMonth[, 'm2']) else match(vectMonths, tagMonth[, 'm1'])
  #   vectMonths <- if(is.null(dfMonths)) vectMonths else dfMonths
  #   h <- data.frame(matchedMonths, vectMonths)
  #   h[order(matchedMonths), ][, -1]
  # }



  # Turn off 3rd Party Warnings while this function executes !!
  suppressWarnings({   # All R code executed beyond this line will NOT THROW 3rd Party Error Messages !!



    ###   < --- --- --- --- --- --- ---  >   ####



    # Create the Ouput Data List Object here already ...
    lstDataOUT_ <- NULL;

    if (bPrintInfoRF || bPrintArgsRF || bPrintStatusRF) {

      if (bPrintInfoRF) {

        # Print the Function Information ...
        base::cat(crayon::cyan(funcInfo_))

        # Print the Function Status ...
        base::cat(crayon::cyan(funcStatus_))

        # Print the Function Arguments ...
        base::cat(crayon::cyan(funcArgs_))

        # Compile the output data object ..
        lstDataOUT_ <- base::list('Info'=funcInfo_, 'Status'=funcStatus_, 'Args'=funcArgs_)

      } else if (bPrintStatusRF) {

        # Print the Function Status ...
        base::cat(crayon::cyan(funcStatus_))

        # Compile the output data object ...
        lstDataOUT_ <- base::list('Status'=funcStatus_)

      } else if (bPrintArgsRF) {

        # Print the Function Arguments ...
        base::cat(crayon::cyan(funcArgs_))

        # Compile the output data object ...
        lstDataOUT_ <- base::list('Args'=funcArgs_)
      }

    } else {

      ####---   SECTION 2 - Run CRITICAL Priming Code Logic   ---####
      beepr::beep(2)   # <- Notify user of FUNCTION EXECUTION START with an audio cue ...

      # Check that the Leaflet Map [argument] is not NULL ...
      if (base::is.null(leafMap)) {
        base::message(base::cat(crayon::red(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | ARGUMENT [ leafMap ] CANNOT BE NULL !!!'))));
        base::stop(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(), ' | ', TAG_FUNC_ID_LONG_," script terminated [ reason: 'leafMap' == NULL ]."));
      }

      # Check that the GPS Data File [argument] is not NULL ...
      sFileGPS_ <- sFileGPS;
      if (base::is.null(sFileGPS)) {
        base::message(base::cat(crayon::red(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | ARGUMENT [ sFileGPS ] CANNOT BE NULL !!!'))));
        base::stop(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(), ' | ', TAG_FUNC_ID_LONG_," script terminated [ reason: 'sFileGPS' == NULL ]."));
      }

      # Check that the GPS Data File Path [argument] is not NULL ...
      sPathData_ <- sPathData;
      if (base::is.null(sPathData)) {
        sPathData_ <- "./02. Map Inputs/01. GPS Data/";
        base::message(base::cat(crayon::blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | ARGUMENT [ sPathData ] WAS NULL !!!'))));
        base::message(base::cat(crayon::yellow(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | ARGUMENT [ sPathData ] SET TO DEFAULT VALUE -> "', sPathData_,'" !!!'))));
      }



      ###   < --- --- --- --- --- --- ---  >   ####



      ####---   SECTION 3 - Execute Function Code Logic   ---####
      ####---   3.1 - Import GPS Input Data   ---####
      base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | READ INPUT GPS DATA SET ...'))));
      dfInGPS_ <- utils::read.csv(base::file.path(sPathData_, sFileGPS));


      ###   < --- --- --- >   ####


      ####---   3.2 - Compile the Actual Map[s]   ---####
      leafMapOut_ <- leafMap; vsCntrlLYRs_ <- NULL; lyrId_FINAL_ <- NULL; sTitleLyrCntrlr_ <- sTitleLyrCntrlr;
      base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | CHECK GPS DATA TYPE ...'))))
      ###? if (enTypeGPS == enTypeGPS$CATCH) {
      base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | CHECK MAP SAVE LOGICAL STATE ...'))))
      if (!bSaveMap) {   # <- DO NOT SAVE the final (output) map[s] to the appropriate locations (has implication for plotting on a SINGLE MAP or not) ...
        base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | CHECK HEX GRID LOGICAL STATE ...'))))

        base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | CHECK GROUP BY SELECT STATUS ...'))))
        if (sGroupBySelect == 'NONE') {   # <- DO NOT SELECT or FILTER any of the 'GroupBy' variable levels or values (i.e. USE THE FULL GROUPBY DATA SET) ...
          if (vsPlotSPP[1] != lstCodeSPP$ALL[1]) {
            base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | APPLY SPECIES DATA SELECTION REQUEST [ SINGLE ] ...'))))
            dfInGPS_ <- subset(dfInGPS_, SP_Scientific_Name == vsPlotSPP[1])

            if (vsPlotSPP[1] == lstCodeSPP$TOP[1]) {
              sTitleLyrCntrlr_ <- 'SEAFO - Patagonian toothfish'
            } else if (vsPlotSPP[1] == lstCodeSPP$YHI[1]) {
              sTitleLyrCntrlr_ <- 'SEAFO - Deep-sea red crab'
            }

            base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | APPLY GROUP BY CODE LOGIC - PART 1 of 3 ...'))))
            vsGroupByVars_ <- unique(as.factor(dfInGPS_[[sGroupBy]]))
            vsColrs_ <-  hcl.colors(length(vsGroupByVars_), palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE)
            vsGroupByVars_ <- vsGroupByVars_[order(vsGroupByVars_, na.last=TRUE, decreasing=T)]
            # print(paste0(paste0('Group By Levels [ ', vsPlotSPP[3],' ]: { ' , vsGroupByVars_, ' }'), collapse = ", "))

            base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | APPLY GROUP BY CODE LOGIC - PART 2 of 3 ...'))))
            itrColrs_ <- 0;
            for (lvl in vsGroupByVars_) {

              itrColrs_ <- itrColrs_ + 1
              lyrId_FINAL_ <- paste0(vsPlotSPP[3], ' - ', lvl)
              vsCntrlLYRs_ <- base::append(vsCntrlLYRs_, lyrId_FINAL_)
              dfInGPS_Sub_ <-  subset(dfInGPS_, dfInGPS_[[sGroupBy]] == lvl)
              dfInGPS_Sub_ <- dfInGPS_Sub_[order(as.numeric(dfInGPS_Sub_[[sVarLAT]]), decreasing = T), ]
              dfInGPS_Sub_ <- dfInGPS_Sub_[order(as.numeric(dfInGPS_Sub_[[sVarLON]]), decreasing = F), ]

              base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | APPLY GROUP BY CODE LOGIC - PART 3 of 3 ...'))))
              if (sPlotRate == enShowRTE$NONE) {

                leafMapOut_ <- leaflet::addCircleMarkers(
                  map = leafMapOut_, data = dfInGPS_Sub_, group = lyrId_FINAL_,
                  lat = ~as.numeric(dfInGPS_Sub_[[sVarLAT]]), lng = ~as.numeric(dfInGPS_Sub_[[sVarLON]]),
                  radius = 3.15, fillOpacity = 1.0, weight = 1.75, color = 'black', fillColor = vsColrs_[itrColrs_], label = lyrId_FINAL_
                );
                # leafMapOut_ <- leaflet::addCircles(
                #   map = leafMapOut_, data = dfInGPS_,
                #   lat = ~as.numeric(.data[[sVarLAT]]), lng = ~as.numeric(.data[[sVarLON]]),
                #   radius = nRadius_VMS_Circles_, fillOpacity = 1.0, weight = nStrokeW_VMS_, color = strStrokeColr_VMS_,
                #   group = lyrId_VMS_Canan_02_, fillColor = ~FillColrs, label = ~paste0(Name, " ( ", GPS_Date, " ) | Speed: ", Speed_kts, " kts")
                # )

              } else {

              }
              # print(head(dfInGPS_Sub_[[sGroupBy]], 1))
            }



            base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | COMPILING HEX-BINNED DATA - PART 1 of 3 !!!'))))
            if (bShowHexGrid) {

              for (row in 1:nrow(dfInGPS_)) {
                # dfInGPS_$HexVal <- round((as.numeric(dfInGPS_[[sVarY]][row]) / 260000) * 100, 2)
                dfInGPS_$HexVal <- base::round((base::as.numeric(dfInGPS_[[sVarY]][row]) / sum(as.numeric(dfInGPS_[[sVarY]]))) * 100, 2)
                base::print(dfInGPS_$HexVal[row])
              }

              base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | COMPILING HEX-BINNED DATA - PART 2 of 3 !!!'))))
              dfInGPS_$lat <- dfInGPS_[[sVarLAT]]
              dfInGPS_$lng <- dfInGPS_[[sVarLON]]

              # enModeHexAGGR$MAX
              # nHexAggregationMODE_ <- enModeHexAggr;
              sHexAggrMode_ <- "max";
              ### sHexAggrMode_ <- NULL;
              ### base::switch(
              ###   enModeHexAggr,
              ###   1 = sHexAggrMode_ <- "count",
              ###   2 = sHexAggrMode_ <- "sum",
              ###   3 = sHexAggrMode_ <- "max",
              ###   4 = sHexAggrMode_ <- "min",
              ###   5 = sHexAggrMode_ <- "mean",
              ###   6 = sHexAggrMode_ <- "median",
              ###   sHexAggrMode_ <- "sum"
              ### );

              base::message(base::cat(blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | COMPILING HEX-BINNED DATA - PART 3 of 3 !!!'))))
              leafMapOut_ <- leaflethex::addHexbin(
                duration = 28,
                map = leafMapOut_, data = dfInGPS_, uniformSize = TRUE, opacity = 0.28, # group = lyrId_FINAL_,
                radius = 28, colorvar = sVarY, highEndColor = "red", lowEndColor = "green", colorSummaryFunction = sHexAggrMode_
                # sizevar = "SP_Scientific_Name", # sizeSummaryFunction = "sum",
              );
            }

          } else {
            base::message(
              base::cat(
                crayon::blue(
                  base::paste0(
                    ' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),
                    ' | APPLY SPECIES DATA SELECTION REQUEST [ ALL ] ...'
                  )
                )
              )
            );
          }
        } else {   # <- APPLY SELECT or FILTER to the 'GroupBy' variable levels or values (i.e. USE ONLY SPECIFIED GROUPBY VALUES or LEVELS) ...

        }

      } else {    # <- SAVE the final (output) map[s] - only return a Leaflet Map Object to the function call ...

      }
      ###? } else if (enTypeGPS == enTypeGPS$VMS) {

      ###? } else if (enTypeGPS == enTypeGPS$BOTH) {

      ###? }
      # dfSeafoCA_ <- base::readRDS(file=file.path(sPathUtils, '00_SEAFO_Convention_Area_WGS84_Transformed.rds'))
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Imported SEAFO Convention Area (SEAFO CA) Polygon Data !!!'))))


      ####---   3.1 B - Import Polygon _ SEAFO Closed Areas _   ---####
      # dfSeafoCLAs_ <- base::readRDS(file=file.path(sPathUtils, '01_SEAFO_Closed_Areas_WGS84_Transformed.rds'))
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Imported SEAFO Closed Areas (CLAs) Polygon Data !!!'))))

      ####---   3.1 C - Import Polygon _ Africa & SEAFO EEZs _   ---####
      # dfSeafoEEZs_ <- base::readRDS(file=file.path(sPathUtils, '02_EEZ_v11_(2019)_WGS84_Transformed_(HIGH_RES)_AFRICA_SEAFO.rds'))
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Imported Exclusive Economic Zones (EEZs) Polygon Data !!!'))))


      ###   < --- --- --- >   ####


      ####---   3.2 - Convert Polygon Data into Map Layers   ---####
      base::message(base::cat(crayon::blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | CONVERT IMPORTED POLYGON DATA INTO MAP LAYERS ...'))))

      ####---   3.2 A - Sort the SEAFO CA Data Layer by Area ID   ---####
      # dfSeafoCA_ <- dfSeafoCA_[base::order(dfSeafoCA_$ET_ID), ]   # Sorts the row data in Alpha-Numeric (ascending order) based on the 'ET_ID' Variable [column] Values !!

      ####---   3.2 B - Add 'Special Variable Tags' as required   ---####
      # dfSeafoCA_$AreaTag <- ifelse(base::nchar(dfSeafoCA_$ET_ID) == 1, base::paste0('SEAFO CA - Sub-Area ', dfSeafoCA_$ET_ID), base::paste0('SEAFO CA - Division ', dfSeafoCA_$ET_ID))

      ####---   3.2 C - Add 'SF' Headers to Polygon Data Frames _ EEZs _   ---####
      # Benguela Current Convention - BCC - Member States ONLY !!
      # base::names(dfSeafoEEZs_)
      # raster::head(dfSeafoEEZs_, 7)
      # sfLyr_EEZs_ <- sfheaders::sf_polygon(
      #   y = "lat",
      #   x = "long",
      #   keep = TRUE,   # Preserves ALL the columns from the original input data frame (into the output 'sf' data object) !!
      #   obj = dfSeafoEEZs_,
      #   polygon_id = 'group'   # "id",
      # )
      # base::class(sfLyr_EEZs_)
      # raster::head(sfLyr_EEZs_, 7)
      base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Converted Exclusive Economic Zones (EEZs) Data to Map Layer !!!'))))

      ####---   3.2 D - Add 'SF' Headers to Polygon Data Frames _ SEAFO CA _   ---####
      # base::names(dfSeafoCA_)
      # raster::head(dfSeafoCA_, 7)
      # base::summary(dfSeafoCA_$piece)
      # sfLyr_SEAFO_CA_ <- sfheaders::sf_polygon(
      #   y = "lat",
      #   x = "long",
      #   keep = TRUE,   # Preserves ALL the columns from the original input data frame (into the output 'sf' data object) !!
      #   obj = dfSeafoCA_,
      #   polygon_id = 'group' # "id",
      # )
      # base::class(sfLyr_SEAFO_CA_)
      # raster::head(sfLyr_SEAFO_CA_, 7)
      base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Converted SEAFO Convention Area (SEAFO CA) Data to Map Layer !!!'))))

      ####---   3.2 D - Add 'SF' Headers to Polygon Data Frames _ SEAFO Closed Areas _   ---####
      # base::names(dfSeafoCLAs_)
      # raster::head(dfSeafoCLAs_, 7)
      # base::summary(dfSeafoCLAs_$piece)
      # sfLyr_SEAFO_CLAs_ <- sfheaders::sf_polygon(
      #   y = "lat",
      #   x = "long",
      #   keep = TRUE,   # Preserves ALL the columns from the original input data frame (into the output 'sf' data object) !!
      #   obj = dfSeafoCLAs_,
      #   polygon_id = 'group', # "id",
      # )
      # base::class(sfLyr_SEAFO_CLAs_)
      # base::head(sfLyr_SEAFO_CLAs_, 7)
      base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Converted SEAFO CLosed Areas (SEAFO CLAs) Data to Map Layer !!!'))))


      ###   < --- --- --- >   ####


      ####--- 3.3 - Prime SEAFO CA Division & Sub-Area Tags ---####
      # vsTagsSEAFO_ <- c(   'A',   'A1',    'B',   'B1',    'C',   'C1',    'D',   'D1')
      # vsTagsSEAFO_FULL_ <- c(   'Division A',   'Sub-Area A1',    'Division B',   'Sub-Area B1',    'Division C',   'Sub-Area C1',    'Division D',   'Sub-Area D1')
      # vnTagsLATs_  <- c(-01.15, -06.15, -21.15, -21.15, -29.15, -29.15, -41.15, -45.85)
      # vnTagsLONs_  <- c(-19.50,  -4.35, -19.50,   3.35, -19.50,   3.35, -19.50,   7.45)


      ###   < --- --- --- >   ####


      ####---   3.4 - Compile Leaflet Base Map   ---####
      base::message(base::cat(crayon::blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | COMPILE LEAFLET BASEMAP ...'))))

      ####---   3.4 A - Initialize Map Control Layers Vector   ---####
      # lyrId_CA_ <- 'SEAFO CA (v01, 2008)'
      # vsCntrlLYRs_ <- c(lyrId_CA_)   # <- Prime the vector with the 1st entry -> 'SEAFO CA' !!

      ####---   3.4 B - Initialize the Leaflet Map Object   ---####
      # leafMap_ <- leaflet();
      # leafMapSansZoom_ <- leaflet(options = leafletOptions(zoomControl = FALSE));
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Leaflet Map Object intialized ...'))))

      ####---   3.4 C - Add Scale Bar Map Object   ---####
      # addGraticule() %>%
      # leafMap_ <- addScaleBar(map=leafMap_, "bottomleft")
      # leafMapSansZoom_ <- addScaleBar(map=leafMapSansZoom_, "bottomleft")
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Leaflet Map Scale Bar Object added ...'))))

      ####---   3.4 D - Set Maps [ init ] View Port Bounds   ---####
      # setView(lng = 5.00114, lat = -24.97204, zoom = 3.49)
      # leafMap_ <- fitBounds(map=leafMap_, lng1 = -25.25, lat1 = 2.75, lng2 = 31.25, lat2 = -52.05)
      # leafMapSansZoom_ <- fitBounds(map=leafMapSansZoom_, lng1 = -25.25, lat1 = 2.75, lng2 = 31.25, lat2 = -52.05)
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Leaflet Map [viewport] bounds set ...'))))

      ####---   3.4 E - Add Measurement Tool Map Object   ---####
      # Only add the Measure Control the the Main Leaflet Map - no need to add it to the 'sansZoom' map...
      # leafMap_ <- addMeasure(map=leafMap_, localization="na", position="topleft", activeColor="#3D535D", completedColor="#7D4479",
      #                        primaryAreaUnit="hectares", secondaryAreaUnit="sqmeters", primaryLengthUnit="kilometers")
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Leaflet Map Measurement Tool added ...'))))

      ####---   3.4 F - Add Mouse Pointer Real-time Tracker Map Object   ---####
      # Add the Mouse Point Co-ordinate Real-time Tracking Information...
      # leafMap_ <- leafMap_ %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Leaflet Map Mouse Pointer Tracking Tool added ...'))))

      ####---   3.4 G - Add Base Map Tiles & Attributes   ---####
      # addTiles() %>% addProviderTiles("Esri.WorldTopoMap", group = "ESRI Topo") %>%
      # addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
      # addProviderTiles("Esri.OceanBasemap", group = "ESRI World Ocean") %>%
      # zoomMin_ <- 0; zoomMax_ <- 12;
      # urlBaseMap_ <- NULL; attrBaseMap_ <- NULL; grpBaseMap_ <- NULL;
      # if (enMapSource == MapSRC$OCEAN) {
      #   zoomMin_ <- 2; zoomMax_ <- 10;
      #   grpBaseMap_ <- 'ESRI OceanBaseMap'
      #   urlBaseMap_ <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}")
      #   attrBaseMap_ <- paste0("Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri")
      # } else if (enMapSource == MapSRC$NATGEO) {
      #   zoomMin_ <- 2; zoomMax_ <- 14;
      #   grpBaseMap_ <- 'ESRI NatGeoWorldMap'
      #   urlBaseMap_ <- paste0('https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}')
      #   attrBaseMap_ <- paste0('Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC')
      # }
      # leafMap_ <- addTiles(map=leafMap_, group=grpBaseMap_, urlTemplate = urlBaseMap_, attribution = attrBaseMap_, options = providerTileOptions(minZoom = zoomMin_, maxZoom = zoomMax_))
      # leafMapSansZoom_ <- addTiles(map=leafMapSansZoom_, group=grpBaseMap_, urlTemplate = urlBaseMap_, attribution = attrBaseMap_, options = providerTileOptions(minZoom = zoomMin_, maxZoom = zoomMax_))
      # base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Leaflet Map base map tiles selected & added ...'))))


      ###   < --- --- --- >   ####


      ####---   3.5 - Add Base Map Layers   ---####
      base::message(base::cat(crayon::blue(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | FINALIZE LEAFLET BASEMAP LAYERS ...'))))

      ####---   3.5 A - Add Map Layer _ SEAFO CA _   ---####
      # if (bShowCA) {
      #   # lyrId_CA_ <- 'SEAFO CA (v01, 2008)'
      #   leafMap_ <- addPolygons(map=leafMap_, data=sfLyr_SEAFO_CA_, weight=nLineWidthCA, fillColor=sColrFillCA, col=sColrLineCA,
      #                           fillOpacity=nFillAlphaCA, opacity = nLineAlphaCA, label=~AreaTag, popup=~AreaTag, group=lyrId_CA_)
      #   leafMapSansZoom_ <- addPolygons(map=leafMapSansZoom_, data=sfLyr_SEAFO_CA_, weight=nLineWidthCA, fillColor=sColrFillCA, col=sColrLineCA,
      #                                   fillOpacity=nFillAlphaCA, opacity = nLineAlphaCA, label=~AreaTag, popup=~AreaTag, group=lyrId_CA_)
      #   base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Added Map Layer - SEAFO Convention Area ...'))))
      # }

      ####---   3.5 B - Add Map Layer _ Africa & SEAFO EEZs _   ---####
      # if (bShowEEZs) {
      #   lyrId_EEZs_ <- 'EEZs - Africa & SEAFO (v11, 2019)'
      #   vsCntrlLYRs_ <- base::append(vsCntrlLYRs_, lyrId_EEZs_)
      #   leafMap_ <- addPolygons(map=leafMap_, data=sfLyr_EEZs_, weight=nLineWidthEEZs, col=sColrLineEEZs, opacity = nLineAlphaEEZs,
      #                           fillColor=sColrFillEEZs, fillOpacity=nFillAlphaEEZs, label=~EezTag, popup=~EezTag, group=lyrId_EEZs_)
      #   leafMapSansZoom_ <- addPolygons(map=leafMapSansZoom_, data=sfLyr_EEZs_, weight=nLineWidthEEZs, col=sColrLineEEZs, opacity = nLineAlphaEEZs,
      #                                   fillColor=sColrFillEEZs, fillOpacity=nFillAlphaEEZs, label=~EezTag, popup=~EezTag, group=lyrId_EEZs_)
      #   base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Added Map Layer - Exclusive Economic Zones (EEZs) ...'))))
      # }

      ####---   3.5 C - Add Map Layer _ SEAFO Closed Areas _   ---####
      # if (bShowCLAs) {
      #   lyrId_CLAs_ <- 'SEAFO Closed Areas'
      #   vsCntrlLYRs_ <- base::append(vsCntrlLYRs_, lyrId_CLAs_)
      #   leafMap_ <- addPolygons(map=leafMap_, data=sfLyr_SEAFO_CLAs_, weight=nLineWidthCLAs, col=sColrLineCLAs, opacity = nLineAlphaCLAs,
      #                           fillColor=sColrFillCLAs, fillOpacity=nFillAlphaCLAs, label=~Name, popup=~Name, group=lyrId_CLAs_)
      #   leafMapSansZoom_ <- addPolygons(map=leafMapSansZoom_, data=sfLyr_SEAFO_CLAs_, weight=nLineWidthCLAs, col=sColrLineCLAs, opacity = nLineAlphaCLAs,
      #                                   fillColor=sColrFillCLAs, fillOpacity=nFillAlphaCLAs, label=~Name, popup=~Name, group=lyrId_CLAs_)
      #   base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Added Map Layer - SEAFO Closed Areas (CLAs) ...'))))
      # }

      ####---   3.5 D - Add Map Layer _ SEAFO Area Tags _   ---####
      # if (bShowLabels) {
      #   vsAreaTagsFINAL_ <- NULL
      #   lyrId_AreaTags_ <- 'SEAFO Labels'
      #   vsCntrlLYRs_ <- base::append(vsCntrlLYRs_, lyrId_AreaTags_)
      #   if (sLabelsFull) {
      #     vsAreaTagsFINAL_ <- vsTagsSEAFO_FULL_
      #   } else {
      #     vsAreaTagsFINAL_ <- vsTagsSEAFO_
      #   }
      #   leafMap_ <- addLabelOnlyMarkers(map=leafMap_, lng=vnTagsLONs_, lat=vnTagsLATs_, label=vsAreaTagsFINAL_,
      #                                   group=lyrId_AreaTags_, labelOptions=labelOptions(
      #                                     noHide=T, textOnly=T, # textsize=85,
      #                                     style = list(
      #                                       "color" = "gray30",
      #                                       "font-weight" = "bold",
      #                                       # "font-family" = "serif",
      #                                       # "font-style" = "italic",
      #                                       # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      #                                       "font-size" = "14px"
      #                                       # "border-color" = "rgba(0,0,0,0.5)"
      #                                       )
      #                                     )
      #                                   )
      #   leafMapSansZoom_ <- addLabelOnlyMarkers(map=leafMapSansZoom_, lng=vnTagsLONs_, lat=vnTagsLATs_, label=vsAreaTagsFINAL_,
      #                                           group=lyrId_AreaTags_, labelOptions=labelOptions(
      #                                             noHide=T, textOnly=T, # textsize=85,
      #                                             style = list(
      #                                               "color" = "gray30",
      #                                               "font-weight" = "bold",
      #                                               # "font-family" = "serif",
      #                                               # "font-style" = "italic",
      #                                               # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      #                                               "font-size" = "14px"
      #                                               # "border-color" = "rgba(0,0,0,0.5)"
      #                                               )
      #                                             )
      #                                           )
      #   base::message(base::cat(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(),' | Added Map Layer - SEAFO Area Labels ...'))))
      # }

      ####---   3.5 E - Add Map Layer Controller Object   ---####
      print(vsCntrlLYRs_)
      if (bShowLyrCntrlr) {
        leafMapOut_ <- addLayersControl(
          # baseGroups = vsBaseLayers,       # The BASE Layers Control Vector (with 3 BASE Layers included)
          overlayGroups = vsCntrlLYRs_,      # The Layers Control Vector (with all active layers included)
          map=leafMapOut_, position=lstLocControls,
          options=layersControlOptions(collapsed = FALSE),
        ) %>% htmlwidgets::onRender(   # Adds a Header ( title ) to the Layer Control Box !!
          base::paste0(" function() { $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">", sTitleLyrCntrlr_,"</label>'); } ")
        )
      }

      ####---   3.5 F - Compile Final Map Result   ---####
      print(leafMapOut_)   # <- Outputs the 'LeafMap' to the R Studio Viewer window ...
      # lstDataOUT_ <- base::list('BaseMap'=leafMapSansZoom_, 'LeafMap'=leafMap_);

    }



    ###   < --- --- --- --- --- --- ---  >   ####



    ####---   SECTION 4 - FUNCTION OUTPUTS - Define any required function outputs here   ---####
    #   Capture Function END DateTime   #
    if (!bPrintInfoRF && !bPrintStatusRF && !bPrintArgsRF) {
      beepr::beep(2)   # <- Notify user of FUNCTION EXECUTION END with an audio cue ...
      sDateTimeSTOP_ <- base::Sys.time();
      funcExeDur_ <- base::difftime(sDateTimeSTOP_, sDateTimeSTART_, units='secs');
      infoExecTime_ <- base::paste0('FUNCTION EXECUTION TIME: ', base::round(funcExeDur_, 3), ' seconds');
      base::message(base::paste0(' --> ', TAG_FUNC_ID_SHORT_, ' ', rf_get.egln(), ' | ', 'CUSTOM FUNCTION [ ', TAG_FUNC_ID_LONG_, ' ] ', 'STOPPED [ ', base::format(sDateTimeSTOP_, sDateTimeFormat_),' - ', infoExecTime_, ' ]'));
    }

    # Hand over the final result[s] to the function call ...
    if (!bPrintInfoRF && !bPrintStatusRF && !bPrintArgsRF) {
      base::return(lstDataOUT_);
    } else {
      base::return(rf_suppress.output(lstDataOUT_));
    }

  });   # The 'suppressWarnings({})' closure bracket or brace !!

}


