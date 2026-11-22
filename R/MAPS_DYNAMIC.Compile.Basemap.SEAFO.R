#? ### ### ### ### ### ### ###
#' @title Extract the Code Editor Gutter Line Number (CELN)
#' @name maps_dynamic.compile.basemap.seafo
#' @family CODE Functions (`SEAFO`)
#'
#'
#' @description
#' A SPECIAL HELPER Function that extracts the Code Editor Gutter Line Number (CEGLN a.k.a. CELN) at
#' the code editor line location from which the `maps_dynamic.compile.basemap.seafo()` function was called. This
#' function was created for the purpose of enhancing the debugging, R Package development and
#' real-time (i.e. interactive) code documentation processes from within the SEAFO Suite of R
#' Functions. However, this function may also be used as a stand-alone function by (or in) other,
#' 3rd Party, libraries and custom R functions - as long as the `SEAFO` library is linked (i.e.
#' previously installed and/or specified as a library dependency).
#'
#'
#' @param ssFuncName ([character]) a String or Text (i.e. [character] [vector]) value that specifies
#'                   the Function Identifier (i.e. name) where the R Code Search or Function ID
#'                   look-up should be conducted (i.e. directed at). The look-up is NOT case
#'                   sensitive - both this value and the R function name are case-folded.
#' @param sbRunByForce ([logical]) a Boolean value that specifies whether the function must execute
#'                     its standard code logic even if the R Project Development DEBUG tracker (
#'                     `RCT_IS_DEBUG_RT_MODE_`) was activated (i.e. found in the call stack & set to
#'                     a value of `TRUE`) or not (default: `FALSE`).
#' @param siCallIndex ([integer]) an Integer value that defines the "Call Index" or "Call Sequence"
#'                    in the localized (function <internal>) call stack in reference to when (i.e.
#'                    in which order) the `maps_dynamic.compile.basemap.seafo()` function was called (i.e. from
#'                    inside a function). An index that exceeds the number of
#'                    `maps_dynamic.compile.basemap.seafo()` calls found in the target function returns
#'                    `NA_integer_` (it is never an error).
#' @param sbUseAlias ([logical]) a Boolean value that specifies whether the function should use the
#'                   standard `maps_dynamic.compile.basemap.seafo()` function identifier or not (default:
#'                   `FALSE`). If set to `TRUE` the function will NOT search for the standard
#'                   `maps_dynamic.compile.basemap.seafo(` text, but rather for the exact text supplied via the
#'                   `ssAliasValue` function argument (e.g. `rasSeafoFetchCELN(`).
#' @param ssAliasValue ([character]) a String or Text (i.e. [character] [vector]) value that
#'                     specifies the alias (i.e. re-assigned function identifier) of the default or
#'                     standard `maps_dynamic.compile.basemap.seafo()` function identifier to search for in the
#'                     R function <internal> code. This value IS a Regular Expression (it is passed
#'                     to `grep()`), so a literal opening bracket must be backslash-escaped.
#'
#'
#' @returns
#' * A single [integer] value - the Code Editor Line Number (CELN) - when `ssFuncName` is found
#'   and `siCallIndex` is within range.
#' * `NA_integer_`, with a [warning], when the target function is not found, when `siCallIndex`
#'   exceeds the number of `maps_dynamic.compile.basemap.seafo()` calls it contains, or when an arg is invalid.
#' * `NULL` (invisibly) when the function is neither in DEBUG mode nor forced via
#'   `sbRunByForce = TRUE` - in that case no search is performed at all.
#'
#'
#' @details
#' The CELN is an ABSOLUTE code-editor line number only while the target function carries a
#' source reference (`srcref`) - i.e. under `devtools::load_all()`, or for an install performed
#' with `R CMD INSTALL --with-keep.source`. Without it the line number is relative to the
#' de-parsed function body, and a warning is raised instead of silently returning it.
#'
#'
#' @examples
#' ### Easily debug custom R function code with this <cool> helper function ...
#' library(SEAFO)   # <- Ensures the `SEAFO` library is <already> installed locally.
#'
#'
#'
#' ### Use with any custom R function as follows ...
#' "my.custom.r.func" <- function(x=7, y=3, z=28) {
#'
#'   RCT_TAG_FUNC_ID <- "my.custom.r.func";  # <- ALWAYS TAG (ID) Custom R Functions accordingly !!!
#'
#'   valSUM <- sum(x, y, z);
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " ,
#'       SEAFO::maps_dynamic.compile.basemap.seafo(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         1L                 # <- Define the call order <call sequence> for the
#'       ),                   #    `maps_dynamic.compile.basemap.seafo()` function calls inside the custom
#'                            #    function code. This is the 1st time the
#'                            #    `maps_dynamic.compile.basemap.seafo()` function is called inside this custom
#'                            #    function so the `siCallIndex` value here should == `1L`.
#'       " | Summed all 3 input values < result: ", valSUM," > !!! \n"
#'     )
#'   );
#'
#'   valMEAN <- mean(x, y, z);
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " ,
#'       SEAFO::maps_dynamic.compile.basemap.seafo(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         2L                 # <- Define the call order <call sequence> for the
#'       ),                   #    `maps_dynamic.compile.basemap.seafo()` function calls inside the custom
#'                            #    function code. This is the 2nd time `maps_dynamic.compile.basemap.seafo()`
#'                            #    function is called inside this custom function so the
#'                            #    `siCallIndex` value here should == `2L`.
#'       " | Averaged the 3 input values < result: ", round(valMEAN, 1)," > !!! \n"
#'     )
#'   );
#'
#'   # Notify function's SUCCESSFUL COMPLETION ( <- is only needed for code illustration) ...
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " ,
#'       SEAFO::maps_dynamic.compile.basemap.seafo(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         3L                 # <- Define the call order <call sequence> for the
#'       ),                   #    `maps_dynamic.compile.basemap.seafo()` function calls inside the custom
#'                            #    function code. This is the 3rd time the
#'                            #    `maps_dynamic.compile.basemap.seafo()` function is called inside this custom
#'                            #    function so the `siCallIndex` value here should == `3L`.
#'       " | CUSTOM R-FUNC `", tolower(RCT_TAG_FUNC_ID), "()` SUCESSFULLY EXECUTED !!! \n\n"
#'     )
#'   );
#'
#'   return(
#'     list("SUM" = valSUM, "MEAN" = valMEAN)
#'   );
#' }
#'
#' ## Execute the custom R Function ...
#' my.custom.r.func()
#'
#' ## Outputs from "my.custom.r.func()" should be as follows ...
#' # ➜ my.custom.r.func 9 | Summed all 3 input values < result: 38 > !!!
#' # ➜ my.custom.r.func 24 | Averaged the 3 input values < result: 12.7 > !!!
#' # ➜ my.custom.r.func 39 | CUSTOM R-FUNC `my.custom.r.func()` SUCESSFULLY EXECUTED !!!
#'
#' # $SUM
#' # [1] 38
#'
#' # $MEAN
#' # [1] 12.66667
#'
#'
#' @export
#? ### ### ###
"maps_dynamic.compile.basemap.seafo" <- function(
    snMapSRC=2, ssHeaderWCL="SEAFO Convention Area", snZoomMin=2, snZoomMax=10, sbShowEEZs=TRUE,
    sbShowCA=TRUE, sbShowClosedAreas=TRUE, sbShowFishFootPrint=TRUE, sbShowLabels=TRUE,
    sbHideWCL=FALSE, sbNotifyRunTimeF=FALSE, ...
) {

  if (sbNotifyRunTimeF) beepr::beep(1);   # <- Notify user of Function Run-Time START (audio cue) ...
  RCT_FUNC_RUNTIME_START_ <- base::Sys.time();   # <- Captures <active> Date Time !!!


  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_TAG_FUNC_LIBR_ID_  <- "SEAFO";                                     # <- R Library Identifier
  RCT_TAG_FUNC_ID_SHORT_ <- "SEAFO::Basemap.SEAFO";                        # <- FuncID - SHORT
  RCT_TAG_FUNC_ID_121_   <- "maps_dynamic.compile.basemap.seafo";          # <- FuncID - 1:1 (121)
  RCT_TAG_FUNC_ID_LONG_  <- "SEAFO::MAPS_Dynamic.Compile.Basemap.SEAFO";   # <- FuncID - LONG
  RCT_FUNC_CELN_START_   <- 140L;   # <- Function Self-ID Markers (Opening Curly Brace Line #)
  RCT_FUNC_CELN_STOP_    <- 427L;   # <- Function Self-ID Markers (Closing Curly Brace Line #)



  ####   STEP 02 - Define "Local Aliases" for ALL Function Actions   ####
  ###    NOTES: Local aliases enable greppable names. This also serves as a concise inventory of
  ###           external functions used in this R function; while at the same time shielding these
  ###           calls from R's infamous search-path (name) masking behaviour.
  rasBaseC       <- base::c;
  rasBaseCAT     <- base::cat;
  rasBaseLIST    <- base::list;
  rasBaseIfELSE  <- base::ifelse;
  rasBasePASTE0  <- base::paste0;
  rasBaseRETURN  <- base::return;
  rasBaseMESSAGE <- base::message;

  rasLeafletLEAFLET             <- leaflet::leaflet;
  rasLeafletFitBOUNDS           <- leaflet::fitBounds;
  rasLeafletAddMEASURE          <- leaflet::addMeasure;
  rasLeafletAddScaleBAR         <- leaflet::addScaleBar;
  rasLeafletOPTIONS             <- leaflet::leafletOptions;
  rasLeafletAddProviderTILES    <- leaflet::addProviderTiles;
  rasLeafletProviderTileOPTIONS <- leaflet::providerTileOptions;

  rasLeafletProvidersLOADED <- leaflet.providers::providers_loaded;

  `%??%`                     <- SEAFO::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasSeafoDEVS               <- SEAFO::RENV_DEVS;
  rasSeafoFSID               <- SEAFO::RENV_FSID;
  rasSeafoICONS              <- SEAFO::RENV_ICONS;
  rasSeafoCOLORS             <- SEAFO::RENV_COLOURS;
  rasSeafoFORMATS            <- SEAFO::RENV_FORMATS;
  rasSeafoInfoPostNOTE       <- SEAFO::info.post.note;
  rasSeafoInfoPostFuncSelfID <- SEAFO::info.post.func.self.id;



  ####   STEP 03 - Internalize ALL Function Arguments   ####
  sbShowCA_            <- sbShowCA;
  snMapSRC_            <- snMapSRC;
  snZoomMin_           <- snZoomMin;
  snZoomMax_           <- snZoomMax;
  sbHideWCL_           <- sbHideWCL;
  sbShowEEZs_          <- sbShowEEZs;
  ssHeaderWCL_         <- ssHeaderWCL;
  sbShowLabels_        <- sbShowLabels;
  sbNotifyRunTimeF_    <- sbNotifyRunTimeF;
  sbShowClosedAreas_   <- sbShowClosedAreas;
  sbShowFishFootPrint_ <- sbShowFishFootPrint;

  vsDotsArgs_     <- rasBaseLIST(...);
  sbPrintPretty_  <- vsDotsArgs_[[rasSeafoFSID$F_ARGS_BOOL_PRINT_PRETTY]] %??% TRUE;
  sbAudioNote_    <- vsDotsArgs_[[rasSeafoFSID$F_ARGS_BOOL_AUDIO_NOTE]]   %??% FALSE;
  sbRunSelfID_    <- vsDotsArgs_[[rasSeafoFSID$F_ARGS_BOOL_RUN_SELF_ID]]  %??% FALSE;
  ssFuncCallerID_ <- vsDotsArgs_[[rasSeafoFSID$F_ARGS_FUNC_CALLER]] %??% "TOP-LVL (rsProjMAIN)";



  ####   STEP 04 - Run Function SELF-ID <ENTRY> (if requested)   ####
  rasSeafoInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_LONG_,
    siFuncMode01 = 1L, ssFuncCallerID = ssFuncCallerID_,
    sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_,
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUNTIME_START_, csTimeStop = RCT_FUNC_RUNTIME_START_
  );



  ####   STEP 05 - Compile Base Map Components   ####
  ###  5.1 - Compile the SEAFO CA BaseMap Layer   ####
  vsCntrlLayers_ <- rasBaseC(ssHeaderWCL_);   # <- Prime the Control Layers vector ...
  ssBaseMapSRC_  <- rasBaseIfELSE(            # <- Prime the BaseMap Source Identifier Tag ...
    snMapSRC_ == 1, "ESRI Ocean BaseMap", "ESRI NatGeo World Map"
  );
  rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
    sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
    ssBody = rasBasePASTE0("Source Selected = `", ssBaseMapSRC_, "` !!!")
  );

  lsProviders_ <- rasLeafletProvidersLOADED()[["providers"]];   # <- Extract the Tile Providers ...
  coBaseMapLM_ <- rasLeafletLEAFLET() |>                        # <- Compile Leaflet Canvas...
    rasLeafletAddProviderTILES(                                 # <- Add <actual> BaseMap tiles ...
      group = ssBaseMapSRC_,
      provider = rasBaseIfELSE(
        snMapSRC_ == 1,
        lsProviders_[["Esri.OceanBasemap"]],
        lsProviders_[["Esri.NatGeoWorldMap"]]
      ),
      options = rasLeafletProviderTileOPTIONS(
        minZoom = snZoomMin_, maxZoom = snZoomMax_
      )
    );

  coBaseMapLM_ <- rasLeafletAddMEASURE(                         # <- Add a Measuring Tool Control ...
    map = coBaseMapLM_, localization = "na", position = "topleft", activeColor = "#3D535D",
    completedColor = "#7D4479", primaryAreaUnit = "hectares", secondaryAreaUnit = "sqmeters",
    primaryLengthUnit = "kilometers"
  );

  coBaseMapLM_ <- rasLeafletAddScaleBAR(                        # <- Add a Map Scale Bar ...
    map = coBaseMapLM_, "bottomleft"
  );

  ### coBaseMapLM_ <- rasLeafletLEAFLET(                            # <- Set ZOOM Snap Configuration ...
  ###   options = rasLeafletOPTIONS(zoomSnap = 0.25, zoomDelta = 0.25)
  ### );

  coBaseMapLM_ <- rasLeafletFitBOUNDS(                          # <- Set OnStart ZOOM Level ...
    map = coBaseMapLM_, lng1 = -25.25, lat1 = 2.75, lng2 = 31.25, lat2 = -52.05
  );








  ####--- < ============================ > ---####
  ####--- STEP 2 - Do whatever processes and/or code logic that need to be done here ---####
  ####--- 3.1 - Read Key SEAFO CA Basemap Data Layers into R Session Memory ---####
  base::message(crayon::blue(
    base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 54),' | READING KEY BASEMAP LAYERS INTO R SESSION MEMORY for INIT-RUN SCRIPT PRIMING PROCESS !!')
  ))

  if (sbShowCA) {
    ####--- 3.1.1 - Read the SEAFO Convention Area (SEAFO CA) Data Frame into R Session Memory ---####
    seafoCAFile <- base::file.path(ssDirMapsData, 'SEAFO_Convention_Area_WGS84_Transformed.rds')
    if (!base::file.exists(seafoCAFile)) base::stop(base::paste0('File not found: ', seafoCAFile))
    dfSeafoCA <- base::readRDS(file = seafoCAFile)

    ####--- 3.1.2 - Read the SEAFO Closed Areas Data Frame into R Session Memory ---####
    if (sbShowClosedAreas) {
      closedAreasFile <- base::file.path(ssDirMapsData, 'SEAFO_Closed_Areas_WGS84_Transformed.rds')
      if (!base::file.exists(closedAreasFile)) base::stop(base::paste0('File not found: ', closedAreasFile))
      dfSeafoClosedAreas <- base::readRDS(file = closedAreasFile)
    }
  }

  if (sbShowEEZs) {
    ####--- 3.1.3 - Read the African EEZ Data Frame into R Session Memory ---####
    eezAfricaFile <- base::file.path(ssDirMapsData, '01_EEZ_v11_(2019)_WGS84_Transformed_(HIGH_RES)_AFRICA.rds')
    if (!base::file.exists(eezAfricaFile)) base::stop(base::paste0('File not found: ', eezAfricaFile))
    dfEezAfrica <- base::readRDS(file = eezAfricaFile)
  }

  base::message(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 54),' | BASEMAP LAYERS SECCESSFULLY READ INTO R SESSION MEMORY !!')))


  ####--- 3.2 - Prime Key Basemap Data Layers Accordingly ---####
  base::message(crayon::blue(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 54),' | PRIMING KEY BASEMAP LAYERS ( as required by INIT-RUN SCRIPT PROCESS ) !!')))

  if (sbShowCA) {

    ####--- 3.2.1 - Sort the SEAFO CA Data Layer by Area ID ---####
    dfSeafoCA <- dfSeafoCA[base::order(dfSeafoCA$ET_ID), ]   # Sorts the row data in Alpha-Numeric (ascending order) based on the 'ET_ID' Variable [column] Values !!

    ####--- 3.2.2 - Add 'Special Variable Tags' as required ---####
    dfSeafoCA$AreaTag <- base::ifelse(nchar(dfSeafoCA$ET_ID) == 1, base::paste0('SEAFO CA - Sub-Area ', dfSeafoCA$ET_ID), base::paste0('SEAFO CA - Division ', dfSeafoCA$ET_ID))


    # SEAFO Convention Area (SEAFO CA) ...
    sfLyrSEAFO_CA <- base::suppressWarnings(
      sfheaders::sf_polygon(
        obj = dfSeafoCA,
        x = "long",
        y = "lat",
        polygon_id = 'group', # "id",
        keep = TRUE   # Preserves ALL the columns from the original input data frame (into the output 'sf' data object) !!
      )
    )


    # SEAFO Closed Areas (SEAFO Closures) ...
    if (sbShowClosedAreas) {

      sfLyrSEAFO_Closed_Areas <- base::suppressWarnings(
        sfheaders::sf_polygon(
          obj = dfSeafoClosedAreas,
          x = "long",
          y = "lat",
          polygon_id = 'group', # "id",
          keep = TRUE   # Preserves ALL the columns from the original input data frame (into the output 'sf' data object) !!
        )
      )

    }


    ####--- 3.2.4 - Prime the SEAFO CA Sub-Area and Division Tags ---####
    if (sbShowLabels) {
      SeafoTags <- rasBaseC(   'A',   'A1',    'B',   'B1',    'C',   'C1',    'D',   'D1');
      LatsTags  <- rasBaseC(-01.15, -06.15, -21.15, -21.15, -29.15, -29.15, -41.15, -45.85);
      LonsTags  <- rasBaseC(-19.50,  -4.35, -19.50,   3.35, -19.50,   3.35, -19.50,   7.45);
    }

  }

  if (sbShowEEZs) {

    ####--- 3.2.3 - Add 'SF' Headers to Polygon Shapefile Data Frames ---####
    # Benguela Current Convention - BCC - Member States ONLY !!
    sfLyrEEZs_Africa <- base::suppressWarnings(
      sfheaders::sf_polygon(
        obj = dfEezAfrica,
        x = "long",
        y = "lat",
        polygon_id = 'group',   # "id",
        keep = TRUE   # Preserves ALL the columns from the original input data frame (into the output 'sf' data object) !!
      )
    )

  }

  ### beepr::beep(2)   # Notify user of task completion with an audio cue...
  base::message(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 54),' | BASEMAP LAYERS SUCCESSFULLY PRIMED FOR INIT-RUN SCRIPT PROCESS !!')))


  ####--- 3.3 - Compile the SEAFO CA Leaflet ( Interactive ) Map ---####

  ####--- 3.3.1 - Initialize the Map Control Layers Vector ---####
  vsCntrlLayers <- c('SEAFO CA')   # Prime the vector with a 'NULL' entry !!!
  # vsBaseLayers <- c('ESRI Topo', 'ESRI Aerial', 'ESRI OceanBaseMap')   # Prime the Base Layers Vector -> 'SEAFO CA' !!


  ####--- 3.3.2 - Create the SEAFO CA Basemap ---####
  leafMap <- leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.25, zoomDelta=0.25));

  leafMap <- leaflet::addScaleBar(map = leafMap, "bottomleft")

  # Define the BaseMap Source Information ...
  # SOURCE 01 -> 'ESRI OceanBaseMap'
  grpBaseMap_Source_01 <- 'ESRI Ocean BaseMap'
  urlBaseMap_Source_01 <- "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}"
  attrBaseMap_Source_01 <- "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"

  # SOURCE 02 -> 'ESRI OceanBaseMap'
  grpBaseMap_Source_02 <- 'ESRI NatGeo World Map'
  urlBaseMap_Source_02 <- 'https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}'
  attrBaseMap_Source_02 <- 'Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC'

  # Define the actual Base map attributes & sources to use ...
  grpBaseMap <- grpBaseMap_Source_02;
  urlBaseMap <- urlBaseMap_Source_02;
  attrBaseMap <- attrBaseMap_Source_02;
  leafMap <- leaflet::addTiles(
    map = leafMap, group = grpBaseMap, urlTemplate = urlBaseMap, attribution = attrBaseMap,
    options = providerTileOptions(minZoom = snZoomMin, maxZoom = snZoomMax)
  );

  # Only add the Measure Control the the Main Leaflet Map - no need to add it to the 'sansZoom' map...
  leafMap <- leaflet::addMeasure(
    map = leafMap, localization = "na", position = "topleft", activeColor = "#3D535D",
    completedColor = "#7D4479", primaryAreaUnit = "hectares", secondaryAreaUnit = "sqmeters",
    primaryLengthUnit = "kilometers"
  );

  # setView(lng = 5.00114, lat = -24.97204, zoom = 3.49)
  leafMap <- leaflet::fitBounds(
    map = leafMap, lng1 = -25.25, lat1 = 2.75, lng2 = 31.25, lat2 = -52.05
  );


  ####--- 3.3.3 - Add Layer - SEAFO Convention Area ---####
  if (sbShowCA) {
    lyrId_SEAFO_CA <- 'SEAFO CA';
    vsCntrlLayers <- base::append(vsCntrlLayers, lyrId_SEAFO_CA);
    leafMap <- leaflet::addPolygons(
      map = leafMap, data = sfLyrSEAFO_CA, weight = 1.5, color = 'black', fillOpacity = 0,
      label = ~AreaTag, popup = ~AreaTag, group = lyrId_SEAFO_CA
    );
  }

  ####--- 3.3.4 - Add Layer - SEAFO Area Tags ---####
  if (sbShowCA && sbShowLabels) {
    lyrId_AreaTags <- 'SEAFO Labels'
    vsCntrlLayers <- base::append(vsCntrlLayers, lyrId_AreaTags)
    leafMap <- leaflet::addLabelOnlyMarkers(
      map = leafMap, lng = LonsTags, lat = LatsTags, label = SeafoTags, group = lyrId_AreaTags,
      labelOptions = leaflet::labelOptions(noHide = T, textOnly = T, textsize = 85)
    );
  }


  # Export the SEAFO CA Basemap to the R Session Global Environment ( for use outside this custom function ) !!
  # baseMapSeafoCA <<- leafMap


  ####--- 3.3.5 - Add Layer - SEAFO Closed Areas ---####
  if (sbShowCA && sbShowClosedAreas) {
    lyrId_ClosedAreas <- "SEAFO Closed Areas";
    vsCntrlLayers <- base::append(vsCntrlLayers, lyrId_ClosedAreas);
    leafMap <- leaflet::addPolygons(
      map = leafMap, data = sfLyrSEAFO_Closed_Areas, weight = 1.5, color = 'red', opacity = 0.5,
      fillColor = 'red', fillOpacity = 0.21, label = ~Name, popup = ~Name, group = lyrId_ClosedAreas
    );
  }


  ####--- 3.3.6 - Add Layer - Africa & SEAFO EEZs ---####
  if (sbShowEEZs) {
    lyrId_EEZs <- "EEZs - Africa & SEAFO (v11, 2019)";
    vsCntrlLayers <- base::append(vsCntrlLayers, lyrId_EEZs);
    leafMap <- leaflet::addPolygons(
      map = leafMap, data = sfLyrEEZs_Africa, weight = 1.5, color = 'grey', opacity = 0.5,
      fillColor = 'white', fillOpacity = 0.28, label = ~EezTag, popup = ~EezTag, group = lyrId_EEZs
    );
  }

  # This is the interactive (fully customizable) Leaflet Map for use only inside this custom function !!
  # leafMapSeafoCA <- leafMap

  base::message(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 54),' | SEAFO CA BASEMAP SUCCESSFULLY COMPILED !!')))

  # Add the Mouse Point Co-ordinate Real-time Tracking Information...
  leafMap <- leafMap |>   # The '<<-' outputs this map to the Global R Environment ...
    leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


  ####--- 3.4 - Add User-Requested Map Features to Basemap ---####
  # print(vsCntrlLayers)
  if (sbHideWCL == FALSE) {   # <- ONLY RUN this code block if 'sbHideWCL' is set to 'FALSE' !!!
    leafMap <- addLayersControl(
      # baseGroups = vsBaseLayers,       # The BASE Layers Control Vector (with 3 BASE Layers included)
      overlayGroups = vsCntrlLayers,     # The Layers Control Vector (with all active layers included)
      map = leafMap, position = 'topleft',
      options = layersControlOptions(collapsed = FALSE),
    ) %>%
      htmlwidgets::onRender(   # Adds a Header ( title ) to the Layer Control Box !!
        base::paste0(
          " function() {
              $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">", ssHeaderWCL, "</label>');
            } "
        )
      )
  }



  # base::print(leafMap)   # Prints the Leaflet Maps to the Interactive Viewer of the R-Studio IDE !!



  ####--- 3.5 - Save Maps to Working Directory ---####
  ####--- < ============================ > ---####
  ####--- STEP 4 - FUNCTION OUTPUTS - Define any required function outputs here ---####
  ####--- Capture Function END DateTime   ####
  endDateTime <- base::Sys.time()
  funcExeDur <- base::difftime(endDateTime, startDateTime, units = 'secs')
  base::message(crayon::yellow(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 54), ' | FUNCTION EXECUTION DURATION: ', base::round(funcExeDur, 3), ' seconds.')))
  base::message(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 55), ' | ', 'EXECUTION of CUSTOM FUNCTION [ ', TAG_FUNC_ID_FULL, ' ] ', ' STOPPED on ', base::format(endDateTime, formatDateTime), ' .')))

  beepr::beep(2);   # Notify user of FUNCTION CODE EXECUTION COMPLETION with an audio cue...
  base::return(base::list(MAP = leafMap));

}




### function(
###   ssFuncName="maps_dynamic.compile.basemap.seafo", sbRunByForce=FALSE,
###   siCallIndex=1L, sbUseAlias=FALSE, ssAliasValue="rasSeafoFetchCELN\\("
### ) {
###
###
###   ####   STEP 01 - Define "Function Self-ID" R Objects   ####
###   RCT_RUNTIME_FUNC_START_ <- base::Sys.time();                  # <- Captures <active> Date Time !!!
###   RCT_TAG_FUNC_LIBR_ID_   <- "SEAFO";                           # <- R Library Identifier !!!
###   RCT_TAG_FUNC_ID_SHORT_  <- "SEAFO::Fetch.CELN";               # <- Function ID - SHORT !!!
###   RCT_TAG_FUNC_ID_121_    <- "maps_dynamic.compile.basemap.seafo";          # <- Function ID - 1:1 (121) !!!
###   RCT_TAG_FUNC_ID_LONG_   <- "SEAFO::CODE_Editor.Fetch.CELN";   # <- Function ID - LONG !!!
###
###
###
###   ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
###   ###    NOTES: Local aliases enable greppable names. This also serves as a concise inventory of
###   ###           external functions used in this R function; while at the same time shielding these
###   ###           calls from R's infamous search-path (name) masking behaviour.
###   rasBaseC           <- base::c;
###   rasBaseLIST        <- base::list;
###   rasBaseNROW        <- base::nrow;
###   rasBaseORDER       <- base::order;
###   rasBaseRETURN      <- base::return;
###   rasBaseIfELSE      <- base::ifelse;
###   rasBaseToLOWER     <- base::tolower;
###   rasBaseSeqLEN      <- base::seq_len;
###   rasBasePASTE0      <- base::paste0;
###   rasBaseWARNING     <- base::warning;
###   rasBaseLENGTH      <- base::length;
###   rasBaseIsNA        <- base::is.na;
###   rasBaseAsINTEGER   <- base::as.integer;
###   rasBaseAsNUMERIC   <- base::as.numeric;
###   rasBaseIsNUMERIC   <- base::is.numeric;
###   rasBaseAsCHARACTER <- base::as.character;
###   rasBaseIsCHARACTER <- base::is.character;
###
###   rasSeafoFindCODE   <- SEAFO::devs.find.code.instances;
###   rasSeafoPollRTMODE <- SEAFO::code.poll.r.runtime.mode;
###
###   ## SPECIAL - Constant - TAG - Aliases ...
###
###
###
###   ####   STEP 03 - Internalize ALL Function Arguments   ####
###   ssFuncName_   <- ssFuncName;
###   siCallIndex_  <- siCallIndex;
###   sbRunByForce_ <- sbRunByForce;
###   sbUseAlias_   <- sbUseAlias;
###   ssAliasValue_ <- ssAliasValue;
###
###   ## SPECIAL: Try to locate & extract the 'isDebugMode' logical (boolean) variable
###   ##          <if set or primed elsewhere> in the current <active> R Project ...
###   RCT_RCO_RT_MODE_ <- rasSeafoPollRTMODE();   # <- R Run-Time Mode/State !!!
###   sbIsDEBUG_       <- RCT_RCO_RT_MODE_$IS_DEBUG;
###   if (sbRunByForce_ || sbIsDEBUG_) {   # <- Run standard code logic if either of these is TRUE !!!
###
###     ####   STEP 04 - Trace Function Call Stack Location   ####
###     ## 4.1 - Validate the <caller-supplied> search parameters up-front (fail soft with `NA`) ...
###     ssFindText_ <- rasBaseIfELSE(
###       sbUseAlias_,
###       ssAliasValue_,                    # <- The user-specified "Alias" value to search for !!!
###       "code_editor\\.fetch\\.celn\\("   # <- NEVER CHANGE THIS (100% CORRECT default value) !!!
###     );
###
###     if (!rasBaseIsCHARACTER(ssFuncName_) || rasBaseLENGTH(ssFuncName_) != 1L ||
###         rasBaseIsNA(ssFuncName_)) {
###       rasBaseWARNING(" `ssFuncName` must be a single <non-NA> string -> returning NA !!!");
###       rasBaseRETURN(NA_integer_);
###     }
###
###     if (!rasBaseIsNUMERIC(siCallIndex_) || rasBaseLENGTH(siCallIndex_) != 1L ||
###         rasBaseIsNA(siCallIndex_) || siCallIndex_ < 1L) {
###       rasBaseWARNING(" `siCallIndex` must be a single positive integer -> returning NA !!!");
###       rasBaseRETURN(NA_integer_);
###     }
###     siCallIndex_ <- rasBaseAsINTEGER(siCallIndex_);
###
###     if (!rasBaseIsCHARACTER(ssFindText_) || rasBaseLENGTH(ssFindText_) != 1L ||
###         rasBaseIsNA(ssFindText_)) {
###       rasBaseWARNING(" `ssAliasValue` must be a single <non-NA> char string -> returning NA !!!");
###       rasBaseRETURN(NA_integer_);
###     }
###
###     ## 4.2 - Run an R Function Code Search to locate all instances of this `maps_dynamic.compile.basemap.seafo()`
###     ##       function in the specified <active> R Function <internal function code> ...
###     ##       NOTE: `coRENVs` is emptied because the Global Environment is already searched via
###     ##             `sbIncludeGlobal` - searching it twice would duplicate every single match.
###     rdfFuncCalls_ <- rasSeafoFindCODE(
###       vsTargetLibs = rasBaseC(RCT_TAG_FUNC_LIBR_ID_), coRENVs = rasBaseLIST(),
###       sbVerboseSearch = FALSE, sbIgnoreCase = FALSE, sbWarnNoSrcRef = TRUE,
###       ssFindText = ssFindText_
###     );
###
###     ## 4.3 - Extract only results that match the specified Function ID ...
###     ##       NOTE: BOTH sides are case-folded - `FUNC_NAME` keeps the <declared> source case,
###     ##             so folding only the <argument> would miss every mixed-case function name.
###     rdfFuncCalls_v02_ <- rdfFuncCalls_[
###       rasBaseToLOWER(rasBaseAsCHARACTER(rdfFuncCalls_[["FUNC_NAME"]])) ==
###         rasBaseToLOWER(ssFuncName_), ,
###       drop = FALSE
###     ];
###
###     ## 4.4 - NO MATCH -> return `NA` (never let `seq_len(0)`/`1:0` build an empty INDEX) ...
###     ##       NOTE: <exact> duplicate rows are already prevented structurally (step 4.2 empties
###     ##             `coRENVs`), so NO de-duplication is done here - de-duplicating on the line
###     ##             number would wrongly collapse two genuine calls that share a source line.
###     if (rasBaseNROW(rdfFuncCalls_v02_) == 0L) {
###       rasBaseWARNING(rasBasePASTE0(
###         " `", ssFuncName_, "()` not found in the ", RCT_TAG_FUNC_LIBR_ID_,
###         " R Library or Global Environment -> returning NA !!!"
###       ));
###       rasBaseRETURN(NA_integer_);
###     }
###
###     ## 4.5 - Order the matches by their <source file> position & prime the INDEX variable ...
###     rdfFuncCalls_v03_ <- rdfFuncCalls_v02_[
###       rasBaseORDER(rasBaseAsNUMERIC(rdfFuncCalls_v02_[["MATCH_ID"]]), decreasing = FALSE), ,
###       drop = FALSE
###     ];
###     rdfFuncCalls_v03_[["INDEX"]] <- rasBaseSeqLEN(rasBaseNROW(rdfFuncCalls_v03_));
###
###     ## 4.6 - An OUT-OF-RANGE <call index> returns `NA` (never a silent `numeric(0)`) ...
###     if (siCallIndex_ > rasBaseNROW(rdfFuncCalls_v03_)) {
###       rasBaseWARNING(rasBasePASTE0(
###         " `siCallIndex` = ", siCallIndex_, " but only ", rasBaseNROW(rdfFuncCalls_v03_),
###         " `", ssFuncName_, "()` call(s) found -> returning NA !!!"
###       ));
###       rasBaseRETURN(NA_integer_);
###     }
###
###     ## 4.7 - Extract the Code Editor Line Number (CELN) accordingly ...
###     rdfAtCELN_ <- rdfFuncCalls_v03_[
###       rdfFuncCalls_v03_[["INDEX"]] == siCallIndex_, ,
###       drop = FALSE
###     ];
###
###
###     ####   STEP 05 - Return Function Outputs   ####
###     rasBaseRETURN(rasBaseAsINTEGER(rdfAtCELN_[["LINE_NUMBER"]]));
###
###   }
###
### }
###
