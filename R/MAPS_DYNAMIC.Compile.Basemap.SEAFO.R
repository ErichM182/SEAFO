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
  snMapSRC=2, ssHeaderWCL="SEAFO Convention Area (CA)", snZoomMin=2, snZoomMax=10,
  sbShowEEZs=TRUE, sbShowCA=TRUE, sbShowClosedAreas=TRUE, sbShowFishFootPrint=TRUE,
  sbShowLabels=TRUE, sbHideWCL=TRUE, sbNotifyRunTimeF=FALSE, ...
) {

  if (sbNotifyRunTimeF) beepr::beep(1);   # <- Notify user of Function Run-Time START (audio cue).
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
  rasBaseC                <- base::c;
  rasBaseCAT              <- base::cat;
  rasBaseLIST             <- base::list;
  rasBaseLOAD             <- base::load;
  rasBaseNCHAR            <- base::nchar;
  rasBaseORDER            <- base::order;
  rasBaseIfELSE           <- base::ifelse;
  rasBasePASTE0           <- base::paste0;
  rasBaseRETURN           <- base::return;
  rasBaseAPPEND           <- base::append;
  rasBaseMESSAGE          <- base::message;
  rasBaseSysTIME          <- base::Sys.time;
  rasBaseDiffTIME         <- base::difftime;
  rasBaseSuppressWARNINGS <- base::suppressWarnings;

  rasBeeprBEEP <- beepr::beep;

  rasHereHERE <- here::here;

  rasHtmlWidgetsOnRENDER <- htmlwidgets::onRender;

  rasLeafletLEAFLET              <- leaflet::leaflet;
  rasLeafletFitBOUNDS            <- leaflet::fitBounds;
  rasLeafletAddMEASURE           <- leaflet::addMeasure;
  rasLeafletAddPOLYGONS          <- leaflet::addPolygons;
  rasLeafletAddScaleBAR          <- leaflet::addScaleBar;
  rasLeafletLabelOPTIONS         <- leaflet::labelOptions;
  rasLeafletOPTIONS              <- leaflet::leafletOptions;
  rasLeafletAddLayersCONTROL     <- leaflet::addLayersControl;
  rasLeafletAddProviderTILES     <- leaflet::addProviderTiles;
  rasLeafletProviderTileOPTIONS  <- leaflet::providerTileOptions;
  rasLeafletAddLabelOnlyMARKERS  <- leaflet::addLabelOnlyMarkers;
  rasLeafletLayersControlOPTIONS <- leaflet::layersControlOptions;

  rasLeafletProvidersLOADED <- leaflet.providers::providers_loaded;

  rasSfHeadersSF_POLYGON <- sfheaders::sf_polygon;

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
  ### sbAudioNote_    <- vsDotsArgs_[[rasSeafoFSID$F_ARGS_BOOL_AUDIO_NOTE]]   %??% FALSE;
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
  vsCntrlLayers_ <- rasBaseC();      # <- Prime the Control Layers vector (with a NULL value) ...
  ssBaseMapSRC_  <- rasBaseIfELSE(   # <- Prime the BaseMap Source Identifier Tag ...
    snMapSRC_ == 1, "ESRI Ocean BaseMap", "ESRI NatGeo World Map"
  );
  rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
    sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
    ssBody = rasBasePASTE0("Base Layer Selected = '", ssBaseMapSRC_, "' !!!")
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
  rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
    sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
    ssBody = rasBasePASTE0("SEAFO Base Layer Added !!!")
  );


  ###  5.2 - Compile the SEAFO Exclusive Economic Zone (EEZ) Layer   ####
  if (sbShowEEZs_) {

    ### 5.2a - Import the SEAFO Exclusive Economic Zone (EEZ) Layer Data ...
    rasBaseLOAD(
      file = rasHereHERE(
        "data", "dfSEAFO_EEZv11.rda"     # <- Read into <active> R Memory as "dfSEAFO_EEZv11" !!!
      )
    );
    dfSEAFO_EEZv11_ <- dfSEAFO_EEZv11;   # <- Assign to a local <func-intern> R Object ...

    ### 5.2b - Convert SEAFO Exclusive Economic Zone (EEZ) layer into correct format ...
    sfLyrEEZs_SEAFO_ <- base::suppressWarnings(
      sfheaders::sf_polygon(             # <- Add 'SF' Headers to Polygon Shapefile Data Frames ...
        obj = dfSEAFO_EEZv11_, x = "long", y = "lat",
        polygon_id = 'group',   # "id",
        keep = TRUE                      # <- Preserves ALL columns from original input data frame
      )                                  #    (into the output 'sf' data object) !!!
    );

    ### 5.2c - Plot <add> Spatial <data> Layer to Leaflet BaseMap ...
    ssLyrId_EEZs_ <- "EEZs - Africa & SEAFO (v11, 2019)";
    vsCntrlLayers_ <- rasBaseAPPEND(vsCntrlLayers_, ssLyrId_EEZs_);
    coBaseMapLM_ <- rasLeafletAddPOLYGONS(
      map = coBaseMapLM_, data = sfLyrEEZs_SEAFO_,
      weight = 1.5, color = 'grey', opacity = 0.5, fillColor = 'white',
      fillOpacity = 0.28, label = ~EezTag, popup = ~EezTag, group = ssLyrId_EEZs_
    );

    rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
      sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
      ssBody = rasBasePASTE0("SEAFO EEZ Layer Added !!!")
    );

  }


  ###  5.3 - Compile the SEAFO Convention Area (CA) Layer   ####
  if (sbShowCA_) {

    ### 5.3a - Import the SEAFO Convention Area (CA) Layer Data ...
    rasBaseLOAD(
      file = rasHereHERE(
        "data", "dfSEAFO_CA.rda"   # <- Read into <active> R Memory as "dfSEAFO_CA" !!!
      )
    );
    dfSEAFO_CA_ <- dfSEAFO_CA;   # <- Assign to a local <func-intern> R Object ...

    ### 5.3b - Sort SEAFO CA Data Layer by Area IDs ...
    dfSEAFO_CA_ <- dfSEAFO_CA_[               # <- Sort entire data set Alpha-Numerically (ascending
      rasBaseORDER(dfSEAFO_CA_[["ET_ID"]]),   #     order) on the 'ET_ID' Variable [column] values.
    ];

    ### 5.3c - Add 'Special Variable Tags' as required ...
    dfSEAFO_CA_[["AreaTag"]] <- rasBaseIfELSE(
      rasBaseNCHAR(dfSEAFO_CA_[["ET_ID"]]) == 1,
      rasBasePASTE0("SEAFO CA - Sub-Area ", dfSEAFO_CA_[["ET_ID"]]),
      rasBasePASTE0("SEAFO CA - Division ", dfSEAFO_CA_[["ET_ID"]])
    );

    ### 5.3d - Convert SEAFO Convention Area (CA) layer into correct format ...
    sfLyrSEAFO_CA_ <- rasBaseSuppressWARNINGS(
      rasSfHeadersSF_POLYGON(
        obj = dfSEAFO_CA_, x = "long", y = "lat",
        polygon_id = "group", # "id",
        keep = TRUE                     # <- Preserves ALL columns from original input data frame
      )                                 #    (into the output 'sf' data object) !!!
    );

    ### 5.3e - Plot <add> Spatial <data> Layer to Leaflet BaseMap ...
    ssLyrId_SEAFO_CA_ <- "SEAFO CA";
    vsCntrlLayers_ <- rasBaseAPPEND(vsCntrlLayers_, ssLyrId_SEAFO_CA_);
    coBaseMapLM_ <- rasLeafletAddPOLYGONS(
      map = coBaseMapLM_, data = sfLyrSEAFO_CA_, weight = 1.5, color = 'black',
      fillOpacity = 0, label = ~AreaTag, popup = ~AreaTag, group = ssLyrId_SEAFO_CA_
    );

    ### 5.3f - Add Layer - SEAFO Area Tags ...
    if (sbShowCA_ && sbShowLabels_) {

      ### 5.3fi - Compile Data for SEAFO Area Tags ...
      vsSeafoTags_ <- rasBaseC(   "A",   "A1",    "B",   "B1",    "C",   "C1",    "D",   "D1");
      vnLatsTags_  <- rasBaseC(-01.15, -06.15, -21.15, -21.15, -29.15, -29.15, -41.15, -45.85);
      vnLonsTags_  <- rasBaseC(-19.50,  -4.35, -19.50,   3.35, -19.50,   3.35, -19.50,   7.45);

      ### 5.3fii - Compile Data for SEAFO Area Tags ...
      ssLyrId_AreaTags_ <- "SEAFO Labels";
      vsCntrlLayers_ <- rasBaseAPPEND(vsCntrlLayers_, ssLyrId_AreaTags_);
      coBaseMapLM_ <- rasLeafletAddLabelOnlyMARKERS(
        map = coBaseMapLM_, lng = vnLonsTags_, lat = vnLatsTags_, label = vsSeafoTags_,
        group = ssLyrId_AreaTags_, labelOptions = rasLeafletLabelOPTIONS(
          noHide = TRUE, textOnly = TRUE, textsize = 85
        )
      );
    }

    rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
      sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
      ssBody = rasBasePASTE0("SEAFO Convention Area (CA) Layer Added !!!")
    );

  }


  ###  5.4 - Compile the SEAFO VME Closures (VMECs) Layer   ####
  if (sbShowCA_ && sbShowClosedAreas_) {

    ### 5.4a - Import the SEAFO VME Closures (VMECs) Layer Data ...
    rasBaseLOAD(
      file = rasHereHERE(
        "data", "dfSEAFO_ClosedAreas.rda"   # <- Read into R Memory as "dfSEAFO_ClosedAreas" !!!
      )
    );
    dfSEAFO_ClosedAreas_ <- dfSEAFO_ClosedAreas;   # <- Assign to local <func-intern> R Object ...

    ### 5.4b - Convert SEAFO Convention Area (CA) layer into correct format ...
    sfLyrSEAFO_Closed_Areas_ <- rasBaseSuppressWARNINGS(
      rasSfHeadersSF_POLYGON(
        obj = dfSEAFO_ClosedAreas_, x = "long", y = "lat",
        polygon_id = 'group', # "id",
        keep = TRUE                     # <- Preserves ALL columns from original input data frame
      )                                 #    (into the output 'sf' data object) !!!
    )

    ### 5.4c - Plot <add> Spatial <data> Layer to Leaflet BaseMap ...
    ssLyrId_ClosedAreas_ <- "SEAFO Closed Areas";
    vsCntrlLayers_ <- rasBaseAPPEND(vsCntrlLayers_, ssLyrId_ClosedAreas_);
    coBaseMapLM_ <- rasLeafletAddPOLYGONS(
      map = coBaseMapLM_, data = sfLyrSEAFO_Closed_Areas_, weight = 1.5, color = 'red',
      opacity = 0.5, fillColor = 'red', fillOpacity = 0.21, label = ~Name,
      popup = ~Name, group = ssLyrId_ClosedAreas_
    );

    rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
      sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
      ssBody = rasBasePASTE0("SEAFO Closed Areas (VMECs) Layer Added !!!")
    );

  }


  ###  5.5 - Compile the SEAFO Controls Layer   ####
  if (sbHideWCL_ != FALSE) {   # <- ONLY RUN this code block if 'sbHideWCL' is set to 'FALSE' !!!

    ## Re-order the Control Layers <logically> ...
    vsCntrlLayers_ <- rasBaseC(
      vsCntrlLayers_[2], vsCntrlLayers_[3], vsCntrlLayers_[4], vsCntrlLayers_[1]
    );

    coBaseMapLM_ <- rasLeafletAddLayersCONTROL(
      # baseGroups = vsBaseLayers,       # The BASE Layers Control Vector (with 3 BASE Layers included)
      overlayGroups = vsCntrlLayers_,     # The Layers Control Vector (with all active layers included)
      map = coBaseMapLM_, position = "topleft",
      options = rasLeafletLayersControlOPTIONS(collapsed = FALSE),
    ) |>
      rasHtmlWidgetsOnRENDER(   # Adds a Header ( title ) to the Layer Control Box !!
        rasBasePASTE0(
          " function() {
              $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">",
              ssHeaderWCL_, "</label>');
            } "
        )
      )
  }



  ####   STEP 06 - Run Function SELF-ID <EXIT> (if requested)   ####
  RCT_FUNC_RUNTIME_STOP_ <- rasBaseSysTIME();
  ### ssFuncRT_ <- rasBaseDiffTIME(
  ###   RCT_FUNC_RUNTIME_STOP_, RCT_FUNC_RUNTIME_START_, units = 'secs'
  ### );
  ### base::message(crayon::yellow(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 54), ' | FUNCTION EXECUTION DURATION: ', base::round(funcExeDur, 3), ' seconds.')))
  ### base::message(crayon::cyan(base::paste0(' --> ', TAG_FUNC_ID, ' ', (segLNM01 + 55), ' | ', 'EXECUTION of CUSTOM FUNCTION [ ', TAG_FUNC_ID_FULL, ' ] ', ' STOPPED on ', base::format(endDateTime, formatDateTime), ' .')))


  ####   STEP 07 - Run Function SELF-ID <EXIT> (if requested)   ####
  rasSeafoInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_LONG_,
    siFuncMode01 = 0L, ssFuncCallerID = ssFuncCallerID_,
    sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_,
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUNTIME_START_, csTimeStop = RCT_FUNC_RUNTIME_STOP_
  );

  ####   STEP 07 - EXIT Functions (results hand-over)   ####
  if (sbNotifyRunTimeF) rasBeeprBEEP(2);   # <- Notify user of Function Run-Time STOP (audio cue).
  rasBaseRETURN(coBaseMapLM_);             # <- Hand-over Function Ouputs to Function Call !!!

}
