#? ### ### ### ### ### ### ###
#' @title Compile the SEAFO Dynamic BaseMap (Leaflet)
#' @name maps_dynamic.compile.basemap.seafo
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
#' supplied via `ssHeaderWCL`. The initial map view is fitted to the SEAFO Convention Area
#' bounding box (i.e. longitudes `-25.25` to `31.25` and latitudes `2.75` to `-52.05`).
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
  RCT_FUNC_CELN_START_   <- 147L;   # <- Function Self-ID Markers (Opening Curly Brace Line #)
  RCT_FUNC_CELN_STOP_    <- 489L;   # <- Function Self-ID Markers (Closing Curly Brace Line #)



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

  coBaseMapLM_ <- rasLeafletAddMEASURE(                         # <- Add Measuring Tool Control ...
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
    );

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
      # baseGroups = vsBaseLayers,       # <- BASE Layers Control Vector (3 BASE Layers included).
      overlayGroups = vsCntrlLayers_,     # <- Layers Control Vector (all active layers included).
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
      );
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


