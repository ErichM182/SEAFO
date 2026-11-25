#? ### ### ### ### ### ### ###
#' @title Compile the SEAFO Static BaseMap (Base Graphics)
#' @name maps_static.compile.basemap
#' @family MAPS Functions (SEAFO)
#'
#'
#' @description
#' A Helper Function that compiles a STATIC (i.e. non-interactive) BaseMap of the South East
#' Atlantic Fisheries Organisation (SEAFO) Convention Area (CA) and its surrounding geographic
#' context. The map is drawn on the ACTIVE GRAPHICS DEVICE with the base R `graphics` package,
#' using the SAME spatial layers, layer toggles and area labels as the interactive
#' [SEAFO::maps_dynamic.compile.basemap()] function (i.e. the SEAFO Convention Area, the
#' SEAFO Exclusive Economic Zones and the SEAFO VME Closures), over a landmass & coastline drawn
#' from the `maps` library, and equips the resulting figure with a legend, a map scale bar and a
#' graticule. This function was created to support the routine data-analysis work conducted by
#' the Scientific Committee (SC) of the SEAFO - most notably the publication-ready figures needed
#' in reports and working documents - but may also be used as a quick "situational awareness"
#' BaseMap in other (3rd Party) R libraries and custom R functions, as long as the `SEAFO`
#' library is linked (i.e. previously installed and/or specified as a library dependency).
#'
#'
#' @param snMapSRC ([numeric]) a Numeric (i.e. [numeric]) value that specifies the
#'                BaseMap source <selector>. A value of `1` selects the "ESRI Ocean
#'                BaseMap" palette (i.e. dark bathymetric blue); any other value (i.e. the
#'                default `2`) selects the "ESRI NatGeo World Map" palette (i.e. pale,
#'                land-and-sea pastel). NB: a static map carries NO tile imagery, so the
#'                selector chooses the colour palette that emulates the named provider - that
#'                is, the sea, landmass, coastline and map-furniture tones.
#' @param ssHeaderWCL ([character]) a String or Text (i.e. [character]) value that
#'                    specifies the caption (i.e. header title) of the map legend (i.e. the
#'                    STATIC counterpart of the interactive Layer Control box), which lists
#'                    the layers that were actually drawn (default: "SEAFO Convention
#'                    Area (CA)"). The caption is only rendered when `sbHideWCL = FALSE`.
#' @param snZoomMin ([numeric]) a Numeric (i.e. [numeric]) value that specifies the
#'                minimum zoom level of the BaseMap (default: `2`). NB: a static map has no
#'                zoom concept, so this argument is accepted for SIGNATURE PARITY with
#'                [SEAFO::maps_dynamic.compile.basemap()] and is otherwise unused.
#' @param snZoomMax ([numeric]) a Numeric (i.e. [numeric]) value that specifies the
#'                maximum zoom level of the BaseMap (default: `10`). NB: a static map has no
#'                zoom concept, so this argument is accepted for SIGNATURE PARITY with
#'                [SEAFO::maps_dynamic.compile.basemap()] and is otherwise unused.
#' @param sbShowEEZs ([logical]) a Boolean value that specifies whether the SEAFO Exclusive
#'                  Economic Zones (EEZs) layer must be drawn on the BaseMap (default: `TRUE`).
#'                  The layer is sourced from `./data/dfSEAFO_EEZv11.rda` (v11, 2019).
#' @param sbShowCA ([logical]) a Boolean value that specifies whether the SEAFO Convention Area
#'                (CA) layer must be drawn on the BaseMap (default: `TRUE`). The layer is
#'                sourced from `./data/dfSEAFO_CA.rda` and is a pre-requisite for both the SEAFO
#'                area labels (`sbShowLabels`) and the SEAFO Closed Areas (`sbShowClosedAreas`).
#' @param sbShowClosedAreas ([logical]) a Boolean value that specifies whether the SEAFO VME
#'                          Closures (VMECs) layer must be drawn on the BaseMap (default:
#'                          `TRUE`). The layer is sourced from
#'                          `./data/dfSEAFO_ClosedAreas.rda` and is only compiled when
#'                          `sbShowCA = TRUE`.
#' @param sbShowFishFootPrint ([logical]) a Boolean value that is reserved for the forthcoming
#'                            fishing-footprint (VMS) overlay. The argument is currently
#'                            internalized only and does NOT yet draw a layer on the BaseMap
#'                            (default: `TRUE`).
#' @param sbShowLabels ([logical]) a Boolean value that specifies whether the SEAFO area labels
#'                    (i.e. "A", "A1", "B", "B1", "C", "C1", "D" and "D1") must be drawn on the
#'                    BaseMap (default: `TRUE`). The labels are only compiled when
#'                    `sbShowCA = TRUE`.
#' @param sbHideWCL ([logical]) a Boolean value that specifies whether the map legend (i.e. the
#'                 STATIC counterpart of the interactive Layer Control box) must be OMITTED from
#'                 the compiled figure (default: `TRUE`). NB: the argument keeps the (inverted)
#'                 semantics of its dynamic counterpart - i.e. `sbHideWCL = TRUE` hides the
#'                 legend, and `sbHideWCL = FALSE` renders it.
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
#' This function draws the compiled SEAFO BaseMap on the ACTIVE graphics device (i.e. the RStudio
#' Plots pane, or whatever device the caller has opened with `grDevices::png()`,
#' `grDevices::pdf()`, etc.) and hands back - INVISIBLY - a list with the compiled map metadata
#' (i.e. `Source` = the selected BaseMap palette, `Xlim` & `Ylim` = the plotted extent, `Layers` =
#' the layers that were actually drawn, and `Palette` = the colour palette used). Saving the
#' figure to a file is therefore the caller's responsibility (i.e. wrap the function call in a
#' `grDevices::png()` / `grDevices::pdf()` device pair).
#'
#'
#' @details
#' ### Static vs. dynamic output
#' This function is the STATIC sibling of [SEAFO::maps_dynamic.compile.basemap()]. The
#' two functions share the same signature (i.e. arguments, defaults and semantics) and consume
#' the SAME spatial layers; only the OUTPUT differs. The dynamic version returns a `leaflet`
#' `htmlwidget` that is panned, zoomed, measured and toggled in a browser, whereas this static
#' version paints a fixed figure with base R `graphics`, which is what is needed for reports,
#' Working Documents, posters and other print media. The static figure needs NO `leaflet`, NO
#' `sf`, NO `ggplot2` and NO tile server: its single additional dependency is the `maps` library,
#' which ships its "world" outline INSIDE the package, so the figure also works in an offline
#' environment.
#'
#' ### BaseMap source
#' A static map carries no tile imagery, so `snMapSRC` selects the colour palette that emulates
#' the requested ESRI provider: `1` renders the "ESRI Ocean BaseMap" look (i.e. a dark
#' bathymetric-blue sea with pale-blue EEZ shading), and any other value (i.e. the default `2`)
#' renders the "ESRI NatGeo World Map" look (i.e. a pale sea-and-land pastel with dark outlines).
#' The selected provider name is still written to the console via [SEAFO::info.post.note()], so
#' the run-time narrative matches the dynamic function exactly.
#'
#' ### Landmass & coastline
#' The dynamic version draws its landmass and coastline from the selected ESRI tile provider. A
#' static map has no tiles, so the same information is painted from `maps::map()` (i.e. the
#' "world" database that ships with the `maps` library), filled with the palette's land tone and
#' outlined with its coastline tone - no download and no tile server are involved. The land layer
#' is part of the BaseMap itself (i.e. it is always drawn, exactly like the tiles of the dynamic
#' version) and therefore does NOT appear as a toggleable layer or as a legend entry.
#'
#' ### Spatial layers
#' The SEAFO spatial layers are read at run-time (i.e. with `base::load()`) from the package's
#' `./data/` folder, which is resolved with `here::here()`. The function therefore expects to be
#' run from the R Project (i.e. package source) root - i.e. the directory that holds the `data/`
#' folder. The canvas is painted sea-first (i.e. the sea fill, then the landmass & coastline, then
#' the graticule), after which the SEAFO layers are drawn in the order below (i.e. back-to-front,
#' so that the smallest features end up on top):
#'
#' * **EEZs** - the SEAFO Exclusive Economic Zones (`dfSEAFO_EEZv11.rda`), drawn when
#'   `sbShowEEZs = TRUE`;
#' * **SEAFO CA** - the SEAFO Convention Area (`dfSEAFO_CA.rda`), drawn when
#'   `sbShowCA = TRUE`;
#' * **SEAFO Labels** - the eight SEAFO area tags (`A`, `A1`, `B`, `B1`, `C`, `C1`, `D` and
#'   `D1`), drawn when both `sbShowCA = TRUE` and `sbShowLabels = TRUE`;
#' * **SEAFO Closed Areas** - the SEAFO VME Closures (`dfSEAFO_ClosedAreas.rda`), drawn when
#'   both `sbShowCA = TRUE` and `sbShowClosedAreas = TRUE`.
#'
#' Multi-part polygons (i.e. the `piece` column) and polygon holes (i.e. the `hole` column) are
#' both honoured, because the rings are painted with `graphics::polypath()` under the
#' even-odd fill rule rather than with one `graphics::polygon()` call per ring.
#'
#' ### Map furniture
#' The figure is completed with a 10-degree graticule, labelled lon/lat axes, a frame, a 500 km
#' scale bar (bottom-left, sized for the plot's own latitude) and - when `sbHideWCL = FALSE` - a
#' legend (top-left) captioned with `ssHeaderWCL`. The plotted extent is identical to the
#' dynamic version's `fitBounds()` call (i.e. longitudes `-25.25` to `31.25` and latitudes
#' `-52.05` to `2.75`), and the axes are aspect-ratio corrected for the mean latitude of that
#' extent so that the map is not stretched east-west.
#'
#' ### Graphics state
#' Any `graphics::par()` settings touched by this function are restored on exit, so the function
#' is safe to call inside an existing multi-panel layout. It does NOT open or close a device -
#' see `@returns` above for the recommended `grDevices::png()` / `grDevices::pdf()` pattern.
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
#' ### Compile the DEFAULT SEAFO Static BaseMap (ESRI NatGeo palette) ...
#' library(SEAFO)   # <- Ensures the `SEAFO` library is <already> installed locally.
#'
#' coMap <- SEAFO::maps_static.compile.basemap()
#' coMap            # <- Draws the static BaseMap on the ACTIVE graphics device !!!
#'
#'
#' ### Save the static BaseMap to a PNG file (i.e. for a report or working document) ...
#' grDevices::png(filename = "SEAFO_BaseMap.png", width = 1600, height = 1500, res = 200);
#'   SEAFO::maps_static.compile.basemap(
#'     snMapSRC = 1,                        # <- Select the ESRI Ocean palette ...
#'     ssHeaderWCL = "SEAFO Layers"         # <- Custom legend caption ...
#'   );
#' grDevices::dev.off();
#'
#'
#' ### ... and the same map as a publication-ready PDF (no legend, no labels) ...
#' grDevices::pdf(file = "SEAFO_BaseMap.pdf", width = 7, height = 7);
#'   SEAFO::maps_static.compile.basemap(
#'     sbShowEEZs   = FALSE,
#'     sbShowLabels = FALSE,
#'     sbHideWCL    = FALSE,
#'     sbRunSelfID  = TRUE,                 # <- <DotsArg> prints the Self-ID notifications ...
#'     ssFuncCallerID = "my.custom.r.func"  # <- <DotsArg> tags the calling function ...
#'   );
#' grDevices::dev.off();
#' }
#'
#'
#'
#' @export
#? ### ### ###
"maps_static.compile.basemap" <- function(
  snMapSRC=2, ssHeaderWCL="SEAFO Convention Area (CA)", snZoomMin=2, snZoomMax=10,
  sbShowEEZs=TRUE, sbShowCA=TRUE, sbShowClosedAreas=TRUE, sbShowFishFootPrint=TRUE,
  sbShowLabels=TRUE, sbHideWCL=TRUE, sbNotifyRunTimeF=FALSE, ...
) {

  if (sbNotifyRunTimeF) beepr::beep(1);   # <- Notify user of Function Run-Time START (audio cue).
  RCT_FUNC_RUNTIME_START_ <- base::Sys.time();   # <- Captures <active> Date Time !!!


  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_TAG_FUNC_LIBR_ID_  <- "SEAFO";                               # <- R Library Identifier
  RCT_TAG_FUNC_ID_SHORT_ <- "SEAFO::STATIC.Basemap";               # <- FuncID - SHORT
  RCT_TAG_FUNC_ID_121_   <- "maps_static.compile.basemap";         # <- FuncID - 1:1 (121)
  RCT_TAG_FUNC_ID_LONG_  <- "SEAFO::MAPS_STATIC.Compile.Basemap";  # <- FuncID - LONG
  RCT_FUNC_CELN_START_   <- 193L;   # <- Function Self-ID Markers (Opening Curly Brace Line #)
  RCT_FUNC_CELN_STOP_    <- 623L;   # <- Function Self-ID Markers (Closing Curly Brace Line #)



  ####   STEP 02 - Define "Local Aliases" for ALL Function Actions   ####
  ###    NOTES: Local aliases enable greppable names. This also serves as a concise inventory of
  ###           external functions used in this R function; while at the same time shielding these
  ###           calls from R's infamous search-path (name) masking behaviour.
  rasBaseC         <- base::c;
  rasBaseLIST      <- base::list;
  rasBaseLOAD      <- base::load;
  rasBaseLENGTH    <- base::length;
  rasBaseORDER     <- base::order;
  rasBaseSPLIT     <- base::split;
  rasBaseUNLIST    <- base::unlist;
  rasBaseLAAPPLY   <- base::lapply;
  rasBaseSEQ_ALONG <- base::seq_along;
  rasBaseIfELSE    <- base::ifelse;
  rasBasePASTE0    <- base::paste0;
  rasBaseRETURN    <- base::return;
  rasBaseSYSTIME   <- base::Sys.time;
  rasBaseSEQ       <- base::seq;
  rasBaseMEAN      <- base::mean;
  rasBaseCOS       <- base::cos;
  rasBaseABS       <- base::abs;
  rasBasePI        <- base::pi;
  rasBaseINVISIBLE <- base::invisible;
  rasBaseONEXIT    <- base::on.exit;

  rasBeeprBEEP <- beepr::beep;

  rasHereHERE <- here::here;

  rasMapsMAP <- maps::map;   # <- STATIC <landmass> coastline (ships with the `maps` library) !!!

  rasGraphicsPLOTNEW    <- graphics::plot.new;
  rasGraphicsPLOTWINDOW <- graphics::plot.window;
  rasGraphicsRECT       <- graphics::rect;
  rasGraphicsPOLYPATH   <- graphics::polypath;
  rasGraphicsABLINE     <- graphics::abline;
  rasGraphicsAXIS       <- graphics::axis;
  rasGraphicsBOX        <- graphics::box;
  rasGraphicsTEXT       <- graphics::text;
  rasGraphicsLEGEND     <- graphics::legend;
  rasGraphicsPAR        <- graphics::par;
  rasGrDevicesAdjustCOLOR <- grDevices::adjustcolor;

  `%??%`                     <- SEAFO::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasSeafoFSID               <- SEAFO::RENV_FSID;
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
  sbRunSelfID_    <- vsDotsArgs_[[rasSeafoFSID$F_ARGS_BOOL_RUN_SELF_ID]]  %??% FALSE;
  ssFuncCallerID_ <- vsDotsArgs_[[rasSeafoFSID$F_ARGS_FUNC_CALLER]] %??% "TOP-LVL (rsProjMAIN)";



  ####   STEP 04 - Run Function SELF-ID <ENTRY> (if requested)   ####
  rasSeafoInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_LONG_,
    siFuncMode01L = 1L, ssFuncCallerID = ssFuncCallerID_,
    sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_,
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUNTIME_START_, csTimeStop = RCT_FUNC_RUNTIME_START_
  );



  ####   ### Compile Useful <internal> Custom Functions here !!!   ####
  ##  Define a custom function that converts a <multi-ring> spatial layer into a SINGLE
  ##  NA-separated coordinate pair for 'graphics::polypath()' ...
  # NB: R (>= 4.1) 'polypath()' has NO 'pathId' argument - NA values break the path into
  #     several sub-paths (and close each one), so polygon HOLES are honoured by the
  #     "evenodd" fill rule while multi-part features remain separate rings !!!
  "rcf_split.rings" <- function(rnLONG, rnLAT, vsGROUP) {

    lsRings_ <- rasBaseSPLIT(          # <- One index vector per ring (empty levels dropped) !!!
      rasBaseSEQ_ALONG(rnLONG), vsGROUP, drop = TRUE
    );
    vnSizes_ <- rasBaseUNLIST(rasBaseLAAPPLY(lsRings_, rasBaseLENGTH), use.names = FALSE);
    lsRings_ <- lsRings_[vnSizes_ >= 3L];   # <- Keep VALID rings only (a path needs >= 3 pts) !!!

    rnPathX_ <- rasBaseUNLIST(                                     # <- Ring coords + NA <gap> !!!
      rasBaseLAAPPLY(lsRings_, function(vnIdx_) rasBaseC(rnLONG[vnIdx_], NA)),
      use.names = FALSE
    );
    rnPathY_ <- rasBaseUNLIST(
      rasBaseLAAPPLY(lsRings_, function(vnIdx_) rasBaseC(rnLAT[vnIdx_], NA)),
      use.names = FALSE
    );

    snPathLEN_ <- rasBaseLENGTH(rnPathX_);
    if (snPathLEN_ > 0L) {             # <- 'polypath()' REJECTS a path that ENDS on a break !!!
      rnPathX_ <- rnPathX_[-snPathLEN_];
      rnPathY_ <- rnPathY_[-snPathLEN_];
    }

    rasBaseLIST("x" = rnPathX_, "y" = rnPathY_);
  }



  ####   STEP 05 - Compile STATIC BaseMap Components   ####
  ###  5.1 - Prepare the STATIC BaseMap Canvas   ####
  vsLayersDRAWN_ <- rasBaseC();      # <- Prime the Drawn Layers vector (with a NULL value) ...
  ssBaseMapSRC_  <- rasBaseIfELSE(   # <- Prime the BaseMap Source Identifier Tag ...
    snMapSRC_ == 1, "ESRI Ocean BaseMap", "ESRI NatGeo World Map"
  );
  rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
    sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
    ssBody = rasBasePASTE0("Base Layer Selected = '", ssBaseMapSRC_, "' !!!")
  );

  ### 5.1a - Compile the STATIC palette that emulates the <selected> ESRI provider ...
  lsPalette_ <- if (snMapSRC_ == 1) {
    rasBaseLIST(                # <- "ESRI Ocean BaseMap" look (dark bathymetric blue) !!!
      SEA     = "#0B2237", LAND = "#173A55", COAST = "#4E7FA0",
      GRID = "#2E5A78", NOTE = "#BBD6E6",
      EEZ_FILL = "#7FA6C4", EEZ_BORD = "#C9DEEB", CA_BORD = "#FFFFFF",
      VMEC_FILL = "#D64545", VMEC_BORD = "#F08C8C", LABEL = "#FFFFFF"
    )
  } else {
    rasBaseLIST(                # <- "ESRI NatGeo World Map" look (pale pastel) !!!
      SEA     = "#D6E6F2", LAND = "#EDE6D2", COAST = "#8C7B5E",
      GRID = "#9EB6C6", NOTE = "#2F4858",
      EEZ_FILL = "#FCF8EE", EEZ_BORD = "#8C7B5E", CA_BORD = "#1A1A1A",
      VMEC_FILL = "#D64545", VMEC_BORD = "#A32020", LABEL = "#1A1A1A"
    )
  };

  ### 5.1b - Set (and later restore) the Graphics Parameters ...
  lsParOLD_ <- rasGraphicsPAR(no.readonly = TRUE);
  rasBaseONEXIT(rasGraphicsPAR(lsParOLD_));
  rnMAR_    <- rasBaseC(4.2, 4.4, 2.2, 1.6);   # <- Plot margins (bottom, left, top, right) !!!
  rnMGP_    <- rasBaseC(3.0, 0.7, 0.0);        # <- Axis label, tick mark & axis line placement !!!

  ### 5.1c - Define the Plot Extent (identical to the dynamic 'fitBounds()' call) !!!
  rnXLIM_ <- rasBaseC(-25.25, 31.25);   # <- Longitudes (West -> East) !!!
  rnYLIM_ <- rasBaseC(-52.05, 2.75);    # <- Latitudes (South -> North) !!!
  rnASPECT_ <- 1 / rasBaseCOS(          # <- Aspect-ratio correction for the mean latitude !!!
    rasBaseMEAN(rnYLIM_) * rasBasePI / 180
  );

  ### 5.1d - Paint the <ocean> canvas & plot frame ...
  rasGraphicsPAR(mar = rnMAR_);             # <- Room for the axis labels & scale bar !!!
  rasGraphicsPLOTNEW();                     # <- Open a <fresh> STATIC canvas (no plot point) !!!
  rasGraphicsPLOTWINDOW(                    # <- Define the map extent & aspect ratio !!!
    xlim = rnXLIM_, ylim = rnYLIM_, asp = rnASPECT_, xaxs = "i", yaxs = "i"
  );
  rnUSR_ <- rasGraphicsPAR("usr");          # <- The ACTUAL window ('asp' pads one dimension) !!!
  rasGraphicsRECT(                         # <- Fill the plot region with the <sea> colour !!!
    xleft = rnUSR_[1], ybottom = rnUSR_[3],
    xright = rnUSR_[2], ytop = rnUSR_[4],
    col = lsPalette_[["SEA"]], border = NA
  );

  ### 5.1e - Paint the Graticule & labelled Axes ...
  vnLonGRID_ <- rasBaseSEQ(-20, 30, by = 10);
  vnLatGRID_ <- rasBaseSEQ(-50, 0, by = 10);
  rasGraphicsABLINE(
    v = vnLonGRID_, h = vnLatGRID_, col = lsPalette_[["GRID"]], lty = 3, lwd = 0.8
  );

  vsLonLABELS_ <- rasBaseIfELSE(           # <- Compile <lon> axis labels (e.g. "20\u00B0W") ...
    vnLonGRID_ == 0, "0\u00B0",
    rasBasePASTE0(rasBaseABS(vnLonGRID_), "\u00B0", rasBaseIfELSE(vnLonGRID_ < 0, "W", "E"))
  );
  vsLatLABELS_ <- rasBaseIfELSE(           # <- Compile <lat> axis labels (e.g. "30\u00B0S") ...
    vnLatGRID_ == 0, "0\u00B0",
    rasBasePASTE0(rasBaseABS(vnLatGRID_), "\u00B0", rasBaseIfELSE(vnLatGRID_ < 0, "S", "N"))
  );
  rasGraphicsAXIS(
    side = 1, at = vnLonGRID_, labels = vsLonLABELS_,
    cex.axis = 0.78, col.axis = lsPalette_[["NOTE"]], col.ticks = lsPalette_[["NOTE"]],
    tcl = -0.25, mgp = rnMGP_
  );
  rasGraphicsAXIS(
    side = 2, at = vnLatGRID_, labels = vsLatLABELS_,
    cex.axis = 0.78, col.axis = lsPalette_[["NOTE"]], col.ticks = lsPalette_[["NOTE"]],
    tcl = -0.25, las = 1, mgp = rnMGP_
  );
  rasGraphicsBOX(col = lsPalette_[["NOTE"]], lwd = 1);


  ### 5.1f - Paint the <landmass> layer (i.e. the STATIC <tile> coastline) ...
  # NB: The dynamic version gets its landmass & coastline from the ESRI tiles - a static map has
  #     no tiles, so the country outlines are drawn from the `maps` library "world" database
  #     (which ships INSIDE the package, i.e. no download & no tile server required) !!!
  rasMapsMAP(                  # <- The <return> value (a map object) is NOT needed here !!!
    database = "world", add = TRUE, fill = TRUE,
    col = lsPalette_[["LAND"]], border = lsPalette_[["COAST"]], lwd = 0.6
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

    ### 5.2b - Paint the EEZ rings (even-odd fill rule honours the <hole> column) ...
    lsEEZPATHS_ <- rcf_split.rings(          # <- Break the rings into NA-separated sub-paths !!!
      dfSEAFO_EEZv11_[["long"]], dfSEAFO_EEZv11_[["lat"]], dfSEAFO_EEZv11_[["group"]]
    );
    rasGraphicsPOLYPATH(
      x = lsEEZPATHS_[["x"]], y = lsEEZPATHS_[["y"]],
      rule = "evenodd", lwd = 0.7,
      col = rasGrDevicesAdjustCOLOR(lsPalette_[["EEZ_FILL"]], alpha.f = 0.28),
      border = rasGrDevicesAdjustCOLOR(lsPalette_[["EEZ_BORD"]], alpha.f = 0.50)
    );
    vsLayersDRAWN_ <- rasBaseC(vsLayersDRAWN_, "EEZs - Africa & SEAFO (v11, 2019)");

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

    ### 5.3c - Paint the CA rings (i.e. OUTLINE ONLY - the dynamic 'fillOpacity = 0') ...
    lsCAPATHS_ <- rcf_split.rings(           # <- Break the rings into NA-separated sub-paths !!!
      dfSEAFO_CA_[["long"]], dfSEAFO_CA_[["lat"]], dfSEAFO_CA_[["group"]]
    );
    rasGraphicsPOLYPATH(
      x = lsCAPATHS_[["x"]], y = lsCAPATHS_[["y"]],
      rule = "evenodd", col = NA, border = lsPalette_[["CA_BORD"]], lwd = 1.5
    );
    vsLayersDRAWN_ <- rasBaseC(vsLayersDRAWN_, "SEAFO CA");

    ### 5.3d - Paint the SEAFO Area Tags ...
    if (sbShowLabels_) {

      ### 5.3di - Compile Data for SEAFO Area Tags ...
      vsSeafoTags_ <- rasBaseC(   "A",   "A1",    "B",   "B1",    "C",   "C1",    "D",   "D1");
      vnLatsTags_  <- rasBaseC(-01.15, -06.15, -21.15, -21.15, -29.15, -29.15, -41.15, -45.85);
      vnLonsTags_  <- rasBaseC(-19.50,  -4.35, -19.50,   3.35, -19.50,   3.35, -19.50,   7.45);

      ### 5.3dii - Plot the SEAFO Area Tags (i.e. the <label-only> markers) ...
      rasGraphicsTEXT(
        x = vnLonsTags_, y = vnLatsTags_, labels = vsSeafoTags_,
        cex = 0.85, font = 2, col = lsPalette_[["LABEL"]]
      );
      vsLayersDRAWN_ <- rasBaseC(vsLayersDRAWN_, "SEAFO Labels");

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

    ### 5.4b - Paint the SEAFO Closed Areas rings ...
    lsVMECPATHS_ <- rcf_split.rings(         # <- Break the rings into NA-separated sub-paths !!!
      dfSEAFO_ClosedAreas_[["long"]], dfSEAFO_ClosedAreas_[["lat"]],
      dfSEAFO_ClosedAreas_[["group"]]
    );
    rasGraphicsPOLYPATH(
      x = lsVMECPATHS_[["x"]], y = lsVMECPATHS_[["y"]],
      rule = "evenodd", lwd = 0.7,
      col = rasGrDevicesAdjustCOLOR(lsPalette_[["VMEC_FILL"]], alpha.f = 0.21),
      border = rasGrDevicesAdjustCOLOR(lsPalette_[["VMEC_BORD"]], alpha.f = 0.50)
    );
    vsLayersDRAWN_ <- rasBaseC(vsLayersDRAWN_, "SEAFO Closed Areas");

    rasSeafoInfoPostNOTE(                       # <- Notify user on <selected> BaseMap Source ...
      sbPostAlways = TRUE, ssHead = RCT_TAG_FUNC_ID_SHORT_, sbShowTail = FALSE,
      ssBody = rasBasePASTE0("SEAFO Closed Areas (VMECs) Layer Added !!!")
    );

  }


  ###  5.5 - Compile the Map Scale Bar (i.e. the STATIC <addScaleBar> counterpart)   ####
  rnBarKM_    <- 500;                                  # <- Scale bar length in kilometres !!!
  rnBarLAT_   <- rnYLIM_[1] + 6.5;                     # <- Place <above> the bottom axis !!!
  rnBarXST_   <- rnXLIM_[1] + 2.0;                     # <- Place <right> of the left axis !!!
  rnBarWIDTH_ <- rnBarKM_ / (111.32 * rasBaseCOS(      # <- Degrees of longitude per <500 km> !!!
    rnBarLAT_ * rasBasePI / 180
  ));
  rasGraphicsRECT(                                     # <- First (light) half of the bar !!!
    xleft = rnBarXST_, ybottom = rnBarLAT_,
    xright = rnBarXST_ + rnBarWIDTH_ / 2, ytop = rnBarLAT_ + 0.9,
    col = lsPalette_[["NOTE"]], border = lsPalette_[["NOTE"]], lwd = 0.6
  );
  rasGraphicsRECT(                                     # <- Second (dark) half of the bar !!!
    xleft = rnBarXST_ + rnBarWIDTH_ / 2, ybottom = rnBarLAT_,
    xright = rnBarXST_ + rnBarWIDTH_, ytop = rnBarLAT_ + 0.9,
    col = lsPalette_[["SEA"]], border = lsPalette_[["NOTE"]], lwd = 0.6
  );
  rasGraphicsTEXT(
    x = rnBarXST_ + rnBarWIDTH_ / 2, y = rnBarLAT_ - 1.6,
    labels = rasBasePASTE0(rnBarKM_, " km"), cex = 0.7, col = lsPalette_[["NOTE"]]
  );


  ###  5.6 - Compile the Map Legend (i.e. the STATIC <addLayersControl> counterpart)   ####
  if (sbHideWCL_ == FALSE) {   # <- Displays the STATIC legend (on map) if 'FALSE' !!!

    vsLeyLABELS_ <- rasBaseC(); vsLeyFILL_ <- rasBaseC(); vsLeyBORD_ <- rasBaseC();
    if (sbShowEEZs_) {
      vsLeyLABELS_ <- rasBaseC(vsLeyLABELS_, "EEZs - Africa & SEAFO (v11, 2019)");
      vsLeyFILL_   <- rasBaseC(vsLeyFILL_, lsPalette_[["EEZ_FILL"]]);
      vsLeyBORD_   <- rasBaseC(vsLeyBORD_, lsPalette_[["EEZ_BORD"]]);
    }
    if (sbShowCA_) {
      vsLeyLABELS_ <- rasBaseC(vsLeyLABELS_, "SEAFO CA");
      vsLeyFILL_   <- rasBaseC(vsLeyFILL_, NA);
      vsLeyBORD_   <- rasBaseC(vsLeyBORD_, lsPalette_[["CA_BORD"]]);
    }
    if (sbShowCA_ && sbShowLabels_) {
      vsLeyLABELS_ <- rasBaseC(vsLeyLABELS_, "SEAFO Labels");
      vsLeyFILL_   <- rasBaseC(vsLeyFILL_, lsPalette_[["LABEL"]]);
      vsLeyBORD_   <- rasBaseC(vsLeyBORD_, lsPalette_[["LABEL"]]);
    }
    if (sbShowCA_ && sbShowClosedAreas_) {
      vsLeyLABELS_ <- rasBaseC(vsLeyLABELS_, "SEAFO Closed Areas");
      vsLeyFILL_   <- rasBaseC(vsLeyFILL_, lsPalette_[["VMEC_FILL"]]);
      vsLeyBORD_   <- rasBaseC(vsLeyBORD_, lsPalette_[["VMEC_BORD"]]);
    }

    if (rasBaseLENGTH(vsLeyLABELS_) > 0) {   # <- Draw the legend only if a layer was painted !!!
      rasGraphicsLEGEND(
        x = "bottomright", legend = vsLeyLABELS_, title = ssHeaderWCL_,
        fill = vsLeyFILL_, border = vsLeyBORD_,
        bty = "o", box.col = lsPalette_[["NOTE"]],
        bg = rasGrDevicesAdjustCOLOR(lsPalette_[["SEA"]], alpha.f = 0.85),
        text.col = lsPalette_[["NOTE"]], title.col = lsPalette_[["NOTE"]],
        cex = 0.72, title.cex = 0.78, title.adj = 0.5, inset = 0.012,
        x.intersp = 0.6, y.intersp = 0.95
      );
    }

  }



  ####   STEP 06 - Run Function SELF-ID <EXIT> (if requested)   ####
  RCT_FUNC_RUNTIME_STOP_ <- rasBaseSYSTIME();
  rasSeafoInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_LONG_,
    siFuncMode01L = 0L, ssFuncCallerID = ssFuncCallerID_,
    sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_,
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUNTIME_START_, csTimeStop = RCT_FUNC_RUNTIME_STOP_
  );



  ####   STEP 07 - EXIT Functions (results hand-over)   ####
  if (sbNotifyRunTimeF) rasBeeprBEEP(2);   # <- Notify user of Function Run-Time STOP (audio cue).
  rasBaseRETURN(                           # <- Hand-over Function Ouputs to Function Call !!!
    rasBaseINVISIBLE(
      rasBaseLIST(
        "Source"  = ssBaseMapSRC_,
        "Xlim"    = rnXLIM_, "Ylim" = rnYLIM_,
        "Layers"  = vsLayersDRAWN_,
        "Palette" = lsPalette_
      )
    )
  );

}
