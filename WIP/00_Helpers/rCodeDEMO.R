


####   INFO -> https://github.com/ErichM182/SEAFO   ####



####   STEP 1 - Install the SEAFO R Package ...   ####
# 1) Get a GitHub installer (skip if you already have remotes or devtools) ...
utils::install.packages("remotes")


# 2) Install the SEAFO R Package (together with its CRAN dependencies) ...
remotes::install_github("ErichM182/SEAFO")


# 3) Verify that the 'SEAFO' R Package installed properly ...
base::library(SEAFO)
utils::packageVersion("SEAFO")
#> [1] '0.0.7'   # <- Production <alpha> Release !!!







####   STEP 2 - Compile the SEAFO CA Base Map ...   ####
coBaseMapLM_ <- SEAFO::maps_dynamic.compile.basemap(
  sbHideWCL = F   # <- Shows the Basemap Controls Widget on the map itself !!!
); coBaseMapLM_







####   STEP 3 - Add Data Layers to Base Map ...   ####
####   3.1 - Plot the VMS Data Layer ...   ####
coBaseMapLM_VMS_ <- SEAFO::maps_dynamic.add.layer.points.data.vms(
  lmMapObj = coBaseMapLM_, snSpeedSplit = 1.25,
  sbPlotOnlyFishingVMS = F, sbShowToggleMonth = T, sbShowShipFlag = T,
  # vsShipColors = base::c("orange", "red", "magenta", "black", "yellow")
); coBaseMapLM_VMS_



####   3.2 - Plot the CPUE Data Layer ...   ####
####   Install a special R Leaflet Package ...
devtools::install_github("rpruim/leaflethex")
####
###   Create 'Function Execution Efficiency Tracker' ( FEET ) variables here ...   ###
FEET_SEAFO_MAP_PP_IS_INIT_RUN <- TRUE;


####---   : Function Helper Objects [ external ]   ---####
###   Create useful and function extension object here ...   ###
# Function (external) Helpers
# A list that defines key species found in the SEAFO CA ...
lstCodeSPP <- base::list(
  'ALL' = base::c('All Spp.', 'All Species', 'SPP'),
  'BYS' = base::c('Beryx splendens', 'Alfonsino', 'BYS'),
  'YHI' = base::c('Chaceon erytheiae', 'Deep-sea red crab', 'YHI'),
  'ORY' = base::c('Hoplostethus atlanticus', 'Orange roughy', 'ORY'),
  'EDR' = base::c('Pentaceros richardsoni', 'Pelagic armourhead', 'EDR'),
  'TOP' = base::c('Dissostichus eleginoides', 'Patagonian toothfish', 'TOP')
);
# class(lstCodeSPP)
# lstCodeSPP$ALL[1]

# A list that defines the grouping levels for this mapping function ...
# enGroupBY <- rf_create.env.safe.enum(AREA, VESSEL, MONTH, SPECIES, YEAR);
lstGroupBY <- base::list(
  'AREA'    = 'SEAFO_Div',
  'YEAR'    = 'Year',
  'MONTH'   = 'Month',
  'VESSEL'  = 'VesselName',
  'SPECIES' = 'SP_Scientific_Name'
);
# class(lstGroupBY)
# lstGroupBY$SPECIES

###   ENUMERATED DATA R OBJECT -  'enTypeGPS' ...   ###
# INFO: the 'GPS Type' enum for the map plotting process ...
enTypeGPS <- SEAFO::code_enums.create.renv.enum(
  vsEnumVals = base::c("BOTH", "CATCH", "VMS")
);   # <- R Custom Enums are abbreviated to avoid potential clashes with similarly named R Base or 3rd Party variables ...
# class(enTypeGPS);
# enTypeGPS$CATCH; enTypeGPS$VMS;

###   ENUMERATED DATA R OBJECT -  'enTypeSPP' ...   ###
# INFO: the 'Species Type' enum for the map plotting process ...
enTypeSPP <- SEAFO::code_enums.create.renv.enum(
  vsEnumVals = base::c("ALL", "TARGET", "BYCATCH", "VME")
);   # <- R Custom Enums are abbreviated to avoid potential clashes with similarly named R Base or 3rd Party variables ...
# class(enTypeSPP);
# enTypeSPP$ALL; enTypeSPP$VME;

###   ENUMERATED DATA R OBJECT -  'enModeHexAGGR' ...   ###
# INFO: the 'Leaflet HEX Aggregation Mode/Type' enum for the map plotting process ...
enModeHexAGGR <- SEAFO::code_enums.create.renv.enum(
  vsEnumVals = base::c("COUNT", "SUM", "MAX", "MIN", "MEAN", "MEDIAN")
);   # <- R Custom Enums are abbreviated to avoid potential clashes with similarly named R Base or 3rd Party variables ...
# class(enModeHexAGGR);
# enModeHexAGGR$MEAN; enModeHexAGGR$COUNT;

###   ENUMERATED DATA R OBJECT -  'enGroupBY' ...   ###
# INFO: the 'Group By' enum for the map facet plotting process ...
enGroupBY <- SEAFO::code_enums.create.renv.enum(
  vsEnumVals = base::c("AREA", "VESSEL", "MONTH", "SPECIES", "YEAR")
);   # <- R Custom Enums are abbreviated to avoid potential clashes with similarly named R Base or 3rd Party variables ...
# class(enGroupBY);
# enGroupBY$AREA; enGroupBY$SPECIES;

###   ENUMERATED DATA R OBJECT -  'enShowRTE' ...   ###
# INFO: the 'Show Rate' enum for the hex grid map layer ...
enShowRTE <- SEAFO::code_enums.create.renv.enum(
  vsEnumVals = base::c("NONE", "CATCH", "CPUE", "SPEED")
);   # <- R Custom Enums are abbreviated to avoid potential clashes with similarly named R Base or 3rd Party variables ...
# class(enShowRTE);
# enShowRTE$AREA; enShowRTE$SPECIES;
