#? ### ### ### ### ### ### ###
#' @title CRAN Code Validation with Code Version Tracking ("SEAFO" DevTools)
#' @name devs.check.code.specs
#' @family SEAFO DevTools
#'
#' @description
#' A Helper Function that executes the CRAN pre-requisite Code Checking Procedure during active R
#' Package Development. This function programmatically regenerates the package documentation and
#' then updates the package version number in the R Project DESCRIPTION file only when a delta is
#' detected in the documentation (i.e. a change in the "./man/*.Rd" files or the "NAMESPACE" file)
#' and/or in the package code base (i.e. a change in the "./R", "./src", "./data", "./tests",
#' "./inst" or "./man/figures" asset trees, or in the "./README.md" file), before running the
#' required CRAN Package Pre-Submission Code Requirement Checks (i.e. CRAN Code Validation)
#' during iterative development cycles. This Helper Function was
#' originally crafted as a technical support tool for the "SEAFO Suite of R Libraries" (aka
#' "SuiteSeafo"), but may also be helpful as a support tool during the R Package Development Cycle
#' of other (3rd Party) R Projects.
#'
#' @param sbCheckDocs a logical (boolean) value that specifies whether to run the standard package
#'                    documentation process only (not the FULL CRAN Code Validation Process/Check).
#'                    The documentation is (re)generated in both modes so that the documentation
#'                    delta <version bump trigger> can be detected prior to the version patch.
#' @param sbCheckCRAN a logical (boolean) value that specifies whether to run the FULL CRAN Code
#'                    Validation Process/Check (inclusive of the pre-check documentation steps).
#' @param ssTimeZone a simple character vector (string) that defines the Time Zone to be used for
#'                   the package documentation.
#' @param sbIsProdRel a logical (boolean) value that captures if the code-check and/or code commit
#'                    process (action) is a "Production Release" code development action or not.
#' @param ... the fall-through function arguments (i.e. DotsArgs) used for nested functions within
#'            this main function. The "DotsArgs" implementation here pertain to the following nested
#'            R functions (i.e. R functions used within this main R function): <br>
#'               1. [SEAFO::info.post.func.self.id()], and <br>
#'               2. [SEAFO::devs.patch.code.dev.trckr.file()].
#'
#' @returns
#' * This function returns the programmatically amended or updated (real-time or active) version
#'   number for the active R Library Project as a list of character objects.
#' * The version number is only incremented when a delta is detected in the documentation (i.e. a
#'   changed "./man/*.Rd" or "NAMESPACE" artefact) and/or in the package code base (i.e. a changed
#'   file in the "./R", "./src", "./data", "./tests", "./inst" or "./man/figures" asset trees, or
#'   a changed "./README.md" file, detected via the "Code Base Fingerprint" recorded in the tracker
#'   file). When neither delta is found the current version number is returned unchanged and both
#'   the "ACT-DEV" tracker file and the project DESCRIPTION file are left untouched.
#' * This function creates a Work-In-Progress (WIP) directory at the root of the active R-Libs
#'   Project (i.e. "./WIP" <- if not already exists). The "./WIP" folder and all of its contents
#'   are included in GIT push-&-pull processes. You must MANUALLY ADD the following code stub
#'   (^WIP$) <sans parentheses> to the ".Rbuildignore" R Project file to ensure the "./WIP"
#'   folder + contents are excluded from the R Project Build Process. (<- this is IMPORTANT !!!)
#' * This function also creates a "DevsVersTimeStamp.txt" file in the "./WIP" folder for secondary
#'   development version tracking purposes (as needed).
#'
#' @import beepr desc devtools
#'
#' @examples
#' \dontrun{   ### <- This function constitutes a development utility !!! This function creates (and
#'             ###    requires) a special development directory ("./WIP") that is created during the
#'             ###    init-run (initial R project setup) phase and is intended to facilitate a user-
#'             ###    friendly R Library development process. For these reasons the code examples
#'             ###    below should not be executed during "R_CMD_CHECK" code check procedures.
#'
#' ### Run R Package DevCode easily as follows ...
#' library(SEAFO)   # <- Loads "SEAFO" library (if already installed) !!!
#'
#' ### Run 2 different types of code check/validation processes ...
#' devs.check.code.specs(sbCheckDocs = TRUE)   # -> Executes only the DevTools Documentation
#'                                             #    Process.
#' devs.check.code.specs(sbCheckCRAN = TRUE)   # -> Executes the more complete (stringent) CRAN Code
#'                                             #    Validation Process.
#'
#' ### NB: The documentation is (re)generated FIRST in both modes so that the documentation delta
#' ###     <a version bump trigger> can be detected. The ACTIVE-DEVELOPMENT version number is only
#' ###     incremented when a DOCUMENTATION or CODE BASE delta is found (i.e. no version churn) !!!
#' devs.check.code.specs(sbCheckDocs = TRUE, sbCheckCRAN = TRUE)
#'
#' }
#'
#' @export
#? ### ### ###
"devs.check.code.specs" <- function(
  sbCheckDocs=TRUE, sbCheckCRAN=FALSE,
  ssTimeZone="Africa/Windhoek", sbIsProdRel = FALSE, ...
) {

  ####   STEP 01 - Prime the "Function Self-ID" Constants   ####
  RCT_TAG_FUNC_LIBR_ID_ <- "SEAFO";                   # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHRT_ <- "Check.Specs";             # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_FULL_ <- "DEVS.Check.Code.Specs";   # <- Function ID - LONG !!!

  base::Sys.setenv(TZ = ssTimeZone);   # <- Set correct Time Zone BEFORE querying System CLOCK !!!
  RCT_FUNC_RUN_TIME_START_ <- base::Sys.time();
  RCT_FUNC_CELN_START_ <- 79L; RCT_FUNC_CELN_STOP_ <- 289L;



  ####   STEP 02 - Define "Local Aliases" for ALL Functions   ####
  ###    NOTES: Local aliases enable greppable names. This also serves as a concise inventory of
  ###           external functions used in this R function; while at the same time shielding these
  ###           calls from R's infamous search-path (name) masking behaviour.
  rasBaseC          <- base::c;
  rasBaseCAT        <- base::cat;
  rasBaseLIST       <- base::list;
  rasBaseTRUNC      <- base::trunc;
  rasBaseROUND      <- base::round;
  rasBaseRETURN     <- base::return;
  rasBasePASTE0     <- base::paste0;
  rasBaseIfELSE     <- base::ifelse;
  rasBaseFORMAT     <- base::format;
  rasBaseUNLIST     <- base::unlist;
  rasBaseLENGTH     <- base::length;
  rasBaseSPRINTF    <- base::sprintf;
  rasBaseDiffTIME   <- base::difftime;
  rasBaseStrSPLIT   <- base::strsplit;
  rasBaseSysTimeNOW <- base::Sys.time;
  rasBaseINVISIBLE  <- base::invisible;
  rasBaseFilePATH   <- base::file.path;
  rasBaseAsNUM      <- base::as.numeric;
  rasBaseDirCREATE  <- base::dir.create;
  rasBaseSysSetENV  <- base::Sys.setenv;
  rasBaseWriteLINES <- base::writeLines;
  rasBaseFileCREATE <- base::file.create;
  rasBaseFileEXISTS <- base::file.exists;
  rasBaseListFILES  <- base::list.files;
  rasBaseIDENTICAL  <- base::identical;
  rasBaseAsCHAR     <- base::as.character;

  rasDescSetVERSION <- desc::desc_set_version;
  rasToolsMD5SUM    <- tools::md5sum;

  rasDevToolsCHECK    <- devtools::check;
  rasDevToolsLoadALL  <- devtools::load_all;
  rasDevToolsDOCUMENT <- devtools::document;
  rasDevToolsCleanDLL <- devtools::clean_dll;

  `%??%`                        <- SEAFO::`%??%`;   # <- VERY COOL Alias <NCO> !!!
  rasSeafoDEVS                  <- SEAFO::RENV_DEVS;
  rasSeafoICONS                 <- SEAFO::RENV_ICONS;
  rasSeafoCOLORS                <- SEAFO::RENV_COLOURS;
  rasSeafoFORMATS               <- SEAFO::RENV_FORMATS;
  rasSeafoDevsPrintCodeCheckRES <- devs.print.code.check.res;
  rasSeafoPullLibrINFO          <- SEAFO::devs.pull.libr.info;
  rasSeafoInfoPostFuncSelfID    <- SEAFO::info.post.func.self.id;
  rasSeafoDevsPatchCodeDevTRCKR <- SEAFO::devs.patch.code.dev.trckr.file;



  ####   STEP 03 - Internalize ALL Function Arguments   ####
  sbCheckDocs_ <- sbCheckDocs;
  sbCheckCRAN_ <- sbCheckCRAN;
  ssTimeZone_  <- ssTimeZone;
  sbIsProdRel_ <- sbIsProdRel;

  vsDotsArgs_     <- rasBaseLIST(...);
  sbPrintPretty_  <- vsDotsArgs_[["sbPrintPretty"]]  %??% TRUE;
  sbAudioNote_    <- vsDotsArgs_[["sbAudioNote"]]    %??% FALSE;
  sbRunSelfID_    <- vsDotsArgs_[["sbRunSelfID"]]    %??% FALSE;
  ssFuncCallerID_ <- vsDotsArgs_[["ssFuncCallerID"]] %??% "TOP-LVL (rsProjMAIN)";



  ####   STEP 04 - Run Function SELF-ID <ENTRY> (if requested)   ####
  rasSeafoInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_,
    siFuncMode01 = 1L, ssFuncCallerID = ssFuncCallerID_,
    sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_,
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUN_TIME_START_, csTimeStop = RCT_FUNC_RUN_TIME_START_
  );



  ####   STEP 05 - Define Critical Constants   ####
  RCT_SYS_DATE_TIME_NOW_ <- rasBaseSysTimeNOW();

  RCT_ANSI_BOLD_    <- rasSeafoFORMATS$BOLD;
  RCT_ANSI_RESET_   <- rasSeafoFORMATS$RESET;
  RCT_ANSI_ITALICS_ <- rasSeafoFORMATS$ITALICS;

  RCT_ICON_POINT_UP_      <- rasSeafoICONS$PointUP;
  RCT_ICON_WHITE_X_       <- rasSeafoICONS$X_White;
  RCT_ICON_SMILEY_SAD_    <- rasSeafoICONS$SmileySad;
  RCT_ICON_OOGLY_EYES_    <- rasSeafoICONS$OoglyEyes;
  RCT_ICON_CHECK_MARK_    <- rasSeafoICONS$CheckMark;
  RCT_ICON_ARROW_RIGHT_   <- rasSeafoICONS$ArrowRIGHT;
  RCT_ICON_SMILEY_PONDER_ <- rasSeafoICONS$SmileyPonder;

  RCT_COLOR_RED_          <- rasSeafoCOLORS$RedFORE;
  RCT_COLOR_BLUE_         <- rasSeafoCOLORS$BlueFORE;
  RCT_COLOR_GREEN_        <- rasSeafoCOLORS$GreenFORE;
  RCT_COLOR_TELLOW_       <- rasSeafoCOLORS$YellowFORE;
  RCT_FOLDER_WIP_PROD_    <- rasSeafoDEVS$PATH_FOLDER_WIP_PROD;
  RCT_FORMAT_TIME_DEV_01_ <- rasSeafoDEVS$FORMAT_TIME_DEV_LOG_V01;
  RCT_FORMAT_TIME_DEV_02_ <- rasSeafoDEVS$FORMAT_TIME_DEV_LOG_V02;
  RCT_FOLDER_WIP_HELPERS_ <- rasSeafoDEVS$PATH_FOLDER_WIP_HELPERS;
  RCT_FILE_DEV_TIME_LOG_  <- rasSeafoDEVS$PATH_FILE_WIP_TIME_STAMP;
  RCT_FILE_R_PKG_DESC_    <- rasSeafoDEVS$PATH_TO_FILE_R_PACKAGE_DESC;



  ####   STEP 06 - Regenerate DOCs & Detect DOCUMENTATION DELTA   ####
  # NB: The documentation artefacts (i.e. "./man/*.Rd" and "NAMESPACE") are (re)generated
  #     FIRST so that any content change they exhibit can be detected and used to gate the
  #     ACTIVE-DEVELOPMENT version number increment (i.e. version bump) in STEP 07 below !!!
  rsbDocsDELTA_ <- FALSE;
  if (sbCheckDocs_ || sbCheckCRAN_ || sbIsProdRel_) {

    ### 6.1 - Snapshot the documentation artefacts BEFORE regeneration ...
    rvsDocsFilesBEFORE_ <- rasBaseC(
      rasBaseListFILES(path = "man", pattern = "[.]Rd$", full.names = TRUE),
      "NAMESPACE"
    );
    rvsDocsFilesBEFORE_ <- rvsDocsFilesBEFORE_[
      rasBaseFileEXISTS(rvsDocsFilesBEFORE_)
    ];
    rcsDocsHashBEFORE_ <- rasToolsMD5SUM(rvsDocsFilesBEFORE_);

    ### 6.2 - Regenerate the documentation artefacts ...
    rasDevToolsDOCUMENT(roclets = rasBaseC('rd', 'collate', 'namespace'));

    ### 6.3 - Snapshot the documentation artefacts AFTER regeneration ...
    rvsDocsFilesAFTER_ <- rasBaseC(
      rasBaseListFILES(path = "man", pattern = "[.]Rd$", full.names = TRUE),
      "NAMESPACE"
    );
    rvsDocsFilesAFTER_ <- rvsDocsFilesAFTER_[
      rasBaseFileEXISTS(rvsDocsFilesAFTER_)
    ];
    rcsDocsHashAFTER_ <- rasToolsMD5SUM(rvsDocsFilesAFTER_);

    ### 6.4 - Establish whether the documentation actually changed (DELTA) ...
    rsbDocsDELTA_ <- !rasBaseIDENTICAL(rcsDocsHashBEFORE_, rcsDocsHashAFTER_);

  }


  ####   STEP 07 - Patch CODE DEV TRACKER (bump upon DOC or CODE DELTA)   ####
  # 2. Extract the current version number from the DESCRIPTION file ...
  # NB: The tracker helper ALSO detects a CODE BASE delta (against the "Code Base Fingerprint"
  #     recorded in the TRCKR file), so the version is only withheld when neither delta exists !!!
  lsCodeVERS_ <- rasSeafoDevsPatchCodeDevTRCKR(
    sbIsProdRel = sbIsProdRel_, sbAudioNote = sbAudioNote_,
    sbPatchVers = (sbIsProdRel_ || rsbDocsDELTA_)   # <- NB -> DOCS delta only (CODE is in-call) !
  );
  ssActProjID_  <- lsCodeVERS_[["CODE_NAME_TAG"]];
  ssVersNewFULL <- lsCodeVERS_[["CODE_VERS_DEVS"]];


  ####   STEP 08 - Run CRAN Code Checks (if requested)   ####
  # 6. Finally - Run the required R-Libs Project CRAN Checks !!!
  if (sbCheckCRAN_ || sbIsProdRel_) {   # -> Runs the COMPLETE CRAN Requirements Check !!!
    rasDevToolsCleanDLL();   # <- ALWAYS run BEFORE 'rasDevToolsLoadALL()' !!!
    rasDevToolsLoadALL();    # <- ALWAYS run AFTER 'rasDevToolsCleanDLL()' !!!

    coCheckResult_ <- rasDevToolsCHECK();
    snLenNOTEs_    <- rasBaseLENGTH(coCheckResult_$notes);
    snLenERRORs_   <- rasBaseLENGTH(coCheckResult_$errors);
    snLenWARNINGs_ <- rasBaseLENGTH(coCheckResult_$warnings);
    rasSeafoDevsPrintCodeCheckRES(
      snLenERRORs = snLenERRORs_, snLenWARNs = snLenWARNINGs_, snLenNOTEs = snLenNOTEs_,
      sbIsProdRel = sbIsProdRel_, ssActProjID = ssActProjID_, ssProjVers = ssVersNewFULL
    );

  }



  ####   STEP 09 - Run Function SELF-ID <EXIT> (if requested)   ####
  RCT_FUNC_RUN_TIME_STOP_ <- rasBaseSysTimeNOW();
  rasSeafoInfoPostFuncSelfID(
    ssFuncSelfID = RCT_TAG_FUNC_ID_FULL_,
    siFuncMode01 = 0L, ssFuncCallerID = ssFuncCallerID_,
    sbRunSelfID = sbRunSelfID_, sbPrintPretty = sbPrintPretty_,
    siStartCELN = RCT_FUNC_CELN_START_, siStopCELN = RCT_FUNC_CELN_STOP_,
    csTimeStart = RCT_FUNC_RUN_TIME_START_, csTimeStop = RCT_FUNC_RUN_TIME_STOP_
  );



  # 7. Return the new created Project Version Number as a character object ...
  rasBaseRETURN(
    rasBaseINVISIBLE(
      rasBaseLIST(
        "ProjID" = ssActProjID_,
        "CodeVers" = rasBasePASTE0("v", ssVersNewFULL),
        "CodeTime" = rasBaseFORMAT(RCT_FUNC_RUN_TIME_STOP_, RCT_FORMAT_TIME_DEV_02_)
      )
    )
  );

}


