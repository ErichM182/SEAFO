#? ### ### ### ### ### ### ###
#' @title Extract the Code Editor Gutter Line Number (CELN)
#' @name code_editor.fetch.celn
#' @family CODE Functions (`SEAFO`)
#'
#'
#' @description
#' A SPECIAL HELPER Function that extracts the Code Editor Gutter Line Number (CEGLN a.k.a. CELN) at
#' the code editor line location from which the `code_editor.fetch.celn()` function was called. This
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
#'                    in which order) the `code_editor.fetch.celn()` function was called (i.e. from
#'                    inside a function). An index that exceeds the number of
#'                    `code_editor.fetch.celn()` calls found in the target function returns
#'                    `NA_integer_` (it is never an error).
#' @param sbUseAlias ([logical]) a Boolean value that specifies whether the function should use the
#'                   standard `code_editor.fetch.celn()` function identifier or not (default:
#'                   `FALSE`). If set to `TRUE` the function will NOT search for the standard
#'                   `code_editor.fetch.celn(` text, but rather for the exact text supplied via the
#'                   `ssAliasValue` function argument (e.g. `rasSeafoFetchCELN(`).
#' @param ssAliasValue ([character]) a String or Text (i.e. [character] [vector]) value that
#'                     specifies the alias (i.e. re-assigned function identifier) of the default or
#'                     standard `code_editor.fetch.celn()` function identifier to search for in the
#'                     R function <internal> code. This value IS a Regular Expression (it is passed
#'                     to `grep()`), so a literal opening bracket must be backslash-escaped.
#'
#'
#' @returns
#' * A single [integer] value - the Code Editor Line Number (CELN) - when `ssFuncName` is found
#'   and `siCallIndex` is within range.
#' * `NA_integer_`, with a [warning], when the target function is not found, when `siCallIndex`
#'   exceeds the number of `code_editor.fetch.celn()` calls it contains, or when an arg is invalid.
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
#'       SEAFO::code_editor.fetch.celn(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         1L                 # <- Define the call order <call sequence> for the
#'       ),                   #    `code_editor.fetch.celn()` function calls inside the custom
#'                            #    function code. This is the 1st time the
#'                            #    `code_editor.fetch.celn()` function is called inside this custom
#'                            #    function so the `siCallIndex` value here should == `1L`.
#'       " | Summed all 3 input values < result: ", valSUM," > !!! \n"
#'     )
#'   );
#'
#'   valMEAN <- mean(x, y, z);
#'   cat(
#'     paste0(
#'       " \u279C ", RCT_TAG_FUNC_ID, " " ,
#'       SEAFO::code_editor.fetch.celn(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         2L                 # <- Define the call order <call sequence> for the
#'       ),                   #    `code_editor.fetch.celn()` function calls inside the custom
#'                            #    function code. This is the 2nd time `code_editor.fetch.celn()`
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
#'       SEAFO::code_editor.fetch.celn(
#'         RCT_TAG_FUNC_ID,   # <- Provide the Function ID (tag) for the Custom R Function ...
#'         TRUE,              # <- <force> Run function even if DEBUG tracking is not activated !!!
#'         3L                 # <- Define the call order <call sequence> for the
#'       ),                   #    `code_editor.fetch.celn()` function calls inside the custom
#'                            #    function code. This is the 3rd time the
#'                            #    `code_editor.fetch.celn()` function is called inside this custom
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
"code_editor.fetch.celn" <- function(
  ssFuncName="code_editor.fetch.celn", sbRunByForce=FALSE,
  siCallIndex=1L, sbUseAlias=FALSE, ssAliasValue="rasSeafoFetchCELN\\("
) {


  ####   STEP 01 - Define "Function Self-ID" R Objects   ####
  RCT_RUNTIME_FUNC_START_ <- base::Sys.time();                  # <- Captures <active> Date Time !!!
  RCT_TAG_FUNC_LIBR_ID_   <- "SEAFO";                           # <- R Library Identifier !!!
  RCT_TAG_FUNC_ID_SHORT_  <- "SEAFO::Fetch.CELN";               # <- Function ID - SHORT !!!
  RCT_TAG_FUNC_ID_121_    <- "code_editor.fetch.celn";          # <- Function ID - 1:1 (121) !!!
  RCT_TAG_FUNC_ID_LONG_   <- "SEAFO::CODE_Editor.Fetch.CELN";   # <- Function ID - LONG !!!



  ####   STEP 02 - Define "Local Aliases" for Key Functions   ####
  ###    NOTES: Local aliases enable greppable names. This also serves as a concise inventory of
  ###           external functions used in this R function; while at the same time shielding these
  ###           calls from R's infamous search-path (name) masking behaviour.
  rasBaseC           <- base::c;
  rasBaseLIST        <- base::list;
  rasBaseNROW        <- base::nrow;
  rasBaseORDER       <- base::order;
  rasBaseRETURN      <- base::return;
  rasBaseIfELSE      <- base::ifelse;
  rasBaseToLOWER     <- base::tolower;
  rasBaseSeqLEN      <- base::seq_len;
  rasBasePASTE0      <- base::paste0;
  rasBaseWARNING     <- base::warning;
  rasBaseLENGTH      <- base::length;
  rasBaseIsNA        <- base::is.na;
  rasBaseAsINTEGER   <- base::as.integer;
  rasBaseAsNUMERIC   <- base::as.numeric;
  rasBaseIsNUMERIC   <- base::is.numeric;
  rasBaseAsCHARACTER <- base::as.character;
  rasBaseIsCHARACTER <- base::is.character;

  rasSeafoFindCODE   <- SEAFO::devs.find.code.instances;
  rasSeafoPollRTMODE <- SEAFO::code.poll.r.runtime.mode;

  ## SPECIAL - Constant - TAG - Aliases ...



  ####   STEP 03 - Internalize ALL Function Arguments   ####
  ssFuncName_   <- ssFuncName;
  siCallIndex_  <- siCallIndex;
  sbRunByForce_ <- sbRunByForce;
  sbUseAlias_   <- sbUseAlias;
  ssAliasValue_ <- ssAliasValue;

  ## SPECIAL: Try to locate & extract the 'isDebugMode' logical (boolean) variable
  ##          <if set or primed elsewhere> in the current <active> R Project ...
  RCT_RCO_RT_MODE_ <- rasSeafoPollRTMODE();   # <- R Run-Time Mode/State !!!
  sbIsDEBUG_       <- RCT_RCO_RT_MODE_$IS_DEBUG;
  if (sbRunByForce_ || sbIsDEBUG_) {   # <- Run standard code logic if either of these is TRUE !!!

    ####   STEP 04 - Trace Function Call Stack Location   ####
    ## 4.1 - Validate the <caller-supplied> search parameters up-front (fail soft with `NA`) ...
    ssFindText_ <- rasBaseIfELSE(
      sbUseAlias_,
      ssAliasValue_,                    # <- The user-specified "Alias" value to search for !!!
      "code_editor\\.fetch\\.celn\\("   # <- NEVER CHANGE THIS (100% CORRECT default value) !!!
    );

    if (!rasBaseIsCHARACTER(ssFuncName_) || rasBaseLENGTH(ssFuncName_) != 1L ||
        rasBaseIsNA(ssFuncName_)) {
      rasBaseWARNING(" `ssFuncName` must be a single <non-NA> string -> returning NA !!!");
      rasBaseRETURN(NA_integer_);
    }

    if (!rasBaseIsNUMERIC(siCallIndex_) || rasBaseLENGTH(siCallIndex_) != 1L ||
        rasBaseIsNA(siCallIndex_) || siCallIndex_ < 1L) {
      rasBaseWARNING(" `siCallIndex` must be a single positive integer -> returning NA !!!");
      rasBaseRETURN(NA_integer_);
    }
    siCallIndex_ <- rasBaseAsINTEGER(siCallIndex_);

    if (!rasBaseIsCHARACTER(ssFindText_) || rasBaseLENGTH(ssFindText_) != 1L ||
        rasBaseIsNA(ssFindText_)) {
      rasBaseWARNING(" `ssAliasValue` must be a single <non-NA> char string -> returning NA !!!");
      rasBaseRETURN(NA_integer_);
    }

    ## 4.2 - Run an R Function Code Search to locate all instances of this `code_editor.fetch.celn()`
    ##       function in the specified <active> R Function <internal function code> ...
    ##       NOTE: `coRENVs` is emptied because the Global Environment is already searched via
    ##             `sbIncludeGlobal` - searching it twice would duplicate every single match.
    rdfFuncCalls_ <- rasSeafoFindCODE(
      vsTargetLibs = rasBaseC(RCT_TAG_FUNC_LIBR_ID_), coRENVs = rasBaseLIST(),
      sbVerboseSearch = FALSE, sbIgnoreCase = FALSE, sbWarnNoSrcRef = TRUE,
      ssFindText = ssFindText_
    );

    ## 4.3 - Extract only results that match the specified Function ID ...
    ##       NOTE: BOTH sides are case-folded - `FUNC_NAME` keeps the <declared> source case,
    ##             so folding only the <argument> would miss every mixed-case function name.
    rdfFuncCalls_v02_ <- rdfFuncCalls_[
      rasBaseToLOWER(rasBaseAsCHARACTER(rdfFuncCalls_[["FUNC_NAME"]])) ==
        rasBaseToLOWER(ssFuncName_), ,
      drop = FALSE
    ];

    ## 4.4 - NO MATCH -> return `NA` (never let `seq_len(0)`/`1:0` build an empty INDEX) ...
    ##       NOTE: <exact> duplicate rows are already prevented structurally (step 4.2 empties
    ##             `coRENVs`), so NO de-duplication is done here - de-duplicating on the line
    ##             number would wrongly collapse two genuine calls that share a source line.
    if (rasBaseNROW(rdfFuncCalls_v02_) == 0L) {
      rasBaseWARNING(rasBasePASTE0(
        " `", ssFuncName_, "()` not found in the ", RCT_TAG_FUNC_LIBR_ID_,
        " R Library or Global Environment -> returning NA !!!"
      ));
      rasBaseRETURN(NA_integer_);
    }

    ## 4.5 - Order the matches by their <source file> position & prime the INDEX variable ...
    rdfFuncCalls_v03_ <- rdfFuncCalls_v02_[
      rasBaseORDER(rasBaseAsNUMERIC(rdfFuncCalls_v02_[["MATCH_ID"]]), decreasing = FALSE), ,
      drop = FALSE
    ];
    rdfFuncCalls_v03_[["INDEX"]] <- rasBaseSeqLEN(rasBaseNROW(rdfFuncCalls_v03_));

    ## 4.6 - An OUT-OF-RANGE <call index> returns `NA` (never a silent `numeric(0)`) ...
    if (siCallIndex_ > rasBaseNROW(rdfFuncCalls_v03_)) {
      rasBaseWARNING(rasBasePASTE0(
        " `siCallIndex` = ", siCallIndex_, " but only ", rasBaseNROW(rdfFuncCalls_v03_),
        " `", ssFuncName_, "()` call(s) found -> returning NA !!!"
      ));
      rasBaseRETURN(NA_integer_);
    }

    ## 4.7 - Extract the Code Editor Line Number (CELN) accordingly ...
    rdfAtCELN_ <- rdfFuncCalls_v03_[
      rdfFuncCalls_v03_[["INDEX"]] == siCallIndex_, ,
      drop = FALSE
    ];


    ####   STEP 05 - Return Function Outputs   ####
    rasBaseRETURN(rasBaseAsINTEGER(rdfAtCELN_[["LINE_NUMBER"]]));

  }

}
