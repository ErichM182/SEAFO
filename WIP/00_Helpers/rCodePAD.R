### ###|       ->   NEVER DELETE THE FIRST 15 LINES OF THIS < Code-Pad > R SCRIPT   <-      |### ###
####|   -> Use this R Script <file> for Random Coding Tasks during Code Development Cycle !!!   ####
### ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~ ### ~### ###


###   R Project Development <Run-Time Mode> Trackers   ###
RCT_IS_DEBUG_RT_MODE_   <- F;   # <- The `DEBUG` <dev> Run-Time Mode Tracker ...
RCT_IS_VERBOSE_RT_MODE_ <- T;   # <- The `VERBOSE` <dev> Run-Time Mode Tracker ...


### R Library CRAN Code and Documentation Checking Function ...
SEAFO::devs.check.code.specs(
  sbCheckCRAN = T,   # <- Set this to `FALSE` if you only want to update the documentation !!!
  sbIsProdRel = F    # <- Set this to `TRUE` if you are running a PRODUCTION Release <test> !!!
);



base::assign(SEAFO::RENV_FSID$CONSTS_BOOL_IS_DEBUG, FALSE)

base::get0(SEAFO::RENV_FSID$CONSTS_BOOL_IS_DEBUG)



ssTestVAR_ <- "129";
is.character(ssTestVAR_);
nchar(ssTestVAR_);



SEAFO::info.post.note(sbPostAlways = T, sbRunByForce = T)
SEAFO::info.post.func.self.id(
  sbRunSelfID = T, sbPrintPretty = T, siFuncMode01L = 0L
)



vsListNames_ <- c("VAR_A", "VAR_B", "VAR_C", "VAR_X", "VAR_Y", "VAR_Z", "VAR_G")
lsListVals_  <- list(
  1982, "Value for VAR_B", "R-Object for VAR_C", FALSE,
  "Value for VAR_Y", TRUE, "R-List for VAR_G"
)
lsListVals_  <- c(
  1982, "Value for VAR_B", "R-Object for VAR_C", FALSE,
  "Value for VAR_Y", TRUE, "R-List for VAR_G"
)
dfListCON_ <- base::data.frame(
  VarA = vsListNames_,
  VarB = lsListVals_
)
dfListCON_[1:3, ]



### Function-use OPTION 1 (main purpose) -> Create Immutable R List Objects ...
rlsListEnvLOCKD_ <- SEAFO::code.return.renv.list(
  vsListNames = vsListNames_, lsListVals = lsListVals_,
  sbLockList = T, sbRunByForce = T, sbPostAlways = T, sbRunSelfID = T
)


SEAFO::devs.create.r.function(
  ssFuncName = "CODE.New.R.FUNC.r",
  ssFuncPath = "./W",
  sbForceCreate = T,
  sbIsLibrFunc = T
  # sbForceFuncPath = T
)




dfProjFiles <- file.info(list.files(path = "./R", full.names = TRUE))
file_info <- dfProjFiles[!dfProjFiles$isdir, ]    # 3. Filter out directories (keep only files)
file_dates <- data.frame(
  file_name = rownames(file_info),
  modification_time = file_info$mtime
)
ordered_files <- file_dates[order(file_dates$modification_time, decreasing = TRUE), ]
rownames(ordered_files) <- NULL

class(file_info)
class(dfProjFiles)






