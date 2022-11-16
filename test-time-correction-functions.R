#### test time taken to use correction functions ####

devtools::load_all()

# Import data
library(data.table)
data(TestData)

t0 = Sys.time()
suppressWarnings(GeneralErrorsDetection(TestData))
Sys.time() - t0


## with larger dataset
# shiny::runGitHub( "VincyaneBadouard/TreeData", subdir = "inst/app")

## get Central Panama 1-ha plots and BCI

if (!file.exists("data/TestDataCPnm.rda") |
    !file.exists("data/TestDataBCI.rda")) {

  ### BCI ####
  dryad_data_path_bci <- rdryad::dryad_download("10.15146/5xcp-0d46")
  zip_files <- c(grep("\\.zip", dryad_data_path_bci$`10.15146/5xcp-0d46`, value = TRUE))
  zip_files <- grep("stem", zip_files, value = TRUE)

  # unzip files
  zip_folders <- sapply(zip_files, function(dir) {
    name <- paste0("data/", gsub("\\.zip", "", data.table::last(strsplit(dir, '/')[[1]])))
    utils::unzip(dir, exdir = name)
  })

  for (file in c(zip_folders)) load(file)
  census_list <- list(bci.stem1, bci.stem2, bci.stem3, bci.stem4,
                      bci.stem5, bci.stem6, bci.stem7, bci.stem8)
  names(census_list) <- 1:8
  dfbci <- rbindlist(census_list, idcol = "IdCensus")

  ## remove unnecessary files
  rm(bci.stem1, bci.stem2, bci.stem3, bci.stem4,
     bci.stem5, bci.stem6, bci.stem7, bci.stem8)
  unlink("data/bci.stem",recursive = TRUE)

  ## species table
  load(grep("spp", dryad_data_path_bci$`10.15146/5xcp-0d46`, value = TRUE))

  dfbci <- merge(dfbci, bci.spptable)

  ## convert into TreeData format ####
  replace_columns <-
    list("Site" = "BCI",
         "Plot" = "1",
         "SubPlot" = dfbci$quadrat,
         "PlotArea" = 50,
         "SubPlotArea" = 0.04,
         "MinDBH" = 1,
         "Year" = as.numeric(tstrsplit(dfbci$ExactDate, "-")[[1]]),
         "Month" = as.numeric(tstrsplit(dfbci$ExactDate, "-")[[2]]),
         "Day" = as.numeric(tstrsplit(dfbci$ExactDate, "-")[[3]]),
         "Date" = dfbci$ExactDate,
         "TreeFieldNum" = dfbci$tag,
         "IdTree" = dfbci$treeID,
         "StemFieldNum" = dfbci$StemTag,
         "IdStem"  = dfbci$stemID,
         "LifeStatus" = (dfbci$status == "A"),
         "Diameter" = dfbci$dbh/10,
         "Circ" = dfbci$dbh/10*pi,
         "HOM" = dfbci$hom,
         "XTreePlot" = dfbci$gx,
         "YTreePlot" = dfbci$gy,
         "ScientificName" = dfbci$Latin,
         "Subspecies" = dfbci$subsp,
         "IdCensus" = dfbci$IdCensus,
         "Family" = dfbci$Family,
         "Genus" = dfbci$Genus,
         "Species" = dfbci$Species,
         "Authority" = dfbci$Authority)

  ## fill missing colnames with NAs
  miss = colnames(TestData)[!colnames(TestData) %in% colnames(dfbci)]

  list_missing <- rep(list(NA), length(miss))
  names(list_missing) <- miss

  dfbci <- as.data.table(c(replace_columns, list_missing))

  TestDataBCI <- dfbci[, colnames(TestData), with = FALSE]

  save(TestDataBCI, file = "data/TestDataBCI.rda")


  ### Marena plots ####

  dryad_data_path_panama <- rdryad::dryad_download("10.15146/mdpr-pm59")

  # select files to unzip, keeping only stem-level information
  zip_files <- c(grep("\\.zip", dryad_data_path_panama$`10.15146/mdpr-pm59`, value = TRUE))
  zip_files <- grep("stem", zip_files, value = TRUE)

  # unzip files
  zip_folders <- sapply(zip_files, function(dir) {
    name <- paste0("data/", gsub("\\.zip", "", data.table::last(strsplit(dir, '/')[[1]])))
    utils::unzip(dir, exdir = name)
  })

  for (file in grep("marena4cns", zip_folders, value=TRUE)) load(file)

  census_list <- list(marena4cns.stem1, marena4cns.stem2, marena4cns.stem3, marena4cns.stem4)
  names(census_list) <- 1:4

  dfmrn <- rbindlist(census_list, idcol = "IdCensus")

   ## remove unecessary files
  rm(marena4cns.stem1, marena4cns.stem2, marena4cns.stem3, marena4cns.stem4)
  unlink("data/marena_stem_17Jul2019/", recursive = TRUE)

  ## get species names from BCI data
  dfmrn <- merge(dfmrn, bci.spptable)

  ## convert into TreeData format ####
  replace_columns <-
    list("Site" = "CentralPanama",
         "Plot" = dfmrn$plot,
         "SubPlot" = dfmrn$quadrat,
         "PlotArea" = c(1,5.96)[1+(dfmrn$plot=="sherman")],
         "SubPlotArea" = 0.04,
         "MinDBH" = 1,
         "Year" = as.numeric(tstrsplit(dfmrn$ExactDate, "-")[[1]]),
         "Month" = as.numeric(tstrsplit(dfmrn$ExactDate, "-")[[2]]),
         "Day" = as.numeric(tstrsplit(dfmrn$ExactDate, "-")[[3]]),
         "Date" = dfmrn$ExactDate,
         "TreeFieldNum" = dfmrn$tag,
         "IdTree" = dfmrn$treeID,
         "StemFieldNum" = dfmrn$StemTag,
         "IdStem"  = dfmrn$stemID,
         "LifeStatus" = (dfmrn$status == "A"),
         "Diameter" = dfmrn$dbh/10,
         "Circ" = dfmrn$dbh/10*pi,
         "HOM" = dfmrn$hom,
         "XTreePlot" = dfmrn$gx,
         "YTreePlot" = dfmrn$gy,
         "ScientificName" = dfmrn$Latin,
         "Subspecies" = dfmrn$subsp,
         "IdCensus" = dfmrn$IdCensus,
         "Family" = dfmrn$Family,
         "Genus" = dfmrn$Genus,
         "Species" = dfmrn$Species,
         "Authority" = dfmrn$Authority)

  ## fill missing colnames with NAs
  miss = colnames(TestData)[!colnames(TestData) %in% colnames(dfmrn)]

  list_missing <- rep(list(NA), length(miss))
  names(list_missing) <- miss

  dfmrn <- as.data.table(c(replace_columns, list_missing))

  TestDataCPnm <- dfmrn[, colnames(TestData), with = FALSE]

  save(TestDataCPnm, file = "data/TestDataCPnm.rda")

}

data(TestDataCPnm)  ## pb when I don't remove NAs > with missing values section L 86 > takes too long // too much memory used?
data(TestDataBCI)

t0 = Sys.time()
suppressWarnings(GeneralErrorsDetection(TestDataCPnm))
Sys.time() - t0


t0 = Sys.time()
suppressWarnings(GeneralErrorsDetection(TestDataCPnm[!is.na(Diameter)]))
Sys.time() - t0


