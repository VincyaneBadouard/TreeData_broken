#' General Errors Detection
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @param ByStem must be equal to TRUE if your inventory contains the stem
#'   level, equal to FALSE if not, and in this case the correction is done by
#'   tree (logical)
#'
#' @details Detect errors
#'   - Remove **duplicated rows**
#'   - Check **missing value** in
#'      X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/
#'      IdTree/IdStem/Diameter/POM/HOM/Family/Genus/Species/VernName
#'   - Check **missing value** (NA/0) in the measurement variables: "Diameter",
#'      "HOM", "TreeHeight", "StemHeight"
#'   - Check **duplicated TreeFieldNum** in plot-subplot association in a census
#'      (at the site scale)
#'   - Check of the *unique association of the idTree with plot, subplot and
#'      TreeFieldNum* (at the site scale)
#'   - Check **duplicated idTree/IdStem** in a census (at the site scale)
#'   - Check for trees **outside the subplot** (not implemented yet)
#'   - Check **invariant coordinates per IdTree/IdStem**
#'   - Check **fix Plot and Subplot number** (not implemented yet)
#'
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#'
#' Rslt <- GeneralErrorsDetection(TestData)
#'
GeneralErrorsDetection <- function(
  Data,
  ByStem = TRUE
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  #### Function ####

  # data.frame to data.table
  setDT(Data)

  # Check duplicate rows ----------------------------------------------------------------------------------------------
  # if there are duplicate rows, delete them

  if(anyDuplicated(Data) != 0)
    Data <- unique(Data)


  # Missing values ----------------------------------------------------------------------------------------------------
  # If the column exists, but have NA values

  # Check bota : Family/Genus/Species/ScientificName/VernName
  # Check size : Diameter, POM(?)
  Vars <- c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
            "Diameter", "POM", "HOM", "TreeHeight", "StemHeight",
            "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName")

  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = is.na(Data[,get(Vars[v])]),
                              comment = paste0("Missing value in ", Vars[v]))

      warning(paste0("Missing value in ", Vars[v]))
    }
  }

  # Data[Comment != ""] # to check (13 comments)


  # Measurement variables = 0 -----------------------------------------------------------------------------------------

  Vars <- c("Diameter", "HOM", "TreeHeight", "StemHeight")

  for (v in 1:length(Vars)) {
    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = Data[,get(Vars[v])] == 0,
                              comment = paste0(Vars[v]," cannot be 0"))

      warning(paste0(Vars[v]," cannot be 0"))

    }
  }
  # Data[get(Vars) == 0] # to check



  # Check duplicated TreeFieldNum in plot-subplot association ---------------------------------------------------------

  # Create "PlotSubNum" = "Site/Year/Plot/Subplot/TreeFieldNum"
  Data[, PlotSubNum := paste(Site, Year, Plot, Subplot, TreeFieldNum, sep = "/")]

  # y = 2017
  # p=1
  # c= 3
  duplicated_num <- num <- vector("character")

  # if any duplicats in this col
  if(anyDuplicated(Data$PlotSubNum) != 0) {
    # For each site
    for (s in unique(na.omit(Data$Site))) {
      # For each census
      for (y in unique(na.omit(Data$Year))) {
        # For each plot
        for (p in unique(na.omit(Data$Plot))) {
          # For each Subplot in this plot
          for (c in unique(na.omit(Data[Data$Plot==p, Subplot]))) {

            num <- Data[Data$Site == s & Data$Year == y
                        & Data$Plot == p & Data$Subplot == c]$TreeFieldNum # all the TreeFieldNum for each Plot-Subplot combination

            # if there are several TreeFieldNum per Plot-Subplot combination
            if(anyDuplicated(num) != 0){
              duplicated_num <- unique(num[duplicated(num)])

              Data <- GenerateComment(Data,
                                      condition =
                                        Data[,Site] == s & Data[,Year] == y
                                      & Data[,Plot] == p & Data[,Subplot] == c
                                      & Data[,TreeFieldNum] %in% duplicated_num,
                                      comment = "Duplicate TreeFieldNum in the same Plot and Subplot")

              num <- vector("character")

              warning("Duplicate TreeFieldNum(s) (",duplicated_num,") in the same Plot (",p,") and Subplot (",c,"), in ",y,"")

            } else {num <- vector("character")}
          } # end subplot loop
        } # end plot loop
      } # end year loop
    } # end site loop
  }

  Data[, PlotSubNum := NULL]
  # Data[TreeFieldNum == duplicated_num,.(Year = sort(Year), Plot, Subplot, TreeFieldNum, Comment)] # to check (1 duplicate)


  # Check of the unique association of the IdTree with Plot-Subplot-TreeFieldNum, at the site scale -------------------

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    correspondances <- na.omit(unique(
      Data[Data$Site == s, .(IdTree, Plot, Subplot, TreeFieldNum)]
    ))

    CorresIDs <- correspondances[, IdTree] # .(IdTree) all the Idtree's having a unique P-SubP-TreeFieldNum combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,IdTree] %in% duplicated_ID,
                              comment = "Non-unique association of the IdTree with Plot, Subplot and TreeFieldNum")

      warning("Non-unique association of the IdTree(s) (",duplicated_ID,") with Plot, Subplot and TreeFieldNum")

    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Plot, Subplot, TreeFieldNum, Comment)]) # to check


  # Check duplicated IdTree/IdStem in a census ------------------------------------------------------------------------

  if(ByStem == TRUE){
    ID <- "IdStem"
  }else if (ByStem == FALSE) {
    ID <- "IdTree"
  }

  # Create "SitYearID" = "Site/Year/ID"
  Data[, SitYearID := paste(Site, Year, get(ID), sep = "/")]

  duplicated_ids <- ids <- vector("character")

  # if any duplicates in this col
  if(anyDuplicated(Data$SitYearID) != 0){
    # For each site
    for (s in unique(na.omit(Data$Site))) {
      # For each census
      for (y in unique(na.omit(Data$Year))) {

        ids <- Data[Data$Site == s & Data$Year == y, get(ID)] # all the IDs for each Site and Year combination

        # if there are several IdTree/IdStem per Site and Year combination
        if(anyDuplicated(ids) != 0){
          duplicated_ids <- unique(ids[duplicated(ids)])

          Data <- GenerateComment(Data,
                                  condition =
                                    Data[,Site] == s & Data[,Year] == y
                                  & Data[,get(ID)] %in% duplicated_ids,
                                  comment = paste0("Duplicated '", ID, "' in the census"))

        }
      } # end year loop
    } # end site loop
  }

  Data[, SitYearID := NULL]

  # Data[IdTree == duplicated_ids,.(Year = sort(Year), Plot, Subplot, TreeFieldNum, IdTree, Comment)] # to check


  # Check for trees outside the subplot (A FAIRE) ---------------------------------------------------------------------
  # Comparer PlotArea avec l'aire du MCP (Minimum Convex Polygon) des arbres a l'interieur de la parcelle.
  # Si aire du MCP > x% plotArea -> error


  # Check invariant coordinates per IdTree/IdStem ---------------------------------------------------------------------

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    CoordIDCombination <- na.omit(unique(
      Data[Data$Site == s, c(ID, "XTreeUTM", "YTreeUTM"), with = FALSE]
    ))

    CorresIDs <- CoordIDCombination[, get(ID)] # .(IdTree) all the Idtree's having a unique X-YTreeUTM) combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,get(ID)] %in% duplicated_ID,
                              comment = paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"'"))

      warning(paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"' (",duplicated_ID,")"))

    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), XTreeUTM, YTreeUTM, Comment)]) # to check


  # Check fix Plot and Subplot number (A FAIRE, Eliot a) --------------------------------------------------------------
  # alerte quand le nombre de sous-parcelles/parcelles varie selon les années


  return(Data)
}
