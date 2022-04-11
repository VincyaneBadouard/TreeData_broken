#' ErrorsDetection
#'
#' @param Data (data.frame or data.table)
#'
#' @param DeathConfirmation Number of times (censuses) needed for an unseen tree
#'   to be considered dead (numeric) (Default = 2 censuses)
#'
#' @param UseSize Use the size presence as a witness of the living status of the
#'   tree (logical) (Default = FALSE)
#'
#' @param MinDBH Minimum diameter of trees inventoried according to your
#'   protocol (in cm) (numeric, 1 value) (Default = 10 cm)
#'
#' @param PositiveGrowthThreshold A tree widening by more than x cm/year is
#'   considered abnormal (numeric, 1 value) (Default = 5 cm)
#'
#' @details Detect errors
#'   - Remove *duplicated rows*
#'   - Check *missing value* in
#'      X-Yutm/PlotArea/Plot/SubPlot/Year/TreeFieldNum/
#'      IdTree/DBH/MeasCode/Family/Genus/Species/VernName
#'   - Check *missing value* (NA/0) in the measurement variables
#'   - Check *duplicated TreeFieldNum* in plot-subplot association in a census
#'      (at the site scale)
#'   - Check of the *unique association of the idTree with plot, subplot and
#'      TreeFieldNum* (at the site scale)
#'   - Check *duplicated idTree* in a census (at the site scale)
#'   - Check for trees *outside the subplot*
#'   - Check *invariant coordinates per IdTree*
#'   - Check *fix Plot and SubPlot number*
#'
#'   - Check botanical identification (BotanicalCorrection)
#'   - Check the life status evolution of the trees (StatusCorrection)
#'   - Check diameter evolution of the trees (DiameterCorrection)
#'   - Check recruitment (RecruitmentCorrection)
#'
#'
#' @seealso \code{\link{StatusCorrection}}
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#'
#' Rslt <- ErrorsDetection(TestData)
#'
ErrorsDetection <- function(
  Data,
  # Life status error detection
  DeathConfirmation = 2,
  UseSize = FALSE,
  # Recruitment error detection
  MinDBH = 10,
  PositiveGrowthThreshold = 5
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize/RemoveRBeforeAlive/RemoveRAfterDeath
  if(!all(unlist(lapply(list(UseSize),
                        inherits, "logical"))))
    stop("The 'UseSize' argument of the 'ErrorsDetection' function must be logicals")

  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if(!"DBH" %in% names(Data)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the DBH column must be present in the dataset")
    }
  }

  #### Function ####

  # ONLY DETECTION NOT CORRECTION
  DetectOnly <- TRUE

  # data.frame to data.table
  setDT(Data)

  #### Check duplicate rows ####
  # if there are duplicate rows, delete them

  if(anyDuplicated(Data) != 0)
    Data <- unique(Data)


  #### Missing values ####
  # If the column exists, but have NA values

  # Check bota : Family/Genus/Species/ScientificName/VernName
  # Check size : DBH, POM(?)
  Vars <- c("Plot", "SubPlot", "Year", "TreeFieldNum", "IdTree",
            "DBH", "POM", "TreeHeight", "StemHeight", "MeasCode",
            "Xutm", "Yutm", "Family", "Genus", "Species", "VernName")

  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = is.na(Data[,get(Vars[v])]),
                              comment = paste0("Missing value in ", Vars[v]))
    }
  }

  # Data[Comment != ""] # to check (13 comments)


  #### Measurement variables = 0 ####

  Vars <- c("DBH", "POM", "TreeHeight", "StemHeight")

  for (v in 1:length(Vars)) {
    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = Data[,get(Vars[v])] == 0,
                              comment = paste0(Vars[v]," cannot be 0"))
    }
  }
  # Data[get(Vars) == 0] # to check



  #### Check duplicated TreeFieldNum in plot-subplot association ####

  # Create "PlotSubNum" = "Site/Year/Plot/SubPlot/TreeFieldNum"
  Data[, PlotSubNum := paste(Site, Year, Plot, SubPlot, TreeFieldNum, sep = "/")]

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
          # For each SubPlot in this plot
          for (c in unique(na.omit(Data[Data$Plot==p, SubPlot]))) {

            num <- Data[Data$Site == s & Data$Year == y
                        & Data$Plot == p & Data$SubPlot == c,]$TreeFieldNum # all the TreeFieldNum for each Plot-SubPlot combination

            # if there are several TreeFieldNum per Plot-SubPlot combination
            if(anyDuplicated(num) != 0){
              duplicated_num <- unique(num[duplicated(num)])

              Data <- GenerateComment(Data,
                                      condition =
                                        Data[,Site] == s & Data[,Year] == y
                                      & Data[,Plot] == p & Data[,SubPlot] == c
                                      & Data[,TreeFieldNum] %in% duplicated_num,
                                      comment = "Duplicate TreeFieldNum in the same Plot and SubPlot")

              num <- vector("character")

            } else {num <- vector("character")}
          } # end subplot loop
        } # end plot loop
      } # end year loop
    } # end site loop
  }

  Data[, PlotSubNum := NULL]
  # Data[TreeFieldNum == duplicated_num,.(Year = sort(Year), Plot, SubPlot, TreeFieldNum, Comment)] # to check (1 duplicate)


  #### Check of the unique association of the idTree with Plot-SubPlot-TreeFieldNum, at the site scale ####

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    correspondances <- na.omit(unique(
      Data[Data$Site == s, .(IdTree, Plot, SubPlot, TreeFieldNum)]
    ))

    CorresIDs <- correspondances[, IdTree] # .(IdTree) all the Idtree's having a unique P-SubP-TreeFieldNum combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,IdTree] %in% duplicated_ID,
                              comment = "Non-unique association of the IdTree with Plot, SubPlot and TreeFieldNum")
    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Plot, SubPlot, TreeFieldNum, Comment)]) # to check


  #### Check duplicated IdTree in a census ####

  # Create "SitYearID" = "Site/Year/IdTree"
  Data[, SitYearID := paste(Site, Year, IdTree, sep = "/")]

  duplicated_ids <- ids <- vector("character")

  # if any duplicats in this col
  if(anyDuplicated(Data$SitYearID) != 0){
    # For each site
    for (s in unique(na.omit(Data$Site))) {
      # For each census
      for (y in unique(na.omit(Data$Year))) {

        ids <- Data[Data$Site == s & Data$Year == y,]$IdTree # all the IdTree for each Site and Year combination

        # if there are several IdTree per Site and Year combination
        if(anyDuplicated(ids) != 0){
          duplicated_ids <- unique(ids[duplicated(ids)])

          Data <- GenerateComment(Data,
                                  condition =
                                    Data[,Site] == s & Data[,Year] == y
                                  & Data[,IdTree] %in% duplicated_ids,
                                  comment = "Duplicated IdTree in the census")
        }
      } # end year loop
    } # end site loop
  }

  Data[, SitYearID := NULL]

  # Data[IdTree == duplicated_ids,.(Year = sort(Year), Plot, SubPlot, TreeFieldNum, IdTree, Comment)] # to check


  ## Check for trees outside the subplot (A FAIRE)
  # Comparer PlotArea avec l'aire du MCP (Minimum Convex Polygon) des arbres a l'interieur de la parcelle.
  # Si aire du MCP > x% plotArea -> error


  #### Check invariant coordinates per IdTree ####

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    CoordIDCombination <- na.omit(unique(
      Data[Data$Site == s, .(IdTree, Xutm, Yutm)]
    ))

    CorresIDs <- CoordIDCombination[, IdTree] # .(IdTree) all the Idtree's having a unique X-Yutm) combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,IdTree] %in% duplicated_ID,
                              comment = "Different coordinates (Xutm, Yutm) for a same IdTree")
    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Xutm, Yutm, Comment)]) # to check


  #### Check fix Plot and SubPlot number (A FAIRE, Eliot a) ####
  # alerte quand le nombre de sous-parcelles/parcelles varie selon les années

  #### Internals ####

  #### Bota ####
  ### Special characters
  ### Typographie
  # é, è or œ

  #### Check invariant botanical informations per IdTree ####
  # "Family", "ScientificName", VernName"

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    BotaIDCombination <- na.omit(unique(
      Data[Data$Site == s, .(IdTree, Family, ScientificName, VernName)]
    ))

    CorresIDs <- BotaIDCombination[, IdTree] # .(IdTree) all the Idtree's having a unique X-Yutm) combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,IdTree] %in% duplicated_ID,
                              comment = "Different botanical informations (Family, ScientificName or VernName) for a same IdTree")
    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Family, ScientificName, VernName, Comment)]) # to check



  ### Gapfill missing botanical names using vernacular names ####
  # based on probabilities of association of vernacular and botanical names.
  # The typographie must already have been corrected.




  #### Life status ####

  Data <- StatusCorrection(Data,
                           DeathConfirmation = DeathConfirmation,
                           UseSize = UseSize,
                           DetectOnly = DetectOnly)

  #### Diameter ####

  #### Recruitment ####

  Data <- RecruitmentCorrection(Data,
                                MinDBH = MinDBH,
                                PositiveGrowthThreshold = PositiveGrowthThreshold,
                                DetectOnly = DetectOnly
  )

  return(Data)
}
