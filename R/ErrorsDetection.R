#' Errors Detection
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @param ByStem must be equal to TRUE if your inventory contains the stem
#'   level, equal to FALSE if not, and in this case the correction is done by
#'   tree (logical)
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
#' @param PositiveGrowthThreshold in cm/year: a tree
#'   widening by more than this value is considered abnormal (numeric, 1 value)
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
#'   - Check botanical identification (*BotanicalCorrection*)
#'   - Check the life status evolution of the trees (*StatusCorrection*)
#'   - Check diameter evolution of the trees (*DiameterCorrection*)
#'   - Check recruitment (*RecruitmentCorrection*)
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

  ByStem = TRUE,

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

  # UseSize/RemoveRBeforeAlive/RemoveRAfterDeath/ByStem
  if(!all(unlist(lapply(list(UseSize),
                        inherits, "logical"))))
    stop("The 'UseSize' argument of the 'ErrorsDetection' function must be logicals")

  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if(!"Diameter" %in% names(Data)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the 'Diameter' column must be present in the dataset")
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
  # Check size : Diameter, POM(?)
  Vars <- c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
            "Diameter", "POM", "TreeHeight", "StemHeight", "HOM",
            "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName")

  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = is.na(Data[,get(Vars[v])]),
                              comment = paste0("Missing value in ", Vars[v]))
    }
  }

  # Data[Comment != ""] # to check (13 comments)


  #### Measurement variables = 0 ####

  Vars <- c("Diameter", "HOM", "TreeHeight", "StemHeight")

  for (v in 1:length(Vars)) {
    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = Data[,get(Vars[v])] == 0,
                              comment = paste0(Vars[v]," cannot be 0"))
    }
  }
  # Data[get(Vars) == 0] # to check



  #### Check duplicated TreeFieldNum in plot-subplot association ####

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

            } else {num <- vector("character")}
          } # end subplot loop
        } # end plot loop
      } # end year loop
    } # end site loop
  }

  Data[, PlotSubNum := NULL]
  # Data[TreeFieldNum == duplicated_num,.(Year = sort(Year), Plot, Subplot, TreeFieldNum, Comment)] # to check (1 duplicate)


  #### Check of the unique association of the idTree with Plot-Subplot-TreeFieldNum, at the site scale ####

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
    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Plot, Subplot, TreeFieldNum, Comment)]) # to check


  #### Check duplicated IdTree/IdStem in a census ####

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


  ## Check for trees outside the subplot (A FAIRE)
  # Comparer PlotArea avec l'aire du MCP (Minimum Convex Polygon) des arbres a l'interieur de la parcelle.
  # Si aire du MCP > x% plotArea -> error


  #### Check invariant coordinates per IdTree/IdStem ####

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
    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), XTreeUTM, YTreeUTM, Comment)]) # to check


  #### Check fix Plot and Subplot number (A FAIRE, Eliot a) ####
  # alerte quand le nombre de sous-parcelles/parcelles varie selon les années

  #### Internals ####

  #### Bota ####
  ### Special characters
  ### Typographie
  # é, è or œ

  #### Check invariant botanical informations per IdTree/IdStem ####
  # "Family", "ScientificName", VernName"

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    BotaIDCombination <- na.omit(unique(
      Data[Data$Site == s, c(ID, "Family", "ScientificName", "VernName"), with = FALSE]
    ))

    CorresIDs <- BotaIDCombination[, get(ID)] # .(IdTree) all the Idtree's having a unique X-YTreeUTM) combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,get(ID)] %in% duplicated_ID,
                              comment = paste0("Different botanical informations (Family, ScientificName or VernName) for a same '", ID,"'"))
    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Family, ScientificName, VernName, Comment)]) # to check



  #### Botanical informations ####


  #### Life status ####

  Data <- StatusCorrection(Data,
                           DeathConfirmation = DeathConfirmation,
                           UseSize = UseSize,
                           DetectOnly = DetectOnly)

  #### Diameter ####

  # Data <- DiameterCorrection(Data,
  #
  #                            ByStem = ByStem,
  #
  #                            DefaultHOM = DefaultHOM,
  #                            MinDBH = MinDBH,
  #                            MaxDBH = MaxDBH,
  #                            PositiveGrowthThreshold = PositiveGrowthThreshold,
  #                            NegativeGrowthThreshold = NegativeGrowthThreshold,
  #
  #                            Pioneers = Pioneers,
  #                            PioneersGrowthThreshold = PioneersGrowthThreshold,
  #
  #                            TrustMeasSet = TrustMeasSet,
  #                            WhatToCorrect = WhatToCorrect,
  #                            CorrectionType = c("taper", "quadratic", "linear", "individual", "phylogenetic hierarchical"),
  #
  #                            DBHRange = 10,
  #                            MinIndividualNbr = 5,
  #                            Digits = 1L,
  #
  #                            TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  #                            TaperFormula = function(DAB, HOM, TaperParameter) DAB / (2 * exp(- TaperParameter*(HOM - DefaultHOM))),
  #
  #
  #                            DetectOnly = DetectOnly)

  #### Recruitment ####

  Data <- RecruitmentCorrection(Data,
                                MinDBH = MinDBH,
                                PositiveGrowthThreshold = PositiveGrowthThreshold,
                                DetectOnly = DetectOnly
  )


  return(Data)
}
