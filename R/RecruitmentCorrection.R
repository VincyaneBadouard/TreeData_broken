#' RecruitmentCorrection
#'
#' @param Data  (data.frame or data.table)
#' The dataset should preferably contain the column of corrected diameters:
#' 'DBHCor', otherwise the function will take the column "DBH"
#'
#' @param MinDBH Minimum diameter of trees inventoried according to your
#'   protocol (in cm) (numeric, 1 value) (Default = 10 cm)
#'
#' @param PositiveGrowthThreshold A tree widening by more than x cm/year is
#'   considered abnormal (numeric, 1 value) (Default = 5 cm)
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (logical)
#'
#' @details If the size of the tree has never changed, or if there is only one
#'   value the same value is kept for the added forgotten recruits.
#'   If the DBH has not been corrected (DBCor column does not exist), the
#'   function will create it for the forgotten recruits. It is strongly
#'   recommended to correct the DBH before correcting the recruits
#'
#' @return  Add rows for forgotten recruits with them estimated DBH in the
#'   'DBHCor' column, create a 'CorrectedRecruit' col (logical) and fill the
#'   'Comment' column : "This DBH is/was the 1st recorded for this tree,
#'   according to its annual growth and the census done for this plot, it should
#'   have been recruited earlier according to your protocol (MinDBH)."
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom stats na.omit lm
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#' setnames(TestData, "DBH", "DBHCor")
#'
#' Rslt <- RecruitmentCorrection(TestData)
#' IdCorr <- Rslt[CorrectedRecruit == TRUE, IdTree]
#' TreesCorr <- Rslt[IdTree %in% IdCorr]
#'
#' library(ggplot2)
#' ggplot(TreesCorr) +
#' aes(x = CensusYear, y = DBHCor) +
#'   geom_line(size = 0.5, colour = "#112446") +
#'   geom_point(shape = "circle", size = 1.5, mapping = aes(color = CorrectedRecruit)) +
#'   theme_minimal() +
#'   facet_wrap(vars(IdTree), scales = "free")
#'
RecruitmentCorrection <- function(
  Data,
  MinDBH = 10,
  PositiveGrowthThreshold = 5,
  InvariantColumns = c("Site",
                       "Genus",
                       "Species",
                       "Family",
                       "ScientificName"),
  DetectOnly = FALSE
){

  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # MinDBH/PositiveGrowthThreshold (numeric, 1 value)
  if(!all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold), length)) %in% 1) |
     !all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'MinDBH' and 'PositiveGrowthThreshold'' arguments
         of the 'RecruitmentCorrection' function must be 1 numeric value each")

  if(DetectOnly %in% FALSE){
    # InvariantColumns
    if (!inherits(InvariantColumns, "character"))
      stop("'InvariantColumns' argument must be of character class")
  }

  # DetectOnly (logical)
  if(!all(unlist(lapply(list(DetectOnly),
                        inherits, "logical"))))
    stop("The 'DetectOnly' argument
         of the 'RecruitmentCorrection' function must be logicals")

  # DBHCor column exists
  if(!"DBH" %in% names(Data) & !"DBHCor" %in% names(Data))
    stop("The 'DBH' (Diameter at Breast Height) or the 'DBHCor' (corrected Diameter at Breast Height)
           column does't exist in the dataset.")

  if(DetectOnly %in% FALSE){
    # Check if the InvariantColumns name exists in Data
    for(c in InvariantColumns){
      if(!c %in% names(Data)){
        stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                   ,c,"is apparently not a dataset's column"))
      }
    }
  }

  #### Function ####

  # data.frame to data.table
  setDT(Data)

  if(!"Comment" %in% names(Data)) Data[, Comment := ""]

  if(DetectOnly %in% FALSE){
    Data[, CorrectedRecruit := FALSE] # The initial rows are not corrected recruits
  }

  # Order IdTrees and times in ascending order
  Data <- Data[order(IdTree, CensusYear)]

  # IdTrees vector
  Ids <- as.vector(na.omit(unique(Data$IdTree))) # Tree Ids

  # Dataset with the rows without IdTree
  DataIDNa <- Data[is.na(IdTree)]

  # Apply for all the trees
  # i = "100635"
  Data <- do.call(rbind, lapply(Ids, function(i) RecruitmentCorrectionByTree(
    Data[IdTree %in% i], # per IdTree, all censuses
    MinDBH = MinDBH,
    PositiveGrowthThreshold = PositiveGrowthThreshold,
    InvariantColumns = InvariantColumns,
    PlotCensuses = as.vector(na.omit( # rm NA
      unique(Data[Plot %in% unique(Data[IdTree %in% i, Plot]),  CensusYear]) # the censuses for the plot in which the tree is
    )),
    DetectOnly = DetectOnly
  )
  )) # do.call apply the 'rbind' to the lapply result

  # Re-put the rows without IdTree
  Data <- rbindlist(list(Data, DataIDNa), use.names=TRUE, fill=TRUE)

  return(Data)

}


#' RecruitmentCorrectionByTree
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree)
#'   measurements (data.frame or data.table). The dataset should preferably
#'   contain the column of corrected diameters: 'DBHCor', otherwise the function
#'   will take the column "DBH"
#'
#' @param MinDBH Minimum diameter of trees inventoried according to your
#'   protocol (in cm) (numeric, 1 value)
#'
#' @param PositiveGrowthThreshold A tree widening by more than x
#'   cm/year is considered abnormal (numeric, 1 value)
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character)
#'
#' @param PlotCensuses Census years for the plot in which the tree is (integer)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (logical)
#'
#' @details If the size of the tree has never changed, or if there is only one
#'   value the same value is kept for the added forgotten recruits.
#'   If the DBH has not been corrected (DBCor column does not exist), the
#'   function will create it for the forgotten recruits. It is strongly
#'   recommended to correct the DBH before correcting the recruits
#'
#' @return  Add rows for forgotten recruits with them estimated DBH in the
#'   'DBHCor' column, create a 'CorrectedRecruit' col (logical) and fill the
#'   'Comment' column : "This DBH is/was the 1st recorded for this tree,
#'   according to its annual growth and the census done for this plot, it should
#'   have been recruited earlier according to your protocol (MinDBH)."
#'
#' @importFrom data.table data.table rbindlist copy
#' @importFrom stats na.omit lm
#'
#' @export
#'
#' @examples
#' library(data.table)
#' DataTree <- data.table(IdTree = "a",
#'                        CensusYear = seq(2000,2008, by = 2), # 2 years/census
#'                        DBHCor  = as.numeric(c(13:17)), # 1cm/census(0.5 cm/year)
#'                        Site = "Imaginary forest"
#' )
#' # 1st DBH = 13 > MinDBH. In 1998 the tree was 12cm, 11cm in 1996 and 10cm in
#' 1994.
#' Rslt <- RecruitmentCorrectionByTree(DataTree,
#'                                     InvariantColumns = "Site",
#'                                     PlotCensuses = seq(1996,2016, by = 2))
#'
RecruitmentCorrectionByTree <- function(
  DataTree,
  MinDBH = 10,
  PositiveGrowthThreshold = 5, # je garde ?
  InvariantColumns = c("Site",
                       "Genus",
                       "Species",
                       "Family",
                       "ScientificName"),
  PlotCensuses,
  DetectOnly = FALSE
){

  #### Arguments check ####
  # DataTree
  if (!inherits(DataTree, c("data.table", "data.frame")))
    stop("DataTree must be a data.frame or data.table")

  # MinDBH/PositiveGrowthThreshold (numeric, 1 value)
  if(!all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold), length)) %in% 1) |
     !all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'MinDBH' and 'PositiveGrowthThreshold'' arguments
         of the 'RecruitmentCorrectionByTree' function must be 1 numeric value each")

  # InvariantColumns
  if (!inherits(InvariantColumns, "character"))
    stop("'InvariantColumns' argument must be of character class")

  # PlotCensuses
  if (!inherits(PlotCensuses, c("numeric", "integer")))
    stop("'PlotCensuses' argument must be numeric or integer")

  # DetectOnly (logical)
  if(!all(unlist(lapply(list(DetectOnly),
                        inherits, "logical"))))
    stop("The 'DetectOnly' argument
         of the 'RecruitmentCorrectionByTree' function must be logicals")

  if(!"DBH" %in% names(DataTree) & !"DBHCor" %in% names(DataTree))
    stop("The 'DBH' (Diameter at Breast Height) or the 'DBHCor' (corrected Diameter at Breast Height)
           column does't exist in the dataset.")

  if(DetectOnly %in% FALSE){
    # DBHCor column exists
    if(!"DBHCor" %in% names(DataTree))
      warning("The 'DBHCor' (corrected Diameter at Breast Height) column does't exist in the dataset.
         We advise to first correct the diameter measurements before correcting the recruitment")
  }

  # if there are several IdTrees
  if(length(unique(DataTree$IdTree)) != 1){
    stop("DataTree must correspond to only 1 same tree so 1 same IdTree
    (the IdTrees: " ,paste0(unique(DataTree$IdTree), collapse = "/"),")")
  }

  # if there are several plots for the same IdTree
  # if(length(as.vector(na.omit(unique(DataTree$Plot)))) != 1){
  #   stop(paste0("Tree ",unique(DataTree$IdTree)," has multiple plots: " ,paste0(unique(DataTree$Plot), collapse = "/")))
  # }

  # Check if the InvariantColumns name exists in DataTree
  for(c in InvariantColumns){
    if(!c %in% names(DataTree)){
      stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                 ,c,"is apparently not a dataset's column"))
    }
  }

  #### Function ####

  # print(unique(DataTree[, IdTree])) # to debug

  # data.frame to data.table
  setDT(DataTree)


  # if there are no 'DBHCor' col, create it from the DBH col
  InitialDT <- copy(DataTree)
  if(!"DBHCor" %in% names(DataTree))
    DataTree[, DBHCor := DBH]

  # Arrange year in ascending order
  PlotCensuses <- sort(PlotCensuses, decreasing = FALSE) # order plot census years
  DataTree <- DataTree[order(CensusYear)] # order years in the data

  #### Compute annual diameter incrementation ####
  DBHCor <- DataTree[,DBHCor]
  CensusYear <- DataTree[,CensusYear]
  # Initialisation
  cresc <- rep(0, length(DBHCor) - 1) # (cresc[1] corresponds to the 2nd DBH)

  if (sum(!is.na(DBHCor)) > 1) { # if there is at least 1 measurement

    cresc[which(!is.na(DBHCor))[-1] - 1] <- # 8 cresc for 9 dbh values ([-1]), shift all indices by 1 to the left (-1)
      diff(DBHCor[!is.na(DBHCor)]) / diff(CensusYear[!is.na(DBHCor)]) # DBH difference between pairwise censuses / time difference between pairwise censuses

  }

  #### Detect the overgrown recruits ####

  RecruitYear <- min(DataTree[, CensusYear], na.rm = TRUE) # recruitment year

  if(RecruitYear > min(PlotCensuses, na.rm = TRUE) & sum(!is.na(DataTree$DBHCor)) > 0){ # if the 1st census of the plot is smaller than the 1st census of the tree, and there are measured DBH

    PrevCens <- PlotCensuses[which(PlotCensuses == RecruitYear)-1] # 1 census before the recruit year among the plot censuses

    FirstDBH <- DataTree[!is.na(DBHCor), DBHCor][1] # 1st measured DBH

    # if(DataTree$DBHCor[1] > (MinDBH + (RecruitYear - PrevCens) * PositiveGrowthThreshold)){ # ah ben il detecte pas mes oublis pcq il considère l'erreur que si l'écart est superieur à la limite de croissance

    # Growth criteria
    if(length(cresc) > 0){ # if there are a growth
      Growth <- cresc[1]
    }else{Growth <- PositiveGrowthThreshold} # if only 1 DBH value (no cresc)

    # Detection
    #### If the 1st DBH is larger than it would have been if at the previous census it was at the minimum DBH
    if(FirstDBH > (MinDBH + (RecruitYear - PrevCens) * Growth)){ # ma proposition

      DataTree <- GenerateComment(DataTree,
                                  condition = DataTree[, CensusYear]  %in% RecruitYear,
                                  comment = "This DBH is/was the 1st recorded for this tree,
                                  according to its annual growth and the census done for this plot,
                                  it should have been recruited earlier according to your protocol (MinDBH).")

      if(DetectOnly %in% FALSE){

        MissingCens <- PlotCensuses[which(PlotCensuses < RecruitYear)] # the previous missing censuses

        #### Create new rows for the forgotten recruits ####
        if(length(MissingCens) > 0){
          if("Plot" %in% names(DataTree)){ # if we have the plot info
            NewRow <- data.table(IdTree = unique(DataTree$IdTree), # the IdTree
                                 CensusYear = NA, # the censuses to add
                                 Plot = unique(DataTree$Plot), # the unique plot of the tree
                                 CorrectedRecruit = TRUE, # there are corrected recruits
                                 stringsAsFactors = FALSE) # do not convert characters into factors
          }
          else{
            NewRow <- data.table(IdTree = unique(DataTree$IdTree), # the IdTree
                                 CensusYear = NA, # the censuses to add
                                 CorrectedRecruit = TRUE, # there are corrected recruits
                                 stringsAsFactors = FALSE) # do not convert characters into factors
          }

          if(length(InvariantColumns) > 0){ # if there are invariant columns

            NewRow[,(InvariantColumns) := NA] # empty the invariant columns for the added rows

            # Fill in the invariant columns in the added rows
            NewRow <- FillinInvariantColumns(NewRow = NewRow,
                                             InvariantColumns = InvariantColumns,
                                             DataTree = DataTree,
                                             IdTree = unique(DataTree$IdTree))
          }

          # Multiply this new row the number of times as well as the number of absents
          NewRows <- do.call("rbind", replicate(n = length(MissingCens), NewRow, simplify = FALSE))
          NewRows[, CensusYear := MissingCens]

          # Add these rows in the dataset
          DataTree <- rbindlist(list(DataTree, NewRows), use.names=TRUE, fill=TRUE)
          DataTree[, CorrectedRecruit := ifelse(is.na(CorrectedRecruit), FALSE, CorrectedRecruit)] # FALSE for the other rows

          DataTree <- DataTree[order(CensusYear)] # order by CensusYear

          #### Linear regression (DBH ~ Year) #### with all the DBH values
          coef <- stats::lm(
            DataTree[!is.na(DBHCor), DBHCor] ~ DataTree[!is.na(DBHCor), CensusYear])$coefficients

          if(is.na(coef[2]) | coef[2] %in% 0) { # if no slope
            ### if only 1 DBH value: replace all non-recruited DBH by this value (pas sure que ce soit une bonne idée)
            DataTree[CensusYear < RecruitYear, ("DBHCor") := unique(DataTree[!is.na(DBHCor), DBHCor])] # DBHCor := coef[1] (ça donne des pptés à la valeur)

          }else{

            # Estimate the recruits DBHCor with linear extrapolation
            RecruitsDBH <- coef[1] + DataTree[CensusYear < RecruitYear, CensusYear]*coef[2] # y = b + ax. Min entre ces DBH inférés et le 1er DBH

            # If estimated DBHCors are higher than the first measured DBHCors, (comment c possible ?)
            # these are replaced by first measured DBHCors.
            for(y in 1: length(RecruitsDBH)){
              RecruitsDBH[y] <- min(RecruitsDBH[y], FirstDBH)
            }
            DataTree[CensusYear < RecruitYear, ("DBHCor") := RecruitsDBH]
          }

          # UselessRows: added trees under the MinDBH
          UselessRows <- ((DataTree[, DBHCor] < MinDBH) & (DataTree[, CensusYear] %in% MissingCens))

          if(any(UselessRows)){
            DataTree <- DataTree[-which(UselessRows)] # remove them
          }


        } # end: missing censuses to add
      } # end: DetectOnly == FALSE
    } # end: overgrown recruit
  } # end: if the plot have previous censuses

  if(!"DBHCor" %in% names(InitialDT)){
    if(DetectOnly %in% TRUE) DataTree[, DBHCor := NULL] # if detect only, remove DBHCor if it didn't exist before

    if(DetectOnly %in% FALSE)
      DataTree[, DBHCor := ifelse(CorrectedRecruit %in% FALSE, NA, DBHCor)] # keep only recruitment correction
  }

  DataTree[is.na(Comment), ("Comment") := ""] # NAs come from NewRow

  return(DataTree)

}
