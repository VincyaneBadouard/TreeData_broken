#' Phylogenetic Hierarchical Correction
#'
#' @description Corrects a series of abnormal diameters of the tree using the
#'   mean growth of other trees (with a minimum number of trees) of the same
#'   species/genus/family/stand of the same diameter class.
#'
#' @param DataTree A dataset corresponding to a single tree/stem's (1
#'   IdTree/IdStem) measurements (data.table)
#'   The dataset must contain the columns:
#'   - `IdStem` (character)
#'   - `ScientificNameCor` (character)
#'   - `GenusCor` (character)
#'   - `FamilyCor` (character)
#'   - `Diameter` (numeric)
#'   - `Year` (numeric)
#'
#' @param Data Complete dataset (data.table)
#'   The dataset must contain the columns:
#'   - `IdStem` (character)
#'   - `ScientificNameCor` (character)
#'   - `GenusCor` (character)
#'   - `FamilyCor` (character)
#'   - `Diameter` (numeric)
#'   - `Year` (numeric)
#'
#' @param cresc Annual diameter increment (numeric)
#' @param cresc_abs Absolute diameter increment (not divided by time between 2
#'   values) (numeric)
#' @param cresc_abn Abnormal diameter increment positions (numeric)
#'
#' @param DBHCor Diameter vector in cm (numeric)
#' @param Time Time variable in years (numeric)
#'
#' @param PositiveGrowthThreshold in cm/year : a tree
#'   widening by more than x cm/year is considered abnormal (numeric, 1 value)
#'
#' @param NegativeGrowthThreshold in cm/census : The possible
#'   positive measurement error (+n) cannot be corrected until the growth
#'   appears abnormal, but a negative measurement error can be allowed until -n
#'   (a tree does not decrease). Thus the positive measurement error (+n) is
#'   "compensated". (numeric, 1 value)
#'
#' @param DBHRange DBH range in cm to take into account to select other trees in
#'   the dataset to apply "phylogenetic hierarchical" correction (Default: 10
#'   cm) (numeric, 1 value)
#' @param MinIndividualNbr Minimum number of individuals to take into account in
#'   "phylogenetic hierarchical" correction (Default: 5) (numeric, 1 value)
#'
#' @return Fill columns:
#'   - *DBHCor*: corrected trees diameter at default HOM
#'   - *DiameterCorrectionMeth* = "species"/"genus"/"family"/"stand"/"shift
#'   realignment"
#'
#' @details It is assumed that these abnormal diameters were measured at a
#'   different height than the default. We therefore correct the 1st value
#'   with the mean growth of other trees. The following values are
#'   corrected from the corrected value of the 1st value, keeping the
#'   originally measured growth.
#'
#' + Correct the 1st value in the series:
#'   - Find other individual of the same diameter class (*DBHRange*) as the last
#'   value before the one to correct, and of the same species, genus, family or
#'   stand than the tree to correct, until the minimum number of individuals
#'   required to consider the correction is reached (*MinIndividualNbr*)
#'
#'   - 1st abnormal DBH in the series = previous value + mean growth of the
#'       other trees
#'
#' + Correct the following values in the series: The other abnormal DBH in the
#'     series = previous value + their original growth.
#'
#' @export
#'
#' @examples
#' data(TestData)
#' DataTree <- TestData[IdTree %in% "100658"]
#'
#' # Inputs
#' DataTree$Year <-  c(2000, 2002, 2004, 2006, 2008, 2010)
#' DataTree$Diameter <- c(13, 14, 15, 12, 13, 14)
#' cresc <- c(0.5, 0.5, NA, 0.5, 0.5)
#' cresc_abs <- c(1, 1, NA, 1, 1)
#' cresc_abn <- 3
#'
#'
#' DataTree_output <- PhylogeneticHierarchicalCorrection(
#'     DataTree = DataTree,
#'     Data = TestData,
#'     cresc = cresc, cresc_abs = cresc_abs, cresc_abn = cresc_abn,
#'     DBHCor = DataTree$Diameter, Time = DataTree$Year,
#'     PositiveGrowthThreshold = 5,
#'     NegativeGrowthThreshold = -2,
#'     DBHRange = 10, MinIndividualNbr = 5
#' )

PhylogeneticHierarchicalCorrection <- function(
  DataTree,
  Data,
  cresc,
  cresc_abs,
  cresc_abn,
  DBHCor,
  Time,
  PositiveGrowthThreshold,
  NegativeGrowthThreshold,
  DBHRange = 10,
  MinIndividualNbr = 5
){

  # Secondary columns
  BotaCol <- c("ScientificName", "Genus", "Family")

  for(c in BotaCol){
    cc <- paste0(c, "Cor")

    ## Corrected col or not corrected?
    if(cc %in% names(DataTree)){
      if(c=="ScientificName") SfcName <- cc
      if(c=="Genus") GenName <- cc
      if(c=="Family") FamName <- cc


    }else if(!cc %in% names(DataTree) & c %in% names(DataTree)){
      if(c=="ScientificName") SfcName <- c
      if(c=="Genus") GenName <- c
      if(c=="Family") FamName <- c

    }else if(!any(c(cc, c) %in% names(DataTree)))

      stop("'DataTree' must contain the ",cc," or ",c," column to apply the phylogenetic hierarchical correction")
  }

  #### Arguments check ####

  # Check if the bota columns exist in Data
  BotaCol <- c(SfcName, GenName, FamName)
  for(B in BotaCol){
    if(!B %in% names(Data)){
      stop("'Data' must contain the ",B," column to apply the phylogenetic hierarchical correction")
    }
  }

  #### Function ####

  # rs = 1
  for(rs in 1:length(cresc_abn)){  # as many rs as POM changes

    for(p in cresc_abn[rs]:1){ # if previous value is NA, take the takes the one before etc

      if(!is.na(DBHCor[p])){ # when previous value is not NA

        # The previous value
        PrevValue <- DBHCor[p]

        break # stop the loop, and p stay in the environnement
      }
    }


    # Find colleagues ---------------------------------------------------------------------------------------------------------
    ## Species level
    Colleagues <- Data[get(SfcName) == unique(DataTree[,get(SfcName)]) &
                         (Diameter > (PrevValue - DBHRange/2) & Diameter < (PrevValue + DBHRange/2))] # Diameter or DBHCor ?

    Colleagues <- Colleagues[IdStem %in% Colleagues[duplicated(IdStem), IdStem]] # more than 1 diameter value


    if(length(unique(Colleagues[, IdStem])) >= MinIndividualNbr){ Method <- "species"

    }else{
      ## Genus level
      Colleagues <- Data[get(GenName) == unique(DataTree[, get(GenName)]) &
                           (Diameter > (PrevValue - DBHRange/2) & Diameter < (PrevValue + DBHRange/2))] # Diameter or DBHCor ?

      Colleagues <- Colleagues[IdStem %in% Colleagues[duplicated(IdStem), IdStem]] # more than 1 diameter value

      if(length(unique(Colleagues[, IdStem])) >= MinIndividualNbr){ Method <- "genus"

      }else{
        ## Family level
        Colleagues <- Data[get(FamName) == unique(DataTree[,get(FamName)]) &
                             (Diameter > (PrevValue - DBHRange/2) & Diameter < (PrevValue + DBHRange/2))] # Diameter or DBHCor ?

        Colleagues <- Colleagues[IdStem %in% Colleagues[duplicated(IdStem), IdStem]] # more than 1 diameter value

        if(length(unique(Colleagues[, IdStem])) >= MinIndividualNbr){ Method <- "family"

        }else{
          ## Stand level
          Colleagues <- Data[Diameter > (PrevValue - DBHRange/2) & Diameter < (PrevValue + DBHRange/2)] # Diameter or DBHCor ?

          Colleagues <- Colleagues[IdStem %in% Colleagues[duplicated(IdStem), IdStem]] # more than 1 diameter value


          if(length(unique(Colleagues[, IdStem])) >= MinIndividualNbr){ Method <- "stand"

          }else if(nrow(Colleagues) == 0){
            stop("There are no individuals of the same species (",unique(DataTree[,get(SfcName)]),")
                     and diameter category(",PrevValue - DBHRange/2,";",PrevValue + DBHRange/2,")
                     as the estimated diameter(",PrevValue,") of the stem ",unique(DataTree$IdStem),"")

          }else{stop("Not enough individuals in your dataset to apply the 'phylogenetic hierarchical' correction even at the 'stand' level.
                       You asked for a minimum of ", MinIndividualNbr," individuals ('MinIndividualNbr' argument)")}

        } # end neither species nor genus or family level
      } # end neither species nor genus level
    } # end not species level

    # Compute mean diameter incrementation of the Colleagues (ColleaguesCrescMean) -----------------------------------------
    ColleaguesCrescMean <- ComputeColleaguesGrowthMean(Colleagues = Colleagues, Data = Data,
                                                       PrevValue = PrevValue,
                                                       PositiveGrowthThreshold = PositiveGrowthThreshold,
                                                       NegativeGrowthThreshold = NegativeGrowthThreshold,
                                                       DBHRange = DBHRange)

    # Correct init shift ------------------------------------------------------------------------------------------------------
    # DBH[init shift] =  previous value + ColleaguesCrescMean

    # p the previous value non-NA
    DBHCor[cresc_abn[rs]+1] <- DBHCor[p] + ColleaguesCrescMean*(Time[cresc_abn[rs]+1]-Time[p]) # Correct with the corrected cresc, the corrected DBH

    # Add the column with the correction method  ------------------------------------------------------------------------

    DataTree <- GenerateComment(DataTree,
                                condition = as.numeric(rownames(DataTree)) %in% (cresc_abn[rs]+1),
                                comment = Method,
                                column = "DiameterCorrectionMeth")

    if(length(DBHCor) > (cresc_abn[rs]+1)){ # if the init shift is not the last diameter value

      # Correct the shift -------------------------------------------------------------------------------------------------------
      for(i in (cresc_abn[rs]+2): min(cresc_abn[rs+1], length(DBHCor), na.rm = TRUE)){ # i = each value in a shift
        # DBH[shift] = previous value + their cresc_abs

        # If NA in cresc_abs replace it by a interpolation value
        cresc_abs_Corr <- RegressionInterpolation(Y = cresc_abs, X = Time[-1], CorrectionType = "linear") # Compute the corrected cresc

        DBHCor[i] <- # then correct the other shift values
          DBHCor[i-1] + # New position of the previous value
          cresc_abs_Corr[i-1] #  cresc_abs of the value we are correcting, not recalculated

        # Add the column with the correction method  ------------------------------------------------------------------------

        DataTree <- GenerateComment(DataTree,
                                    condition = as.numeric(rownames(DataTree)) %in% (i),
                                    comment = "shift realignment",
                                    column = "DiameterCorrectionMeth")

      } # end i loop

    } # end : if the init shift is not the last diameter value

  } # end rs loop

  # 'DBHCor' vector in DataTree -------------------------------------------------------------------------------------------
  if("DBHCor" %in% names(DataTree)){
    DataTree[, DBHCor := NULL] # remove the DBHCor col to avoid conflict
  }

  DataTree[, DBHCor := DBHCor]

  return(DataTree)

}

