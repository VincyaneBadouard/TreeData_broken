#' Phylogenetic Hierarchical Correction
#'
#' @description Corrects a series of abnormal diameters of the tree using the
#'   mean growth of other trees (with a minimum number of trees) of the same
#'   species/genus/family/stand of the same diameter class.
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree)
#'   measurements (data.table)
#'
#' @param Data (data.table)
#'   The dataset must contain the columns:
#'   - 'IdTree' (character)
#'   - 'Diameter' (numeric)
#'   - 'Year' (numeric)
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
#'   - Estimate the value that the 1st abnormal DBH should have: Estimated DBH =
#'   previous DBH value + estimated growth by regression interpolation
#'
#'   - Find other individual of the the same diameter class (*DBHRange*) and of
#'   the same species, genus, family or stand than the tree to correct, until
#'   the minimum number of individuals required to consider the correction is
#'   reached (*MinIndividualNbr*)
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

  # Estimate the shifted DBH ------------------------------------------------------------------------------------------------
  if(length(cresc[!is.na(cresc)]) > 0){
    # EstDBH <- previous value + estimated cresc by regression interpolation

    # Check that only non-abnormal growths are kept
    if(length(which(cresc[!is.na(cresc)] >= PositiveGrowthThreshold | cresc_abs[!is.na(cresc_abs)] < NegativeGrowthThreshold))==0){

      # Estimate cresc by regression
      cresc_Corr <- RegressionInterpolation(Y = cresc, X = Time[-1], CorrectionType = "quadratic")

      EstDBH <- vector("numeric")
      # rs = 1
      for(rs in 1:length(cresc_abn)){  # as many rs as POM changes
        # EstDBH = previous value + estimated cresc by regression interpolation
        EstDBH[rs] <- DBHCor[cresc_abn[rs]] + cresc_Corr[cresc_abn[rs]]*diff(Time)[cresc_abn[rs]]


        # Find colleagues ---------------------------------------------------------------------------------------------------------
        ## Species level
        Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                             ScientificName == unique(DataTree$ScientificName) &
                             (Diameter > (EstDBH - DBHRange/2) & Diameter < (EstDBH + DBHRange/2))] # Diameter or DBHCor ?

        if(length(unique(Colleagues[, IdTree])) >= MinIndividualNbr){ Method <- "species"

        }else{
          ## Genus level
          Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                               Genus == unique(DataTree$Genus) &
                               (Diameter > (EstDBH - DBHRange/2) & Diameter < (EstDBH + DBHRange/2))] # Diameter or DBHCor ?

          if(length(unique(Colleagues[, IdTree])) >= MinIndividualNbr){ Method <- "genus"

          }else{
            ## Family level
            Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                                 Family == unique(DataTree$Family) &
                                 (Diameter > (EstDBH - DBHRange/2) & Diameter < (EstDBH + DBHRange/2))] # Diameter or DBHCor ?

            if(length(unique(Colleagues[, IdTree])) >= MinIndividualNbr){ Method <- "family"

            }else{
              ## Stand level
              Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                                   Diameter > (EstDBH - DBHRange/2) & Diameter < (EstDBH + DBHRange/2)] # Diameter or DBHCor ?

              if(length(unique(Colleagues[, IdTree])) >= MinIndividualNbr){ Method <- "stand"

              }else{stop("Not enough individuals in your dataset to apply the 'phylogenetic hierarchical' correction even at the 'stand' level.
                       You asked for a minimum of ", MinIndividualNbr," individuals ('MinIndividualNbr' argument)")}

            } # end neither species nor genus or family level
          } # end neither species nor genus level
        } # end not species level

        # Compute mean diameter incrementation of the Colleagues (ColleaguesCrescMean) -----------------------------------------
        ColleaguesCrescMean <- ComputeColleaguesGrowthMean(Colleagues = Colleagues, Data = Data,
                                                           EstDBH = EstDBH,
                                                           PositiveGrowthThreshold = PositiveGrowthThreshold,
                                                           NegativeGrowthThreshold = NegativeGrowthThreshold,
                                                           DBHRange = DBHRange)

        # Correct init shift ------------------------------------------------------------------------------------------------------
        # DBH[init shift] =  previous value + ColleaguesCrescMean
        DBHCor[cresc_abn[rs]+1] <- DBHCor[cresc_abn[rs]] + ColleaguesCrescMean*diff(Time)[cresc_abn[rs]] # Correct with the corrected cresc, the corrected DBH

        # Add the column with the correction method  ------------------------------------------------------------------------
        DataTree[cresc_abn[rs]+1, DiameterCorrectionMeth := Method]

        # Correct the shift -------------------------------------------------------------------------------------------------------
        for(i in (cresc_abn[rs]+2): min(cresc_abn[rs+1], length(DBHCor), na.rm = TRUE)){ # i = each value in a shift
          # DBH[shift] = previous value + their cresc_abs
          DBHCor[i] <- # then correct the other shift values
            DBHCor[i-1] + # New position of the previous value
            cresc_abs[i-1] #  cresc_abs of the value we are correcting, not recalculated

          # Add the column with the correction method  ------------------------------------------------------------------------
          DataTree[i, DiameterCorrectionMeth := "shift realignment"]

        }

      } # end rs loop

    }else{stop("There are still abnormal growths not detected upstream (method to be improved)")}

  }else{stop("ah indeed the case no cresc exists, code the method in this case")}


  # 'DBHCor' vector in DataTree -------------------------------------------------------------------------------------------
  DataTree[,DBHCor := DBHCor]

  return(DataTree)

}

