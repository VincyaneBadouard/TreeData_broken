## Phylogenetic hieracchical correction
# + *init shift*
#   - Estimer la valeur que devrait avoir le DBH décalé :
#          - if min 1 heathy growth exists : EstDBH = previous value + estimated cresc by regression interpolation
#          - else :   EstDBH = previous value + stand median cresc in :
#                                if TrustMeasSet = "first" :  [HealthyValue ; MaxDBH]
#                                or if TrustMeasSet = "last" :  [MinDBH ; HealthyValue]

#   - AT species, genus or family level : Colleagues = n ind[EstDBH -DBHRange/2 ; EstDBH +DBHRange/2] > MinIndividualNbr
#   - DBH[init shift] = previous value +  mean(cresc[Colleagues]
#
# + *le shift* : DBH[shift] = previous value + their cresc_abs
#
# EXemple:
# Case enough cresc
# Time <- c(2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016, 2020)
# DBHCor <- c(13, 14, 15, 16, 12, 14, 15, 11, 13)
# Case not enough cresc  # en fait ça n'arrive pas en shift. s'il n'y a que deux valeurs de DBH dont une annormale, c'est du ponctuel.
# DBHCor <- c(NA, NA, NA, 16, 12, NA, NA, NA, NA)
# TestData case
# Data <- TestData
# id <- "100658"
# DataTree <- Data[IdTree %in% id]
# IdTree <- unique(Data[IdTree %in% id, IdTree])
#
# plot(Time, DBHCor)
# Compute diameter incrementation without the inits shift
# cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
# cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)
# Detect abnormal growth
# cresc_abn <- which(cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold) # abnormal values indices
# Remove abnormal growths
# cresc[cresc_abn] <- NA
# cresc_abs[cresc_abn] <- NA

PhylogeneticHierarchicalCorrection <- function(
  Data,
  cresc,
  cresc_abn,
  DBHCor,
  Time,
  CorrectionType,
  PositiveGrowthThreshold,
  NegativeGrowthThreshold,
  DBHRange = 10,
  MinIndividualNbr = 2
){

  # Estimate the shifted DBH ------------------------------------------------------------------------------------------------
  if(length(cresc[!is.na(cresc)]) > 0){
    # EstDBH <- previous value + estimated cresc by regression interpolation

    # Check that only non-abnormal growths are kept
    if(length(which(cresc[!is.na(cresc)] >= PositiveGrowthThreshold | cresc_abs[!is.na(cresc_abs)] < NegativeGrowthThreshold))==0){

      # Replace NA by the correction ---------------------------------------------------------------------------------
      cresc_Corr <- RegressionInterpolation(Y = cresc, X = Time[-1], CorrectionType = CorrectionType) # Compute the corrected cresc

      EstDBH <- vector("numeric")
      # rs = 1
      for(rs in 1:length(cresc_abn)){  # as many rs as POM changes
        # EstDBH = previous value + estimated cresc by regression interpolation
        EstDBH[rs] <- DBHCor[cresc_abn[rs]] + cresc_Corr[cresc_abn[rs]]*diff(Time)[cresc_abn[rs]] # Correct with the corrected cresc, the corrected DBH


        # Find colleagues ---------------------------------------------------------------------------------------------------------
        ## Species level
        Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                             ScientificName == unique(DataTree$ScientificName) &
                             (DBH > (EstDBH - DBHRange/2) & DBH < (EstDBH + DBHRange/2))] # DBH or DBHCor ?

        if(nrow(Colleagues) >= MinIndividualNbr){
          # Compute mean(Colleaguescresc)
          ## Compute diameter incrementation of the Colleagues
          cresc <- ComputeIncrementation(Var = Colleagues$DBH, Type = "annual", Time = Colleagues$Year) # DBH or DBHCor ?

          Method <- "species"
        }else{
          ## Genus level
          Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                               Genus == unique(DataTree$Genus) &
                               (DBH > (EstDBH - DBHRange/2) & DBH < (EstDBH + DBHRange/2))] # DBH or DBHCor ?

          if(nrow(Colleagues) >= MinIndividualNbr){
            # Compute mean(Colleaguescresc)
            ## Compute diameter incrementation of the Colleagues

            Method <- "genus"
          }else{
            ## Family level
            Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                                 Family == unique(DataTree$Family) &
                                 (DBH > (EstDBH - DBHRange/2) & DBH < (EstDBH + DBHRange/2))] # DBH or DBHCor ?

            if(nrow(Colleagues) >= MinIndividualNbr){
              # Compute mean(Colleaguescresc)
              ## Compute diameter incrementation of the Colleagues

              Method <- "family"
            }else{
              ## Stand level
              Colleagues <- Data[IdTree != unique(DataTree$IdTree) & # colleagues, not the tree to correct
                                   DBH > (EstDBH - DBHRange/2) & DBH < (EstDBH + DBHRange/2)] # DBH or DBHCor ?

              if(nrow(Colleagues) >= MinIndividualNbr){
                # Compute mean(Colleaguescresc)
                ## Compute diameter incrementation of the Colleagues

                Method <- "stand"
              }else{stop("Not enough individuals in your dataset to apply the 'phylogenetic hierarchical' correction even at the 'stand' level.
                       You asked for a minimum of ", MinIndividualNbr," individuals ('MinIndividualNbr' argument)")}

            } # end neither species nor genus or family level
          } # end neither species nor genus level
        } # end not species level

        # Correct init shift ------------------------------------------------------------------------------------------------------
        # DBH[init shift] =  previous value + mean(Colleaguescresc)
        # DBHCor[cresc_abn[rs]+1] <- DBHCor[cresc_abn[rs]] + cresc_Coll[cresc_abn[rs]]*diff(Time)[cresc_abn[rs]] # Correct with the corrected cresc, the corrected DBH

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


}

