test_that("PhylogeneticHierarchicalCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  data(TestData)
  DataTree <- TestData[IdStem %in% "100621_1_auto"]

  DataTree$Year <-  c(2000, 2002, 2004, 2006, 2008)
  DataTree$Diameter <- c(13, 14, 11, 12, 13)
  cresc <- c(0.5, NA, 0.5, 0.5)
  cresc_abs <- c(1, NA, 1, 1)
  cresc_abn <- 3

  NoSctficDataTree <- DataTree[, !c("ScientificName")]
  NoSctficData <- TestData[, !c("ScientificName")]


  # Check the function argument ----------------------------------------------------------------------------------------

  expect_error(PhylogeneticHierarchicalCorrection(NoSctficDataTree),
               regexp = "'DataTree' must contain the 'ScientificName' column to apply the phylogenetic hierarchical correction")

  expect_error(PhylogeneticHierarchicalCorrection(DataTree, NoSctficData),
               regexp = "'Data' must contain the 'ScientificName' column to apply the phylogenetic hierarchical correction")


  # Check the function work ---------------------------------------------------------------------------------------------------------

  Rslt <- PhylogeneticHierarchicalCorrection(DataTree = DataTree,
                                             Data = TestData,
                                             cresc = cresc, cresc_abs = cresc_abs, cresc_abn = cresc_abn,
                                             DBHCor = DataTree$Diameter, Time = DataTree$Year,
                                             PositiveGrowthThreshold = 5,
                                             NegativeGrowthThreshold = -2,
                                             DBHRange = 10, MinIndividualNbr = 5)


  expect_true(all(c("DBHCor", "DiameterCorrectionMeth") %in% names(Rslt)))

  # Add a "DiameterCorrectionMeth" value when "Diameter" != "DBHCor"
  Methode <- Rslt[, DiameterCorrectionMeth] != ""

  compareNA <- function(v1,v2) { # function to compare values, including NA
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  expect_true(all(!compareNA(Rslt$Diameter, Rslt$DBHCor) == Methode))

  # Check the value of the "DiameterCorrectionMeth" column
  expect_true(all(Rslt$DiameterCorrectionMeth[Methode] %in% c(
    "species","genus","family","stand","shift realignment")
  ))

})
