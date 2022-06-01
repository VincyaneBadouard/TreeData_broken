test_that("PhylogeneticHierarchicalCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  data(TestData)
  DataTree <- TestData[IdTree %in% "100658"]

  DataTree$Year <-  c(2000, 2002, 2004, 2006, 2008, 2010)
  DataTree$DBH <- c(13, 14, 15, 12, 13, 14)
  cresc <- c(0.5, 0.5, NA, 0.5, 0.5)
  cresc_abs <- c(1, 1, NA, 1, 1)
  cresc_abn <- 3

  # Check the function work ---------------------------------------------------------------------------------------------------------

  Rslt <- PhylogeneticHierarchicalCorrection(DataTree = DataTree,
                                             Data = TestData,
                                             cresc = cresc, cresc_abs = cresc_abs, cresc_abn = cresc_abn,
                                             DBHCor = DataTree$DBH, Time = DataTree$Year,
                                             PositiveGrowthThreshold = 5,
                                             NegativeGrowthThreshold = -2,
                                             DBHRange = 10, MinIndividualNbr = 5)


  expect_true(all(c("DBHCor", "DiameterCorrectionMeth") %in% names(Rslt)))

  # Add a "DiameterCorrectionMeth" value when "DBH" != "DBHCor"
  Methode <- !is.na(Rslt[, DiameterCorrectionMeth])

  compareNA <- function(v1,v2) { # function to compare values, including NA
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  expect_true(all(!compareNA(Rslt$DBH, Rslt$DBHCor) == Methode))

  # Check the value of the "DiameterCorrectionMeth" column
  expect_true(all(Rslt$DiameterCorrectionMeth[!is.na(Rslt$DiameterCorrectionMeth)] %in% c(
    "species","genus","family","stand","shift realignment")
  ))

})
