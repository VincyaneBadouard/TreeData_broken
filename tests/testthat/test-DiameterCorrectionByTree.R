test_that("DiameterCorrectionByTree", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  TestData <- data.table(IdTree = "c",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))


  # Create test data ---------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(TestData)
  TwoInd <- copy(TestData)
  TwoInd[Year == 2014, ("IdTree") := "b"] # a NA in the "e" Diameter seq


  # Check the function argument ----------------------------------------------------------------------------------------

  expect_error(DiameterCorrectionByTree(MatrixData),
               regexp = "DataTree must be a data.frame or data.table")

  expect_error(DiameterCorrectionByTree(TwoInd),
               regexp = "DataTree must correspond to only 1 same tree so 1 same IdTree")

  # Check the function work --------------------------------------------------------------------------------------------

  ## Case only 1 Diameter value ---------------------------------------------------------------------------------------------
  OnlyOne <- data.table(IdTree = "c",
                        Year = 2000,
                        Diameter = 12,
                        POM = 0,
                        HOM = 1.3)


  Rslt <- DiameterCorrectionByTree(OnlyOne,
                                   TestData,
                                   WhatToCorrect = c("POM change", "punctual", "shift"),
                                   CorrectionType = c("taper", "quadratic", "linear",
                                                      "individual", "phylogenetic hierarchical"))

  expect_true(Rslt$Diameter == OnlyOne$Diameter) # same Diameter value
  expect_true(!"DBHCor" %in% names(Rslt)) # no correction

  ## Case NA in the Diameter column -----------------------------------------------------------------------------------------

  # Taper correction ---------------------------------------------------------------------------------------------------

  # Correction with POM ------------------------------------------------------------------------------------------------
  ## individual correction ---------------------------------------------------------------------------------------------
  ## phylogenetic hierarchical correction ------------------------------------------------------------------------------

  # Punctual error -----------------------------------------------------------------------------------------------------

  # Shift error --------------------------------------------------------------------------------------------------------
  ## individual correction ---------------------------------------------------------------------------------------------
  ## phylogenetic hierarchical correction ------------------------------------------------------------------------------

  # Mix cases
  # punctual + shift error
  # taper + punctual error
  # taper + shift error
  # POM change + punctual error
  # POM change + shift error
  # taper + POM change


})

#
