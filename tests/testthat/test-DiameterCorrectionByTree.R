test_that("DiameterCorrectionByTree", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  DataTree <- data.table(IdTree = "c",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))


  # Create test data ---------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(DataTree)
  TwoInd <- copy(DataTree)
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
                                   OnlyOne,
                                   WhatToCorrect = c("POM change", "punctual", "shift"),
                                   CorrectionType = c("taper", "quadratic", "linear",
                                                      "individual", "phylogenetic hierarchical"))

  expect_true(Rslt$Diameter == OnlyOne$Diameter) # same Diameter value
  expect_true(!"DBHCor" %in% names(Rslt)) # no correction


  # Taper correction ---------------------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = "POM change",
                                   CorrectionType = "taper")

  expect_true(all(Rslt[HOM != 1.3, DBHCor] != DataTree[HOM != 1.3, Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  expect_true(all(Rslt[HOM != 1.3, DiameterCorrectionMeth] == "taper")) # and "taper" in 'DiameterCorrectionMeth'

  # Correction with POM A FAIRE------------------------------------------------------------------------------------------------
  ## individual correction ---------------------------------------------------------------------------------------------
  ## phylogenetic hierarchical correction ------------------------------------------------------------------------------

  # Punctual error -----------------------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         Year = seq(2000,2008, by = 2), # 5 censuses
                         Diameter = c(13, 14, 15, 30, 17), # 0.5 cm/year. The 4th value is abnormal
                         POM = c(0, 0, 1, 1, 2),
                         HOM = c(1.3, 1.3, 1.5, 1.5, 2))

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = "punctual",
                                   CorrectionType = "quadratic")
  # expect_true(Rslt[4, DBHCor] == 16) # the corrected value is 16, NOP c resté NA!!!

  ## Case NA in the Diameter column -----------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         Year = seq(2000,2008, by = 2), # 5 censuses
                         Diameter = c(13, 14, 15, NA, 17), # 0.5 cm/year
                         POM = c(0, 0, 1, 1, 2),
                         HOM = c(1.3, 1.3, 1.5, 1.5, 2))

  # Rslt <- DiameterCorrectionByTree(DataTree,
  #                                  DataTree,
  #                                  WhatToCorrect = "punctual",
  #                                  CorrectionType = "quadratic")
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

  ## Case NA in the Diameter column in any case -----------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         Year = seq(2000,2008, by = 2), # 5 censuses
                         Diameter = c(13, 14, 15, NA, 17), # 0.5 cm/year
                         POM = c(0, 0, 1, 1, 2),
                         HOM = c(1.3, 1.3, 1.5, 1.5, 2))

  # Rslt <- DiameterCorrectionByTree(DataTree,
  #                                  DataTree,
  #                                  WhatToCorrect = c("POM change", "punctual", "shift"),
  #                                  CorrectionType = c("taper", "quadratic", "linear",
  #                                                     "individual", "phylogenetic hierarchical"))

})

#
