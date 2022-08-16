test_that("DiameterCorrectionByTree", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "A",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))


  # Create test data ---------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(DataTree)
  TwoInd <- copy(DataTree)
  TwoInd[Year == 2014, ("IdTree") := "b"]


  # Check the function argument ----------------------------------------------------------------------------------------

  expect_error(DiameterCorrectionByTree(MatrixData),
               regexp = "DataTree must be a data.frame or data.table")

  expect_error(DiameterCorrectionByTree(TwoInd),
               regexp = "DataTree must correspond to only 1 same tree/stem so 1 same IdTree/IdStem")

  # Check the function work --------------------------------------------------------------------------------------------

  ## Case only 1 Diameter value ----------------------------------------------------------------------------------------
  OnlyOne <- data.table(IdTree = "c",
                        ScientificName = "A",
                        Year = 2000,
                        Diameter = 12,
                        POM = as.factor(0),
                        HOM = 1.3)


  Rslt <- DiameterCorrectionByTree(OnlyOne,
                                   OnlyOne,
                                   WhatToCorrect = c("POM change", "punctual", "shift"),
                                   CorrectionType = c("quadratic", "linear",
                                                      "individual", "phylogenetic hierarchical"))

  expect_true(Rslt$Diameter == OnlyOne$Diameter) # same Diameter value
  expect_true(Rslt$DBHCor == OnlyOne$Diameter) # Keep the original Diameter value

  # Only one but too big
  OnlyOne <- data.table(IdTree = "c",
                        ScientificName = "A",
                        Year = 2000,
                        Diameter = 900,
                        POM = as.factor(0),
                        HOM = 1.3)

  Rslt <- DiameterCorrectionByTree(OnlyOne,
                                   OnlyOne,
                                   WhatToCorrect = c("POM change", "punctual", "shift"),
                                   CorrectionType = c("quadratic", "linear",
                                                      "individual", "phylogenetic hierarchical"))
  expect_true(is.na(Rslt$DBHCor)) # impossible value


  # Only one but = 0 (impossible)
  OnlyOne <- data.table(IdTree = "c",
                        ScientificName = "A",
                        Year = 0,
                        Diameter = 900,
                        POM = as.factor(0),
                        HOM = 1.3)

  Rslt <- DiameterCorrectionByTree(OnlyOne,
                                   OnlyOne,
                                   WhatToCorrect = c("POM change", "punctual", "shift"),
                                   CorrectionType = c("quadratic", "linear",
                                                      "individual", "phylogenetic hierarchical"))
  expect_true(is.na(Rslt$DBHCor)) # impossible value


  # Taper correction ---------------------------------------------------------------------------------------------------
  # DataTree <- data.table(IdTree = "c",
  #                        ScientificName = "A",
  #                        Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
  #                        Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
  #                        POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)),
  #                        HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))
  #
  # Rslt <- suppressWarnings(DiameterCorrectionByTree(DataTree,
  #                                  DataTree,
  #                                  WhatToCorrect = "POM change",
  #                                  CorrectionType = "taper")
  # )
  #
  # expect_true(all(Rslt[HOM != 1.3, DBHCor] != DataTree[HOM != 1.3, Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  # expect_true(all(Rslt[HOM != 1.3, DiameterCorrectionMeth] == "taper")) # and "taper" in 'DiameterCorrectionMeth'
  # expect_warning(DiameterCorrectionByTree(DataTree,
  #                                        DataTree,
  #                                        WhatToCorrect = "POM change",
  #                                        CorrectionType = "taper"),
  #                regexp = "There are still abnormal growths")
  #

  # Punctual error -----------------------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "A",
                         Year = seq(2000,2008, by = 2), # 5 censuses
                         Diameter = c(13, 14, 15, 30, 17), # 0.5 cm/year. The 4th value is abnormal
                         POM = as.factor(c(0, 0, 1, 1, 2)),
                         HOM = c(1.3, 1.3, 1.5, 1.5, 2))

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = "punctual",
                                   CorrectionType = "linear")
  expect_true(Rslt[4, DBHCor] == 16) # the corrected value is 16
  expect_true(all(Rslt[-4, DBHCor] == DataTree[-4, Diameter])) # the other values remain the same
  expect_true(Rslt[4, DiameterCorrectionMeth] == "linear") # and "linear" in 'DiameterCorrectionMeth'
  expect_true(Rslt[4, Comment] == "Abnormal diameter value (punctual error)") # info in 'Comment'
  expect_true(all(Rslt[-4, DiameterCorrectionMeth] == "")) # no correction meth for the other values
  expect_true(all(Rslt[-4, Comment] == "")) # no comment for the other values

  ## Case NA in the Diameter column -----------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "A",
                         Year = seq(2000,2008, by = 2), # 5 censuses
                         Diameter = c(13, 14, 15, NA, 17), # 0.5 cm/year
                         POM = as.factor(c(0, 0, 1, 1, 2)),
                         HOM = c(1.3, 1.3, 1.5, 1.5, 2))

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = "punctual", # (local: 2 pts around the missing value)
                                   CorrectionType = "linear") # we can do only linear regression and not quadratic with 2 points (local)

  expect_true(Rslt[4, DBHCor] == 16) # the corrected value is 16, NOP c resté NA!!!
  expect_true(all(Rslt[-4, DBHCor] == DataTree[-4, Diameter])) # the other values remain the same
  expect_true(Rslt[4, DiameterCorrectionMeth] == "linear") # and "linear" in 'DiameterCorrectionMeth'
  expect_true(all(Rslt[-4, DiameterCorrectionMeth] == "")) # no correction meth for the other values
  expect_true(all(Rslt[, Comment] == "")) # no comment because no error

  # Correction with POM ------------------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "A",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)))
  ## individual correction ---------------------------------------------------------------------------------------------

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = "POM change",
                                   CorrectionType = c("individual", "linear"))

  expect_true(all(Rslt[POM != "0", DBHCor] != DataTree[POM != "0", Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  expect_true(all(Rslt[c(5,8), Comment] == "POM change")) # detect the POM change
  expect_true(all(Rslt[c(5,8), DiameterCorrectionMeth] == "linear")) # the 1st value is correct by linear regression
  expect_true(all(Rslt[c(6,7,9), DiameterCorrectionMeth] == "shift realignment")) # the other value of the shift are just realigned
  expect_true(all(Rslt[, DBHCor] == c(13, 14, 15, 16, 17, 19, 20, 21, 23)))

  ## phylogenetic hierarchical correction ------------------------------------------------------------------------------
  data(TestData)
  DataTree <- TestData[IdTree %in% "100628"]
  DataTree <- DataTree[order(Year)] # order de dt
  DataTree$Diameter <- c(13, 14, 15, 12, 13)
  DataTree$POM = as.factor(c(0, 0, 0, 1, 1))

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   TestData,
                                   WhatToCorrect = "POM change",
                                   CorrectionType = c("phylogenetic hierarchical", "linear"))

  expect_true(all(Rslt[POM != "0", DBHCor] != DataTree[POM != "0", Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  expect_true(Rslt[4, Comment] == "POM change") # detect the POM change
  expect_true(Rslt[4, DiameterCorrectionMeth] %in% c("species","genus","family","stand")) # the 1st value is correct by linear regression
  expect_true(Rslt[5, DiameterCorrectionMeth] == "shift realignment") # the other value of the shift are just realigned

  # Shift error --------------------------------------------------------------------------------------------------------
  ## individual correction ---------------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "A",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)))

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = "shift",
                                   CorrectionType = c("individual", "linear"))

  expect_true(all(Rslt[POM != "0", DBHCor] != DataTree[POM != "0", Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  expect_true(all(Rslt[c(5,8), Comment] == "Abnormal diameter value (shift error)")) # detect the POM change
  expect_true(all(Rslt[c(5,8), DiameterCorrectionMeth] == "linear")) # the 1st value is correct by linear regression
  expect_true(all(Rslt[c(6,7,9), DiameterCorrectionMeth] == "shift realignment")) # the other value of the shift are just realigned
  expect_true(all(Rslt[, DBHCor] == c(13, 14, 15, 16, 17, 19, 20, 21, 23)))

  ## phylogenetic hierarchical correction ------------------------------------------------------------------------------
  data(TestData)
  DataTree <- TestData[IdTree %in% "100771"]
  DataTree <- DataTree[order(Year)] # order de dt
  DataTree$Diameter <- c(13, 14, 11, 12, 13)
  DataTree[, POM := NULL]

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   TestData,
                                   WhatToCorrect = "shift",
                                   CorrectionType = c("phylogenetic hierarchical", "quadratic"))

  expect_true(all(Rslt[3:5, DBHCor] != DataTree[3:5, Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  expect_true(Rslt[3, Comment] == "Abnormal diameter value (shift error)") # detect the POM change
  expect_true(Rslt[3, DiameterCorrectionMeth] %in% c("species","genus","family","stand")) # the 1st value is correct by linear regression
  expect_true(all(Rslt[4:5, DiameterCorrectionMeth] == "shift realignment")) # the other value of the shift are just realigned



  # Mix cases -------------------------------------------------------------------------------------------------------
  # punctual + shift error -------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "A",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13, 14, 24, 16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2)) # 0.5 cm/year

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = c("punctual", "shift"),
                                   CorrectionType = c("individual", "quadratic"))

  expect_true(all(Rslt[3, Comment] == "Abnormal diameter value (punctual error)")) # detect the POM change
  expect_true(all(Rslt[c(5,8), Comment] == "Abnormal diameter value (shift error)")) # detect the POM change
  expect_true(all(Rslt[c(3,5,8), DiameterCorrectionMeth] == "quadratic")) # the 1st value is correct by quadratic regression
  expect_true(all(Rslt[c(6,7,9), DiameterCorrectionMeth] == "shift realignment")) # the other value of the shift are just realigned
  expect_true(all(Rslt[, DBHCor] == c(13, 14, 15, 16, 17, 19, 20, 21, 23)))


  # taper + punctual error --------------------------------------------------------------------------------------------
  # DataTree <- data.table(IdTree = "c",
  #                        ScientificName = "A",
  #                        Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
  #                        Diameter = c(13, 14, 24, 16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
  #                        POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)),
  #                        HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))
  #
  # Rslt <- suppressWarnings(DiameterCorrectionByTree(DataTree,
  #                                  DataTree,
  #                                  WhatToCorrect = c("POM change", "punctual"),
  #                                  CorrectionType = c("taper", "quadratic"))
  #                         )
  #
  # expect_true(all(Rslt[HOM != 1.3, DBHCor] != DataTree[HOM != 1.3, Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  # expect_true(all(Rslt[HOM != 1.3, DiameterCorrectionMeth] %in% "taper"))
  # expect_true(Rslt[3, DiameterCorrectionMeth] %in% "quadratic") # and "quadratic"
  # expect_warning(DiameterCorrectionByTree(DataTree,
  #                                         DataTree,
  #                                         WhatToCorrect = c("POM change", "punctual"),
  #                                         CorrectionType = c("taper", "quadratic")),
  #                regexp = "There are still abnormal growths")

  # taper + shift error ------------------------------------------------------------------------------------
  # DataTree <- data.table(IdTree = "c",
  #                        ScientificName = "A",
  #                        Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
  #                        Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
  #                        POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)),
  #                        HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))
  #
  # Rslt <- DiameterCorrectionByTree(DataTree,
  #                                  DataTree,
  #                                  WhatToCorrect = c("POM change", "shift"),
  #                                  CorrectionType = c("taper", "individual", "linear"))
  #
  # Meth <- Rslt[HOM != 1.3, DiameterCorrectionMeth]
  #
  # expect_true(all(Rslt[HOM != 1.3, DBHCor] != DataTree[HOM != 1.3, Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  # expect_true(all(grepl("taper", Meth, fixed = TRUE))) # "taper" if HOM change
  # expect_true(all(grepl("linear", Meth, fixed = TRUE) | grepl("shift realignment", Meth, fixed = TRUE))) # and "linear" or "shift realignment"

  # POM change + punctual error ------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "A",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13, 14, 24, 16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)))

  Rslt <- DiameterCorrectionByTree(DataTree,
                                   DataTree,
                                   WhatToCorrect = c("POM change", "punctual"),
                                   CorrectionType = c("individual", "quadratic"))

  expect_true(all(Rslt[POM != "0", DBHCor] != DataTree[POM != "0", Diameter])) # corrected DBH when HOM is different of the the 1st HOM
  expect_true(all(Rslt[c(5,8), Comment] == "POM change")) # detect the POM change
  expect_true(all(Rslt[3, Comment] == "Abnormal diameter value (punctual error)")) # detect the punctual error
  expect_true(all(Rslt[c(3,5,8), DiameterCorrectionMeth] == "quadratic")) # the 1st value is correct by linear regression
  expect_true(all(Rslt[c(6,7,9), DiameterCorrectionMeth] == "shift realignment")) # the other value of the shift are just realigned
  expect_true(all(Rslt[, DBHCor] == c(13, 14, 15, 16, 17, 19, 20, 21, 23)))


  # POM change + shift error A FAIRE------------------------------------------------------------------------------------
  # taper + POM change A FAIRE------------------------------------------------------------------------------------------

  # All corrections -----------------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "Pradosia cochlearia",
                         Genus = "Pradosia",
                         Family = "Sapotaceae",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13, 14, 24, 16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))


  Rslt <- DiameterCorrectionByTree(
    DataTree, TestData,
    WhatToCorrect = c("POM change", "punctual", "shift"),
    CorrectionType = c("quadratic", "linear", "individual"))

  # Compute corrected diameter incrementation
  cresc <- ComputeIncrementation(Var = Rslt$DBHCor, Type = "annual", Time = Rslt$Year)
  cresc_abs <- ComputeIncrementation(Var = Rslt$DBHCor, Type = "absolute", Time = Rslt$Year)

  # Check that no abnormal increments remain
  expect_true(all(cresc < 5 | cresc_abs > -2))

  # Case NA in the Diameter column in any case -----------------------------------------------------------------------------------------
  DataTree <- data.table(IdTree = "c",
                         ScientificName = "Pradosia cochlearia",
                         Genus = "Pradosia",
                         Family = "Sapotaceae",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
                         Diameter = c(13, 14, 24, 16, 16-4, NA, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = as.factor(c(0, 0, 0, 0, 1, 1, 1, 2, 2)),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))


  Rslt <- DiameterCorrectionByTree(DataTree,
                                   TestData,
                                   WhatToCorrect = c("POM change", "punctual", "shift"),
                                   CorrectionType = c("quadratic", "linear", "phylogenetic hierarchical"))

  # Compute corrected diameter incrementation
  cresc <- ComputeIncrementation(Var = Rslt$DBHCor, Type = "annual", Time = Rslt$Year)
  cresc_abs <- ComputeIncrementation(Var = Rslt$DBHCor, Type = "absolute", Time = Rslt$Year)

  # Check that no abnormal increments remain
  expect_true(all(cresc < 5 | cresc_abs > -2))

  expect_true(all(Rslt[DBHCor != Diameter, DiameterCorrectionMeth] != "")) # method when the DBH has been corrected

  expect_true(all(Rslt[DBHCor != Diameter & DiameterCorrectionMeth !=  "shift realignment", Comment] != "")) # comment when the DBH has been corrected

})

#
