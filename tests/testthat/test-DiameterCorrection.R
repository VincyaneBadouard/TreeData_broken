test_that("DiameterCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  data(TestData)

  # Remove other errors types (non-unique idTree, missing Year)
  TestData <- TestData[!IdTree %in% c("100898", "101686")]

  # Create test data ----------------------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  HOMData <- copy(TestData[IdTree == "100658"])
  HOMData[, HOM := 1.3] # data with HOM
  POMData <- copy(TestData[IdTree == "100658"])
  POMData[, POM := as.factor(1)] # data with POM


  # Check the function argument -----------------------------------------------------------------------------------------------------

  expect_error(DiameterCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(DiameterCorrection(NoDBHData),
               regexp = "column does't exist in the dataset")

  expect_error(DiameterCorrection(TestData, DefaultHOM = "a",
                                  # MaxDBH = c(1,2),
                                  PositiveGrowthThreshold = c("a", "b"),
                                  NegativeGrowthThreshold = "-2",
                                  PioneersGrowthThreshold = F,
                                  DBHRange = "10",
                                  MinIndividualNbr = "5"),
               regexp = "The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         of the 'DiameterCorrection' function must be 1 numeric value each")

  expect_error(DiameterCorrection(TestData, Pioneers = T),
               regexp = "'Pioneers' argument must be a characters vector, or NULL")

  expect_error(DiameterCorrection(TestData, WhatToCorrect = "diameter"),
               regexp = "'arg' should be one of “POM change”, “punctual”, “shift”")

  expect_error(DiameterCorrection(TestData, CorrectionType = "best"),
               regexp = "'arg' should be one of “individual”, “phylogenetic hierarchical”")

  expect_warning(expect_warning(DiameterCorrection(TestData, Digits = 1.2),
                 regexp = "The 'Digits' argument must be an integer"), "All your HOM are NA, so we can't do taper corrections")

  expect_error(DiameterCorrection(TestData, DetectOnly = "TRUE"),
               regexp = "The 'DetectOnly' argument must be a logical")

  expect_message(DiameterCorrection(POMData, CorrectionType = "individual", WhatToCorrect = "punctual", UseTaperCorrection = F),
                 regexp = "You have the 'POM' information in your dataset")



  # Check the function work ---------------------------------------------------------------------------------------------------------

  ## Detect Only --------------------------------------------------------------------------------------------------------------------
  Rslt <- DiameterCorrection(
    TestData,
    CorrectionType = "phylogenetic hierarchical",
    DetectOnly = TRUE)

  # No correction, only comments
  expect_true(!"Diameter_TreeDataCor" %in% names(Rslt) & "Comment_TreeData" %in% names(Rslt))

  ## Correction --------------------------------------------------------------------------------------------------------------------
  # options(warn = 2) # trace warning
  # options(warn = 0) # when debug is over

  Rslt <- DiameterCorrection(
    TestData,

    UseTaperCorrection = F,

    PositiveGrowthThreshold = 5,
    NegativeGrowthThreshold = -2,

    Pioneers = c("Cecropia","Pourouma"),
    PioneersGrowthThreshold = 7.5,

    WhatToCorrect = c("POM change", "punctual", "shift"),
    CorrectionType = c("phylogenetic hierarchical"),

    DBHRange = 10,
    MinIndividualNbr = 1,
    Digits = 2L,

    DetectOnly = FALSE)

  # Growth > 5 cm DBH/year & < -2 cm DBH/census

  # Comment and Methode if correction
  expect_true(all(!is.na(Rslt[Diameter_TreeDataCor != round(Diameter, digits = 2L), DiameterCorrectionMeth_TreeData]))) # method when the DBH has been corrected

  # expect_true(all(Rslt[Diameter_TreeDataCor != round(Diameter, digits = 2L), Comment] != "")) # comment when the DBH has been corrected



})

