test_that("DiameterCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  data("TestData")
  # TestData <-

  # Create test data ----------------------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("DBH")]


  # Check the function argument -----------------------------------------------------------------------------------------------------

  expect_error(DiameterCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(DiameterCorrection(NoDBHData),
               regexp = "column does't exist in the dataset")

  # expect_error(DiameterCorrection(TestData, DefaultHOM = "a",
  #                                 MinDBH = TRUE,
  #                                 MaxDBH = c(1,2),
  #                                 PositiveGrowthThreshold = c("a", "b"),
  #                                 NegativeGrowthThreshold = "-2",
  #                                 PioneersGrowthThreshold = F,
  #                                 DBHRange = "10",
  #                                 MinIndividualNbr = "5"),
  #              regexp = "The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
  #        of the 'DiameterCorrection' function must be 1 numeric value each")

  expect_error(DiameterCorrection(TestData, Pioneers = T),
               regexp = "'Pioneers' argument must be a characters vector")

  expect_error(DiameterCorrection(TestData, TrustMeasSet = T),
               regexp = "The 'TrustMeasSet' argument value must be equal to 'first' or 'last'")

  # expect_error(DiameterCorrection(TestData, WhatToCorrect = "diameter"),
  #              regexp = "The 'WhatToCorrect' argument value must be among 'POM change', 'punctual' and 'shift'")
  #
  # expect_error(DiameterCorrection(TestData, CorrectionType = "best"),
  #              regexp = "The 'CorrectionType' argument value must be among
  #        'taper', 'quadratic', 'linear', 'individual' and 'phylogenetic hierarchical'")

  # expect_error(DiameterCorrection(TestData, Digits = 1.2),
  #              regexp = "The 'Digits' argument must be an integer")

  # expect_error(DiameterCorrection(TestData, DetectOnly = "TRUE"),
  #              regexp = "The 'DetectOnly' argument must be a logical")


  # Check the function work ---------------------------------------------------------------------------------------------------------

  ## Detect Only --------------------------------------------------------------------------------------------------------------------
  # Rslt <- DiameterCorrection(
  #   TestData,
  #   CorrectionType = c("quadratic", "linear", "individual", "phylogenetic hierarchical"),
  #   DetectOnly = TRUE)
  #
  # # No correction, only comments
  # expect_true(!"DBHCor" %in% names(Rslt) & "Comment" %in% names(Rslt))
  # expect_true(all(Rslt$DBH == TestData$DBH)) # no change in the original column
  #
  # ## Correction --------------------------------------------------------------------------------------------------------------------
  # Rslt <- DiameterCorrection(
  #   TestData,
  #   PositiveGrowthThreshold = 5,
  #   NegativeGrowthThreshold = -2,
  #
  #   Pioneers = c("Cecropia","Pourouma"),
  #   PioneersGrowthThreshold = 7.5,
  #
  #   TrustMeasSet = "first",
  #   WhatToCorrect = c("POM change", "punctual", "shift"),
  #   CorrectionType = c("taper", "quadratic", "linear", "individual", "phylogenetic hierarchical"),
  #
  #   DBHRange = 10,
  #   MinIndividualNbr = 5,
  #   Digits = 1L,
  #
  #   TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  #   TaperFormula = function(DAB, HOM, TaperParameter) DAB / (2 * exp(- TaperParameter*(HOM - DefaultHOM))),
  #
  #
  #   DetectOnly = FALSE)

  # Growth > 5 cm DBH/year & < -2 cm DBH/census


})

