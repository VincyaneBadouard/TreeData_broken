test_that("DiameterCorrectionByTree", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  TestData <- data.table(IdTree = "c",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 DBH values
                         DBH = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))


  # Create test data ----------------------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(TestData)
  TwoInd <- copy(TestData)
  TwoInd[Year == 2014, ("IdTree") := "b"] # a NA in the "e" DBH seq


  # Check the function argument -----------------------------------------------------------------------------------------------------

  expect_error(DiameterCorrectionByTree(MatrixData),
               regexp = "DataTree must be a data.frame or data.table")

  expect_error(DiameterCorrectionByTree(TwoInd),
               regexp = "DataTree must correspond to only 1 same tree so 1 same IdTree")

  # Check the function work ---------------------------------------------------------------------------------------------------------


})

#
