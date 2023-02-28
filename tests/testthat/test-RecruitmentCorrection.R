test_that("RecruitmentCorrection", {

  # Import data
  suppressWarnings(library(data.table))
  data(TestData)

  # Create test data
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  NoDBHCorData <- TestData

  setnames(TestData, "Diameter", "Diameter_TreeDataCor", skip_absent=TRUE)


  # Check the function argument
  expect_error(RecruitmentCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(RecruitmentCorrection(TestData, MinDBH = c(5, 10, 20), PositiveGrowthThreshold = TRUE),
               regexp = "MinDBH must be numeric value of length 1")

  expect_error(RecruitmentCorrection(TestData, OnlyDetectMissedRecruits = "no"),
               regexp = "The 'OnlyDetectMissedRecruits' argument
         of the 'RecruitmentCorrection' function must be logicals")

  expect_error(RecruitmentCorrection(NoDBHData),
               regexp = "column does't exist in the dataset.")



  expect_warning(RecruitmentCorrection(TestData), regexp = "We added rows for trees that were supposed to be recruited earlier based on growth pattern and MinDBH")

  # Check the function works
  Rslt <- RecruitmentCorrection(TestData, OnlyDetectMissedRecruits = T)

})
