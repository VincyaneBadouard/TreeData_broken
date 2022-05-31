test_that("TaperCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  DataTree <- data.table(IdTree = "c",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 DBH values
                         DBH = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))



  # Check the function argument -----------------------------------------------------------------------------------------------------
  expect_error(TaperCorrection(DataTree,
                               TaperParameter = "0.156 - 0.023 * log(DAB) - 0.021 * log(HOM)",
                               TaperFormula = 2*c(3,8,9)
  ),
  regexp = "The 'TaperParameter' and 'TaperFormula' arguments must be functions")


  # Check the function work ---------------------------------------------------------------------------------------------------------

})
