test_that("StatusCorrectionByTree", {

  # Import data
  library(data.table)
  TestData <- data.table(Site = "Nowhere",
                         Plot = "1",
                         IdTree = "a", # 1 ind
                         Year = seq(2012,2028, by=2), # 9 census
                         Diameter = NA_real_,
                         LifeStatus = c(FALSE, TRUE, NA, FALSE, TRUE, NA, NA, FALSE, NA))


  # Create test data
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  TwoInd <- copy(TestData)
  TwoPlot <- copy(TestData)
  TwoInd[Year == 2014, ("IdTree") := "b"] # a NA in the "e" Diameter seq
  TwoPlot[Year == 2014, ("Plot") := "2"] # a NA in the "e" Diameter seq



  # Check the function argument
  expect_error(StatusCorrectionByTree(MatrixData),
               regexp = "DataTree must be a data.frame or data.table")

  expect_error(StatusCorrectionByTree(TestData, PlotCensuses = "2010"),
               regexp = "'PlotCensuses' argument must be numeric or integer")

  expect_error(StatusCorrectionByTree(TestData, PlotCensuses = 2010, InvariantColumns = c(1:3)),
               regexp = "'InvariantColumns' argument must be of character class")

  expect_error(StatusCorrectionByTree(TestData, PlotCensuses = 2010, InvariantColumns = c("a","b"), DeathConfirmation = TRUE),
               regexp = "'DeathConfirmation' argument must be numeric")

  expect_error(StatusCorrectionByTree(TestData, PlotCensuses = 2010,
                                      UseSize = "yes",
                                      DetectOnly = "no",
                                      RemoveRBeforeAlive = 1,
                                      RemoveRAfterDeath = "FALSE"),
               regexp = "The 'UseSize', 'DetectOnly', 'RemoveRBeforeAlive' and 'RemoveRAfterDeath' arguments
         of the 'StatusCorrectionByTree' function must be logicals")

  expect_error(StatusCorrectionByTree(TwoInd, PlotCensuses = 2010),
               regexp = "DataTree must correspond to only 1 same tree so 1 same IdTree")

  expect_error(StatusCorrectionByTree(TwoPlot, PlotCensuses = 2010),
               regexp = "has multiple plots")

  expect_error(StatusCorrectionByTree(TestData, PlotCensuses = 2010, InvariantColumns = "a"),
               regexp = "InvariantColumns argument must contain one or several column names")


  expect_error(StatusCorrectionByTree(NoDBHData, PlotCensuses = 2010, InvariantColumns = "Site",
                                      UseSize = TRUE),
               regexp = "the 'Diameter' column must be present in the dataset")

  # Check the function work
  Rslt <- StatusCorrectionByTree(TestData, PlotCensuses = TestData$Year, InvariantColumns = "Site",
                                 RemoveRBeforeAlive = TRUE,
                                 RemoveRAfterDeath = TRUE)

  SeqCor <- Rslt[, LifeStatusCor]

  ## If RemoveRBeforeAlive = TRUE, the 1st value is ALIVE
  expect_true(SeqCor[1])

  ## If RemoveRAfterDeath = TRUE, there are only 1 DEAD in the seq and it's the last value.
  Deaths <- which(SeqCor %in% FALSE)

  expect_true(length(Deaths)==1 & SeqCor[length(SeqCor)] == FALSE)

  ## Test that the function has added rows for missing rows
  PlotCensuses = c(2011:2028)
  Rslt <- StatusCorrectionByTree(TestData, PlotCensuses = PlotCensuses, InvariantColumns = "Site")

  all(TestData$Year %in% Rslt$Year)
  all(Rslt$Year %in% PlotCensuses)

})
