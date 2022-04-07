test_that("FillinInvariantColumns", {

  # Import data
  library(data.table)
  TestData <- data.table(Site = "Nowhere",
                         Plot = "1",
                         SubPlot = "3",
                         IdTree = "a", # 1 ind
                         CensusYear = seq(2012,2028, by=2) # 9 census
  )

  NewRow <- data.table(IdTree = "a",
                       CensusYear = 2013)

  InvariantColumns = c("Site", "Plot", "SubPlot")
  NewRow[,(InvariantColumns) := NA] # empty the invariant columns for the added rows

  # Create test data
  MatrixData <- as.matrix(TestData)
  TwoInd <- copy(TestData)
  TwoInd[CensusYear == 2014, ("IdTree") := "b"]
  VarPlot <- copy(TestData)
  VarPlot[CensusYear == 2014, ("Plot") := "2"] # variant data
  NoVal <- copy(TestData)
  NoVal[, ("SubPlot") := NA] # no value


  # Check the function argument
  expect_error(FillinInvariantColumns(MatrixData),
               regexp = "'NewRow' argument of the 'FillinInvariantColumns' function must be a data.table")

  expect_error(FillinInvariantColumns(NewRow, InvariantColumns = "Site", DataTree = MatrixData),
               regexp = "DataTree must be a data.table")

  expect_error(FillinInvariantColumns(NewRow, InvariantColumns = "Site", DataTree = TwoInd),
               regexp = "DataTree must correspond to only 1 same tree so 1 same IdTree")

  expect_error(FillinInvariantColumns(NewRow, InvariantColumns = "Forest", DataTree = TestData),
               regexp = "InvariantColumns argument must contain one or several column names")

  expect_error(FillinInvariantColumns(NewRow, InvariantColumns = "Site", DataTree = TestData, IdTree = 2),
               regexp = "'IdTree' argument must be of character class")

  # A finir
  # expect_error(FillinInvariantColumns(NewRow, InvariantColumns = InvariantColumns, DataTree = VarPlot, IdTree = "a"),
  #              regexp = "has multiple values")
  #
  # expect_error(FillinInvariantColumns(NewRow, InvariantColumns = InvariantColumns, DataTree = NoVal, IdTree = "a"),
  #              regexp = "has no value")


  # Check the function work
  Rslt <- FillinInvariantColumns(NewRow, InvariantColumns = InvariantColumns, TestData, "a")



})
