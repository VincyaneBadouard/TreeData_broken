test_that("StatusCorrection", {

  # Import data
  library(data.table)
  TestData <- data.table(Site = "Nowhere",
                         Plot = "1",
                         IdTree = c("a", "b", "c", "d", "e"), # 5 ind
                         CensusYear = rep(c(2012:2020), 5), # 9 census
                         DBH = NA_real_)
  TestData <- TestData[order(IdTree, CensusYear)]
  TestData[,LifeStatus := c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, # "a"
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, # "b"
    TRUE, TRUE, TRUE,  NA, FALSE, TRUE, TRUE, TRUE, FALSE, # "c"
    TRUE, TRUE, TRUE, TRUE, TRUE, NA, NA, FALSE, NA, # "d"
    FALSE, TRUE, NA, FALSE, TRUE, NA, NA, FALSE, NA) # "e"
  ]

  TestData[IdTree %in% "e", ("DBH") := c(13:21)]

  # Create test data
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("DBH")]

  # Check the function argument
  expect_error(StatusCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(StatusCorrection(TestData, InvariantColumns = c(1:3)),
               regexp = "'InvariantColumns' argument must be of character class")

  expect_error(StatusCorrection(TestData, InvariantColumns = c("a","b"), DeathConfirmation = TRUE),
               regexp = "'DeathConfirmation' argument must be numeric")

  expect_error(StatusCorrection(TestData,
                                UseSize = "yes",
                                DetectOnly = "no",
                                RemoveRBeforeAlive = 1,
                                RemoveRAfterDeath = "FALSE"),
               regexp = "The 'UseSize', 'DetectOnly', 'RemoveRBeforeAlive' and 'RemoveRAfterDeath' arguments
         of the 'SatusCorrection' function must be logicals")


  expect_error(StatusCorrection(TestData, InvariantColumns = "a"),
               regexp = "InvariantColumns argument must contain one or several column names")


  expect_error(StatusCorrection(Data = NoDBHData, InvariantColumns = "Site",
                                UseSize = TRUE),
               regexp = "the DBH column must be present in the dataset")

  # Check the function work

  Rslt <- StatusCorrection(TestData, InvariantColumns = "Site", UseSize = TRUE)

  Ids <- as.vector(na.omit(unique(TestData$IdTree))) # Tree Ids

  i = "b"
  for(i in Ids){

    seq <- Rslt[IdTree %in% i, LifeStatusCor]

    ## No "dead" before "alive"
    LastAlive <- max(which(seq %in% TRUE))
    Deaths <- which(seq %in% FALSE)
    expect_true(all(Deaths > LastAlive))

    ## No "NA" between the first and last "alive"
    FirstAlive <- min(which(seq %in% TRUE))
    Unseen <- which(seq %in% NA)
    expect_true(all(Unseen < FirstAlive | Unseen > LastAlive))

    ## after the death always the death (no "NA")
    if(length(Deaths) > 0){
    FirstDead <- min(Deaths)

    expect_true(all(Unseen < FirstDead))
    }

    ## if no "dead" but "NA" nbr >= DeathConfirmation -> "dead" in "DBHCor"
    ## if UseSize : if DBH != NA -> Alive
    ## Add a "Comment" column when "DBH" != "DBHCor"
  }

})


# No "dead" before "alive"
# No "NA" or "dead" between the first and last "alive"
# after the death always the death (no "NA")
# if no "dead" but "NA" nbr >= DeathConfirmation -> "dead" in "DBHCor"
# if UseSize : if DBH != NA -> Alive
