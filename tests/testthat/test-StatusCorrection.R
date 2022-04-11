test_that("StatusCorrection", {

  # Import data
  library(data.table)
  TestData <- data.table(Site = "Nowhere",
                         Plot = "1",
                         IdTree = c("a", "b", "c", "d", "e"), # 5 ind
                         Year = rep(c(2012:2020), 5), # 9 census
                         DBH = NA_real_)
  TestData <- TestData[order(IdTree, Year)]
  TestData[,LifeStatus := c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, # "a"
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, # "b"
    TRUE, TRUE, TRUE, NA, FALSE, TRUE, TRUE, TRUE, FALSE, # "c"
    TRUE, TRUE, TRUE, TRUE, TRUE, NA, NA, FALSE, NA, # "d"
    FALSE, TRUE, NA, FALSE, TRUE, NA, NA, FALSE, NA) # "e"
  ]

  TestData[IdTree %in% "e", ("DBH") := c(13:21)] # "e" DBH seq
  TestData[IdTree %in% "e" & Year == 2014, ("DBH") := NA] # a NA in the "e" DBH seq


  # Create test data
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("DBH")]
  NoPlotData <- TestData[, !c("Plot")]


  # Check the function argument
  expect_error(StatusCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(StatusCorrection(NoPlotData),
               regexp = "The column 'Plot' must be present in the dataset")

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

  # Ids = "a"
  # i = "d"
  for(i in Ids){

    Seq <- Rslt[IdTree %in% i, LifeStatus]
    SeqCor <- Rslt[IdTree %in% i, LifeStatusCor]

    ## No "dead" before "alive"
    LastAlive <- max(which(SeqCor %in% TRUE))
    Deaths <- which(SeqCor %in% FALSE)
    expect_true(all(Deaths > LastAlive))

    ## No "NA" between the first and last "alive"
    FirstAlive <- min(which(SeqCor %in% TRUE))
    Unseen <- which(SeqCor %in% NA)
    expect_true(all(Unseen < FirstAlive | Unseen > LastAlive))

    ## After the death always the death (no "NA")
    if(length(Deaths) > 0){
      FirstDead <- min(Deaths)
      expect_true(all(Unseen < FirstDead))
    }

    ## If no "dead" but "NA" nbr >= DeathConfirmation -> "dead" in "DBHCor"
    # Alive NA NA DEAD NA
    # Alive Alive NA NA
    DeathConfirmation <- 2
    Unseen_seq <- which(Seq %in% NA)
    Deaths_seq <- which(Seq %in% FALSE)

    if(length(Deaths_seq) == 0){ # if no death (Alive Alive NA NA)
      if(length(Unseen_seq) >= DeathConfirmation)
        expect_true(all(SeqCor[Unseen_seq] == FALSE))

    }else{ # if death in the seq (Alive NA NA DEAD NA)
      FirstDeath <- min(Deaths_seq)
          UnseenBfDeath <- sum(Unseen_seq < FirstDeath) # nbr of NA before the death

          if(UnseenBfDeath >= DeathConfirmation)
            expect_true(all(SeqCor[Unseen_seq] == FALSE))
    }

    ## If UseSize : if DBH != NA -> Alive
    Sizes <-!is.na(Rslt[IdTree %in% i, DBH])
    DBHprst <- which(Sizes==T)
    if(length(DBHprst) > 0){
    expect_true(all(DBHprst %in% which(SeqCor==T)))
}
    ## Add a "Comment" column when "LifeStatus" != "LifeStatusCor"
    Comment <- Rslt[IdTree %in% i, Comment] != ""

    compareNA <- function(v1,v2) {
      same <- (v1 == v2) | (is.na(v1) & is.na(v2))
      same[is.na(same)] <- FALSE
      return(same)
    }

    expect_true(all(!compareNA(Seq, SeqCor) == Comment))
  }

})


# No "dead" before "alive"
# No "NA" between the first and last "alive"
# after the death always the death (no "NA")
# if no "dead" but "NA" nbr >= DeathConfirmation -> "dead" in "DBHCor"
# if UseSize : if DBH != NA -> Alive
