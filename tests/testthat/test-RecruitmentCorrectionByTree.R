test_that("RecruitmentCorrectionByTreeByTree", {

  # Import data
  library(data.table)
  TestData <- data.table(Site = "Imaginary forest",
                         IdTree = "a",
                         CensusYear = seq(2000,2008, by = 2), # 2 years/census
                         DBHCor  = as.numeric(c(13:17)), # 1cm/census(0.5 cm/year)
                         CorrectedRecruit = FALSE, # The initial rows are not corrected recruits
                         Comment = ""
  )
  # 1st DBH = 13 > MinDBH. In 1998 the tree was 12cm, 11cm in 1996 and 10cm in 1994.


  # Create test data
  MatrixData <- as.matrix(TestData)
  TwoInd <- copy(TestData)
  TwoInd[CensusYear == 2002, ("IdTree") := "b"]
  NoDBHData <- TestData[, !c("DBHCor")]
  NoDBHCorData <- copy(TestData)
  setnames(NoDBHCorData, "DBHCor", "DBH") # only DBH
  OneDBHVal <- copy(TestData)
  OneDBHVal[, ("DBHCor") := 13]

  # Check the function argument
  expect_error(RecruitmentCorrectionByTree(MatrixData),
               regexp = "DataTree must be a data.frame or data.table")

  expect_error(RecruitmentCorrectionByTree(TestData, MinDBH = c(5, 10, 20), PositiveGrowthThreshold = TRUE),
               regexp = "The 'MinDBH' and 'PositiveGrowthThreshold'' arguments
         of the 'RecruitmentCorrectionByTree' function must be 1 numeric value each")

  expect_error(RecruitmentCorrectionByTree(TestData, InvariantColumns = c(1:3)),
               regexp = "'InvariantColumns' argument must be of character class")

  expect_error(RecruitmentCorrectionByTree(TestData, PlotCensuses = "yes"),
               regexp = "'PlotCensuses' argument must be numeric or integer")

  expect_error(RecruitmentCorrectionByTree(TestData, PlotCensuses = 2001,
                                           DetectOnly = "no"),
               regexp = "The 'DetectOnly' argument
         of the 'RecruitmentCorrectionByTree' function must be logicals")

  expect_error(RecruitmentCorrectionByTree(NoDBHData, PlotCensuses = 2001),
               regexp = "column does't exist in the dataset.")
  expect_warning(RecruitmentCorrectionByTree(NoDBHCorData, InvariantColumns = "Site", PlotCensuses = 2001),
                 regexp = "column does't exist in the dataset.")

  expect_error(StatusCorrectionByTree(TwoInd, PlotCensuses = 2001),
               regexp = "DataTree must correspond to only 1 same tree so 1 same IdTree")


  expect_error(RecruitmentCorrectionByTree(TestData, InvariantColumns = "a", PlotCensuses = 2001),
               regexp = "InvariantColumns argument must contain one or several column names")


  # Check the function work
  MinDBH = 10
  PlotCensuses = seq(1994,2016, by = 2)
  Rslt <- RecruitmentCorrectionByTree(TestData,
                                      MinDBH = MinDBH,
                                      InvariantColumns = "Site",
                                      PlotCensuses = PlotCensuses)

  # Min(DBHCor) >= MinDBH
  expect_true(min(Rslt$DBHCor) >= MinDBH)

  # Create new rows  and Fill CorrectedRecruit = TRUE for the previous missing censuses
  RecruitYear <- min(TestData[, CensusYear])
  FirstDBH <- TestData[!is.na(DBHCor), DBHCor][1] # 1st measured DBH
  PrevCens <- PlotCensuses[which(PlotCensuses == RecruitYear)-1] # 1 census before the recruit year among the plot censuses

  if(FirstDBH > (MinDBH + (RecruitYear - PrevCens) * 0.5)){ # in my exemple the cresc = 0.5

    MissingCens <- PlotCensuses[which(PlotCensuses < RecruitYear)] # the previous missing censuses

    expect_true(all(Rslt[CensusYear %in% MissingCens, CorrectedRecruit] %in% TRUE)) # CorrectedRecruit = TRUE in the missing censuses

  # Fill the 'Comment column' of the "fake" recruit
  expect_true(!any(is.na(Rslt$Column)))  # No NA but "" in the "Comment" column

  expect_true(Rslt[CensusYear %in% RecruitYear, Comment] != "")

  }else{stop("No recruitment error detected in this test!")}


  # If only 1 DBH value : keep this value for the forgotten recruits
  Rslt <- RecruitmentCorrectionByTree(OneDBHVal ,
                                      MinDBH = MinDBH,
                                      InvariantColumns = "Site",
                                      PlotCensuses = PlotCensuses)

  ForgRecruits <- unique(Rslt[CorrectedRecruit %in% TRUE, DBHCor])
  MesurVal <- unique(OneDBHVal[CorrectedRecruit %in% FALSE, DBHCor])

 # (MesurVal==ForgRecruits) # ça renvoie FALSE alors que les 2 sont 13...et numeriques

})

# Create new rows  and Fill CorrectedRecruit = TRUE for the previous missing censuses (yes)
# min(Rslt$DBHCor) >= MinDBH (yes)
# Fill the 'Comment column' (yes)
# if only 1 DBH value : DataTree[CorrectedRecruit %in% TRUE, DBHCor] == DataTree[CorrectedRecruit %in% FALSE, DBHCor]
# all(cresc < PositiveGrowthThreshold & cresc_abs > NegativeGrowthThreshold) # no aberrant growth



