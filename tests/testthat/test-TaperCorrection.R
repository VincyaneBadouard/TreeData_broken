test_that("TaperCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  DataTree <- data.table(IdTree = "c",
                         Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 DBH values
                         DBH = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
                         POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
                         HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))

  NoHOM <- copy(DataTree)
  NoHOM[, HOM := NULL] # remove HOM column



  # Check the function argument -----------------------------------------------------------------------------------------------------
  expect_error(TaperCorrection(NoHOM),
               regexp = "You have chosen to make a 'taper' correction,
       but you do not have the necessary 'HOM' column in your dataset")


  expect_error(TaperCorrection(DataTree,
                               DefaultHOM = "DBH"),
               regexp = "The 'DefaultHOM' argument must be numeric")


  expect_error(TaperCorrection(DataTree,
                               TaperParameter = "0.156 - 0.023 * log(DAB) - 0.021 * log(HOM)",
                               TaperFormula = 2*c(3,8,9)),
               regexp = "The 'TaperParameter' and 'TaperFormula' arguments must be functions")


  # Check the function work ---------------------------------------------------------------------------------------------------------


  ## Detect Only --------------------------------------------------------------------------------------------------------------------
  Rslt <- TaperCorrection(DataTree, DetectOnly = T)

  # No correction, only comments
  expect_true(all(!c("DBHCor", "DiameterCorrectionMeth") %in% names(Rslt)) & "Comment" %in% names(Rslt))
  expect_true(all(Rslt$DBH == DataTree$DBH)) # no change in the original column

  ## Correction ---------------------------------------------------------------------------------------------------------------------
  Rslt <- TaperCorrection(DataTree)

  expect_true(all(c("DBHCor", "DiameterCorrectionMeth", "Comment") %in% names(Rslt)))

  # Add a "Comment" and "DiameterCorrectionMeth" value when "DBH" != "DBHCor"
  Comment <- Rslt[, Comment] != ""
  Methode <- !is.na(Rslt[, DiameterCorrectionMeth])

  compareNA <- function(v1,v2) { # function to compare values, including NA
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  expect_true(all(!compareNA(Rslt$DBH, Rslt$DBHCor) == Comment) & all(Comment == Methode))

  # Check the value of the "DiameterCorrectionMeth" column
  expect_true(all(Rslt$DiameterCorrectionMeth[!is.na(Rslt$DiameterCorrectionMeth)] == "taper"))



})
