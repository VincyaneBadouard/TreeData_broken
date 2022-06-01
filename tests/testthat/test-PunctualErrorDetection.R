test_that("PunctualErrorDetection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(data.table)
  Time <- c(2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016, 2020)

  # 1 ind at a time
  ## Punctual error case
  DBHCor <- c(13, 14, 15, 16, 30, 19, 15, 21, 23)
  plot(Time, DBHCor)

  PunctualErrorDetection(DBHCor = DBHCor, Time = Time)

  ## 2 shifts error case
  DBHCor <- c(13, 14, 15, 16, 12, 14, 15, 11, 13)
  plot(Time, DBHCor)


  # Check the function work ---------------------------------------------------------------------------------------------------------

  Rslt <- PunctualErrorDetection(DBHCor = DBHCor, Time = Time)

  # Detect Only A FAIRE

  # Check the different reading direction (TrustMeasSet) A FAIRE

  })
