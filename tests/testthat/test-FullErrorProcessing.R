test_that("FullErrorProcessing", {

  data(TestData)

  Rslt_Test <- suppressWarnings(FullErrorProcessing(TestData, DetectOnly = TRUE))
  # Rslt_Test <- FullErrorProcessing(TestData, Source = "WFO", WFOData = WFO_Backbone)
  # Rslt_Panama <- FullErrorProcessing(PanamaFormated, DetectOnly = TRUE)


  # Check general errors detection
  # Check botanical correction
  # Check the life status correction
  # Check taper correction
  # Check diameter correction
  # Check recruitment correction

  })
