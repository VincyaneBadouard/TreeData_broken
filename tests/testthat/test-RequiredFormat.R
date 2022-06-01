test_that("RequiredFormat", {


  data(ParacouSubset)
  data(ParacouProfile)
  Data <- ParacouSubset
  input <- ParacouProfile

  Dateformated <- RequiredFormat(Data, input )

  # make sure no IdTree is NA
  expect_false(any(is.na(Dateformated$IdTree)))

  # make sure DBH or Circ units were converted correctly
  if(!input$DBH %in% "none")   expect_equal(Dateformated$DBH, Data[,input$DBH] * switch(input$DBHUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))
  if(!input$Circ %in% "none")   expect_equal(Dateformated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100))
})
