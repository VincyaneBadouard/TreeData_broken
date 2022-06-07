test_that("ReversedRequiredFormat", {

  # WITH ParacouSubset ####
  data(ParacouSubsetFormated)
  data(ForestGeoProfile)
  Data <- ParacouSubsetFormated
  input <- ForestGeoProfile

  DataFormated <- ReversedRequiredFormat(Data, input )

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated[, get(input$IdTree)])))

  # make sure DBH or Circ units were converted correctly
  if(!input$DBH %in% "none")   expect_equal(DataFormated[, get(input$DBH)], Data$DBH * switch(input$DBHUnitMan , mm = 10, cm = 1, dm = 0.1, m = 0.01))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.01)


  # make sure DBH is calculated correctly if only Circ is given
  if(input$DBH %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated[, get(input$DBH)], round(DataFormated[, get(input$Circ)]/ pi, 2))


  # cAREFUL, EDITING DATA OR INPUT AFTER THIS LINE #


  # make sure date format is handled correctly

  for(DateFormat in c("yyyy-mm-dd", "dd/mm/yyyy", "decimal", "numeric")){
    input$DateFormat = DateFormat
    if(DateFormat %in% "yyyy-mm-dd") expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("\\d{4}-\\d{2}-\\d{2}", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))
    if(DateFormat %in% "dd/mm/yyyy") expect_true(all(grepl("\\d{2}/\\d{2}/\\d{4}", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("\\d{4}-\\d{2}-\\d{2}", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))
    if(DateFormat %in% "numeric") expect_true(all(grepl("^\\d{5}$", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("^\\d{5}$", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))
    if(DateFormat %in% "Decimal") expect_true(all(grepl("\\.", ReversedRequiredFormat(Data, input )[, get(input$Date)])) | all(is.na(ReversedRequiredFormat(Data, input )[, get(input$Date)][!grepl("\\.", ReversedRequiredFormat(Data, input )[, get(input$Date)])])))

  }

  input$DateFormat = ForestGeoProfile$DateFormat

  # make sure measurement units gets converted correctly or throw error if units not selected
  Data$HOM <- 1.3
  input$BCirc = "none"
  Data$BD <- Data$DBH
  input$BD = "BD"
  input$BDUnitMan = "cm"
  Data$BHOM <- 0.1
  input$BHOM = "BHOM"
  input$BHOMUnitMan = "cm"
  Data$TreeHeight = 20
  input$TreeHeight = "TreeHeight"
  input$TreeHeightUnitMan = "m"
  Data$Xsubplot <- Data$Xplot
  Data$Ysubplot <- Data$Yplot
  input$Xsubplot = "Xsubplot"
  input$Ysubplot = "Ysubplot"
  input$subplotUnitMan = "cm"

  for(i in c("mm", "cm", "dm", "m")){


    if(!input$DBH %in% "none") {
      input$DBHUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$DBH)], Data$DBH * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01))
      input$DBHUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$Circ %in% "none") {
      input$CircUnitMan = i
     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$Circ)], Data$Circ * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.1)
      input$CircUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$HOM %in% "none") {
      input$HOMUnitMan = i
     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$HOM)], Data$HOM * switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
      input$HOMUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$BD %in% "none") {
      input$BDUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$BD)], Data$BD * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.1)
      input$BDUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$BCirc %in% "none")  {
      input$BCircUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$BCirc)], Data$BCirc * switch(i, mm =10, cm = 1, dm = 0.1, m = 0.01), tolerance = 0.1)
      input$BCircUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$BHOM %in% "none") {
      input$BHOMUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$BHOM)], Data$BHOM * switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
      input$BHOMUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$TreeHeight %in% "none")  {
      input$TreeHeightUnitMan = i
     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$TreeHeight)], Data$TreeHeight *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
      input$TreeHeightUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$Xutm %in% "none")  {
      input$utmUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$Xutm)], Data$Xutm *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
      input$utmUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$Xplot %in% "none") {
      input$plotUnitMan = i
     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$Xplot)], Data$Xplot *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
      input$plotUnitMan = ParacouProfile$plotUnitMan
    }

    if(!input$Xsubplot %in% "none"){
      input$subplotUnitMan = i
     expect_equal(ReversedRequiredFormat(Data, input )[, get(input$Xsubplot)], Data$Xsubplot *  switch(i , mm = 1000, cm = 100, dm = 10, m = 1))
      input$subplotUnitMan = "cm" # so that does not through an error anymore
    }

  }

  # put parameters back to what they were
  # input$HOM = ParacouProfile$HOM
  # input$BCirc = ParacouProfile$BCirc
  # input$BD = ParacouProfile$BD
  # input$BHOM = ParacouProfile$BHOM
  # input$TreeHeight = ParacouProfile$TreeHeight
  # input$HOMUnitMan = ParacouProfile$HOMUnitMan
  # input$BHOMUnitMan = ParacouProfile$BHOMUnitMan
  # input$CircUnitMan = ParacouProfile$CircUnitMan
  # input$BDUnitMan = ParacouProfile$BDUnitMan
  # input$BCircUnitMan = ParacouProfile$BCircUnitMan
  # input$TreeHeightUnitMan = ParacouProfile$TreeHeightUnitMan
  # input$utmUnitMan = ParacouProfile$utmUnitMan
  # input$plotUnitMan = ParacouProfile$plotUnitMan
  # input$subplotUnitMan = ParacouProfile$subplotUnitMan



  # make sure AREA gets converted correctly or throw error if units not selected
  Data$SubPlotArea <- Data$PlotArea
  input$SubPlotArea <- "SubPlotArea"
  input$SubPlotAreaUnitMan <- "ha"



  for(i in c("m2", "ha", "km2")){

    if(!input$PlotArea %in% "none")   {
      input$PlotAreaUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$PlotArea)], Data$PlotArea * switch(i , m2 = 10000, ha = 1, km2 = 0.01))
      input$PlotAreaUnitMan = "ha"
    }

    if(!input$SubPlotArea %in% "none")  {
      input$SubPlotAreaUnitMan = i
      expect_equal(ReversedRequiredFormat(Data, input )[, get(input$SubPlotArea)], Data$SubPlotArea *  switch(i , m2 = 10000, ha = 1, km2 = 0.01))
      input$SubPlotAreaUnitMan = "ha"
    }


  }

  # put parameters back to what they were
  # input$SubPlotArea = ParacouProfile$SubPlotArea
  # input$PlotAreaUnitMan = ParacouProfile$PlotAreaUnitMan
  # input$SubPlotAreaMan = ParacouProfile$SubPlotAreaMan


})

