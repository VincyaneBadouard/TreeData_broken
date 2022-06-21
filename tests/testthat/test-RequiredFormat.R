test_that("RequiredFormat", {

  # WITH ParacouSubset ####
  data(ParacouSubset)
  data(ParacouProfile)
  Data <- ParacouSubset
  input <- ParacouProfile

  expect_warning(DataFormated <- RequiredFormat(Data, input ), "MinDBH was calculated")

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated$IdTree)))

  # make sure Diameter or Circ units were converted correctly
  if(!input$Diameter %in% "none")   expect_equal(DataFormated$Diameter, Data[,input$Diameter] * switch(input$DiameterUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.01)


  # make sure Diameter is calculated correcly if only Circ is given
  if(input$Diameter %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated$Diameter, round(DataFormated$Circ / pi, 2))


  # make sure date format is handled correctly (all are in yyyy-mm-dd format, and if not it is NA)
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)) |  all(is.na(DataFormated$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)])))


  # expect ScientificName to be filled
  if(input$ScientificName %in% "none" & !input$Genus %in% "none") expect_true(all(!is.na(DataFormated$ScientificName[!is.na(DataFormated$Genus)])))


  # cAREFULL, EDITING DATA OR INPUT AFTER THIS LINE #
 input$MinDBHMan = 1 # adding this so we don't get warnings
  # expect error is size units are not correct
  input$DiameterUnitMan = input$CircUnitMan = "centimeter"

  expect_error(RequiredFormat(Data, input ), "Your tree size units are not one of: mm, cm, dm, m")

  input$DiameterUnitMan <- ParacouProfile$DiameterUnitMan
  input$CircUnitMan <- ParacouProfile$CircUnitMan

  expect_error(expect_error(RequiredFormat(Data, input ))) # don't expect the error anymore, but still warning

  # expect warning if no IdTree and no Tree Tag
  input$IdTree = input$TreeFieldNum = "none"
  expect_warning(RequiredFormat(Data, input ), "You do not have a column with unique tree IDs")

  input$IdTree <- ParacouProfile$IdTree
  input$TreeFieldNum <- ParacouProfile$TreeFieldNum
  expect_error(expect_warning(RequiredFormat(Data, input ))) # don't expect the warning anymore

  # expect IdTree to be have Tree Tag if IdTree is "none"
  input$IdTree =  "none"
  if( input$IdTree %in% "none" & !  input$TreeFieldNum %in% "none") expect_true(all(apply(RequiredFormat(Data, input ), 1, function(x) grepl(x["TreeFieldNum"] , x["IdTree"]))))

  input$IdTree <- ParacouProfile$IdTree
  input$TreeFieldNum <- ParacouProfile$TreeFieldNum
  expect_error(expect_warning(RequiredFormat(Data, input ))) # don't expect the warning anymore

  # expect IdTree to be filled with "auto" if is NA
  Data[, input$IdTree][sample(10)] <- NA

  expect_true(all(grepl("_auto", RequiredFormat(Data, input )$IdTree[is.na(Data[, input$IdTree])])))

  input$IdTree <- ParacouProfile$IdTree
  input$TreeFieldNum <- ParacouProfile$TreeFieldNum
  expect_error(expect_warning(RequiredFormat(Data, input ))) # don't expect the warning anymore

  # expect IdTree to have Site, Plot and Subplot name, even when those are not in columns
  input$IdTree =  "none"
  expect_true(all(apply(RequiredFormat(Data, input ), 1, function(x) {all(
    grepl(x["Site"] , x["IdTree"]) &
      grepl(x["Plot"] , x["IdTree"]) &
      grepl(x["SubPlot"] , x["IdTree"]))})))

  input$Site <- "none"
  input$SiteMan = ""
  input$Plot <- "none"
  input$PlotMan = "BB"
  input$SubPlot <- "none"
  input$SubPlotMan = ""

  expect_warning(expect_warning(expect_true(all(apply(RequiredFormat(Data, input ), 1, function(x) {all(
    grepl("SiteA" , x["IdTree"]) &
      grepl(x["Plot"] , x["IdTree"]) &
      grepl("SubPlotA" , x["IdTree"]))}))), "SiteA"), "SubPlotA")

  # RequiredFormat(Data, input )$IdTree

  input$IdTree <- ParacouProfile$IdTree
  input$Site <- ParacouProfile$Site
  input$SiteMan <- ParacouProfile$SiteMan
  input$Plot <- ParacouProfile$Plot
  input$PlotMan <- ParacouProfile$PlotMan
  input$SubPlot <- ParacouProfile$SubPlot
  input$SubPlotMan <- ParacouProfile$SubPlotMan




 # make sure measurement units gets converted correctly or throw error if units not selected
 Data$HOM <- 1.3
 input$HOM = "HOM"
 input$HOMUnitMan = "cm"
 Data$BCirc <- Data[, input$Circ]
 input$BCirc = "BCirc"
 input$BCircUnitMan = "cm"
 input$BD = "none"
 Data$BHOM <- 0.1
 input$BHOM = "BHOM"
 input$BHOMUnitMan = "cm"
 Data$TreeHeight = 20
 input$TreeHeight = "TreeHeight"
 input$TreeHeightUnitMan = "m"
 Data$Xsubplot <- Data$Xfield
 Data$Ysubplot <- Data$yfield
 input$Xsubplot = "Xsubplot"
 input$Ysubplot = "Ysubplot"
 input$subplotUnitMan = "cm"

  for(i in c("mm", "cm", "dm", "m", "none")){


    if(!input$Diameter %in% "none") {
      input$DiameterUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "size units") else expect_equal(RequiredFormat(Data, input )$Diameter, Data[,input$Diameter] * switch(i, mm = 0.1, cm = 1, dm = 10, m = 100))
      input$DiameterUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$Circ %in% "none") {
      input$CircUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "size units") else expect_equal(RequiredFormat(Data, input )$Circ, Data[,input$Circ] * switch(i, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.1)
      input$CircUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$HOM %in% "none") {
      input$HOMUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "HOM units") else expect_equal(RequiredFormat(Data, input )$HOM, Data[,input$HOM] * switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))
      input$HOMUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$BD %in% "none") {
      input$BDUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "basal size") else expect_equal(RequiredFormat(Data, input )$BD, Data[,input$BD] * switch(i, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.1)
      input$BDUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$BCirc %in% "none")  {
      input$BCircUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "basal size") else expect_equal(RequiredFormat(Data, input )$BCirc, Data[,input$BCirc] * switch(i, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.1)
      input$BCircUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$BHOM %in% "none") {
      input$BHOMUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "basal HOM units") else expect_equal(RequiredFormat(Data, input )$BHOM, Data[,input$BHOM] * switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))
      input$BHOMUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$TreeHeight %in% "none")  {
      input$TreeHeightUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "height units") else expect_equal(RequiredFormat(Data, input )$TreeHeight, Data[,input$TreeHeight] *  switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))
      input$TreeHeightUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$Xutm %in% "none")  {
      input$utmUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "utm units") else expect_equal(RequiredFormat(Data, input )$Xutm, Data[,input$Xutm] *  switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))
      input$utmUnitMan = "cm" # so that does not through an error anymore
    }

    if(!input$Xplot %in% "none") {
      input$plotUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "\\<plot coordinates units") else expect_equal(RequiredFormat(Data, input )$Xplot, Data[,input$Xplot] *  switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))
      input$plotUnitMan = ParacouProfile$plotUnitMan
    }

    if(!input$Xsubplot %in% "none"){
      input$subplotUnitMan = i
      if(i %in% "none") expect_error(RequiredFormat(Data, input ), "subplot coordinates units") else expect_equal(RequiredFormat(Data, input )$Xsubplot, Data[,input$Xsubplot] *  switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))
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



 for(i in c("m2", "ha", "km2", "none")){

     if(!input$PlotArea %in% "none")   {
       input$PlotAreaUnitMan = i
       if(i %in% "none") expect_error(RequiredFormat(Data, input ), "\\<plot area units") else expect_equal(RequiredFormat(Data, input )$PlotArea, Data[,input$PlotArea] * switch(i , m2 = 0.0001, ha = 1, km2 = 100))
       input$PlotAreaUnitMan = "ha"
     }

     if(!input$SubPlotArea %in% "none")  {
       input$SubPlotAreaUnitMan = i
       if(i %in% "none") expect_error(RequiredFormat(Data, input ), "subplot area units") else expect_equal(RequiredFormat(Data, input )$SubPlotArea, Data[,input$SubPlotArea] *  switch(i , m2 = 0.0001, ha = 1, km2 = 100))
       input$SubPlotAreaUnitMan = "ha"
     }


 }

 # put parameters back to what they were
 # input$SubPlotArea = ParacouProfile$SubPlotArea
 # input$PlotAreaUnitMan = ParacouProfile$PlotAreaUnitMan
 # input$SubPlotAreaMan = ParacouProfile$SubPlotAreaMan


  # expect year to be filled
  input$Year = "none"
  expect_equal(RequiredFormat(Data, input )$Year, DataFormated$Year)

  input$Year <- ParacouProfile$Year

  # expect Genus and species to be filled
  Data$Latin = DataFormated$ScientificName
  Data$Genus = NULL
  Data$Species = NULL
  input$ScientificName = "Latin"
  input$Genus =  input$Species = "none"
  input$ScientificNameSepMan = " "

  DataFormated <- RequiredFormat(Data, input )

  expect_true(all(!is.na(DataFormated$Genus[!is.na(DataFormated$ScientificName)])) & all(!is.na(DataFormated$Species[!is.na(DataFormated$ScientificName)])))

  # make sure date format is handled correctly even when numeric or decimal
  Data[,  input$Date] <- as.numeric(DataFormated$Date)
  input$DateFormatMan = "numeric"
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input )$Date)) |  all(is.na(RequiredFormat(Data, input )$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input )$Date)])))


  Data[,  input$Date] <- lubridate::decimal_date(DataFormated$Date)
  input$DateFormatMan = "decimal"
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input )$Date)) |  all(is.na(RequiredFormat(Data, input )$Date[!grepl("\\d{4}-\\d{2}-\\d{2}",RequiredFormat(Data, input )$Date)])))


  # expect warning if some dates were not translated correctly
  Data[sample(10), input$Date] <- "doubidou"
  expect_warning(RequiredFormat(Data, input ), "Some dates were translated as NA")

  # WITH ForestGEO ####
  data(ForestGeoSubset)
  data(ForestGeoProfile)
  Data <- ForestGeoSubset
  input <- ForestGeoProfile

  expect_warning(expect_warning(expect_warning(RequiredFormat(Data, input), "did not specify a Site column or name"), "did not specify a plot area"), "did not specify a subplot area")




  # WITH ForestPlot example ####
  appdir <- system.file(package = "TreeData", "app")
  Data <- merge( data.table::fread(paste0(appdir, "/tests/shinytest/ForestPlots_test2_trees_small.csv")),
                 data.table::fread(paste0(appdir, "/tests/shinytest/ForestPlots_test2_plots_small.csv")), by.x= "PlotID", by.y = "Plot ID", suffixes = c("", ".y"))
  input <- readRDS(paste0(appdir, "/tests/shinytest/ForestPlots_test2_trees_small_Profile.rds"))
  input$MinDBH = "none"
  input$MinDBHMan =1
  input$MinDBHUnitMan = "none"
  input$DateFormatMan = input$DateFormat
  input$DateFormat = NULL

  DataFormated <- RequiredFormat(Data, input )

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated$IdTree)))

  # make sure Diameter or Circ units were converted correctly
  if(!input$Diameter %in% "none")   expect_equal(DataFormated$Diameter, Data[,input$Diameter] * switch(input$DiameterUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100))


  # make sure Diameter is calculated correcly if only Circ is given
  if(input$Diameter %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated$Diameter, round(DataFormated$Circ / pi, 2))


  # make sure date format is handled correctly (all are in yyyy-mm-dd format, and if not it is NA)
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)) |  all(is.na(DataFormated$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)])))


  # expect ScientificName to be filled
  if(input$ScientificName %in% "none" & !input$Genus %in% "none") expect_true(all(!is.na(DataFormated$ScientificName[!is.na(DataFormated$Genus)])))


})
