test_that("RequiredFormat", {

  # WITH ParacouSubset ####
  data(ParacouSubset)
  data(ParacouProfile)
  Data <- ParacouSubset
  input <- ParacouProfile

  DataFormated <- RequiredFormat(Data, input )

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated$IdTree)))

  # make sure DBH or Circ units were converted correctly
  if(!input$DBH %in% "none")   expect_equal(DataFormated$DBH, Data[,input$DBH] * switch(input$DBHUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.01)


  # make sure DBH is calculated correcly if only Circ is given
  if(input$DBH %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated$DBH, round(DataFormated$Circ / pi, 2))


  # make sure date format is handled correctly (all are in yyyy-mm-dd format, and if not it is NA)
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)) |  all(is.na(DataFormated$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)])))


  # expect ScientificName to be filled
  if(input$ScientificName %in% "none" & !input$Genus %in% "none") expect_true(all(!is.na(DataFormated$ScientificName[!is.na(DataFormated$Genus)])))


  # cAREFULL, EDITING DATA OR INPUT AFTER THIS LINE #

  # expect error is size units are not correct
  input$DBHUnitMan = input$CircUnitMan = "centimeter"

  expect_error(RequiredFormat(Data, input ))

  input$DBHUnitMan <- ParacouProfile$DBHUnitMan
  input$CircUnitMan <- ParacouProfile$CircUnitMan

  expect_error(expect_error(RequiredFormat(Data, input ))) # don't expect the error anymore

  # expect warning if no IdTree and no Tree Tag
  input$IdTree = input$TreeFieldNum = "none"
  expect_warning(RequiredFormat(Data, input ))

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
 Data$BCirc <- Data[, input$Circ]
 input$BCirc = "BCirc"
 input$BD = "none"
 Data$TreeHeight = 20
 input$TreeHeight = "TreeHeight"


  for(i in c("mm", "cm", "dm", "m", "none")){
    input$HOMUnitMan = i
    input$DBHUnitMan = i
    input$CircUnitMan = i
    input$BDUnitMan = i
    input$BCircUnitMan = i
    input$TreeHeightUnitMan = i


    if(i %in% "none") expect_error(RequiredFormat(Data, input ))
    else {
      if(!input$HOM %in% "none") expect_equal(RequiredFormat(Data, input )$HOM, Data[,input$HOM] * switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))

      if(!input$DBH %in% "none")   expect_equal(RequiredFormat(Data, input )$DBH, Data[,input$DBH] * switch(input$DBHUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))

      if(!input$Circ %in% "none")  expect_equal(RequiredFormat(Data, input )$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.1)

      if(!input$BD %in% "none")  expect_equal(RequiredFormat(Data, input )$BD, Data[,input$BD] * switch(input$BDUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.1)

      if(!input$BCirc %in% "none")  expect_equal(RequiredFormat(Data, input )$BCirc, Data[,input$BCirc] * switch(input$BCircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100), tolerance = 0.1)

      if(!input$TreeHeight %in% "none")  expect_equal(RequiredFormat(Data, input )$TreeHeight, Data[,input$TreeHeight] *  switch(i , mm = 0.001, cm = .01, dm = .10, m = 1))

    }
  }

 # put parameters back to what they were
 input$HOM = ParacouProfile$HOM
 input$BCirc = ParacouProfile$BCirc
 input$BD = ParacouProfile$BD
 input$TreeHeight = ParacouProfile$TreeHeight
 input$HOMUnitMan = ParacouProfile$HOMUnitMan
 input$DBHUnitMan = ParacouProfile$DBHUnitMan
 input$CircUnitMan = ParacouProfile$CircUnitMan
 input$BDUnitMan = ParacouProfile$BDUnitMan
 input$BCircUnitMan = ParacouProfile$BCircUnitMan
 input$TreeHeightUnitMan = ParacouProfile$TreeHeightUnitMan

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
  input$ScientificNameSep = " "

  DataFormated <- RequiredFormat(Data, input )

  expect_true(all(!is.na(DataFormated$Genus[!is.na(DataFormated$ScientificName)])) & all(!is.na(DataFormated$Species[!is.na(DataFormated$ScientificName)])))

  # make sure date format is handled correctly even when numeric or decimal
  Data[,  input$Date] <- as.numeric(DataFormated$Date)
  input$DateFormat = "numeric"
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input )$Date)) |  all(is.na(RequiredFormat(Data, input )$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input )$Date)])))


  Data[,  input$Date] <- lubridate::decimal_date(DataFormated$Date)
  input$DateFormat = "decimal"
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", RequiredFormat(Data, input )$Date)) |  all(is.na(RequiredFormat(Data, input )$Date[!grepl("\\d{4}-\\d{2}-\\d{2}",RequiredFormat(Data, input )$Date)])))


  # expect warning if some dates were not translated correctly
  Data[sample(10), input$Date] <- "doubidou"
  expect_warning(RequiredFormat(Data, input ), "Some dates were translated as NA")

  # WITH ForestGEO ####
  data(ForestGeoSubset)
  data(ForestGeoProfile)
  Data <- ForestGeoSubset
  input <- ForestGeoProfile

  DataFormated <- RequiredFormat(Data, input )




  # WITH ForestPlot example ####
  appdir <- system.file(package = "TreeData", "app")
  Data <- merge( data.table::fread(paste0(appdir, "/tests/shinytest/ForestPlots_test2_trees_small.csv")),
                 data.table::fread(paste0(appdir, "/tests/shinytest/ForestPlots_test2_plots_small.csv")), by.x= "PlotID", by.y = "Plot ID", suffixes = c("", ".y"))
  input <- readRDS(paste0(appdir, "/tests/shinytest/ForestPlots_test2_trees_small_Profile.rds"))

  DataFormated <- RequiredFormat(Data, input )

  # make sure no IdTree is NA
  expect_false(any(is.na(DataFormated$IdTree)))

  # make sure DBH or Circ units were converted correctly
  if(!input$DBH %in% "none")   expect_equal(DataFormated$DBH, Data[,input$DBH] * switch(input$DBHUnitMan , mm = 0.1, cm = 1, dm = 10, m = 100))
  if(!input$Circ %in% "none")   expect_equal(DataFormated$Circ, Data[,input$Circ] * switch(input$CircUnitMan, mm = 0.1, cm = 1, dm = 10, m = 100))


  # make sure DBH is calculated correcly if only Circ is given
  if(input$DBH %in% "none" & !input$Circ %in% "none") expect_equal(DataFormated$DBH, round(DataFormated$Circ / pi, 2))


  # make sure date format is handled correctly (all are in yyyy-mm-dd format, and if not it is NA)
  expect_true(all(grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)) |  all(is.na(DataFormated$Date[!grepl("\\d{4}-\\d{2}-\\d{2}", DataFormated$Date)])))


  # expect ScientificName to be filled
  if(input$ScientificName %in% "none" & !input$Genus %in% "none") expect_true(all(!is.na(DataFormated$ScientificName[!is.na(DataFormated$Genus)])))


})
