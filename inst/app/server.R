
# Fichier pour gérer les interactions de l'application Shiny


# increase size limit to 10MB
options(shiny.maxRequestSize=10*1024^2)

# my function to change first letter in uppercase (e.g for updatePickerInput)
firstUpper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# read in csv file that has all we want to show in app
x <- read.csv("data/interactive_items.csv")
x <- x[x$Activate, ]
x1 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x2 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x3 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x4 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x5 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none == "none", ]
x6 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none != "none", ]

xCorr <- read.csv("data/interactive_items_CorrerctionFunctions.csv")

# for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])

# install TreeData package
# devtools::install_github("VincyaneBadouard/TreeData")
library(TreeData)


server <- function(input, output, session) {

  output$CodeRunApp <- renderText('shiny::runGitHub( "VincyaneBadouard/TreeData", subdir = "inst/app")')

  output$ui_uploadTables <- renderUI({

    lapply(1:input$nTable, function(i) {

      column(width = 3,
             # load button for main data file (csv format)
             box(title = paste("Table", i),
                 width = NULL,
                 fileInput(inputId = paste0("file", i), "Choose CSV File", accept = ".csv"),
                 # does the dataframe have a header?
                 checkboxInput( paste0("header", i), "Header", TRUE),
                 # choose separator
                 selectInput(inputId = paste0("cbSeparator", i),
                             label = "Separator",
                             choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
                             selected = "auto"
                 ),
                 textInput(inputId = paste0("TableName", i),
                           label = "Give an explicit UNIQUE and SHORT name to this table. No space, no special character, no accent.",
                           value = paste0("Table", i)
                 )
             )
      )


    })



  })


  # read file(s) ####


  observeEvent(input$submitTables, {

    if(input$nTable == 1 & length(Data()) == 1) {
      updateTabItems(session, "tabs", "Tidying")
    } else {
      updateCheckboxGroupButtons(session, "TablesToStack",
                                 choices = unname(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]))

      updateTabItems(session, "tabs", "Stacking")
    }

  })


  Data <- eventReactive(input$submitTables, {
    req(input$file1)
    setNames(
      sapply(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)], function(n) {

        i = which(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)] %in% n)
        file <- input[[paste0("file", i)]]
        ext <- tools::file_ext(file$datapath)

        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))

        data.table::fread(file$datapath,
                          header = input[[paste0("header", i)]],
                          sep = input[[paste0("cbSeparator", i)]])
      }, simplify = F),
      reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)])

  })



  # stack tables ####

  StackedTables <- eventReactive(input$Stack, {
    do.call(rbind, Data()[input$TablesToStack])
  })

  observeEvent(input$Stack, {
    shinyjs::hide("SkipStack")

    output$StackedTables <- renderDT(StackedTables(), rownames = FALSE,
                                     options = list(pageLength = 8, scrollX=TRUE))

    output$StackedTablesSummary <- renderPrint(summary(StackedTables()))


    if(all(names(Data()) %in% input$TablesToStack)) {

      shinyjs::show("SkipMerge")
      shinyjs::hide("GoToMerge")
    } else {

      shinyjs::show("GoToMerge")
      shinyjs::hide("SkipMerge")

    }

  })

  observe({
    if(is.null(input$TablesToStack))   shinyjs::show("SkipStack")
    if(!is.null(input$TablesToStack))   shinyjs::hide("SkipStack")
  })


  # merge tables ####

  observeEvent(input$GoToMerge | input$SkipStack, {


    updateTabItems(session, "tabs", "Merging")



    options_to_merge <- names(Data())
    column_options_list <- lapply(Data(), colnames)


    if(!is.null(input$TablesToStack)){

      options_to_merge <- c(names(Data())[!names(Data()) %in% input$TablesToStack], "StackedTables")
      column_options_list[names(Data()) %in% input$TablesToStack] <- NULL
      column_options_list <- c(column_options_list, StackedTables = list(colnames(StackedTables())))

    }
    updatePickerInput(session, "leftTable", choices = options_to_merge, selected =  "")
    updatePickerInput(session, "rightTable", choices = options_to_merge, selected =  "")

    observeEvent(input$selectLeft, {
      updatePickerInput(session, "leftKey", choices = column_options_list[[input$leftTable]])
    })

    observeEvent(input$selectRight, {
      updatePickerInput(session, "rightKey", choices = column_options_list[[input$rightTable]])
    })



  }, ignoreInit = T)


  MergedTables <- eventReactive(input$Merge, {

    if(input$leftTable == "StackedTables") x <-  get(input$leftTable)() else x <- Data()[[input$leftTable]]
    if(input$rightTable == "StackedTables") y <-  get(input$rightTable)() else y <- Data()[[input$rightTable]]

    merge(x, y, by.x=input$leftKey, by.y=input$rightKey, all.x=TRUE)
  })

  observeEvent(input$Merge, {

    output$mergedTables <- renderDT(MergedTables(), rownames = FALSE,
                                    options = list(pageLength = 8, scrollX=TRUE))


    shinyjs::show("GoToTidy")

  })


  # tidy tables ####
  observeEvent(input$GoToTidy | input$SkipMerge, {
    updateTabItems(session, "tabs", "Tidying")}, ignoreInit = T)

  OneTable <- eventReactive(input$submitTables | input$GoToTidy | input$SkipMerge, {

    if(input$nTable == 1 & length(Data()) == 1) {
      Data()[[1]]
    } else {
      if(exists("MergedTables")) MergedTables() else  StackedTables()
    }


  }, ignoreInit = TRUE)



  observe({
    groupNames <- split(names(OneTable()), cutree(hclust(stringdistmatrix(names(OneTable()))), h = 2))
    groupNames <- groupNames[sapply(groupNames, length) > 1]
    names(groupNames) <- sapply(groupNames, function(x) paste(Reduce(intersect, strsplit(x,"")), collapse=""))

    output$meltUI <- renderUI({
      lapply(c(1:length(groupNames)), function(i)
      {
        box(width = 12,
            column(1,         awesomeCheckbox(
              inputId = paste0("TickedMelt", i),
              label = "",
              value = FALSE,
              status = "info"
            )),
            column(11, textInput(paste0("ValueName", i), "What type of measurement is repeated horizontally? (Give a column name without space)", value = names(groupNames)[i]),
                   pickerInput(
                     inputId = paste0("Variablecolumns", i),
                     label = "Select the columns that are repeats of measurements",
                     choices = colnames(OneTable()),
                     selected = groupNames[[i]],
                     multiple = T
                   )
            ))
      })

    })
  })

  observeEvent(input$ClearValueName,{
    updateRadioButtons(session,"VariableName",selected = character(0))
  })

  observe({
    if(!is.null(input$VariableName)) shinyjs::hide("SkipTidy")
    if(is.null(input$VariableName)) shinyjs::show("SkipTidy")


  })

  TidyTable <- reactiveVal(NULL)

  observeEvent(input$Tidy, {

    Variablecolumns <- reactiveValuesToList(input)[grep("Variablecolumns\\d", names(input))]
    Variablecolumns<- Variablecolumns[order(names(Variablecolumns))]

    ValueName <- reactiveValuesToList(input)[grep("ValueName\\d", names(input))]
    ValueName <- ValueName[order(names(ValueName))]

    TickedMelt <- unlist(reactiveValuesToList(input)[grep("TickedMelt\\d", names(input))])
    TickedMelt <- TickedMelt[order(names(TickedMelt))]

    names(Variablecolumns) <- unlist(ValueName)

    TidyTable(melt(OneTable(), measure.vars	= Variablecolumns[TickedMelt], variable.name =  input$VariableName,  value.name = names(Variablecolumns[TickedMelt]), variable.factor = FALSE))


  }, ignoreInit = TRUE)

  observeEvent(input$SkipTidy, {
    TidyTable(OneTable())
  }, ignoreInit = TRUE)



  observeEvent(input$Tidy, {
    shinyjs::show("GoToHeaders")

    output$TidyTable <- renderDT(TidyTable(), rownames = FALSE,
                                 options = list(pageLength = 8, scrollX=TRUE))

    output$TidyTableSummary <- renderPrint(summary(TidyTable()))
  })


  observeEvent(input$GoToHeaders | input$SkipTidy, {
    updateTabItems(session, "tabs", "Headers")

    }, ignoreInit = T)

  observe({
    if(!input$Date %in% "none")
      shinyjs::show("AttentionDates")
    output$sampleDates <- renderText(head(as.character(TidyTable()[[input$Date]])))
  })



  ### other stuff ####
  gimme_value <- reactiveVal(0)

observe( {
  if(input$predefinedProfile != "No" )
  shinyjs::show("UseProfile")
  updateActionButton(session, inputId = "UseProfile", label = "Click Twice here to use Profile")
  gimme_value(0)
})

  observeEvent(input$profile, {
    shinyjs::show("UseProfile")
    updateActionButton(session, inputId = "UseProfile", label = "Click Twice here to use Profile")
    gimme_value(0)
  })

  observeEvent(input$UseProfile, {

    if(input$predefinedProfile == "No") {
    file <- input$profile$datapath
    ext <- tools::file_ext(file)
    } else {
      file <- paste0("data/", input$predefinedProfile, "Profile.rds")
      ext <- tools::file_ext(file)
    }

    req(file)
    validate(need(ext == "rds", "Please upload a .rds file"))

    profile <- readRDS(file)

    for(i in which(x$ItemID %in% names(profile))) {
      eval(parse(text = paste0(paste0("update", firstUpper(x$ItemType[i])), "(session,inputId = x$ItemID[i],", ifelse(x$argument[i] %in% "choices", "selected", "value"), "= profile[[x$ItemID[i]]])")))

      # eval(parse(text = paste0("updateTextInput(session, '", x$ItemID[i], "', value = profile$", x$ItemID[i], ")")))
      # updateTextInput(session, "Site", value = profile$Site)
    }

   if(gimme_value() == 1) {
      updateActionButton(session, inputId = "UseProfile", label = "Thanks!")
      }

    if(gimme_value() == 0) {
      updateActionButton(session, inputId = "UseProfile", label = "click one more time!")
      gimme_value(gimme_value() + 1)
    }


  })


  # render data table
  output$tabData <- renderDT({
    if (!is.null(input$file1$name))
      Data()
  }, rownames = FALSE,
  options = list(pageLength = 8, scrollX=TRUE))



  # create options to choose from:

  ColumnOptions <- eventReactive(TidyTable(), { c("none", colnames(TidyTable())) })

  UnitOptions <- eventReactive(TidyTable(),
                               {c("none", "mm", "cm", "dm", "m")
                               })

  AreaUnitOptions <- eventReactive(input$PlotArea,
                                   {c("none", "m2", "ha", "km2")
                                   })


  LifeStatusOptions <- eventReactive(input$LifeStatus, {
    sort(unique(TidyTable()[[input$LifeStatus]]))})

  CommercialOptions <- eventReactive(input$CommercialSp, {
    sort(unique(TidyTable()[[input$CommercialSp]]))})

  OtherOptions <- eventReactive(TidyTable(), {""})

  LogicalOptions <- reactive(c(TRUE, FALSE))

  # enter column names for each element of the RequiredFormat function
  observe({

    lapply(1:nrow(x1), function(i) {

      eval(parse(text = paste(paste0("update", firstUpper(x1$ItemType[i])), "(session, inputId = x1$ItemID[i],", x1$argument[i],"= get(x1$argValue[i])())")))

    })

  })


  observe({

    lapply(c(1:nrow(x2)), function(i) {
      if(input[[x2$if_X1_is_none[i]]] %in% "none") {
        eval(parse(text = paste(paste0("update", firstUpper(x2$ItemType[i])), "(session, inputId = x2$ItemID[i],", x2$argument[i], "= get(x2$argValue[i])())")))

        shinyjs::show( x2$ItemID[i])

      } else {
        eval(parse(text = paste0(paste0("update", firstUpper(x2$ItemType[i])), "(session, inputId = x2$ItemID[i],", x2$argument[i], "='",  x2$default[i], "')")))

        shinyjs::hide( x2$ItemID[i])
      }

    })


    lapply(c(1:nrow(x3)), function(i) {
      if(input[[x3$if_X1_is_none[i]]] %in% "none" & !input[[x3$if_X2_isnot_none[i]]] %in% "none" ) {

        eval(parse(text = paste(paste0("update", firstUpper(x3$ItemType[i])), "(session, inputId = x3$ItemID[i],", x3$argument[i], "= get(x3$argValue[i])())")))

        shinyjs::show( x3$ItemID[i])


      } else {
        eval(parse(text = paste0(paste0("update", firstUpper(x3$ItemType[i])), "(session, inputId = x3$ItemID[i],", x3$argument[i], "='",  x3$default[i], "')")))

        shinyjs::hide( x3$ItemID[i])
      }

    })


    lapply(c(1:nrow(x4)), function(i) {
      if(!input[[x4$if_X2_isnot_none[i]]] %in% "none" ) {
        eval(parse(text = paste0(paste0("update", firstUpper(x4$ItemType[i])), "(session,inputId = x4$ItemID[i],", x4$argument[i], "= get(x4$argValue[i])())")))

        shinyjs::show( x4$ItemID[i])

      } else {

        eval(parse(text = paste0(paste0("update", firstUpper(x4$ItemType[i])), "(session,inputId = x4$ItemID[i],", x4$argument[i], "='", x4$Default[i], "')")))

        shinyjs::hide( x4$ItemID[i])

      }

    })

    lapply(c(1:nrow(x5)), function(i) {
      if(input[[x5$if_X1_is_none[i]]] %in% "none" &  input[[x5$if_X2_is_none[i]]] %in% "none") {
        eval(parse(text = paste0(paste0("update", firstUpper(x5$ItemType[i])), "(session,inputId = x5$ItemID[i],", x5$argument[i], "= get(x5$argValue[i])())")))

        shinyjs::show( x5$ItemID[i])

      } else {

        eval(parse(text = paste0(paste0("update", firstUpper(x5$ItemType[i])), "(session,inputId = x5$ItemID[i],", x5$argument[i], "='", x5$Default[i], "')")))

        shinyjs::hide( x5$ItemID[i])

      }

    })

   if(nrow(x6) > 0 ) lapply(c(1:nrow(x6)), function(i) {
      if(input[[x6$if_X2_is_none[i]]] %in% "none" & !input[[x6$if_X2_isnot_none[i]]] %in% "none") {
        eval(parse(text = paste0(paste0("update", firstUpper(x6$ItemType[i])), "(session,inputId = x6$ItemID[i],", x6$argument[i], "= get(x6$argValue[i])())")))

        shinyjs::show( x6$ItemID[i])

      } else {

        eval(parse(text = paste0(paste0("update", firstUpper(x6$ItemType[i])), "(session,inputId = x6$ItemID[i],", x6$argument[i], "='", x6$Default[i], "')")))

        shinyjs::hide( x6$ItemID[i])

      }

    })

  })



  # format data usin the input

  DataFormated <- eventReactive(input$LaunchFormating | input$UpdateTable, {

    tryCatch({
      RequiredFormat(Data = TidyTable(), isolate(reactiveValuesToList(input)), x, ThisIsShinyApp = T)
    },
    # warning = function(warn){
    #   showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning', duration = NULL)
    # },
    error = function(err){
      showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", err), type = 'err', duration = NULL)
    })



  }, ignoreInit = T)

  FormatedColumnOptions <- eventReactive(input$LaunchFormating, { c("none", colnames(DataFormated()))
  })

  observeEvent(input$LaunchFormating , {
    shinyjs::show("GoToCorrect")
       }, ignoreInit = T)

  observeEvent(input$LaunchFormating , {

  lapply(which(xCorr$argument %in% "choices"), function(i) {

    eval(parse(text = paste0(paste0("update", firstUpper(xCorr$ItemType[i])), "(session,inputId = xCorr$ItemID[i],", xCorr$argument[i], "= get(xCorr$argValue[i])()", ifelse(xCorr$argument2[i] != FALSE, paste0(", ", xCorr$argument2[i], ifelse(xCorr$Default[i] %in% c("TRUE", "FALSE"), paste0(" = '", xCorr$Default[i], "'"), paste0(" = eval(parse(text = '",xCorr$Default[i], "'))")), ")")))))
  })

  })


  # Visualize output
  output$FormatedTable <- renderDT(DataFormated(), rownames = FALSE,
                                 options = list(pageLength = 8, scrollX=TRUE))

  output$FormatedTableSummary <- renderPrint(summary(DataFormated()))


  observeEvent(input$GoToCorrect, {
    updateTabItems(session, "tabs", "Correct")
  }, ignoreInit = TRUE)


  observeEvent(input$GoToDownload | input$SkipCorrections , {
    updateTabItems(session, "tabs", "Save")
  }, ignoreInit = T)


  # apply corrections

  observe({
    for(f in unique(xCorr$Function)) {
      if(input[[f]] %in% "Yes") shinyjs::show(paste0(f, "Yes"))
      else shinyjs::hide(paste0(f, "Yes"))

    }

    if(any(unlist(reactiveValuesToList(input)[unique(xCorr$Function)]) %in% "Yes")) {
      shinyjs::show("ApplyCorrections")
      shinyjs::hide("SkipCorrections")
    } else {
      shinyjs::hide("ApplyCorrections")
      shinyjs::show("SkipCorrections")
    }
  })

  observeEvent(input$ApplyCorrections,{
    shinyjs::show("GoToDownload")
    })

  DataCorrected <- eventReactive(input$ApplyCorrections, {

    Rslt <- DataFormated()
    lapply(
      unique(xCorr$Function),
      FUN = function(f){
       if(input[[f]] %in% "Yes") {
         cl <- str2lang(paste0(f, "(", paste("Data = Rslt,", paste(paste(xCorr$Label[xCorr$Function %in% f], "=",reactiveValuesToList(input)[xCorr$ItemID[xCorr$Function %in% f]]), collapse = ", ")),")"))
         Rslt <<- eval(cl)


         tryCatch({
           eval(cl)
         },
         # warning = function(warn){
         #   showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning', duration = NULL)
         # },
         error = function(err){
           showNotification(err, type = 'err', duration = NULL)
         })
       }
      }
    )
    Rslt
  }, ignoreInit = T)

  DataCorrected <- eventReactive(input$SkipCorrections, {DataFormated()})

    output$CorrectedTable <- renderDT(DataCorrected(), rownames = FALSE,
                                 options = list(pageLength = 8, scrollX=TRUE))

    output$CorrectedTableSummary <- renderPrint(summary(DataCorrected()))





  # save final data table

    DataOutput <- reactiveVal(NULL)


    observe( {
      if(input$predefinedProfileOutput != "No" )
        shinyjs::show("UseProfileOuput")
    })

    observeEvent(input$profileOutput, {
      shinyjs::show("UseProfileOuput")
    })

    observeEvent(input$UseProfileOuput, {
      shinyjs::show("DontUseProfileOuput")


      if(input$predefinedProfileOutput == "No") {
        file <- input$profileOutput$datapath
        ext <- tools::file_ext(file)
      } else {
        file <- paste0("data/", input$predefinedProfileOutput, "Profile.rds")
        ext <- tools::file_ext(file)
      }

      req(file)
      validate(need(ext == "rds", "Please upload a .rds file"))

      profileOutput <- readRDS(file)

      DataOutput(
        ReversedRequiredFormat(DataCorrected(), profileOutput, x, ThisIsShinyApp = T)
        )


    })

    observeEvent(input$DontUseProfileOuput, {
      shinyjs::hide("DontUseProfileOuput")
      shinyjs::hide("UseProfileOuput")

      DataOutput(DataCorrected())
    })


    # Visualize output
    output$DataOutput <- renderDT(DataOutput(), rownames = FALSE,
                                     options = list(pageLength = 8, scrollX=TRUE))

    output$DataOutputSummary <- renderPrint(summary(DataOutput()))


  output$dbFile <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_formated.csv', sep = '')
    },
    content = function(file) {
      write.csv(DataFormated(), file, row.names = FALSE)
    }
  )

  # save profile Rdata file

  output$dbProfile <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_Profile.rds', sep = '')
    },
    content = function(file) {
      inputs_to_save <- c(names(input)[names(input) %in% x$ItemID],
                          "Tidy", "VariableName", grep("Variablecolumns|TickedMelt|ValueName", names(input), value = T))
      Profile <- list()
      for(input.i in inputs_to_save){
        Profile[[input.i]] <-  input[[input.i]]
      }
      saveRDS( Profile, file = file)
    }
  )

  # save code needed to produce the table

  output$dbCode <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "",input$file1$name), '_Code.R', sep = '')
    },
    content = function(file) {
      text_upload <- glue::glue(
        "
      # install TreeData package
      githubinstall::githubinstall('VincyaneBadouard/TreeData')
      library(TreeData)

      # upload the data
       Data <- data.table::fread('{input$file1$name}', header = {input$header}, sep = '{input$cbSeparator}')

      # upload your profile (saved via shiny app)
      Profile <- readRDS(paste0(gsub('.csv', '', '{input$file1$name}'), '_Profile.rds'))

      # format your data
      DataFormated <- RequiredFormat( Data, input = Profile)
      ")
      writeLines(text_upload, file)
    }
  )

  # Save metadata

  output$dbMetadata <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_Metadata.csv', sep = '')
    },
    content = function(file) {
      columns_to_save <- colnames(DataFormated())
      Metadata <- data.frame(Field = columns_to_save,
                             Description = x$Description[match(columns_to_save, x$ItemID)])

      write.csv(Metadata, file = file, row.names = F)
    }
  )




}
