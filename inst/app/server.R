
# Fichier pour gérer les interactions de l'application Shiny


# increase size limit to 10MB
options(shiny.maxRequestSize=10*1024^2)

# source the REquiredFormat function to get the list of arguments
# source(paste0(dirname(dirname(getwd())), "/R/RequiredFormat.R")) # ***make this better!!**
# x <- as.list(formals(RequiredFormat)[-1])

# read in csv file that has all we want to show in app
x <- read.csv("data/interactive_items.csv")
x <- x[x$Activate, ]
for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])

# install TreeData package
# devtools::install_github("VincyaneBadouard/TreeData")
library(TreeData)

server <- function(input, output, session) {

  # generate the upload UI  ####
  # observeEvent(input$nTable,
  #              {
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
                 ),
                 # textInput(inputId = paste0("TableDescription", i),
                 #           label = "Give an explicit description of your table.",
                 #           value = paste0("My Table", i)
                 # ),
                 tags$textarea(id=paste0("TableDescription", i),
                               label = "Give an explicit description of your table...",
                               rows=5,
                               cols = 6,
                               placeholder = paste0("My Table", i))
             )
      )


    })



  })
               # })

  # read file(s) ####


  observeEvent(input$submitTables, {


  updateCheckboxGroupButtons(session, "TablesToStack",
                    choices = unname(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]))


  updateTabItems(session, "tabs", "Stacking")


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

  observeEvent(input$submitTables, {
    if(input$nTable == 1 & length(Data()) == 1) {
      updateTabItems(session, "tabs", "Tidying")
      OneTable <<- Data()[[1]]
      updatePickerInput(session, "Variablecolumns", choices = colnames(OneTable))
      }
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



    # if(is.null(input$TablesToStack)) {
    options_to_merge <- names(Data())
    column_options_list <- lapply(Data(), colnames)
    # }

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
    updateTabItems(session, "tabs", "Tidying")

    if(exists("MergedTables")) OneTable <<- MergedTables() else OneTable <<- StackedTables()

    updatePickerInput(session, "Variablecolumns", choices = colnames(OneTable))

  }, ignoreInit = TRUE)



  observeEvent(input$ClearValueName,{
    updateRadioButtons(session,"VariableName",selected = character(0))
  })

  observe({
    if(!is.null(input$VariableName)) shinyjs::hide("SkipTidy")
    if(is.null(input$VariableName)) shinyjs::show("SkipTidy")


  })


  observeEvent(input$Tidy, {
    TidyTable <- melt(OneTable, measure.vars	= input$Variablecolumns, variable.name = input$VariableName, value.name = input$ValueName)

    output$TidyTable <- renderDT(TidyTable, rownames = FALSE,
                                     options = list(pageLength = 8, scrollX=TRUE))

    output$TidyTableSummary <- renderPrint(summary(TidyTable))
  })


#   values <- reactiveValues(n_merge = 0)
#
# # add a merge
#   observeEvent(input$addMerge, {
#
#
#     values$n_merge <- values$n_merge + 1
#     add <- values$n_merge
#
#   insertUI(
#         selector = "#addMerge",
#         where = "afterEnd",
#         ui =
#           tags$div(
#             box(width = 12,
#                 column(3, selectInput(paste0("leftTable", input$add), "Take table", choices = options_to_merge)),
#                 column(3, selectInput(paste0("rightTable", input$add), "add to it this table", choices = options_to_merge)),
#                 column(3, selectInput(paste0("leftKey", input$add), "Using this column(s) from first table", choices = "", multiple = T)),
#                 column(3,  selectInput(paste0("rightKey", input$add), "and this column(s) from second table", choices = "", multiple = T))
#                 ),
#                 box(
#                   title = "inputID", width = 12, background = "black",
#
#                   paste0("Number_Product1_", input$add))  #### inputID's of the selectizeinput "Product 1"
#
#
#           )
#       )
#
#   output$test <- renderText(input$add)
#
#
#   })






  # submit tables
  observeEvent(input$submit, {
    output$uiheader <- renderUI({

      lapply(1:isolate(input$nTable), function(i) {
        X <- isolate(colnames(Data()[[i]]))
        title <- isolate(reactiveValuesToList(input)[paste0("TableName", i)])
        box(
          title = title,
          lapply(X, function(x) pickerInput(inputId = paste(title, x, sep = "_"), label = x, choices = choices, multiple = T, options = list(`live-search` =T, width = F), choicesOpt = list(content = subtext), width = '75%'))

        )
      })


    })

    updateTabItems(session, "tabs", "Headers")


  })






### other stuff ####


  observeEvent(input$profile, {
    file <- input$profile
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "rds", "Please upload a csv file"))

    profile <- readRDS(file$datapath)

    for(i in which(x$ItemID %in% names(profile))) {
      eval(parse(text = paste0("updateTextInput(session, '", x$ItemID[i], "', value = profile$", x$ItemID[i], ")")))
      # updateTextInput(session, "Site", value = profile$Site)
    }




  })


  # render data table
  output$tabData <- renderDT({
    if (!is.null(input$file1$name))
      Data()
  }, rownames = FALSE,
  options = list(pageLength = 8, scrollX=TRUE))

  # Avoid seeing errors
  text_reactive <- reactiveValues(
    NoData = "No data has been submitted yet."
  )

  output$ui1 <- renderUI({
    p(text_reactive$NoData)
  })
  output$ui2 <- renderUI({
    p(text_reactive$NoData)
  })
  output$ui3 <- renderUI({
    p(text_reactive$NoData)
  })
  output$ui4 <- renderUI({
    p(text_reactive$NoData)
  })

  # create options to choose from:

  ColumnOptions <- eventReactive(Data(), { c("none", colnames(Data())) })

  UnitOptions <- eventReactive(Data(),
                                {c("mm", "cm", "dm", "m")
                                })

  AreaUnitOptions <- eventReactive(Data(),
                                   {c("m2", "ha", "km2")
                                   })


  LifeStatusOptions <- eventReactive(input$LifeStatus, {
    sort(unique(Data()[[input$LifeStatus]]))})

  CommercialOptions <- eventReactive(input$CommercialSp, {
    sort(unique(Data()[[input$CommercialSp]]))})

  OtherOptions <- eventReactive(Data(), {""})

  # enter column names for each element of the RequiredFormat function

  observeEvent(input$file1,
               {
                 output$ui1 <- renderUI({
                   lapply(1:nrow(x1), function(i) {

                     eval(parse(text = paste(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$argument[i],"= get(x1$argValue[i])()", ifelse(x1$Options[i] != FALSE, paste0(", options = ", x1$Options[i]), ""), ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })

                 })

                 output$ui2 <- renderUI({

                   lapply(c(1:nrow(x2)), function(i) {
                     if(input[[x2$if_X1_is_none[i]]] %in% "none")
                       eval(parse(text = paste(x2$ItemType[i], "(inputId = x2$ItemID[i], label = ifelse(x2$helpText[i] %in% '', x2$Label[i], paste0(x2$Label[i], ' (', x2$helpText[i], ')')),", x2$argument[i], "= get(x2$argValue[i])()", ifelse(x2$Options[i] != FALSE, paste0(", options = ", x2$Options[i]), ""), ifelse(x2$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

                 output$ui3 <- renderUI({

                   lapply(c(1:nrow(x3)), function(i) {
                     if(input[[x3$if_X1_is_none[i]]] %in% "none" & !input[[x3$if_X2_isnot_none[i]]] %in% "none" )

                       eval(parse(text = paste(x3$ItemType[i], "(inputId = x3$ItemID[i], label = ifelse(x3$helpText[i] %in% '', x3$Label[i], paste0(x3$Label[i], ' (', x3$helpText[i], ')')),", x3$argument[i], "= get(x3$argValue[i])()", ifelse(x3$Options[i] != FALSE, paste0(", options = ", x3$Options[i]), ""), ifelse(x3$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

                 output$ui4 <- renderUI({

                   lapply(c(1:nrow(x4)), function(i) {
                     if(!input[[x4$if_X2_isnot_none[i]]] %in% "none" )
                       eval(parse(text = paste(x4$ItemType[i], "(inputId = x4$ItemID[i], label = ifelse(x4$helpText[i] %in% '', x4$Label[i], paste0(x4$Label[i], ' (', x4$helpText[i], ')')),", x4$argument[i], "= get(x4$argValue[i])()", ifelse(x4$Options[i] != FALSE, paste0(", options = ", x4$Options[i]), ""), ifelse(x4$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

                 output$ui5 <- renderUI({

                   lapply(c(1:nrow(x5)), function(i) {
                     if(input[[x5$if_X1_is_none[i]]] %in% "none" & input[[x5$if_X2_is_none[i]]] %in% "none" )

                       eval(parse(text = paste(x5$ItemType[i], "(inputId = x5$ItemID[i], label = ifelse(x5$helpText[i] %in% '', x5$Label[i], paste0(x5$Label[i], ' (', x5$helpText[i], ')')),", x5$argument[i], "= get(x5$argValue[i])()", ifelse(x5$Options[i] != FALSE, paste0(", options = ", x5$Options[i]), ""), ifelse(x5$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

               })


  # format data usin the input

  DataFormated <- eventReactive(input$LaunchFormating | input$UpdateTable, {

    tryCatch({
      RequiredFormat(Data = Data(), isolate(reactiveValuesToList(input)), x, ThisIsShinyApp = T)
    },
    warning = function(warn){
      showNotification(gsub("in RequiredFormat\\(Data = Data\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning', duration = NULL)
    },
    error = function(err){
      showNotification(gsub("in RequiredFormat\\(Data = Data\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", err), type = 'err', duration = NULL)
    })
  })

  # Visualize output
  output$tabDataFormated <- renderDT({
    # validate(
    #   need(req(DataFormated()), "AA")
    # )
      DataFormated()
  }, rownames = FALSE,
  options = list(pageLength = 8, scrollX=TRUE))

  # save final data table

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
      inputs_to_save <- names(input)[names(input) %in% x$ItemID]
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
      DataFormated <- ParacouSubsetFormated <- RequiredFormat( Data, input = Profile)
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
