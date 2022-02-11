
# Fichier pour gérer les interactions de l'application Shiny


# source the REquiredFormat function to get the list of arguments
# source(paste0(dirname(dirname(getwd())), "/R/RequiredFormat.R")) # ***make this better!!**
# x <- as.list(formals(RequiredFormat)[-1])

# read in csv file that has all we want to show in app
x <- read.csv("data/interactive_items.csv")
x <- x[x$Activate, ]
for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])

# get the function here (ugly solution but not sure how esle to do it- unless installing packgae from GitHub)
source("../../R/RequiredFormat.R")
server <- function(input, output) {

  # read file and create data table
  Data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    data.table::fread(file$datapath,
                      header = input$header,
                      sep = input$cbSeparator)
  })


  # render data table
  output$tabData <- renderDT({
    if (!is.null(input$file1$name))
      Data()
  }, rownames = FALSE,
  options = list(pageLength = 10, scrollX=TRUE))

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
                                {c("none", "mm", "cm", "dm", "m")
                                })

  OtherOptions <- eventReactive(Data(), { ""})

  # enter column names for each element of the RequiredFormat function

  observeEvent(input$file1,
               {
                 output$ui1 <- renderUI({
                   # dropdown(
                   lapply(1:nrow(x1), function(i) {
                     # box(
                       # h3(x1$Label[i]),
                       # helpText(x1$helpText[i]),
                       eval(parse(text = paste(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$argument[i], "= get(x1$argValue[i])(), options = ", x1$Options[i], ")")))
                     # )


                   })
                   # )
                 })

                 output$ui2 <- renderUI({
                   # req(x2)


                   lapply(c(1:nrow(x2)), function(i) {
                     if(input[[x2$if_X1_is_none[i]]] %in% "none")
                       # box(
                       #   h3(x2$Label[i]),
                       #   helpText(x2$helpText[i]),
                         eval(parse(text = paste(x2$ItemType[i], "(inputId = x2$ItemID[i], label = ifelse(x2$helpText[i] %in% '', x2$Label[i], paste0(x2$Label[i], ' (', x2$helpText[i], ')')),", x2$argument[i], "= get(x2$argValue[i])())")))
                       #   br()
                       #
                       # )



                   })
                 })

                 output$ui3 <- renderUI({
                   # req(x3)

                   lapply(c(1:nrow(x3)), function(i) {
                     if(input[[x3$if_X1_is_none
                               [i]]] %in% "none" & !input[[x3$if_X2_isnot_none[i]]] %in% "none" )
                       # box(
                         # h5(input[[x$Dependant_on[i]]] %in% "none"),
                         # h3(x3$Label[i]),
                         # helpText(x3$helpText[i]),
                         eval(parse(text = paste(x3$ItemType[i], "(inputId = x3$ItemID[i], label = ifelse(x3$helpText[i] %in% '', x3$Label[i], paste0(x3$Label[i], ' (', x3$helpText[i], ')')),", x3$argument[i], "= get(x3$argValue[i])())")))
                       #   br()
                       #
                       # )


                   })
                 })

                 output$ui4 <- renderUI({
                   # req(x4)


                   lapply(c(1:nrow(x4)), function(i) {
                     if(!input[[x4$if_X2_isnot_none[i]]] %in% "none" )
                       # box(
                       #   h3(x4$Label[i]),
                       #   helpText(x4$helpText[i]),
                       eval(parse(text = paste(x4$ItemType[i], "(inputId = x4$ItemID[i], label = ifelse(x4$helpText[i] %in% '', x4$Label[i], paste0(x4$Label[i], ' (', x4$helpText[i], ')')),", x4$argument[i], "= get(x4$argValue[i])())")))
                     #   br()
                     #
                     # )



                   })
                 })

               })


  # format data usin the input

  DataFormated <- eventReactive(input$LaunchFormating | input$UpdateTable, {

    tryCatch({
      RequiredFormat(Data = Data(), isolate(reactiveValuesToList(input)), x, ThisIsShinyApp = T)
    },
    warning = function(warn){
      showNotification(gsub("in RequiredFormat\\(Data = Data\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning')
    },
    error = function(err){
      showNotification(gsub("in RequiredFormat\\(Data = Data\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", err), type = 'err')
    })
  })

  # Visualize output
  output$tabDataFormated <- renderDT({
    # validate(
    #   need(req(DataFormated()), "AA")
    # )
      DataFormated()
  }, rownames = FALSE,
  options = list(pageLength = 10, scrollX=TRUE))

  # save final data table

  # rf2 <- reactiveValues()
  # observe({
  #   if(!is.null(input[[names(input) %in% x$ItemID]]))
  # eventReactive(input$LaunchFormating | input$UpdateTable, {
  #       profile <- input[[which(names(input) %in% x$ItemID)]]
  #     })
  # })

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

}
