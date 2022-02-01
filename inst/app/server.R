
# Fichier pour gérer les interactions de l'application Shiny


# source the REquiredFormat function to get the list of arguments
# source(paste0(dirname(dirname(getwd())), "/R/RequiredFormat.R")) # ***make this better!!**
# x <- as.list(formals(RequiredFormat)[-1])

# read in csv file that has all we want to show in app
x <- read.csv("data/interactive_items.csv")
x <- x[x$Activate, ]
for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])

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

  # Avoid seeing error when data has no been submitted yet
  text_reactive <- reactiveValues(
    text = "No data has been submitted yet."
  )
  output$ui1 <- renderUI({
    box(text_reactive$text)
  })
  output$ui2 <- renderUI({
    box(text_reactive$text)
  })
  output$ui3 <- renderUI({
    box(text_reactive$text)
  })

  # create options to choose from:

  column_options <- eventReactive(Data(), { c("none", colnames(Data())) })

  unit_options <- eventReactive(Data(),
                                {c("none", "mm", "cm", "dm", "m")
                                })

  other_options <- eventReactive(Data(), { ""})

  # enter column names for each element of the RequiredFormat function

  observeEvent(input$file1,
               {
                 output$ui1 <- renderUI({
                   # dropdown(
                   lapply(1:nrow(x1), function(i) {
                     # box(
                       # h3(x1$Label[i]),
                       # helpText(x1$helpText[i]),
                       eval(parse(text = paste(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$argument[i], "= get(x1$argValue[i])())")))
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


               })
  # render data table
  output$tabData <- renderDT({
    if (!is.null(input$file1$name))
      Data()
  }, rownames = FALSE,
  options = list(pageLength = 10))

  # save final data table
  output$dbFile <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '_data.csv', sep = '')
    },
    content = function(file) {
      write.csv(Data(), file, row.names = FALSE)
    }
  )

  # save code needed to produce the table
  output$dbCode <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '_code.R', sep = '')
    },
    content = function(file) {
      text_upload <- glue::glue("# upload the data
                         Data <- data.table::fread('{input$file1$name}', header = {input$header}, sep = '{input$cbSeparator}')")
      writeLines(text_upload, file)
    }
  )

}
