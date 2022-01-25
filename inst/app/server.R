
# Fichier pour gérer les interactions de l'application Shiny

server <- function(input, output) {

  # read file and create data table
  invData <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    data.table::fread(file$datapath,
                      header = input$header,
                      sep = input$cbSeparator)
  })

  # enter column names for each element of the RequiredFormat function

  output$ui1 <- renderUI({
    # req(x)
    lapply(c(1:nrow(x)), function(i) {
      fluidRow(
        selectInput(x$ItemID[i], label = h3(x$Label[i]), choices = eval(parse(text = x$Choices[i])))
      )


    })
  })

  # render data table
  output$tabData <- renderDT({
    if (!is.null(input$file1$name))
      invData()
  }, rownames = FALSE,
  options = list(pageLength = 10))

  # save final data table
  output$dbFile <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '_data.csv', sep = '')
    },
    content = function(file) {
      write.csv(invData(), file, row.names = FALSE)
    }
  )

  # save code needed to produce the table
  output$dbCode <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), '_code.R', sep = '')
    },
    content = function(file) {
      text_upload <- glue::glue("# upload the data
                         invData <- data.table::fread('{input$file1$name}', header = {input$header}, sep = '{input$cbSeparator}')")
      writeLines(text_upload, file)
    }
  )

}
