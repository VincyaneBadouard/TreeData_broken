
# Fichier pour gérer les interactions de l'application Shiny


# source the REquiredFormat function to get the list of arguments
# source(paste0(dirname(dirname(getwd())), "/R/RequiredFormat.R")) # ***make this better!!**
# x <- as.list(formals(RequiredFormat)[-1])

# read in csv file that has all we want to show in app
x <- read.csv("data/interactive_items.csv")

for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])
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
    # req(x1)
    lapply(c(1:nrow(x1)), function(i) {
      fluidRow(
        selectInput(x1$ItemID[i], label = h3(x1$Label[i]), choices = eval(parse(text = x1$Choices[i])))
      )


    })
  })


  output$ui2 <- renderUI({
    # req(x2)

    lapply(c(1:nrow(x2)), function(i) {
      if(input[[x2$Dependant_on[i]]] %in% "none")
        fluidRow(
        selectInput(x2$ItemID[i], label = h3(x2$Label[i]), choices = eval(parse(text = x2$Choices[i])))
      )


    })
  })

  output$ui3 <- renderUI({
    # req(x3)

    lapply(c(1:nrow(x3)), function(i) {
      if(input[[x3$Dependant_on[i]]] %in% "none")
        fluidRow(
          numericInput(x3$ItemID[i], label = h3(x3$Label[i]), value = eval(parse(text = x3$Value[i])))
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
