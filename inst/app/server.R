
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
  
  # render data table
  output$tabData <- renderDT({
    if (!is.null(input$file1$name))
      invData()
  }, rownames = FALSE, # n'affiche pas les noms de lignes
  options = list(pageLength = 10)) # nombre de lignes par page
  
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
