
# Fichier pour gérer les interactions de l'application Shiny

# source script to get VegX_tree
# source("data/My_VegX.R")
VegXtree <- readRDS("data/VegXtree.rds")
VegXtree$Do(function(x) x$inputId <-gsub("/", "_", x$pathString))

# tree <- ToListExplicit(VegXtree)$children
tree <- cbind(ToDataFrameTypeCol(VegXtree), ToDataFrameTable(VegXtree, "name", "inputId", "annotation"))

tree$subtext <- paste("<strong>", tree$name, "</strong>", stringr::str_replace_all( stringr::str_wrap(tree$annotation, width = 50), "\\n", "<br>"))
# tree <- tree[tree$name != "NULL",] # remove "NULL names
tree <- split(tree, factor(tree$level_2, levels = unique(tree$level_2)))

choices <- sapply(tree, function(x) setNames(as.list(gsub("VegX_", "", x$inputId)), x$name))
subtext <- unlist(sapply(tree, "[[", "subtext"), recursive = T)

# subtext <- unlist(sapply(tree, "[[", "annotation"))
# subtext <- stringr::str_wrap(subtext, width = 75)
# subtext <- paste("<strong>", unlist(choices), "</strong>", stringr::str_replace_all(subtext, "\\n", "<br>"))

# level3 <-  sapply(tree, "[[", "level_3")
# choices <- mapply(FUN = function(x, y) split(x, factor(y, levels = unique(y))), x = choices, y = level3)
# subtext <- mapply(FUN = function(x, y) split(x, factor(y, levels = unique(y))), x = subtext, y = level3)

# test to show we can only have 1 2 levels of nestedness
# choices = list(A = c("A", B = c("b", "B")), C = c("C", "D"))
# subtext = unlist(choices)

server <- function(input, output, session) {

  observeEvent(input$nTable,
               {
                 output$ui_uploadTables <- renderUI({
                   lapply(1:input$nTable, function(i) {

                     column(width = 3,
                            # load button for main data file (csv format)
                            box(title = paste("Upload your table", i),
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
                                          label = "Give an explicit UNIQUE name to this table. No space, no special character, no accent.",
                                          value = paste0("Table", i)
                                )
                            )
                     )


                   })


                 })
               })

  # read file and create data table
  Data <- reactive({
    req(input$file1)
    sapply(c(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]), function(n) {

      i = which(c(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]) %in% n)
      file <- input[[paste0("file", i)]]
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))

      data.table::fread(file$datapath,
                        header = input[[paste0("header", i)]],
                        sep = input[[paste0("cbSeparator", i)]])
    }, simplify = F)

  })


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
    if (!is.null(input$file0$name))
      Data()
  }, rownames = FALSE,
  options = list(pageLength = 8, scrollX=TRUE))

  # Avoid seeing errors
  text_reactive <- reactiveValues(
    NoData = "No data has been submitted yet."
  )



  # observe({
  #
  #
  #
  #   output$uiheader <- renderUI({
  #     choices <-  isolate(unlist(sapply(names(Data()), function(x) setNames(paste(x, colnames(Data()[[x]]), sep = "_"), paste(x, colnames(Data()[[x]]), sep = "_")), simplify = FALSE)))
  #     lapply(tree, function(x) {
  #     div(h2(x$level_2[1]),
  #         lapply(1:nrow(x), function(i) {
  #           pickerInput(inputId = x$inputId[i] ,
  #                       label =  div(h4(x$name[i]), p(x$annotation[i])),
  #                       choices = choices,
  #                       inline = T)
  #         })
  #     )
  #
  #
  #   })})
  #
  #
  #   })






  observeEvent(input$submit, {
  output$uiheader <- renderUI({

    lapply(1:isolate(input$nTable), function(i) {
      X <- isolate(colnames(Data()[[i]]))
      title <- isolate(reactiveValuesToList(input)[paste0("TableName", i)])
      box(
        title = title,
        lapply(X, function(x) pickerInput(inputId = paste(title, x, sep = "_"), label = x, choices = choices, multiple = T, options = list(liveSearch =T, width = F), choicesOpt = list(content = subtext), width = '75%'))

      )
      })


  })

  updateTabItems(session, "tabs", "headers")
  })




  # observeEvent(Data(), {output$test <- renderText({unlist(sapply(names(Data()), function(x) paste(x, colnames(Data()[[x]]), sep = "_")))})})
  # observeEvent(Data(), {output$test <-renderText({ paste(paste(names(Data())[1], colnames(Data()[[1]]), sep = "_"), collapse = "") })})
  # observeEvent(Data(), {output$test <-renderText({ column_names() })})

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

  observeEvent(input$file0,
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
      paste(gsub(".csv", "", input$file0$name), '_formated.csv', sep = '')
    },
    content = function(file) {
      write.csv(DataFormated(), file, row.names = FALSE)
    }
  )

  # save profile Rdata file

  output$dbProfile <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file0$name), '_Profile.rds', sep = '')
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
      paste(gsub(".csv", "",input$file0$name), '_Code.R', sep = '')
    },
    content = function(file) {
      text_upload <- glue::glue(
        "
      # install TreeData package
      githubinstall::githubinstall('VincyaneBadouard/TreeData')
      library(TreeData)

      # upload the data
       Data <- data.table::fread('{input$file0$name}', header = {input$header}, sep = '{input$cbSeparator}')

      # upload your profile (saved via shiny app)
      Profile <- readRDS(paste0(gsub('.csv', '', '{input$file0$name}'), '_Profile.rds'))

      # format your data
      DataFormated <- ParacouSubsetFormated <- RequiredFormat( Data, input = Profile)
      ")
      writeLines(text_upload, file)
    }
  )

  # Save metadata

  output$dbMetadata <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file0$name), '_Metadata.csv', sep = '')
    },
    content = function(file) {
      columns_to_save <- colnames(DataFormated())
      Metadata <- data.frame(Field = columns_to_save,
                             Description = x$Description[match(columns_to_save, x$ItemID)])

      write.csv(Metadata, file = file, row.names = F)
    }
  )

}
