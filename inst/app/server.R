
# Fichier pour gérer les interactions de l'application Shiny


# increase size limit to 10MB
options(shiny.maxRequestSize=25*1024^2)

# my function to change first letter in uppercase (e.g for updatePickerInput)
firstUpper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# my function to repeat headers at bottom
FotterWithHeader <- function(x) {
  paste0("<table>",
         tableHeader(x),
         tableFooter(x),"</table>"
  )
}

# read in csv file that has all we want to show in app
xall <- read.csv("data/interactive_items.csv")
x <- xall[xall$Activate, ]
x1 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x2 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x3 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x4 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x5 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none == "none", ]
x6 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none != "none", ]

xCorr <- read.csv("data/interactive_items_CorrerctionFunctions.csv")


CodeOptions <-  read.csv("data/CodeOptions.csv")


selector <- function(id, CodeOptions){ # --- this is to edit CODES table
  CodeOptionSplit <- split(CodeOptions, CodeOptions$OptionGroup)

  options <- HTML(paste0(unlist(lapply(1:length(CodeOptionSplit), function(I) {

    opt <- tags$optgroup(label = names(CodeOptionSplit)[I],
                         lapply( 1:nrow(CodeOptionSplit[[I]]),
                                 function(i){
                                   value <- CodeOptionSplit[[I]]$Definition[i]
                                   title <- CodeOptionSplit[[I]]$Source[i]
                                   if(i == 1L & I == 1){
                                     tags$option(value = value, title=title, selected = "selected", value)
                                   }else{
                                     tags$option(value = value, title = title, value)
                                   }
                                 } ))
    as.character(opt)
  })), collapse = ""))
  as.character(tags$select(id = id, options))
}

js <- c( # --- this is to edit CODES table
  "function(settings) {",
  "  var table = this.api().table();",
  "  var $tbl = $(table.table().node());",
  "  var id = $tbl.closest('.datatables').attr('id');",
  "  var nrows = table.rows().count();",
  "  function selectize(i) {",
  "    var $slct = $('#slct' + i);",
  "    $slct.select2({",
  "      width: '100%',",
  "      tags: true,",
  "      closeOnSelect: true",
  "    });",
  "    $slct.on('change', function(e) {",
  "      var info = [{",
  "        row: i,",
  "        col: 4,",
  "        value: $slct.val()",
  "      }];",
  "      Shiny.setInputValue(id + '_cell_selection:DT.cellInfo', info);",
  "    });",
  "  }",
  "  for(var i = 1; i <= nrows; i++) {",
  "    selectize(i);",
  "  }",
  "}"
)



# for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])

# install TreeData package
# devtools::install_github("VincyaneBadouard/TreeData")
library(TreeData)


server <- function(input, output, session) { # server ####

  output$uiUploadTables <- renderUI({

    lapply(1:input$nTable, function(i) {

      column(width = 6,
             # load button for main data file (csv format)
             box(title = paste("Table", i),
                 width = NULL,
                 fileInput(inputId = paste0("file", i), "Choose CSV File (max 25MB)", accept = ".csv"),
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
                 span(textOutput(outputId = paste0("CSVWarning", i)), style="color:red")
             )
      )


    })



  })

  output$uiViewTables <- renderUI({
    req(input$file1)
    do.call(tabsetPanel, c(id='t', type = "tabs", lapply(names(Data()), function(i) {
      tabPanel(
        title=i,
        DTOutput(outputId = i)
      )
    })))


  })
  # read file(s) ####




  # give a red text if not a csv file
  observe({
    lapply(1:input$nTable, function(i){
      file <- input[[paste0("file", i)]]
      req(file)
      ext <- tools::file_ext(file$datapath)
      if(ext != "csv") output[[paste0("CSVWarning", i)]] <-renderText( "This is not a csv file!!")
    })

  })

  Data <- reactiveVal()
  observe({
    req(input$file1)
    Data(setNames(
      lapply(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)], function(n) {

        i = which(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)] %in% n)
        file <- input[[paste0("file", i)]]
        ext <- tools::file_ext(file$datapath)

        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))

        x <- data.table::fread(file$datapath,
                               header = input[[paste0("header", i)]],
                               sep = input[[paste0("cbSeparator", i)]],
                               check.names = T)
        colnames(x) <- iconv(colnames(x), from = '', to = 'ASCII//TRANSLIT')
        return(x)
      }),
      reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)])
    )
  })

  observe({
    req(Data()[[1]])

    cnames = lapply(Data(), colnames)

    lapply(names(Data()), function(i) output[[i]] <- renderDT(Data()[[i]] , rownames = FALSE,
                                                              options = list(pageLength = 8, scrollX=TRUE),
                                                              container = FotterWithHeader(Data()[[i]]),
                                                              selection = "none")
    )
  })


  # render data table
  # output$tabData <- renderDT({
  #   if (!is.null(input$file1$name))
  #     Data()
  # }, rownames = FALSE,
  # options = list(pageLength = 8, scrollX=TRUE),
  # selection = "none")


  observeEvent(input$submitTables, {

    if(input$nTable == 1 & length(Data()) == 1) {
      updateTabItems(session, "tabs", "Tidying")
    } else {
      updateCheckboxGroupButtons(session, "TablesToStack",
                                 choices = unname(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]))

      updateTabItems(session, "tabs", "Stacking")
    }

  })

  # stack tables ####

  StackedTables <- eventReactive(input$Stack, {
    do.call(rbind, Data()[input$TablesToStack])
  })

  observeEvent(input$Stack, {
    shinyjs::hide("SkipStack")

    output$StackedTables <- renderDT(StackedTables(), rownames = FALSE,
                                     options = list(pageLength = 8, scrollX=TRUE),
                                     container = FotterWithHeader(StackedTables()),
                                     selection = "none")

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
  n_tables_after_stack <- reactiveVal()

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
      updateVirtualSelect("leftKey", choices = column_options_list[[input$leftTable]])
    })

    observeEvent(input$selectRight, {
      updateVirtualSelect( "rightKey", choices = column_options_list[[input$rightTable]])
    })


    n_tables_after_stack(length(options_to_merge))

  }, ignoreInit = T)

  observeEvent("addMerge", {


  }, ignoreInit = T)


  MergedTables <- reactiveVal()

  observeEvent(input$Merge2, {

    if(input$leftTable2 == "StackedTables") x <-  get(input$leftTable2)()
    if(input$leftTable2 == "MergedTables") x <-  get(input$leftTable2)()
    if(input$leftTable2 != "MergedTables" & input$leftTable2 != "StackedTables") x <- Data()[[input$leftTable2]]


    if(input$rightTable2 == "StackedTables") y <-  get(input$rightTable2)()
    if(input$rightTable2 == "MergedTables") y <-  get(input$rightTable2)()
    if(input$rightTable2 != "MergedTables" & input$rightTable2 != "StackedTables") y <- Data()[[input$rightTable2]]

    MergedTables(merge(x, y, by.x=input$leftKey2, by.y=input$rightKey2, all.x=TRUE, suffixes = c("", ".y")))

    shinyjs::show("GoToTidy")
    shinyjs::hide("addMerge")



  }, ignoreInit = T)




  observeEvent(input$Merge, {

    if(input$leftTable == "StackedTables") x <-  get(input$leftTable)() else x <- Data()[[input$leftTable]]
    if(input$rightTable == "StackedTables") y <-  get(input$rightTable)() else y <- Data()[[input$rightTable]]

    MergedTables(merge(x, y, by.x=input$leftKey, by.y=input$rightKey, all.x=TRUE, suffixes = c("", ".y")))

    if(n_tables_after_stack() > 2 ) {
      shinyjs::show("addMerge")
      shinyjs::show("Merge2Div")
      shinyjs::hide("GoToTidy")


      options_to_merge <- names(Data())
      column_options_list <- lapply(Data(), colnames)


      if(input$leftTable == "StackedTables" | input$rightTable == "StackedTables"){

        options_to_merge <- c(names(Data())[!names(Data()) %in% c(input$TablesToStack, input$leftTable, input$rightTable)], "MergedTables")
        column_options_list[names(Data()) %in% c(input$TablesToStack, input$leftTable, input$rightTable)] <- NULL
        column_options_list <- c(column_options_list, MergedTables = list(colnames(MergedTables())))

      } else {
        if(!is.null(input$TablesToStack)) {
          options_to_merge <- c(names(Data())[!names(Data()) %in% c(input$leftTable, input$rightTable)], "MergedTables", "StackedTables")
          column_options_list[names(Data()) %in% c(input$leftTable, input$rightTable)] <- NULL
          column_options_list <- c(column_options_list, MergedTables = list(colnames(MergedTables())), StackedTables = list(colnames(StackedTables())))
        } else {
          options_to_merge <- c(names(Data())[!names(Data()) %in% c(input$leftTable, input$rightTable)], "MergedTables")
          column_options_list[names(Data()) %in% c(input$leftTable, input$rightTable)] <- NULL
          column_options_list <- c(column_options_list, MergedTables = list(colnames(MergedTables())))
        }

      }

      updatePickerInput(session, "leftTable2", choices = options_to_merge, selected =  "")
      updatePickerInput(session, "rightTable2", choices = options_to_merge, selected =  "")

      observeEvent(input$selectLeft2, {
        updateVirtualSelect("leftKey2", choices = column_options_list[[input$leftTable2]])
      })

      observeEvent(input$selectRight2, {
        updateVirtualSelect( "rightKey2", choices = column_options_list[[input$rightTable2]])
      })


      # n_tables_after_stack(length(options_to_merge))

    }

    if(n_tables_after_stack() == 2 ) {
      shinyjs::hide("addMerge")
      shinyjs::hide("Merge2Div")
      shinyjs::show("GoToTidy")
    }

    output$mergedTablesSummary <- renderPrint(summary(MergedTables()))

    output$mergedTables <- renderDT(MergedTables(), rownames = FALSE,
                                    options = list(pageLength = 8, scrollX=TRUE),
                                    container = FotterWithHeader(MergedTables()),
                                    selection = "none")


    shinyjs::show("SelectColumns")

    updatePickerInput(session, "SelectedMergedColumns", choices =colnames(MergedTables()), selected = colnames(MergedTables())[!grepl("\\.y$", colnames(MergedTables()))])


  }, ignoreInit = T)


  # tidy tables ####
  observeEvent(input$GoToTidy | input$SkipMerge, {
    updateTabItems(session, "tabs", "Tidying")}, ignoreInit = T)

  OneTable <- eventReactive(input$submitTables | input$GoToTidy | input$SkipMerge, {

    if(input$nTable == 1 & length(Data()) == 1) {
      Data()[[1]]
    } else {
      if(!is.null(MergedTables())) MergedTables() else  StackedTables()
    }


  }, ignoreInit = TRUE)



  observe({
    groupNames <- split(names(OneTable()), cutree(hclust(stringdistmatrix(names(OneTable()))), h = 2))
    groupNames <- groupNames[sapply(groupNames, length) > 1]
    names(groupNames) <- sapply(groupNames, function(x) paste(Reduce(intersect, strsplit(x,"")), collapse=""))
    groupNames[(length(groupNames)+1):(length(groupNames)+2)] <- ""

    output$uiMelt <- renderUI({
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

    Variablecolumns <- reactiveValuesToList(input)[sort(grep("Variablecolumns\\d{1,}$", names(input), value = T))]

    ValueName <- unlist(reactiveValuesToList(input)[sort(grep("ValueName\\d{1,}$", names(input), value = T))])

    TickedMelt <- unlist(reactiveValuesToList(input)[sort(grep("TickedMelt\\d{1,}$", names(input), value = T))])

    names(Variablecolumns) <- ValueName

    TidyTable(melt(OneTable(), measure.vars	= Variablecolumns[TickedMelt], variable.name =  input$VariableName, variable.factor = FALSE)) #,  value.name = names(Variablecolumns[TickedMelt])
  }, ignoreInit = TRUE)

  observeEvent(input$SkipTidy, {
    TidyTable(OneTable())
  }, ignoreInit = TRUE)



  observeEvent(input$Tidy, {
    shinyjs::show("GoToHeaders")

    output$TidyTable <- renderDT(TidyTable(), rownames = FALSE,
                                 options = list(pageLength = 8, scrollX=TRUE),
                                 container = FotterWithHeader(TidyTable()),
                                 selection = "none")

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


  UserProfile <- reactiveVal()

  observeEvent(input$UseProfile, {

    if(input$predefinedProfile == "No") {
      file <- input$profile$datapath
      ext <- tools::file_ext(file)
    } else {
      file <- paste0("data/", input$predefinedProfile, "Profile.rds")
      ext <- tools::file_ext(file)
    }

    need(ext != "rds", "This is not a .rds file! Please upload a .rds file.")
    # if(ext != "rds") output$RDSWarning <- renderText("This is not a .rds file! Please upload a .rds file.")


    profile <- tryCatch({ readRDS(file)},
                        # warning = function(warn){
                        #   showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning', duration = NULL)
                        # },
                        error = function(err){
                          showNotification("This is not a .rds file! Please upload a .rds file.", type = 'err', duration = NULL)
                        })

    if(!is.null(profile$AllCodes)) {
      if(!profile$AllCodes[1,1] %in% "You have not selected columns for codes" & !all(profile$AllCode$Definition == ""))
      shinyjs::show("UseProfileCodes")
    }


    MissingItemIDProfile <- setdiff(x$ItemID, names(profile))
    MissingItemIDProfile <- MissingItemIDProfile[!profile[x$if_X2_isnot_none[match(MissingItemIDProfile, x$ItemID)]] %in% "none"] # this is to avoid flagging something that does not need too be filled out... but it is not be doing a good job for items other than those in x4...

    if(length(MissingItemIDProfile) > 0 & gimme_value() == 1) {
      showNotification(paste("The profile you selected is missing the following latest items:\n", paste0(MissingItemIDProfile, " (in ", x$Group[match(MissingItemIDProfile, x$ItemID)], ")",  collapse = ",\n"), ".\n Please, fill out those items by hand and double check that the info in the second column is filled out properly. Then, save your new profile."), type = 'err', duration = NULL)
    }

    ValidItemID <- names(profile)[sapply(profile, function(p) all(p %in% c(names(TidyTable()), "none"))) | grepl("Man", names(profile))] # this is to avoid the app from crashing if we have new items in x, that do not exist in data

    InValidItemID <- setdiff(names(profile), ValidItemID)
    InValidItemID <- InValidItemID[InValidItemID %in% x$ItemID]

    if(length(InValidItemID) > 0 & gimme_value() == 1) {
      showNotification(paste("The profile you selected does not seem to correspond to your data. The items that do not match your data are:", paste0(InValidItemID, " (in ", x$Group[match(InValidItemID, x$ItemID)], ")",  collapse = ",\n"), ".\n Please, fill out those items by hand (or make sure you picked the right profile). Also, please double check that the info in the second column is filled out properly."), type = 'err', duration = NULL)
    }
    #
    # for(i in which(x$ItemID %in% names(profile) & reactiveValuesToList(input)[x$ItemID] %in% names(TidyTable()))) {

    for(i in which(x$ItemID %in% ValidItemID)) {    # used to be for(i in which(x$ItemID %in% names(profile)))
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

    UserProfile(profile)
  })



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
    sort(unique(TidyTable()[[input$CommercialSp]]))
  })

  OtherNumericOptions <- reactiveVal(-999)
  OtherCharacterOptions <- reactiveVal("")

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



  # format data using the input

  DataFormated <- eventReactive(input$LaunchFormating | input$UpdateTable, {

    withCallingHandlers({
      RequiredFormat(Data = TidyTable(), isolate(reactiveValuesToList(input)), x, ThisIsShinyApp = T)
    },
    warning = function(warn){
      showNotification(paste(gsub("simpleWarning in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\), :", "", warn), collapse = ". "), type = 'warning', duration = NULL)
    },
    error = function(err){
      showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", err), type = 'err', duration = NULL)
    })



  }, ignoreInit = T)

  FormatedColumnOptions <- reactiveVal()
  observe({FormatedColumnOptions(names(DataFormated()))})
  FormatedScientificNameOptions <- reactiveVal()
  observe({FormatedScientificNameOptions(sort(unique(DataFormated()$ScientificName)))})


  observeEvent(input$LaunchFormating , {
    shinyjs::show("GoToCodes")
  }, ignoreInit = T)

  observeEvent(input$LaunchFormating , {

    lapply(which(xCorr$argument %in% "choices"), function(i) {

      eval(parse(text = paste0(paste0("update", firstUpper(xCorr$ItemType[i])), "(session,inputId = xCorr$ItemID[i],", xCorr$argument[i], ifelse(xCorr$ReactiveArgValue[i], "= get(xCorr$argValue[i])()", "= eval(str2lang(xCorr$argValue[i]))"), ifelse(xCorr$argument2[i] != FALSE, paste0(", ", xCorr$argument2[i], ifelse(xCorr$Default[i] %in% c("TRUE", "FALSE"), paste0(" = '", xCorr$Default[i], "'"), paste0(" = eval(parse(text = '",xCorr$Default[i], "'))")), ")")))))
    })

  })


  # Visualize output
  output$FormatedTable <- renderDT(DataFormated(), rownames = FALSE,
                                   options = list(pageLength = 8, scrollX=TRUE),
                                   container = FotterWithHeader(DataFormated()),
                                   selection = "none")

  output$FormatedTableSummary <- renderPrint(summary(DataFormated()))

  ## codes ####
  observeEvent(input$GoToCodes, {
    updateTabItems(session, "tabs", "Codes")
  }, ignoreInit = TRUE)

  AllCodes <- reactiveVal(data.frame(Column = "You have not selected columns for codes",
                                     Value = "You have not selected columns for codes",
                                     Definition = "You have not selected columns for codes"))



  observe({
    req(input$TreeCodes)
    AllCodes(cbind(rbindlist(apply(TidyTable()[,input$TreeCodes, with = F], 2, function(x) data.frame(Value = unique(unlist(strsplit(as.character(x), "[[:punct:]]"))))), idcol = "Column" ), Definition = ""))

  })

  observeEvent(input$UseProfileCodes, {
    dat <- AllCodes()
    m <- match(dat$Value, UserProfile()$AllCodes$Value)

    if(any(is.na(m))) showNotification(paste("WARNING: The following codes are not in your profile, you will need to fill them manually in the table:", paste(paste(dat$Value[is.na(m)], "in column", dat$Column[is.na(m)]), collapse = ", ")), type = 'err', duration = NULL)

    ExtraCodesInProfile <- setdiff(UserProfile()$AllCodes$Value, dat$Value)

    if(length(ExtraCodesInProfile)>0) showNotification(paste("WARNING: The following codes are not in your profile, but are not currently in your data. They will be ignored:", paste(paste(ExtraCodesInProfile, "in column", UserProfile()$AllCodes$Column[match(ExtraCodesInProfile, UserProfile()$AllCodes$Value)]), collapse = ", ")), type = 'err', duration = NULL)


    dat$Definition <- UserProfile()$AllCodes$Definition[m]
    AllCodes(dat)
  })


  observe({
    dat <- AllCodes()
    # AllCodes(dat)
    for(i in 1L:nrow(dat)){
      dat$DefinitionSelector[i] <-
        # selector(id = paste0("slct", i), values = CodeOptions$Definition, titles = CodeOptions$Source)
        selector(id=paste0("slct", i), CodeOptions = CodeOptions)
    }

    AllCodes(dat)

  })


  output[["CodeTable"]] <- renderDT({
    datatable(
      data =
        AllCodes(),
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      container = FotterWithHeader(AllCodes()),
      options = list(
        paging = F,
        searching = F,
        initComplete = JS(js),
        preDrawCallback = JS(
          "function() { Shiny.unbindAll(this.api().table().node()); }"
        ),
        drawCallback = JS(
          "function() { Shiny.bindAll(this.api().table().node()); }"
        )
      )
    )
  }, server = TRUE)

  observeEvent(input[["CodeTable_cell_selection"]], {
    info <- input[["CodeTable_cell_selection"]]
    dToEdit <- AllCodes()
    dToEdit[info$row,info$col-1] <- info$value # have to add the +1 because for some reason the indexing starts at 0 (probably because of the rbindlist function)
    AllCodes(dToEdit)
  })


  output[["NewCodeTable"]] <- renderTable({
    AllCodes()[, c(1:3)]
  })

  # Correct ####

  observeEvent(input$GoToCorrect, {
    updateTabItems(session, "tabs", "Correct")
  }, ignoreInit = TRUE)


  observeEvent(input$GoToOutput, {
    updateTabItems(session, "tabs", "OutputFormat")

  })
  observeEvent(input$GoToDownload, {
    updateTabItems(session, "tabs", "Save")


  }, ignoreInit = T)



  # apply corrections

  # show corrections arguments or not

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

  # place holder to put either corrected data or non corrected data
  DataDone <- reactiveVal()

  #decide what DataDone is going to be

  observeEvent(input$SkipCorrections,
               {
                 DataDone(DataFormated())
                 updateTabItems(session, "tabs", "OutputFormat")

               })

  observeEvent(input$ApplyCorrections,{
    shinyjs::show("GoToOutput")
    DataDone(DataCorrected())
  })

  DataCorrected <- eventReactive(input$ApplyCorrections, {
    Rslt <- DataFormated()
    lapply(
      unique(xCorr$Function),
      FUN = function(f){
        if(input[[f]] %in% "Yes") {
          # cl <- str2lang(paste0(f, "(", paste("Data = Rslt,", paste(paste(gsub(f, "", xCorr$ItemID[xCorr$Function %in% f]), "=",reactiveValuesToList(input)[xCorr$ItemID[xCorr$Function %in% f]]), collapse = ", ")),")"))
          cl <- paste0(f, "(", paste("Data = Rslt,", gsub("list\\(", "", paste(deparse(set_names(reactiveValuesToList(input)[xCorr$ItemID[xCorr$Function %in% f]], gsub(f, "", xCorr$ItemID[xCorr$Function %in% f]))), collapse = ""))))
          cl <- gsub('"FALSE"', "FALSE", cl)
          cl <- gsub('"TRUE"', "TRUE", cl)
          cl <- gsub('\"function', "function", cl)
          cl <- gsub(')\"', ")", cl)
          cl <- gsub(' (\\d*)L', " \\1", cl)

          cl <- str2lang(str2lang(deparse(cl)))

          Rslt <<- eval(cl)


          # tryCatch({
          #   eval(cl)
          # },
          # # warning = function(warn){
          # #   showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning', duration = NULL)
          # # },
          # error = function(err){
          #   showNotification(err, type = 'err', duration = NULL)
          # })
        }
      }
    )
    Rslt
  })

  output$CorrectedTable <- renderDT(DataCorrected(), rownames = FALSE,
                                    options = list(pageLength = 8, scrollX=TRUE),
                                    container = FotterWithHeader(DataCorrected()),
                                    selection = "none")

  output$CorrectedTableSummary <- renderPrint(summary(DataCorrected()))



  # save final data table

  DataOutput <- reactiveVal(NULL)
  profileOutput <- reactiveVal(NULL)
  CodeTranslationFinal <- reactiveValues(dt = NULL, output = NULL)

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


    profileOutput <- tryCatch({ readRDS(file)},
                              # warning = function(warn){
                              #   showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning', duration = NULL)
                              # },
                              error = function(err){
                                showNotification("This is not a .rds file! Please upload a .rds file.", type = 'err', duration = NULL)
                              })



    DataOutput(ReversedRequiredFormat(DataDone(), profileOutput, x, ThisIsShinyApp = T))

    if(!is.null(profileOutput$AllCodes) & !AllCodes()[1,1] %in% "You have not selected columns for codes") {
      shinyjs::show("CodeTranslationsDiv")
      output$uiCodeTranslations <-  renderUI({
        div(DTOutput("CodeTranslationTable"),
            br(),
            br(),
            DTOutput("CodeTranslationFinal"),
            actionBttn(
              inputId = "ApplyCodeTranslation",
              label = "Apply Code Translation",
              style = "float",
              size = "sm",
              color = "success"
            )
        )
        })

      AllCodesInput <- AllCodes()
      AllCodesOutput <- profileOutput$AllCodes

      AllCodesInput$Value[is.na(AllCodesInput$Value)] <- "NA"

      CodeTranslationTable <- matrix(paste(AllCodesOutput$Column, AllCodesOutput$Value, sep = "_"), ncol = nrow(AllCodesOutput),
             nrow = nrow(AllCodesInput), dimnames = list(AllCodesInput$Value, AllCodesOutput$Value), byrow = T)


      for (i in seq_len(nrow(CodeTranslationTable))) {
        for(j in seq_len(ncol(CodeTranslationTable))) {
       if(AllCodesInput$Definition[i] %in% AllCodesOutput$Definition[j]) CodeTranslationTable[i, j] = sprintf(
          '<input type="radio" name="%s_%s" value="%s" checked="checked"/>',
          AllCodesInput$Column[i], AllCodesInput$Value[i], CodeTranslationTable[i,j]) else CodeTranslationTable[i, j] = sprintf(
            '<input type="radio" name="%s_%s" value="%s"/>',
            AllCodesInput$Column[i],  AllCodesInput$Value[i], CodeTranslationTable[i,j])
        }
      }

      sketch = HTML(paste0("<table><thead><tr><th colspan = 2></th>", paste(paste0("<th colspan =", table(AllCodesOutput$Column)[unique(AllCodesOutput$Column)], " style='text-align:left'>",unique(AllCodesOutput$Column), "</th>"), collapse = ""), "</tr><tr><th></th><th></th>",paste(paste0("<th style= font-weight:400>", colnames(CodeTranslationTable), "</th>"), collapse = ""), "</tr></thead><tfoot><tr><th></th><th></th>",paste(paste0("<th style= font-weight:400>", colnames(CodeTranslationTable), "</th>"), collapse = ""), "</tr></tfoot></table>"))



      output$CodeTranslationTable <- renderDT(
        datatable(
          data = cbind(AllCodesInput$Column, CodeTranslationTable),
          selection = 'none',
          escape = FALSE,
          extensions = c('RowGroup', 'FixedColumns'),
          options = list(dom = 't', paging = FALSE, ordering = FALSE, scrollX=TRUE,
                         rowGroup = list(dataSrc=c(1)),
                         columnDefs = list(list(visible=FALSE, targets=c(1))),
                         fixedColumns = list(leftColumns = 1)),
       container = sketch,
        callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[1]+'_'+this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
        ),
       server = FALSE)

      # output$CodeTranslationTable <- renderHtmlTableWidget(
      #   htmlTableWidget(
      #       addHtmlTableStyle( CodeTranslationTable,
      #                          css.rgroup = "text-align:left; font-weight:900",
      #                          css.cgroup = "text-align:left; font-weight:900",
      #                          css.header	= "text-align:center; font-weight:400"
      #       ),
      #       rgroup = unique(AllCodesInput$Column),
      #       n.rgroup = c(table(AllCodesInput$Column)[unique(AllCodesInput$Column)]),
      #       cgroup = unique(AllCodesOutput$Column),
      #       n.cgroup =table(AllCodesOutput$Column)[unique(AllCodesOutput$Column)],
      #     number_of_entries = nrow(CodeTranslationTable) + length(unique(AllCodesInput$Column)))
      #   )


      # output$CodeTranslationTable <- renderDT(profileOutput$AllCodes)
    }

    profileOutput(profileOutput)
    # CodeTranslationTable(CodeTranslationTable)
    CodeTranslationFinal$dt <- data.frame(InputColumn = AllCodesInput$Column,
                                          InputValue = AllCodesInput$Value,
                                          InputDefinition = AllCodesInput$Definition)

  })



  # CodeTranslationFinal <- reactiveVal(data.frame(input = rownames(CodeTranslationTable), output = colnames(CodeTranslationTable)))

  observe({
    req(CodeTranslationFinal$dt$InputValue)
    # req(input$codes_MAIN)
    dt <- CodeTranslationFinal$dt
    dt$OutputValue <- sapply(paste(dt$InputColumn, dt$InputValue, sep = "_"), function(x) input[[x]])
    dt$OutputColumn <- profileOutput()$AllCodes$Column[match(dt$OutputValue, paste(profileOutput()$AllCodes$Column, profileOutput()$AllCodes$Value, sep = "_"))]
    dt$OutputDefinition <- profileOutput()$AllCodes$Definition[match(dt$OutputValue, paste(profileOutput()$AllCodes$Column, profileOutput()$AllCodes$Value, sep = "_"))]
    dt$OutputValue <-  lapply(dt$OutputValue, function(x) if(!is.null(x)) strsplit(x,"_")[[1]][[2]] else NULL)# remove the column part in the value
    CodeTranslationFinal$output <- dt
  })

  output$CodeTranslationFinal <- renderDT({
    req(CodeTranslationFinal$output)
    datatable(CodeTranslationFinal$output[c("InputColumn", "InputValue", "OutputColumn", "OutputValue", "InputDefinition", "OutputDefinition")], options = list( paging = FALSE, scrollX=TRUE))
  })


  observeEvent(input$ApplyCodeTranslation, {
    DataDone <- DataDone()

    idx <- which(names(DataDone) %in% paste0("Original_", input$TreeCodes))

    CodesInput <- DataDone[,..idx]
    # names(CodesInput) <- gsub("Original_", "", names(CodesInput))

    for(j in names(CodesInput)) {

      CodeTranslation <- CodeTranslationFinal$output[CodeTranslationFinal$output$InputColumn %in% gsub("Original_", "", j), ]
      CodeTranslation <- CodeTranslation[!is.na(CodeTranslation$OutputColumn),]

      CodesInput[,OriginalTranslated := CodesInput[,..j]]

      for(i in seq_len(nrow(CodeTranslation))) {

        CodesInput[,OriginalTranslated:=gsub(paste0("\\<", CodeTranslation$InputValue[i], "\\>"), paste(CodeTranslation$OutputColumn[i], CodeTranslation$OutputValue[i], sep = "_"), OriginalTranslated)]

      }

      CodesInput[,paste0(j, "_Translated") := OriginalTranslated]
      CodesInput[, OriginalTranslated:=NULL]

    }

    CodesInput[, Translation:=do.call(paste, c(.SD, sep = ";")), .SDcols=-seq_along(idx)]
    CodesInput[, grep("_Translated", colnames(CodesInput)):=NULL]

    CodesInput[, Translation:=gsub("\\<[A-Z]*\\>[[:punct:]]", "", Translation)] # remove codes that don't have an equivalence
    CodesInput[, Translation:=gsub("\\b(\\w+)\\b\\s*\\W\\s*(?=.*\\1)", "", Translation, perl = T)] # remove duplicated -  this deals with n-1 relationship (if different input refer to the same output code )

    OutCols <- unique(na.omit(CodeTranslationFinal$output$OutputColumn))

    for(j in OutCols) {
      CodesInput[,Final:=gsub(paste0(j, "_|\\<(\\w*?_\\w*?)\\>"), "", Translation)]

      colnames(CodesInput) <- gsub("Final", j, colnames(CodesInput))
      CodesInput[,Final := NULL]
    }




    DataOutput(cbind(DataDone, CodesInput[, ..OutCols]))
  })


  observeEvent(input$DontUseProfileOuput, {
    shinyjs::hide("DontUseProfileOuput")

    DataOutput(DataDone())
  })

  observeEvent(input$DontUseProfileOuput | input$UseProfileOutput, {
    shinyjs::show("GoToDownload")

  }, ignoreInit = T)


  # Visualize output
  output$DataOutput <- renderDT(DataOutput(), rownames = FALSE,
                                options = list(pageLength = 8, scrollX=TRUE),
                                container = FotterWithHeader(DataOutput()),
                                selection = "none")

  output$DataOutputSummary <- renderPrint(summary(DataOutput()))


  output$dbFile <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_formated.csv', sep = '')
    },
    content = function(file) {
      write.csv(DataOutput(), file, row.names = FALSE)
    }
  )

  # save profile Rdata file

  output$dbProfile <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_Profile.rds', sep = '')
    },
    content = function(file) {
      inputs_to_save <- names(input) # c(names(input)[names(input) %in% x$ItemID], "Tidy", "VariableName", grep("Variablecolumns|TickedMelt|ValueName", names(input), value = T))
      Profile <- list()
      for(input.i in inputs_to_save){
        Profile[[input.i]] <-  input[[input.i]]
      }
      Profile[["AllCodes"]] <- AllCodes()
      Profile[["CodeTranslationFinal"]] <- CodeTranslationFinal$output
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
       Data <- data.table::fread('{input$file1$name}', header = {input$header}, sep = '{input$cbSeparator}', check.names = T, encoding = 'UTF-8')

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

      YourInputColumn <- reactiveValuesToList(input)[xall$ItemID[match(colnames(DataDone()), xall$ItemID)]]
      OurStandardColumn <- colnames(DataDone())

      if(!is.null(profileOutput())) {
        m <- match(OurStandardColumn, xall$ItemID)
        OutputColumn <-  profileOutput()[xall$ItemID[m]]
        OutputColumn[which(is.na(names(OutputColumn)))] <- xall$ItemID[m[which(is.na(names(OutputColumn)))]]
        Description = paste0(xall$Description[m], ifelse(!xall$Unit[m] %in% c("-", "year"), paste(" in", profileOutput()[paste0(gsub("^X|^Y", "", xall$ItemID[m]),"UnitMan")]), ""))

      } else {
        OutputColumn <- OurStandardColumn
        Description = paste0(xall$Description[match(OurStandardColumn, xall$ItemID)], ifelse(!xall$Unit[match(OurStandardColumn, xall$ItemID)] %in% c("-", "year"), paste(" in", xall$Unit[match(OurStandardColumn, xall$ItemID)]), ""))
      }

      YourInputColumn[is.na(names(YourInputColumn))|YourInputColumn%in%"none"] <- NA
      names(YourInputColumn)[is.na(names(YourInputColumn))] <- "NA"
      # OutputColumn[is.na(names(OutputColumn))|OutputColumn%in%"none"] <- NA
      OutputColumn[OutputColumn%in%"none"] <- NA


      Metadata <- data.frame(YourInputColumn = unlist(YourInputColumn),
                             OurStandardColumn,
                             OutputColumn = unlist(OutputColumn),
                             Description = Description)

      #remove lines with for columns that are msising in input and output
      Metadata <- Metadata[!(is.na(Metadata$YourInputColumn) & is.na(Metadata$OutputColumn)),]

      write.csv(Metadata, file = file, row.names = F)
    }
  )

  # Help stuff
  output$AppGeneralWorkflow <- renderImage(
    list(src = "www/AppGeneralWorkflow.png",
         contentType = "image/png",
         alt = "test",
         width = "100%",
         align = "center"),
    deleteFile = F)


}
