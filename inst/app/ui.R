#list of packages required
library(shinydashboard)
library(bslib)
library(DT)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(data.tree)
library(stringr)
library(stringdist)
library(data.table)
library(TreeData)
library(shinycssloaders)

# read in csv file that has all we want to ask about the headers
x <- read.csv("data/interactive_items.csv")
x <- x[x$Activate, ]
x1 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x2 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none == "none", ]
x3 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x4 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none == "none" & x$if_X2_isnot_none != "none", ]
x5 <- x[x$if_X1_is_none != "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none == "none", ]
x6 <- x[x$if_X1_is_none == "none" & x$if_X2_is_none != "none" & x$if_X2_isnot_none != "none", ]

if(!all(unlist(sapply(list(x1, x2, x3, x4, x5, x6), "[[", "ItemID")) %in% x$ItemID)) stop ("not all interactive items are implemented in the app")



xCorr <- read.csv("data/interactive_items_CorrerctionFunctions.csv")


# header with title
header <- dashboardHeader(title = "Data harmonisation")

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id = "tabs", # see here for icons https://fontawesome.com/v5/search
              menuItem("Upload your file(s)", tabName = "Upload", icon = icon("upload")),
              menuItem("Stack tables", tabName = "Stacking", icon = icon("layer-group")),
              menuItem("Merge tables", tabName = "Merging", icon = icon("key")),
              menuItem("Tidy table", tabName = "Tidying", icon = icon("check")),
              menuItem("Headers and Units", tabName = "Headers", icon = icon("arrows-alt")),
              menuItem("Codes", tabName = "Codes", icon = icon("table", verify_fa = F)),
              menuItem("Apply corrections", tabName = "Correct", icon = icon("check-circle")),
              # menuItem("Visualise results", tabName="Visualise", icon = icon("eye")),
              menuItem("Download formatted data", tabName="Save", icon = icon("save")),
              menuItem("Help", tabName = "Help", icon = icon("book"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(25%);
             @import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);
      }


             .dropdown-menu{z-index:10000 !important;}
             .sw-dropdown-content {z-index: 3000005 !important;}
             .sw-dropdown-in {z-index: 3000006 !important;}
             .vscomp-search-container {z-index: 3000005 !important;}
             .vscomp-dropbox-container {z-index: 3000005 !important;}

           "

      )
    ) # to make notification show up at top of page
  ),
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(

              actionBttn(
                inputId = "inactivebutton1",
                label = div(
                  strong("If your connexion is slow and/or your data is very large, you may want to run this app locally. For that, open R Studio and type:"),
                  br()),
                style = "stretch",
                color = "success"),
              box(width = 12,
                  # helpText("Some text and then ", code("some code"), "."),
                  helpText(code('shiny::runGitHub( "VincyaneBadouard/TreeData", subdir = "inst/app")'),
                           br(),
                           br(),
                           '# If you have run this app in the past and you think/know the TreeData package has been updated since, you may need to restart you R session and re-install TreeData package (using code below) before running the app again',
                           br(),
                           code('devtools::install_github("VincyaneBadouard/TreeData", build_vignettes = TRUE)')


                           ),
                  # textOutput("CodeRunApp"),
                  tags$head(tags$style("#CodeRunApp{
                  color: red;
                  font-family: courier;
                  font-size: 100%;
                                 }"
                  ))),
              br(),
              br(),
              box(width = 12,

                 dropdownButton(width = NULL,

                   prettyCheckbox(
                     inputId = "inactivechck1",
                     label = "Tables that will need to be stacked have the exact same column, in same order and with same names.",
                     value = TRUE,
                     status = "warning"
                   ),
                   prettyCheckbox(
                     inputId = "inactivechck2",
                     label = "The key columns of tables that will be merged have information that is correctly spelled and capitalized.",
                     value = TRUE,
                     status = "warning"
                   ),
                   prettyCheckbox(
                     inputId = "inactivechck3",
                     label = "...",
                     value = TRUE,
                     status = "warning"
                   ),

                circle = TRUE, status = "danger",
                label  = tags$h2("Checklist before you upload"),
                icon = icon("cog"),
                inline =T,
                tooltip = tooltipOptions(title = "Click to see checklist !")),
                span("Checklist before you upload")),
              br(),
              br(),
              column(width = 8,
                     actionBttn(
                       inputId = "inactivebutton2",
                       label = "1",
                       style = "pill",
                       color = "warning"),
                     strong("How many tables do you wish to upload?"),
                     h4("Select this before you move to step 2."),
                     h4("Changing this value after uploading a table will clear the upload."),
                     numericInput(inputId = "nTable",
                                  label = "",
                                  value = 1,
                                  min = 1,
                                  max = NA
                     )
              )),

            fluidRow(
              column(width = 6,
                     actionBttn(
                       inputId = "inactivebutton3",
                       label = "2",
                       style = "pill",
                       color = "warning"),
                     strong("Upload your tables"),

                     uiOutput("uiUploadTables"),

                     actionBttn(
                       inputId = "inactivebutton4",
                       label = "3",
                       style = "pill",
                       color = "warning"),
                     actionBttn(
                       inputId = "submitTables",
                       label = "submit",
                       style = "material-flat",
                       color = "success"
                     )),
              column(6,
                     uiOutput("uiViewTables"))

            )

    ),  ## end of "upload" panel


      tabItem(tabName = "Stacking",

              fluidRow(
                # column(width = 12,
                       actionBttn(
                         inputId = "inactivebutton5",
                         label = " ! ",
                         style = "pill",
                         color = "danger"),
                       strong("make sure you clicked on 'Submit' in Upload tab")
                       # )
              ),
                fluidRow(
                column(width = 12,

                       h1("Stacking tables"),
                       h3("Select the tables that have the same set of columns and can be stacked on top of each other (e.g. one table per census, or one table per plot etc...)"),
                      code("If you have no tables to stack, skip this step."),
                      checkboxGroupButtons("TablesToStack", choices = ""),
                      actionBttn(
                        inputId = "Stack",
                        label = "Stack tables",
                        style = "material-flat",
                        color = "success"
                      ),
                      actionBttn(
                        inputId = "SkipStack",
                        label = "Skip this step",
                        style = "material-flat",
                        color = "warning"
                      ),
                      # insertUI("#Stack", "afterEnd",
                     hidden( actionBttn(
                        inputId = "GoToMerge",
                        label = "Go To Merge",
                        style = "material-flat",
                        color = "success"
                      ),
                      actionBttn(
                        inputId = "SkipMerge",
                        label = "Skip Merging since all your data is now stacked",
                        style = "material-flat",
                        color = "warning"
                      ))
                      #)
                )
                ),
              fluidRow(

                column(width = 12,
                       h4("summary of your stacked tables:"),
                       verbatimTextOutput("StackedTablesSummary"),
                       h4("View of your stacked tables:"),
                       DTOutput(outputId = "StackedTables")
                )
                # ,
                # actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
              )


      ),  ## end of "Stacking" panel
    tabItem(tabName = "Merging",

            fluidRow(
              # column(width = 12,
              actionBttn(
                inputId = "inactivebutton6",
                label = " ! ",
                style = "pill",
                color = "danger"),
              strong("make sure you clicked on 'Sumbit' in Upload tab (and `Stack tables` in Stack tab, if used) ")
              # )
            ),
            fluidRow(
              column(width = 12,

                     h1("Merging tables"),
                     h4("Select the tables that need to be merged and the key to merge them."),
                     h4("The table on the left should be the most exhaustive table (the one you want to keep all the rows from.)"),
                     # actionButton("addMerge", "Add a Merging relationship"),
                     # uiOutput("MergeTablesUI"),
                     # textOutput("test"),
                     # verbatimTextOutput("test2"),
                     # checkboxGroupButtons("TablesToMerge", choices = ""),

                     box(width = 12,

                         fluidRow(column(3, pickerInput("leftTable", "Merge this table", choices = "")),
                                  column(1, br(),actionBttn("selectLeft", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  virtualSelectInput("leftKey", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),

                         fluidRow(column(3, pickerInput("rightTable", "And this table", choices = "")),
                                  column(1, br(),actionBttn("selectRight", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  virtualSelectInput("rightKey", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),


                     #   hidden(div(id = "SelectColumns",
                     #       box(width = 12,
                     #           # fluidRow(
                     #
                     #             pickerInput("SelectedMergedColumns", div("Select only the columns you want to keep moving forward", br(), em("By default (recommended), columns that are repeats in your second table are unselected.")), choices = "", multiple = T)
                     #       ))
                     #
                     # ),

                       actionBttn(
                       inputId = "Merge",
                       label = "Merge tables",
                       style = "material-flat",
                       color = "success")
                     ),
                     fluidRow(
                       hidden(actionBttn(inputId = "addMerge",  label =  span(icon("plus"), em("Add a Merging relationship", strong("(You need to end up with only one table)"))),
                                     style = "material-flat",
                                     color = "danger")),
                     ),
                     hidden(div(id ="Merge2Div", box(width = 12,

                         fluidRow(column(3, pickerInput("leftTable2", "Merge this table", choices = "")),
                                  column(1, br(),actionBttn("selectLeft2", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  virtualSelectInput("leftKey2", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),

                         fluidRow(column(3, pickerInput("rightTable2", "And this table", choices = "")),
                                  column(1, br(),actionBttn("selectRight2", "", icon = icon("arrow-right"), size = "sm")),
                                  column(8,  virtualSelectInput("rightKey2", div("Using this/these KEY column(s)", br(), em("if you need multiple columns for the merge, the order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6))),
                         actionBttn(
                           inputId = "Merge2",
                           label = "Merge tables",
                           style = "material-flat",
                           color = "success"
                         )
                     ))),

                     hidden( actionBttn(
                       inputId = "GoToTidy",
                       label = "Go To Tidy",
                       style = "material-flat",
                       color = "success"
                     ))
              )
            ),
            fluidRow(

              column(width = 12,
                     h4("summary of your merged tables:"),
                     verbatimTextOutput("mergedTablesSummary"),
                     h4("View of your stacked tables:"),
                     DTOutput(outputId = "mergedTables")
              )
            #   # ,
            #   # actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
            )


    ),  ## end of "Merging" panel

    tabItem(tabName = "Tidying",
            h3("This is where we want to make your data 'tidy'"),
            p("This means that we want one row per observation. An observation is one measurement (of one stem, at one census, and one height)."),
            p("If you have stored several measurements on a same row (for example, you have several DBH columns, one for each census), we need to tidy your data..."),
            p("This is called wide-to-long reshaping. If you already have one observation per row, you can skip this step"),
            actionBttn(
              inputId = "SkipTidy",
              label = "Skip this step",
              style = "material-flat",
              color = "warning"
            ),
            box(width = 12,
                radioButtons(
              "VariableName",
              "Why do you have repeated column?",
              choices = c("One column per census" = "CensusID", "One column per height of measurement, measurement method, ..." = "MeasureID", "One column per stem" = "StemID", "One column per year" = "Year"),
              selected = "",
              inline = FALSE
            ),
            actionButton("ClearValueName","Clear")),
            br()
,            h3("Tick the grouping(s) that should be applied and fix the prefilled information if necessary."),

            uiOutput("uiMelt"),

            # box(
            # textInput("ValueName", "What type of measurement is repeated horizontally? (Give a column name without space)", value = "DBH"),
            # radioButtons(
            #   "VariableName",
            #   "What is the meaning of the repeated column?",
            #   choices = c("CensusID", "Year", "POM", "StemID"),
            #   selected = "",
            #   inline = FALSE
            # ),
            # actionButton("ClearValueName","Clear"),
            # pickerInput("Variablecolumns", label = "Select the columns that are repeats of measurements", choices = "", multiple = T, options = list(size = 10)),
            # ),
            actionBttn(
              inputId = "Tidy",
              label = "Tidy",
              style = "material-flat",
              color = "success"
            ),
            hidden( actionBttn(
              inputId = "GoToHeaders",
              label = "Go To Headers",
              style = "material-flat",
              color = "success"
            )),



            fluidRow(

              column(width = 12,
                     h4("summary of your tidy table:"),
                     verbatimTextOutput("TidyTableSummary"),
                     h4("View of your tidy table:"),
                     DTOutput(outputId = "TidyTable")
              ))


            ), ## end of "Tidy" panel
    tabItem(tabName = "Headers",
            fluidRow(
              # inform if profile already exists
              box(width = 12,
                  radioButtons(inputId = "predefinedProfile",
                               label = div("Use a predifined format?", br(), em("(if your data follows one of the following network template)")),
                               choices = list("No thanks!" = "No",
                                              # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
                                              "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGEO",
                                              "App's profile (if the data you upload was downloaded from this app, using this app's standards)" = "App"#,
                                              # "RBA: Red de Bosques Andinos" = "RBA"
                               ),
                               selected = "No"),

                  # load a profile it one already exists
                  fileInput(inputId = "profile", div("You may also load your own profile", br(), em("(if you already used this app and saved your profile (.rds))")), accept = ".rds"),
                  hidden(actionBttn(
                    inputId = "UseProfile",
                    label = "Click Twice here to use Profile",
                    style = "pill",
                    color = "success")
                  )),
              hidden(div( id = "AttentionDates",
                          box(width = 12,
                              actionBttn(
                                inputId = "inactivebutton7",
                                label = "!",
                                style = "pill",
                                color = "danger"),
                              strong("pay attention to your Date format and double check it in step 2, even if you imported a profile."),
                              p("A sample or your dates look like this:"),
                              textOutput("sampleDates")))),

              column(width = 6,
                     actionBttn(
                       inputId = "inactivebutton8",
                       label = "1",
                       style = "pill",
                       color = "warning"),
                     strong("  Match your columns to ours (when you can)"),
                     br(),
                     br(),
                     box(
                       # title = "Match your columns to ours (if you can)",
                         width = NULL,
                         # status = "primary",
                         # solidHeader = TRUE,
                         # uiOutput("ui1"),
                         div(id="mainWrapper",

                        lapply(unique(x1$Group), function(g) {div(h3(g),
                          dropdown(
                            h3(g),
                            do.call(div, lapply(which(x1$Group %in% g), function(i) {

                              eval(parse(text = paste0(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$argument[i]," ='",  x1$Default[i],"'", ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                            })),
                            label = g,
                            icon = icon("sliders", verify_fa = FALSE),
                            size = "lg",
                            circle = FALSE,
                            tooltip = tooltipOptions(title = "Click to see inputs !")

                         )
                        )
                         })
                         )

                           # lapply(1:nrow(x1), function(i) {
                           #
                           #   eval(parse(text = paste0(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$argument[i]," ='",  x1$Default[i],"'", ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                           #
                           # })

                         # actionBttn("Header1Next", "next", style = "fill", color = "primary")
                         )
              ),
              column(width = 6,

                     div(
                       actionBttn(
                         inputId = "inactivebutton9",
                         label = "2",
                         style = "pill",
                         color = "warning")
                       ,   strong("  Fill in information that is not in your columns"),
                       p("ATTENTION: do this after completing step 1 otherwise it will turn blank again."),
                       lapply(which(x$ItemID %in% unlist(lapply(list(x2, x3, x4, x5, x6), "[[", "ItemID"))), function(i) {

                         eval(parse(text = paste0(x$ItemType[i], "(inputId = x$ItemID[i], label = ifelse(x$helpText[i] %in% '', x$Label[i], paste0(x$Label[i], ' (', x$helpText[i], ')')),", x$argument[i], "='", x$Default[i], "'", ifelse(x$Options[i] != FALSE, paste0(", options = ", x$Options[i]), ""), ifelse(x$Multiple[i] %in% TRUE, paste0(", multiple = TRUE, selected = '", x$Default[i], "')"), ")"))))

                       }),
                       actionBttn("LaunchFormating", label = "Apply changes!", style = "material-flat", color = "success") #style = "color: #fff; background-color: #009e60; border-color: #317256")
                     ),
                     box(title = "Save your profile",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbProfile", label = "Save profile")),
                     hidden( actionBttn(
                       inputId = "GoToCorrect",
                       label = "Go To Correct",
                       style = "material-flat",
                       color = "success"
                     ))),

                     fluidRow(

                       column(width = 12,
                              h4("summary of your formatted table:"),
                              verbatimTextOutput("FormatedTableSummary"),
                              h4("View of your formatted table:"),
                              DTOutput(outputId = "FormatedTable")
                       ))


    )),
tabItem("Codes",
        h3("This is where we are going to try to understand the tree codes you have..."),
        strong("This is not functional yet, you can skip this step for now... (click on 'Apply Corrections' on the left pannel"),
        uiOutput("uiCodes"),
        DTOutput("CodeTable")),

    tabItem(tabName = "Correct",

            lapply(unique(xCorr$Function), function(f) {
              box(
                title = f,
                radioButtons(inputId = f, label = paste("Apply", f, "?"), choices = list("Yes" = "Yes", "No" = "No"), selected = "No"),
                hidden(div(id = paste0(f, "Yes"),

                lapply(which(xCorr$Function %in%f), function(i) {
                  eval(parse(text = paste0(xCorr$ItemType[i], "(inputId = xCorr$ItemID[i], label = ifelse(xCorr$helpText[i] %in% '', xCorr$Label[i], paste0(xCorr$Label[i], ' (', xCorr$helpText[i], ')')),", xCorr$argument[i], " = eval(parse(text = '", xCorr$Default[i], "'))", ifelse(xCorr$argument2[i] != FALSE, paste0(", ", xCorr$argument2[i], " = eval(parse(text = '",xCorr$Default[i], "'))"), ""), ifelse(xCorr$Options[i] != FALSE, paste0(", options = ", xCorr$Options[i]), ""), ifelse(xCorr$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                })
              )
              ))
            }),
            # radioButtons(inputId = "taper", label = "Apply taper corrections? (NOT IMPLEMENTED YET", choices = list("Yes" = "Yes", "No" = "No"), selected = "No"),
            hidden(actionBttn(
              inputId = "ApplyCorrections",
              label = "Apply Corrections",
              style = "material-flat",
              color = "success"
            )),
            actionBttn(
              inputId = "SkipCorrections",
              label = "Skip Corrections",
              style = "material-flat",
              color = "warning"
            ),
            hidden(actionBttn(
              inputId = "GoToDownload",
              label = "Go To Download",
              style = "material-flat",
              color = "success"
            )),
            fluidRow(

              column(width = 12,
                     h4("summary of your corrected table:"),
                     withSpinner(verbatimTextOutput("CorrectedTableSummary"), color="#0dc5c1", id = "spinner"),
                     h4("View of your corrected table:"),
                     withSpinner(DTOutput(outputId = "CorrectedTable"), color="#0dc5c1", id = "spinner")
              ))
            ),

    tabItem(tabName = "Save",

            fluidRow(box(width = 12,
                         radioButtons(inputId = "predefinedProfileOutput",
                                      label = div("Use a predifined format for your output?"),
                                      choices = list("No thanks! I want my data in this app's standards" = "No",
                                                     # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
                                                     "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGEO"#,
                                                     # "RBA: Red de Bosques Andinos" = "RBA"
                                      ),
                                      selected = "No"),

                         # load a profile it one already exists
                         fileInput(inputId = "profileOutput", div("You may also load a profile you have on your machine", br(), em("(if you or a colleague already used this app and saved a profile (.rds))")), accept = ".rds"),
                         hidden(actionBttn(
                           inputId = "UseProfileOuput",
                           label = "Apply Profile",
                           style = "pill",
                           color = "success")),
                         actionBttn(
                           inputId = "DontUseProfileOuput",
                           label = "Don't use profile",
                           style = "pill",
                           color = "success")

            )
            ),
            fluidRow(

              column(width = 12,
                     h4("summary of your final table:"),
                     withSpinner(verbatimTextOutput("DataOutputSummary"),color="#0dc5c1", id = "spinner"),
                     h4("View of your final table:"),
                     withSpinner(DTOutput(outputId = "DataOutput"),color="#0dc5c1", id = "spinner")
              )),
            fluidRow(
              column(width = 4,
                     box(title = "Save file",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbFile", label = "Save file")),
                     # box(title = "Save code",
                     #     width = NULL,
                     #     status = "primary",
                     #     solidHeader = TRUE,
                     #     downloadButton(outputId = "dbCode", label = "Save code")),

                     # p("ATTENTION:, LifeStatus and CommercialSp were not converted to your desired output profile because we cannot interprete TRU/FALSE to your desired profile's code system!"),
                     box(title = "Save metadata",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbMetadata", label = "Save metadata")
                         )
              )
            )
    ), # end of "save" panel
    tabItem(tabName = "Help",
            tabsetPanel(
              tabPanel(title = "General",
                       imageOutput("AppGeneralWorkflow")
                     # img(src = "AppGeneralWorkflow.PNG", width = "100%")
                     ),
              tabPanel(title = "Upload",
                       p("This is an example where we upload two tables, one for tree data and one for plot data."),
                       p("You can give your tables a name so they re easily recognisable in the next steps."),
                       img(src = "Upload.gif", width = "100%")),
              tabPanel(title = "Stack",
                       p("You can stack multiple tables on top of eachother, if they have the" , strong("same set of colums"), "(with exact same names) but represent measurements for different sets of trees (e.g. at different sites), or maybe different censuses."),
                       p("In this example, we uploaded four tables. Three of them are individual census tables, with the same set of colums (Tables", code("Census1"),",",code("Census2"), ", and ", code('Census3'), "). The fourth table,", code('SpeciesTable'), ", is different, and we will merge its information at the next step."),
                       p("To stack our three tables, we select each of them and click 'Stack Tables'. The new 'unified' table appears at the bottom of the page."),
                       img(src = "Stack.gif", width = "100%")),
              tabPanel(title = "Merge",
                       p("At this stage, you should only have two tables. They can be the two tables you uploaded (if you skipped the stacking process), or they can be your stacked table and an extra table that as information you need to merge in. The latter case is our situation here."),
                       p("In this example, we have our stacked tables (", code("StackedTables"), "we stacked data from 3 censuses at the step before), and our species table", code("SpeciesTable"), ", which has information that we want to merge in our inventory data."),
                         p("Because we want to keep all the rows of the longer data set, with all the inventory data, we first select", code("StackedTables"), "We then indicate what column in that dataset is the 'key' column that should be used to link it to our species table. We then do the same to our species table."),
                           p("NOTE: If you need multiple columns to indicate the 'key' that links the two tables together, you can select multiple columns. If you are doing so, make sure that you select them in the right order."),
                       img(src = "Merge.gif", width = "100%")),
              tabPanel(title = "Tidy",
                       img(src = "Tidy.gif", width = "100%")),
              tabPanel(title = "Headers",img(src = "Headers.gif", width = "100%")),
              tabPanel(title = "Corrections"),
              tabPanel(title = "Downloads")
            )

    ) # end of "Help" panel
  )
)


ui <- dashboardPage(header, sidebar, body)
