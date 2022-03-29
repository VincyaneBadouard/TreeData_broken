#list of packages required
list.of.packages <- c("shiny","bslib","DT","shinydashboard","shinyjs", "shinyWidgets", "data.table", "stringdist")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
# if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load required packages' libraries
lapply(as.list(list.of.packages), require, character.only = T)



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
              menuItem("Identify headers", tabName = "Headers", icon = icon("arrows-alt")),
              menuItem("Apply corrections", tabName = "Correct", icon = icon("check-circle")),
              menuItem("Visualise results", tabName="Visualise", icon = icon("eye")),
              menuItem("Save codes and data", tabName="Save", icon = icon("save")),
              menuItem("Help", tabName = "Manual", icon = icon("book"))
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
             "
      )
    ) # to make notification show up at top of page
  ),
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(
              column(width = 8,
                     actionBttn(
                       inputId = "inactivebutton",
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
              column(width = 12,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "2",
                       style = "pill",
                       color = "warning"),
                     strong("Upload your tables"),
                     uiOutput("ui_uploadTables"))),


            fluidRow(
              column(width = 3,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "3",
                       style = "pill",
                       color = "warning"),
                     actionBttn(
                       inputId = "submitTables",
                       label = "submit",
                       style = "material-flat",
                       color = "success"
                     ))

            )

    ),  ## end of "upload" panel


      tabItem(tabName = "Stacking",

              fluidRow(
                # column(width = 12,
                       actionBttn(
                         inputId = "inactivebutton",
                         label = " ! ",
                         style = "pill",
                         color = "danger"),
                       strong("make sure you clicked on 'Sumbit' in Upload tab")
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
                       DTOutput(outputId = "StackedTables"),
                )
                # ,
                # actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
              )


      ),  ## end of "Stacking" panel
    tabItem(tabName = "Merging",

            fluidRow(
              # column(width = 12,
              actionBttn(
                inputId = "inactivebutton",
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
                     h4("The table on the left should be the most exhaustive table (the one you want to keep all the rows from."),
                     # actionButton("addMerge", "Add a Merging relationship"),
                     # uiOutput("MergeTablesUI"),
                     # textOutput("test"),
                     # verbatimTextOutput("test2"),
                     # checkboxGroupButtons("TablesToMerge", choices = ""),

                     box(width = 12,

                         fluidRow(column(3, pickerInput("leftTable", "Merge this table", choices = "")),
                                  column(1, br(),actionBttn("selectLeft", "", icon = icon("arrow-right"), size = "sm")),
                                  column(6,  pickerInput("leftKey", "Using this/these column(s) - ORDER MATTERS", choices = "", multiple = T))),

                         fluidRow(column(3, pickerInput("rightTable", "And this table", choices = "")),
                                  column(1, br(),actionBttn("selectRight", "", icon = icon("arrow-right"), size = "sm")),
                                  column(6,  pickerInput("rightKey", "Using this/these column(s) - ORDER MATTERS", choices = "", multiple = T)))
                     #
                     #     fluidRow(column(3, pickerInput("rightTable", "Take table", choices = "")),
                     #              column(3,actionBttn("selectRight", "ok")))
                     #
                     #     fluidRow(column(3, pickerInput("leftTable", "Take table", choices = "")),
                     #              column(3,actionBttn("selectLeft", "ok")))
                     #
                     #     fluidRow(column(3, pickerInput("leftTable", "Take table", choices = "")),
                     #              column(3,actionBttn("selectLeft", "ok")))
                     #
                     # column(3, pickerInput("rightTable", "add to it this table", choices = "")),
                     # column(3, pickerInput("leftKey", "Using this column(s) from first table", choices = "", multiple = T)),
                     # column(3,  pickerInput("rightKey", "and this column(s) from second table", choices = "", multiple = T))
                     ),

                     actionBttn(
                       inputId = "Merge",
                       label = "Merge tables",
                       style = "material-flat",
                       color = "success"
                     ),
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
                     DTOutput(outputId = "mergedTables"),
              )
            #   # ,
            #   # actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
            )


    ),  ## end of "Merging" panel

    tabItem(tabName = "Tidying",
            h3("This is where we want to make your data 'tidy'"),
            h3("This means that we want one row per observation. An observation is one measurement (of one stem, at one census, and one height)."),
            h4("If you have stored several measurements on a same row (for example, you have several DBH columns, one for each census), we need to tidy your data..."),
            h5("This is called wide-to-long reshaping."),
            h4("If you already have one observation per row, skip this step"),
            actionBttn(
              inputId = "SkipTidy",
              label = "Skip this step",
              style = "material-flat",
              color = "warning"
            ),
            box(width = 12,
                radioButtons(
              "VariableName",
              "What is the meaning of the repeated column?",
              choices = c("CensusID", "CensusYear", "POM", "StemID"),
              selected = "",
              inline = FALSE
            ),
            actionButton("ClearValueName","Clear")),
            h3("Tick the groupings that should be applied and fix the prefilled information if necessary."),

            uiOutput("meltUI"),

            # box(
            # textInput("ValueName", "What type of measurement is repeated horizontally? (Give a column name without space)", value = "DBH"),
            # radioButtons(
            #   "VariableName",
            #   "What is the meaning of the repeated column?",
            #   choices = c("CensusID", "CensusYear", "POM", "StemID"),
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
              # box(width = 12,
              #     radioButtons(inputId = "predefinedProfile",
              #                  label = div("Use a predifined format?", br(), em("(if your data follows one of the following network template)")),
              #                  choices = list("No thanks!" = "No", "ATDN: The Amazon Tree Diversity Network" = "ATDN", "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGEO", "RBA: Red de Bosques Andinos" = "RBA"),selected = "No"),
              #
              #     # load a profile it one already exists
              #     fileInput(inputId = "profile", div("Load your own profile", br(), em("(if you already used this app and saved your profile (.rds))")), accept = ".rds")
              #     ),

              # inform if long or wide format
              # box(width = 12,
              #     radioButtons(inputId = "format",
              #              label = div(actionBttn(
              #                inputId = "inactivebutton",
              #                label = "1",
              #                style = "pill",
              #                color = "danger"),
              #                "Is your data in long or wide format?", br(), em("(Wide format not implemented yet)")),
              #              choices = list("Long" = "long", "Wide" = "wide"))),
              column(width = 6,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "1",
                       style = "pill",
                       color = "warning")
                     ,   strong("  Match your columns to ours (if you can)"),
                     br(),
                     br(),
                     box(
                       # title = "Match your columns to ours (if you can)",
                         width = NULL,
                         # status = "primary",
                         # solidHeader = TRUE,
                         uiOutput("ui1"))
              ),
              column(width = 6,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "2",
                       style = "pill",
                       color = "warning")
                     ,   strong("  Fill in information that is not in your columns"),
                     br(),
                     br(),
                     box(
                       # title = "",
                         width = NULL,
                         # column(width = 5,
                         # box(title = "Tell us more about your plot",
                         #   width = NULL,
                         #   status = "primary",
                         #   solidHeader = TRUE,
                         #   h4("Only fill this infomation if it is not in a column!"),
                         uiOutput("ui2"),
                         #)
                         # ),


                         # column(width = 5,
                         # box(title = "Tell us about your units",
                         #   width = NULL,
                         #   status = "primary",
                         #   solidHeader = TRUE,
                         #   h4("Only fill this infomation if it is not in a column!"),
                         #   p("Note: we are not able to handle units varying by rows yet..."),
                         uiOutput("ui3"),
                         #        ),
                         # column(width = 5,
                         # box(title = "A couple more things...",
                         #     width = NULL,
                         #     status = "primary",
                         #     solidHeader = TRUE,
                         uiOutput("ui4"),
                         # )
                         # ),
                         # column(width = 5,
                         # box(title = "and lastly...",
                         #     width = NULL,
                         #     status = "primary",
                         #     solidHeader = TRUE,
                         uiOutput("ui5")
                         # )


                     ),


              actionButton("LaunchFormating", label = "Launch formating!", style = "color: #fff; background-color: #009e60; border-color: #317256") #;   position: fixed
              )


    )),

    tabItem(tabName = "Correct",
            radioButtons(inputId = "taper", label = "Apply taper corrections?", choices = list("Yes" = "Yes", "No" = "No"), selected = "No")
            ),

    tabItem(tabName = "Visualise",

            fluidRow(

              column(width = 10,
                     DTOutput(outputId = "tabDataFormated")
              ),
              actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
            )


    ),  ## end of "visualize" panel

    tabItem(tabName = "Save",
            fluidRow(
              column(width = 3,
                     box(title = "Save file",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbFile", label = "Save file")),
                     box(title = "Save profile",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbProfile", label = "Save profile")),
                     box(title = "Save code",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbCode", label = "Save code")),
                     box(title = "Save metadata",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbMetadata", label = "Save metadata"))
              )
            )
    ) # end of "save" panel

  )
)


ui <- dashboardPage(header, sidebar, body)
