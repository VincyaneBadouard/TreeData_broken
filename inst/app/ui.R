#list of packages required
list.of.packages <- c("shiny","bslib","DT","shinydashboard","shinyjs")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load required packages' libraries
lapply(as.list(list.of.packages), library, character.only = T)

# source the REquiredFormat function to get the list of arguments
source(paste0(dirname(dirname(getwd())), "/R/RequiredFormat.R")) # ***make this better!!**

x <- as.list(formals(RequiredFormat)[-1])

# header with title
header <- dashboardHeader(title = "Data harmonisation")

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(menuItem("Short Manual", tabName = "Manual", icon = icon("book")),
              menuItem("Upload your file", tabName = "Upload", icon = icon("upload")),
              menuItem("Identify headers", tabName = "headers", icon = icon("arrows-alt")),
              menuItem("Apply corrections", tabName = "Correct", icon = icon("check-circle")),
              menuItem("Visualise results", tabName="Visualise", icon = icon("eye")),
              menuItem("Save codes and data", tabName="Save", icon = icon("save"))
  )
)

body <- dashboardBody(
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(

              column(width = 3,

                     # load button for main data file (csv format)
                     box(title = "Actions",
                         width = NULL,
                         fileInput(inputId = "file1", "Choose CSV File", accept = ".csv"),

                         # does the dataframe have a header?
                         checkboxInput("header", "Header", TRUE),

                         # choose separator
                         selectInput(inputId = "cbSeparator",
                                     label = "Separator",
                                     choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
                                     selected = "auto"
                         )
                     )
              ),

              column(width = 9,
                     DTOutput(outputId = "tabData")
              )

            )
    ),  ## end of "upload" panel

    tabItem(tabName = "headers",
            fluidPage(

              dashboardBody(column(4, uiOutput("ui1")))
            )),

    tabItem(tabName = "Save",
            fluidRow(
              column(width = 3,
                     box(title = "Save file",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbFile", label = "Save file")),
                     box(title = "Save code",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbCode", label = "Save code"))
              )
            )
    ) # end of "save" panel
  )
)


dashboardPage(header, sidebar, body)
