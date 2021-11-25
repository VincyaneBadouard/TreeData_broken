library(shiny)
library(bslib)
library(DT)
library(shinydashboard)
library(shinyjs)

# header with title
header <- dashboardHeader(title = "Data harmonisation")

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(menuItem("Short Manual", tabName = "Manual", icon = icon("book")),
              menuItem("Upload your file", tabName = "Upload", icon = icon("upload")),
              menuItem("Change format", tabName = "Reformat", icon = icon("arrows-alt")),
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
                     fileInput(inputId = "file1", "Choose CSV File", accept = ".csv"),

                     # does the dataframe have a header?
                     checkboxInput("header", "Header", TRUE),

                     # choose separator
                     selectInput(inputId = "cbSeparator",
                                 label = "Separator",
                                 choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
                                 selected = "auto"
                     )
              ),

              column(width = 9,
                     DTOutput(outputId = "tabData")
              )

            )
    ),  ## end of "upload" panel

    tabItem(tabName = "Save",
            fluidRow(
              downloadButton(outputId = "dbFile", label = "Save file"),
              downloadButton(outputId = "dbCode", label = "Save code")
            )
    ) # end of "save" panel
  )
)


dashboardPage(header, sidebar, body)

# tabPanel("Upload",
#          sidebarLayout(
#
#            sidebarPanel(
#              # load button for main data file (csv format)
#              fileInput(inputId = "file1", "Choose CSV File", accept = ".csv"),
#
#              # does the dataframe have a header?
#              checkboxInput("header", "Header", TRUE),
#
#              # choose separator
#              selectInput(inputId = "cbSeparator",
#                          label = "Separator",
#                          choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
#                          selected = "auto"
#              )
#            ),
#
#            mainPanel(
#              DTOutput(outputId = "tabData"))
#          )
# ),

# tabPanel("Change format"),

# tabPanel("Correct"),

# tabPanel("Visualise"),
#
#     tabPanel("Save",
#              downloadButton(outputId = "dbFile", label = "Save file"),
#              downloadButton(outputId = "dbCode", label = "Save code")
# )

# )


