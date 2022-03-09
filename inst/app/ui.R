#list of packages required
list.of.packages <- c("shiny","bslib","DT","shinydashboard","shinyjs", "shinyWidgets", "data.table", "data.tree")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
# if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load required packages' libraries
lapply(as.list(list.of.packages), require, character.only = T)

# source script to get VegX_tree
# source("data/My_VegX.R")
VegXtree <- readRDS("data/VegXtree.rds")
VegXtree$Do(function(x) x$inputId <- gsub("/", "_", x$pathString))

# tree <- ToListExplicit(VegXtree)
tree <- cbind(ToDataFrameTypeCol(VegXtree), ToDataFrameTable(VegXtree, "name", "inputId", "annotation"))
tree <- split(tree, factor(tree$level_2, levels = unique(tree$level_2)))

my_pickerInput <- function(x) {
  pickerInput(inputId = x$itemID ,
              label =  div(h3(x$name2), p(x$annotation)),
              choices = "")
}

my_lapply <- function(x) {
  lapply(x, function(y) {
    if(length(y) ==1) my_pickerInput(y) else my_dropdown(y)
  })
}


my_dropdown <-     function(x) {
  div(tags$h3(x$name), dropdown(
    circle = F,
    tooltip = T,

    my_lapply(x[-1])

  ))

}




# header with title
header <- dashboardHeader(title = "Data harmonisation")

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id = "tabs",
              menuItem("Upload your file(s)", tabName = "Upload", icon = icon("upload")),
              menuItem("Identify headers", tabName = "headers", icon = icon("arrows-alt")),
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
             }
             "
      )
    ) # to make notification show up at top of page
  ),
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(
              column(width = 3,
              numericInput(inputId = "nTable",
                          label = "How many tables do you wish to upload?",
                          value = 1,
                          min = 1,
                          max = NA
                          )
              ),
              uiOutput("ui_uploadTables"),
              actionBttn(
                inputId = "submit",
                label = "submit",
                style = "material-flat",
                color = "success"
              )

            )

            #   column(width = 3,
            #
            #          # load button for main data file (csv format)
            #          box(title = "Upload your data",
            #              width = NULL,
            #              fileInput(inputId = "file0", "Choose CSV File", accept = ".csv"),
            #              # does the dataframe have a header?
            #              checkboxInput("header", "Header", TRUE),
            #              # choose separator
            #              selectInput(inputId = "cbSeparator",
            #                          label = "Separator",
            #                          choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
            #                          selected = "auto"
            #              )
            #          )
            #   ),
            #
            #   column(width = 9,
            #          DTOutput(outputId = "tabData")
            #   )
            #
            # )
    ),  ## end of "upload" panel

    tabItem(tabName = "headers",
            fluidRow(h5("This may take a few seconds to show..."),
            column(width = 12,
              uiOutput("uiheader")
            ))
            )
    ,

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
