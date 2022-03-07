#list of packages required
list.of.packages <- c("shiny","bslib","DT","shinydashboard","shinyjs", "shinyWidgets", "data.table")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
# if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load required packages' libraries
lapply(as.list(list.of.packages), require, character.only = T)

# source script to get VegX_tree
source("../data-raw/My_VegX.R")


# header with title
header <- dashboardHeader(title = "Data harmonisation")

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
              menuItem("Upload your file", tabName = "Upload", icon = icon("upload")),
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

                     # load button for main data file (csv format)
                     box(title = "Upload your data",
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

            fluidRow(
              lapply(ToListSimple(tree)[-1], function(x) {

                box(tags$h3(x$name), dropdown(
                  circle = F,
                  tooltip = T,

                  lapply(x[-1], function(y) {

                    div(tags$h3(y$name), dropdown(
                      circle = F,
                      tooltip = T,

                      lapply(y[-1], function(z) {
                        if(length(z) ==1) {
                          pickerInput(inputId = z$name,
                                      label =  z$name,
                                      choices = names(iris))
                        } else {
                          div(tags$h3(z$name), dropdown(
                            circle = F,
                            tooltip = T,

                            lapply(z[-1], function(w) {
                            if(length(w) ==1) {
                              pickerInput(inputId = w$name,
                                          label =  w$name,
                                          choices = names(iris))

                            } else {
                              warning("more nested stuff")
                            }
                            })
                          ))
                        }
                      })
                    ))
                  })


                ))
              })
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
