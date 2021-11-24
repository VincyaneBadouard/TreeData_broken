
library(shiny)
library(bslib)
library(DT)

ui <- fluidPage(

  # esthetique
  theme = bs_theme(bootswatch = "superhero"),
  titlePanel("Data harmonisation"),

  tabsetPanel(

    tabPanel("Upload",
             sidebarLayout(

               sidebarPanel(
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

               mainPanel(
                 DTOutput(outputId = "tabData"))
             )
    ),

    tabPanel("Change format"),

    tabPanel("Correct"),

    tabPanel("Visualise"),

    tabPanel("Save",
             downloadButton(outputId = "dbFile", label = "Save file"),
             downloadButton(outputId = "dbCode", label = "Save code")
    )

  )

)
