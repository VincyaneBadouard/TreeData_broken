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
library(htmlTable)
library(TreeData)

useSweetAlert()


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


# correction function interactive items

xCorr <- read.csv("data/interactive_items_CorrerctionFunctions.csv")



## in the Codes tab (tree code pre-defined options)
CodeOptions <-  read.csv("data/CodeOptions.csv")


# function to make unitque IDs (mostly for inactive buttons)

makeUniqueID <- NS(character(0))

# languages <- c("en" = "English", "es" = "Spanish")
#
# flags <- c(
#   "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg",
#   "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg"
#
# )
