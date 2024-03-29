#' Plot Botanical correction results
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` (character)
#'   - `ScientificName` (character)
#'   - `ScientificName_TreeDataCor` (character)
#'
#' @return Table of corrected species IDs
#'
#' @importFrom DT datatable
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' data(TestData)
#' Rslt <- BotanicalCorrection(TestData)$Data
#' BotanicalCorrectionPlot(Rslt)
#'}
#'
BotanicalCorrectionPlot <- function(
    Data
){

  ThisIsShinyApp =  shiny::isRunning() # this is for internal use when function used by Shiny app

  #### Arguments check ####

  # Data
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")



  # Columns
  # ScientificName_TreeDataCor
  if(!all(c("ScientificName", "ScientificName_TreeDataCor") %in% names(Data)))
    stop("'ScientificName', and 'ScientificName_TreeDataCor' should be columns of Data")


  #### Function ####

 # Only corrected stems ----------------------------------------------------------------------------------------------
    IDCor <- Data[ScientificName != ScientificName_TreeDataCor, IdTree] #  corrected stems

    DataCor <- Data[IdTree %in% IDCor,.(ScientificName_TreeDataCor,ScientificName, IdTree )] #  corrected stems

    DataCor <- DataCor[, .(N = .N), by = .(ScientificName_TreeDataCor, IdTree , ScientificName)]

    DataCorIncongruence <- DataCor[IdTree %in% names(which(table(IdTree)>1)), ]


 DT::datatable(
    DataCor[order(ScientificName_TreeDataCor), ],
    rownames = F,
    extensions = c('Scroller', 'RowGroup'),
    options = list(rowGroup = list(
      dataSrc = c(0,1)),
      deferRender = TRUE,
      scrollY = T,
      scroller =TRUE ,
      columnDefs = list(list(visible=FALSE, targets=c(0,1)))),
    selection = 'none'
  )

return(list(DataCor = DataCor, DataCorIncongruence = DataCorIncongruence))
}
