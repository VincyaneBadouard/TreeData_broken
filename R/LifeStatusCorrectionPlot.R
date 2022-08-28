#' Plot life status correction result
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` (character)
#'   - `Year` (numeric)
#'   - `LifeStatus` (logical or character)
#'   - `LifeStatusCor` (logical or character)
#'
#' @param OnlyCorrected TRUE: plot only corrected trees, FALSE: plot all trees
#'
#' @return The plots of the initial diameter values and proposed corrections, by
#'   IdStem.
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes theme_minimal
#'    scale_colour_manual labs vars
#' @importFrom ggforce facet_wrap_paginate
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' pdf("LifeStatusCorrectionPlots.pdf", width = 25, height = 10)
#' LifeStatusCorrectionPlot(Rslt, SeveralWindows = FALSE)
#' dev.off()
#'}
#'
LifeStatusCorrectionPlot <- function(
  Data,
  OnlyCorrected = FALSE,
  # CorCol = "LifeStatusCor",
  # InitialCol = "LifeStatus",
  # FileName = "LifeStatusCorrectionPlots.pdf"
  SeveralWindows = TRUE
){

  #### Arguments check ####

  # Data
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Columns
  # IdTree, Year, LifeStatus, LifeStatusCor
  if(!all(c("IdTree", "Year", "LifeStatus", "LifeStatusCor") %in% names(Data)))
    stop("'IdTree', 'Year', 'LifeStatus', 'LifeStatusCor' should be columns of Data")


  #### Function ####

  Data[,LifeStatusCor := as.character(LifeStatusCor)]
  Data[,LifeStatus := as.character(LifeStatus)]
  Data[is.na(LifeStatusCor), LifeStatusCor := "NA"]
  Data[is.na(LifeStatus), LifeStatus := "NA"]

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(IdTree, Year)]

  if(OnlyCorrected == TRUE){
    # Only corrected stems ----------------------------------------------------------------------------------------------
    IdTreeCor <- Data[LifeStatus != LifeStatusCor, IdTree] #  corrected stems

    DataCor <- Data[IdTree %in% IdTreeCor] #  corrected stems

  }else{
    DataCor <- Data
    IdTreeCor <- Data[, IdTree]
  }


  # Define nrow and ncol for the facet
  n <- length(unique(IdTreeCor))
  if(n<3) { i = 1
  }else{ i = 3}


  # Plot --------------------------------------------------------------------------------------------------------------
  # pdf(FileName, width = 25, height = 10)

  if(SeveralWindows == TRUE)
  dev.new()

  for(p in 1:(ceiling(length(unique(IdTreeCor))/9))){
    print(ggplot(DataCor) +
      aes(x = Year) +

      # Initial
      geom_point(aes(y = LifeStatus,
                     color = ifelse(LifeStatus != LifeStatusCor, 'Initial', 'Conserved')),
                 shape = "circle", size = 3.9) +

      geom_line(aes(y = LifeStatus, color = ifelse(LifeStatus != LifeStatusCor, 'Initial', 'Conserved'))) +


      # Corrected
      geom_line(aes(y = LifeStatusCor, color = ifelse(LifeStatus != LifeStatusCor, 'Corrected', 'Conserved'))) +
      geom_point(aes(y = LifeStatusCor,
                     color = ifelse(LifeStatus != LifeStatusCor | is.na(LifeStatus), 'Corrected', 'Conserved')),
                 shape = "circle", size = 3.9) +


      # Colours
      scale_colour_manual(name = "Status", values = c("Conserved" = "black",
                                                      "Initial" = "red",
                                                      "Corrected" = "forestgreen")) +
      theme_minimal() +

      # Titles
      labs(
        # title =  paste("IdTree: ",unique(DataCor$IdTree),""),
        x = "Year", y = "LifeStatus") +

      ggforce::facet_wrap_paginate(vars(IdTree), scales = "free", ncol = min(n,3), nrow = i, page = p)

    )

    if(SeveralWindows == TRUE & p < ceiling(length(unique(IdTreeCor))/9))
      dev.new()


  }
  # dev.off()


  # return(Pl)

}
