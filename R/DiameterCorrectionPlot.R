#' Plot diameter correction result
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdStem` (character)
#'   - `Year` (numeric)
#'   - `Diameter` (numeric)
#'   - `DBHCor` (numeric)
#'   - `HOM` (Height Of Measurement) (numeric)
#'   - `HOMCor` (Corrected Height Of Measurement) (numeric)
#'
#' @param OnlyCorrected TRUE: plot only corrected stems, FALSE: plot all stems
#'
#' @param CorCol Corrected column name (character)
#'
#' @return The plots of the initial diameter values and proposed corrections, by
#'   IdStem.
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes theme_minimal
#'   position_nudge scale_colour_manual labs vars
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom ggrepel geom_text_repel
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' pdf("DiameterCorrectionPlots_TestData.pdf", width = 25, height = 10)
#' DiameterCorrectionPlot(Rslt, OnlyCorrected = TRUE)
#' dev.off()
#'}
#'
DiameterCorrectionPlot <- function(
  Data,
  OnlyCorrected = FALSE,
  CorCol = "DBHCor"
  # InitialCol = "Diameter",
  # FileName = "DiameterCorrectionPlots.pdf"
){

  #### Arguments check ####

  # Data ---------------------------------------------------------------------------------------------------------------
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # POM or HOM? ----------------------------------------------------------------------------------------------------------
  # If no HOM take POM
  if((!"HOM" %in% names(Data) | all(is.na(Data$HOM))) &
     ("POM" %in% names(Data) & any(!is.na(Data$POM))) ){ POMv <- "POM"

  }else{ POMv <- "HOM"}

  if((!"HOMCor" %in% names(Data) | all(is.na(Data$HOMCor))) &
     ("POMCor" %in% names(Data) & any(!is.na(Data$POMCor))) ){ POMcorv <- "POMCor"

  }else{ POMcorv <- "HOMCor"}

  # Columns --------------------------------------------------------------------------------------------------------------
  # IdStem, Year, Diameter, DBHCor, HOM, HOMCor
  if(!all(c("IdStem", "Year", "Diameter", CorCol, POMv, POMcorv) %in% names(Data)))
    stop(paste0("'IdStem', 'Year', 'Diameter', '",CorCol,"', '",POMv,"', ",POMcorv,"' should be columns of Data"))


  #### Function ####

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(IdStem, Year)]

  if(OnlyCorrected == TRUE){
    # Only corrected stems ----------------------------------------------------------------------------------------------
    IdStemCor <- Data[Diameter != get(CorCol), IdStem] #  corrected stems

    DataCor <- Data[IdStem %in% IdStemCor] #  corrected stems

  }else{
    DataCor <- Data
    IdStemCor <- Data[, IdStem]
  }


  # Define nrow and ncol for the facet
  n <- length(unique(IdStemCor))
  if(n<3) { i = 1
  }else{ i = 3}

  # Plot --------------------------------------------------------------------------------------------------------------
  for(p in 1:(ceiling(length(unique(IdStemCor))/9))){
    Pl <- ggplot(DataCor) +
      aes(x = Year) +

      # Duplicated measurement
      {
        if(nrow(subset(DataCor, !is.na(Diameter) & is.na(get(CorCol)))) > 0)
          geom_point(data = subset(DataCor, !is.na(Diameter) & is.na(get(CorCol))),
                     aes(y = Diameter,
                         color = 'Duplicated measurement'),
                     shape = "circle", size = 3.9) } +
          # Initial
          geom_point(data = subset(DataCor, !is.na(Diameter)),
                     aes(y = Diameter,
                         color = ifelse(Diameter != get(CorCol), 'Initial', 'Conserved')),
                     shape = "circle", size = 3.9) +
          geom_line(data = subset(DataCor, !is.na(Diameter)),
                    aes(y = Diameter, color = ifelse(Diameter != get(CorCol), 'Initial', 'Conserved'))) +

          ggrepel::geom_text_repel(data = subset(DataCor, (!is.na(Diameter) & !is.na(get(POMv)))),
                                   aes(y = Diameter, label = get(POMv), colour = "HOM"),
                                   point.size = 3.9, size = 3, direction = "y") +

          # Corrected
          geom_line(data = subset(DataCor, !is.na(get(CorCol))),
                    aes(y = get(CorCol), color = ifelse(Diameter != get(CorCol), 'Corrected', 'Conserved'))) +
          geom_point(data = subset(DataCor, !is.na(get(CorCol))),
                     aes(y = get(CorCol),
                         color = ifelse(Diameter != get(CorCol) | is.na(Diameter), 'Corrected', 'Conserved')),
                     shape = "circle", size = 3.9) +

          ggrepel::geom_text_repel(data = subset(DataCor,
                                                 (!is.na(get(CorCol)) & !is.na(get(POMcorv)) & (Diameter != get(CorCol)) | is.na(Diameter))),
                                   aes(y = get(CorCol), label = get(POMcorv), colour = "HOM"),
                                   point.size = 3.9, size = 3, direction = "y") +
          ggrepel::geom_text_repel(data = subset(DataCor, (!is.na(get(CorCol)) & DiameterCorrectionMeth != "")),
                                   aes(y = get(CorCol), label = DiameterCorrectionMeth, colour = "Methode"),
                                   point.size = 10, size = 3) +

          # Colours
          scale_colour_manual(name = "Status", values = c("Conserved" = "black",
                                                          "Duplicated measurement" = "grey",
                                                          "Initial" = "red",
                                                          "Corrected" = "forestgreen",
                                                          "Methode" = "purple",
                                                          "HOM" = "blue")) +
          theme_minimal() +

          # Titles
          labs(
            # title =  paste("IdStem: ",unique(DataCor$IdStem),""),
            x = "Year", y = "Diameter (cm)") +


          ggforce::facet_wrap_paginate(vars(IdStem, ScientificName), scales = "free", ncol = min(n,3), nrow = i, page = p)

      }

    return(Pl)

  }
