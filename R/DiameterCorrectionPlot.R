#' Diameter Correction Plot
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdStem` (character)
#'   - `Year` (numeric)
#'   - `Diameter` (numeric)
#'   - `DBHCor` (numeric)
#'     `HOM` (Height Of Measurement) (numeric)
#'     `HOMCor` (Corrected Height Of Measurement) (numeric)
#'
#' @return Saves a pdf to the root of your R project with the plots of the
#'   initial diameter values and proposed corrections, by IdStem.
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes theme_minimal
#'   position_nudge scale_colour_manual labs vars
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom grDevices pdf dev.off
#' @importFrom ggrepel geom_text_repel
#'
#' @export
#'
#' @examples
#'
#' # DiameterCorrectionPlot(Rslt)
#'
DiameterCorrectionPlot <- function(
  Data
){

  #### Arguments check ####

  # Data
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Columns
  # IdStem, Year, Diameter, DBHCor, HOM, HOMCor
  if(!all(c("IdStem", "Year", "Diameter", "DBHCor", "HOM", "HOMCor") %in% names(Data)))
    stop("'IdStem', 'Year', 'Diameter', 'DBHCor', 'HOM', 'HOMCor' should be columns of Data")


  #### Function ####

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(IdStem, Year)]

  # Only corrected stems ----------------------------------------------------------------------------------------------
  IdStemCor <- Data[Diameter != DBHCor, IdStem] #  corrected stems

  DataCor <- Data[IdStem %in% IdStemCor] #  corrected stems

  # Plot --------------------------------------------------------------------------------------------------------------
  pdf("DiameterCorrectionPlots.pdf", width = 25, height = 10)
  for(p in 1:(ceiling(length(unique(IdStemCor))/9))){
    print(ggplot(DataCor) +
            aes(x = Year) +
            # Duplicated measurement
            geom_point(data = subset(DataCor, !is.na(Diameter)),
                       aes(y = Diameter,
                           color = if(is.na(DBHCor)) 'Duplicated measurement'),
                       shape = "circle", size = 3.9) +
            # Initial
            geom_point(data = subset(DataCor, !is.na(Diameter)),
                       aes(y = Diameter,
                           color = ifelse(Diameter != DBHCor, 'Initial', 'Conserved')),
                       shape = "circle", size = 3.9) +
            geom_line(data = subset(DataCor, !is.na(Diameter)),
                      aes(y = Diameter, color = ifelse(Diameter != DBHCor, 'Initial', 'Conserved'))) +

            geom_text_repel(data = subset(DataCor, (!is.na(Diameter) & !is.na(HOM))),
                            aes(y = Diameter, label = HOM, colour = "HOM"),
                            point.size = 3.9, size = 3, direction = "y") +

            # Corrected
            geom_line(data = subset(DataCor, !is.na(DBHCor)),
                      aes(y = DBHCor, color = ifelse(Diameter != DBHCor, 'Corrected', 'Conserved'))) +
            geom_point(data = subset(DataCor, !is.na(DBHCor)),
                       aes(y = DBHCor,
                           color = ifelse(Diameter != DBHCor | is.na(Diameter), 'Corrected', 'Conserved')),
                       shape = "circle", size = 3.9) +

            geom_text_repel(data = subset(DataCor,
                                          (!is.na(DBHCor) & !is.na(HOMCor) & (Diameter != DBHCor) | is.na(Diameter))),
                            aes(y = DBHCor, label = HOMCor, colour = "HOM"),
                            point.size = 3.9, size = 3, direction = "y") +
            geom_text_repel(data = subset(DataCor, (!is.na(DBHCor) & DiameterCorrectionMeth != "")),
                            aes(y = DBHCor, label = DiameterCorrectionMeth, colour = "Methode"),
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


            ggforce::facet_wrap_paginate(vars(IdStem), scales = "free", ncol = 3, nrow = 3, page = p)

    )

  }
  dev.off()

  # return()

}
