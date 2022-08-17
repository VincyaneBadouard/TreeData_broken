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
#' @importFrom ggplot2 ggplot geom_point geom_text geom_line aes theme_minimal
#'   position_nudge scale_colour_manual labs vars
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom grDevices pdf dev.off
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

  # Only corrected stems
  IdStemCor <- Data[Diameter != DBHCor, IdStem] #  corrected stems

  DataCor <- Data[IdStem %in% IdStemCor] #  corrected stems

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
            geom_text(data = subset(DataCor, (!is.na(Diameter) & !is.na(HOM))),
                      aes(y = Diameter, label = HOM, colour = "HOM"), position = position_nudge(y = -0.3)) +
            geom_line(data = subset(DataCor, !is.na(Diameter)),
                      aes(y = Diameter, color = ifelse(Diameter != DBHCor, 'Initial', 'Conserved'))) +

            # Corrected
            geom_point(data = subset(DataCor, !is.na(DBHCor)),
                       aes(y = DBHCor,
                           color = ifelse(Diameter != DBHCor, 'Corrected', 'Conserved')),
                       shape = "circle", size = 3.9) +
            geom_text(data = subset(DataCor, (!is.na(DBHCor) & !is.na(HOMCor))),
                      aes(y = DBHCor, label = HOMCor, colour = "HOM"), position = position_nudge(y = -0.3)) +
            geom_line(data = subset(DataCor, !is.na(DBHCor)),
                      aes(y = DBHCor, color = ifelse(Diameter != DBHCor, 'Corrected', 'Conserved'))) +

            theme_minimal() +
            scale_colour_manual(name = "Status", values = c("HOM" = "blue",
                                                            "Conserved" = "black",
                                                            "Initial" = "red",
                                                            "Corrected" = "forestgreen",
                                                            "Duplicated measurement" = "grey")) +
            labs(
              # title =  paste("IdStem: ",unique(DataCor$IdStem),""),
              x ="Year", y = "Diameter (cm)") +


            ggforce::facet_wrap_paginate(vars(IdStem), scales = "free", ncol = 3, nrow = 3, page = p) # why only 8?

    )

  }
  dev.off()

  # return()

}
