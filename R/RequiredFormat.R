#' RequiredFormat
#'
#' @param Data Input forest inventory (data.frame)
#'
#' @param Plot The name of the column containing plot indices (character)
#'
#' @param SubPlot The name of the column containing sub-plot indices (character)
#'
#' @param Time The name of the column containing census years (character)
#'
#' @param IdTree The name of the column containing tree unique identifiers
#'   (character)
#'
#' @param LifeStatus The name of the column containing tree vital status
#'   (character)
#'
#' @param Size The name of the column containing tree size measurements,
#'   diameter or circumference (character)
#' @param SizeUnit The 'Size' measurement unit of the tree. The possible values
#'   are 'mm' for millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm'
#'   for metres"(character)
#'
#' @param POM The name of the column containing Point of Measurement (POM)
#'   values: height at which the 'Size' was measured, or POM code (character)
#' @param POMUnit The 'POM' unit of the tree. The possible values are 'mm' for
#'   millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm' for metres"
#'   (character)
#'
#' @param PlotArea The name of the column containing the area of each plot, in
#'   hectares (character)
#'
#' @param X The name of the column containing the tree X coordinate (character)
#' @param Y The name of the column containing the tree Y coordinate (character)
#'
#' @param ScientificName The name of the column containing the binary scientific
#'   name of the tree (character)
#'
#' @param VernName The name of the column containing the vernacular name of the
#'   tree (character)
#'
#' @param Family The name of the column containing the family name of the tree
#'   (character)
#'
#' @param Genus The name of the column containing the genus name of the tree
#'   (character)
#' @param Species The name of the column containing the species name of the tree
#'   (character)
#'
#' @param CommercialSp The name of the column containing the commercial or
#'   non-commercial nature of the tree species (character)
#'
#' @param TreeHeight The name of the column containing the the measured height
#'   of the tree (character)
#' @param TreeHeightUnit The 'TreeHeight' unit. The possible values are 'mm' for
#'   millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm' for
#'   metres"(character)
#'
#' @return Input inventory (data.frame) in the required package format.
#'
#' @export
#'
#' @importFrom data.table copy setDT setDF :=
#'
#' @examples
#'\dontrun{
#' RequiredFormat(Data,
#'                Plot = NULL,
#'                SubPlot = NULL,
#'                Time = NULL,
#'                IdTree = NULL,
#'                LifeStatus = NULL,
#'                Size = NULL,
#'                SizeUnit = NULL,
#'                POM = NULL,
#'                POMUnit = NULL,
#'                PlotArea = NULL,
#'                X = NULL,
#'                Y = NULL,
#'                ScientificName = NULL,
#'                VernName = NULL,
#'                Family = NULL,
#'                Genus = NULL,
#'                Species = NULL,
#'                CommercialSp = NULL,
#'                TreeHeight = NULL,
#'                TreeHeightUnit = NULL)
#'                }
#'
RequiredFormat <- function(
  Data,
  Plot = NULL,
  SubPlot = NULL,
  Time = NULL,
  IdTree = NULL,
  LifeStatus = NULL,
  Size = NULL,
  SizeUnit = NULL,
  POM = NULL,
  POMUnit = NULL,
  PlotArea = NULL,
  X = NULL,
  Y = NULL,
  ScientificName = NULL,
  VernName = NULL,
  Family = NULL,
  Genus = NULL,
  Species = NULL,
  CommercialSp = NULL,
  TreeHeight = NULL,
  TreeHeightUnit = NULL
){

  # Check arguments

  args <- c(Plot, SubPlot, Time, IdTree, LifeStatus,
            Size, SizeUnit, POM, POMUnit,
            PlotArea, X, Y,
            ScientificName, VernName, Family, Genus, Species, CommercialSp,
            TreeHeight, TreeHeightUnit)

  argsCol <- args[!args %in% c(SizeUnit, POMUnit, TreeHeightUnit)] # columns names arguments
  argsUnit <- args[args %in% c(SizeUnit, POMUnit, TreeHeightUnit)] # units arguments

  ## Argument class

  if (!inherits(Data, "data.frame"))
    stop("Data must be a data.frame")

  # if the argument is not NULL
  # for(i in 1:length(args))
  #   if(!is.null(args[i])){

  for(i in 1:length(args)) # all args
    if(!inherits(args[i], "character"))
      stop(paste0("must be in character"[i]))

  ## Units

  if(length(argsUnit) > 0){
    for(i in 1:length(argsUnit))
      if(!argsUnit[i] %in% c("mm", "millimetre", "millimeter", "milimetro", "milimetrica",
                             "cm", "centimetre", "centimeter", "centimetro",
                             "dm", "decimetre", "decimeter", "decimetro",
                             "m", "metre", "meter", "metro")
      )
        stop("Please indicate units with 'mm' for millimetres, 'cm' for
          centimetres, 'dm' for decimetres and 'm' for metres")
  }


  ## NULL case
  ## The column name exists

  for(i in 1:length(argsCol))
    if(!argsCol[i] %in% names(Data))
      stop(paste0("is not a column name of your dataset"[i]))

  # } # end "if the argument is not NULL"

  # if the variable exists but the unit is NULL
  if(is.null(SizeUnit) & !is.null(Size))
    stop("Please indicate in which unit ('SizeUnit') the 'Size' of your tree is measured")

  if(is.null(POMUnit) & !is.null(POM))
    stop("Please indicate in which unit ('POMUnit') the 'POM'
         (Point Of Measurement) of your tree is measured")


  if(is.null(TreeHeightUnit) & !is.null(TreeHeight))
    stop("Please indicate in which unit ('TreeHeightUnit') the 'TreeHeight' is measured")

  # Formatting

  DataInput <- copy(Data) # input data copy

  ## data.frame to data.table
  setDT(Data) # with "set" "<-" is not necessary

  ## Wide to long format (detect and change)
  # detect : each census is a col ?
  # str(Data) # dt structure

  # melt(Data,
  #      id.vars = c("col1", "col2"), # columns that remain columns
  #      measure.vars = patterns("^a", "^b"), # cols to rows (years?) : 4 digits, noms de var repetes, chiffres dans plsrs colonnes
  #      variable.name = "Time", # name of the new column that contains the names of the transposed variables
  #      value.name = "Size") # name of the new column that contains the values of the transposed variables

  ## Class
  CharacVar <- c(Plot, SubPlot, IdTree, ScientificName, VernName, Family, Genus, Species) # character variables

  for(v in 1:length(CharacVar))
    Data[, get(CharacVar[v]) := as.character(get(CharacVar[v]))]

  NumVar <- c(Time, Size, PlotArea, X, Y, TreeHeight) # numeric variables

  for(v in 1:length(NumVar))
    Data[, get(NumVar[v]) := as.numeric(get(NumVar[v]))]

  LogicVar <- c(LifeStatus, CommercialSp) # logical variables

  for(v in 1:length(LogicVar))
    Data[, get(LogicVar[v]) := as.logical(get(LogicVar[v]))]

  ## Units


  ## Necessary variables creation from the existing

  ### IdTree

  ### POM ? (detect pom is a code?)

  ### PlotArea (not a column but a value/values vector)

  ### Scientific name but no genus species columns

  ### Genus & species columns but no ScientificName column


  # Return in data.frame
  setDF(Data)


}
