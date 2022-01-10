#' RequiredFormat
#'
#' @param Data Input forest inventory (data.frame)
#'
#' @param Plot The name of the column containing plot indices (character)
#'
#' @param SubPlot The name of the column containing sub-plot indices (character)
#'
#' @param Time The name of the column(s) containing census years (character or vector of character)
#'
#' @param TreeFieldNum The name of the column containing the tree unique
#'   identifiers within the subplot (matching the tag number in the field)
#'   (character)
#'
#' @param IdTree The name of the column containing tree unique identifiers
#'   (character)
#'
#' @param LifeStatus The name of the column containing tree vital status
#'   (character)
#'
#' @param Size The name of the column(s) containing tree size measurements,
#'   diameter or circumference (character or vector of character)
#' @param SizeUnit The 'Size' measurement unit of the tree. The possible values
#'   are 'mm' for millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm'
#'   for metres", or "code" if it is a code and not a measure (character)
#'
#' @param POM The name of the column containing Point of Measurement (POM)
#'   values: height at which the 'Size' was measured, or POM code (character)
#' @param POMUnit The 'POM' unit of the tree. The possible values are 'mm' for
#'   millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm' for metres",
#'   or "code" if it is a code and not a measure (character)
#'
#' @param PlotArea The name of the column containing the area of the plot, in
#'   hectares (character), or the area of the plot in numeric
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
#'   metres", or "code" if it is a code and not a measure (character)
#'
#' @return Input inventory (data.frame) in the required package format.
#'
#' @export
#'
#' @importFrom data.table copy setDT setDF :=
#'
#' @examples
#'\dontrun{
#' data(ParacouSubsetWide)
#' Required_Format <- RequiredFormat(
#'   ParacouSubsetWide,
#'   Plot = "SubPlot",
#'   SubPlot = "SubSubPlot",
#'   Time = c("2016","2017","2018","2019","2020"),
#'   TreeFieldNum = "TreeFieldNum",
#'   IdTree = "idTree",
#'   LifeStatus = "CodeAlive",
#'   Size = "Circ",
#'   SizeUnit = "m",
#'   POM = "MeasCode",
#'   POMUnit = "code",
#'   PlotArea = "PlotArea",
#'   X = "Xutm",
#'   Y = "Yutm",
#'   ScientificName = NULL,
#'   VernName = "VernName",
#'   Family = "Family",
#'   Genus = "Genus",
#'   Species = "Species",
#'   CommercialSp = "CommercialSp",
#'   TreeHeight = NULL,
#'   TreeHeightUnit = NULL)
#'                }
#'
RequiredFormat <- function(
  Data,
  Plot = NULL,
  SubPlot = NULL,
  Time = NULL,
  TreeFieldNum = NULL,
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

  #### Check arguments ####

  args <- c(Plot, SubPlot, Time, TreeFieldNum, IdTree, LifeStatus,
            Size, SizeUnit, POM, POMUnit, X, Y,
            ScientificName, VernName, Family, Genus, Species, CommercialSp,
            TreeHeight, TreeHeightUnit)
  # a value of NULL deletes the corresponding item of the list.

  argsCol <- args[!args %in% c(SizeUnit, POMUnit, TreeHeightUnit)] # columns names arguments
  argsUnit <- args[args %in% c(SizeUnit, POMUnit, TreeHeightUnit)] # units arguments

  ## Argument class

  if (!inherits(Data, "data.frame"))
    stop("Data must be a data.frame")

  if(!any(is.character(PlotArea) || is.numeric(PlotArea)))
    stop(cat(PlotArea, "must be in character or numeric"))

  if(is.character(PlotArea))
    argsCol <- c(argsCol, PlotArea) # PlotArea is a column name


  for(i in 1:length(args)) # all args
    if(!inherits(args[i], "character"))
      stop(paste0(args[i], "must be in character"))

  ## Units

  if(length(argsUnit) > 0){
    for(i in 1:length(argsUnit))
      if(!argsUnit[i] %in% c("code", "mm", "millimetre", "millimeter", "milimetro", "milimetrica",
                             "cm", "centimetre", "centimeter", "centimetro",
                             "dm", "decimetre", "decimeter", "decimetro",
                             "m", "metre", "meter", "metro")
      )
        stop("Please indicate units with 'mm' for millimetres, 'cm' for
          centimetres, 'dm' for decimetres and 'm' for metres")
  }

  ## The column name exists

  for(i in 1:length(argsCol))
    if(!argsCol[i] %in% names(Data))
      stop(paste0(argsCol[i]," is not a column name of your dataset"))

  # if the variable exists but the unit is NULL
  if(is.null(SizeUnit) & !is.null(Size))
    SizeUnit <- readline(cat("In what unit is your variable '", Size, "'?")) # question to the user


  if(is.null(POMUnit) & !is.null(POM))
    POMUnit <- readline(cat("In what unit is your variable '", POM, "'?")) # question to the user. "code" is a possible answer


  if(is.null(TreeHeightUnit) & !is.null(TreeHeight))
    TreeHeightUnit <- readline(cat("In what unit is your variable '", TreeHeight, "'?")) # question to the user


  #### Objects renaming to avoid conflict ####

  # Plot_col = Plot
  # SubPlot_col = SubPlot
  # Time_col = Time
  # IdTree_col = IdTree
  # LifeStatus_col = LifeStatus
  # Size_col = Size
  # POM_col = POM
  # PlotArea_col = PlotArea
  # X_col = X
  # Y_col = Y
  # ScientificName_col = ScientificName
  # VernName_col = VernName
  # Family_col = Family
  # Genus_col = Genus
  # Species_col = Species
  # CommercialSp_col = CommercialSp
  # TreeHeight_col = TreeHeight
  # rm(Plot, SubPlot, Time, IdTree, LifeStatus, Size, POM, X, Y,
  #    ScientificName, VernName, Family, Genus, Species, CommercialSp,
  #    TreeHeight)

  # argsName <- c("Plot", "SubPlot", "Time", "IdTree", "LifeStatus", "Size", "POM", "X", "Y",
  #           "ScientificName", "VernName", "Family", "Genus", "Species", "CommercialSp",
  #           "TreeHeight")
  #
  # for(c in 1:length(argsCol)){
  #   for(n in 1:length(argsName)){
  #     if(exists(quote(eval(argsName[1]))))
  # .GlobalEnv[[paste0(argsName[n],"_col")]] <- argsCol[c]
  #   }
  # }

  #### Formatting ####

  # QUOI FAIRE AC LES ARGS NULLS ?
  # QUOI FAIRE AC LES ARGS = vecteurs ?
  for(c in 1:length(argsCol)){
  if(is.null(argsCol[c]))
    argsCol[c] <- "NULL"
  }
  # environment
  env <- lapply(list(.Plot = Plot,
                     .SubPlot = SubPlot,
                     .Time = Time,                      # if it's a vector as.name keeps only the first column name
                     .IdTree = IdTree,
                     .LifeStatus = LifeStatus,
                     .Size = Size,
                     .POM = POM,
                     .PlotArea = PlotArea,
                     .X = X,
                     .Y = Y,
                     # .ScientificName = "NULL", # doesn't work if the value is NULL
                     # .VernName = VernName,            # doesn't work if the value is NULL
                     .Family = Family,
                     .Genus = Genus,
                     .Species = Species,
                     .CommercialSp = CommercialSp
                     # .TreeHeight = TreeHeight         # doesn't work if the value is NULL
  ), as.name) # refer to R object by their name

  DataInput <- copy(Data) # input data copy

  ## data.frame to data.table
  setDT(Data) # with "set" "<-" is not necessary

  eval(substitute(
    {

  ## Wide to long format (detect and change)
  # detect : arguments pour lesquels un vecteur est renseigné

  # melt(Data,
  #      id.vars = c("col1", "col2"), # columns that remain columns (arguments pour lesquels il n'ya qu'1 val renseignée)
  #      measure.vars = patterns("^a", "^b"), # cols to rows (arguments pour lesquels un vecteur est renseigné)
  #      variable.name = "Time", # name of the new column that contains the names of the transposed variables
  #      value.name = "Size") # name of the new column that contains the values of the transposed variables

  ## Class changing

  ### if it's a code
  # for(u in 1:length(argsUnit)){ # argsUnit: SizeUnit, POMUnit, TreeHeightUnit
  #   if(argsUnit[u] == "code"){
  #
  #   }
  # }

  ### as.character
  CharacVar <- c(.Plot, .SubPlot, .TreeFieldNum, .IdTree, .ScientificName, .VernName, .Family, .Genus, .Species) # character variables

  for(v in 1:length(CharacVar))
    Data[, c(CharacVar[v]) := as.character(CharacVar[v])]

  ### as.numeric
  NumVar <- c(.Time, .Size, .PlotArea, .X, .Y, .TreeHeight) # numeric variables

  for(v in 1:length(NumVar))
    Data[, c(NumVar[v]) := as.numeric(NumVar[v])]

  ### as.logical
  LogicVar <- c(.LifeStatus, .CommercialSp) # logical variables

  for(v in 1:length(LogicVar))
    Data[, c(LogicVar[v]) := as.logical(LogicVar[v])]

  ## Units changing
  ### Size in cm
  if(.Size %in% names(Data)){

    if (substr(SizeUnit, 1, 2) == "mm" | substr(SizeUnit, 1, 2) == "mi")
      Data[, c(Size) := .Size/10] # mm -> cm

    if (substr(SizeUnit, 1, 1) == "d")
      Data[, c(Size) := .Size*10] # dm -> cm

    if (substr(SizeUnit, 1, 1) == "m")
      Data[, c(Size) := .Size*100] # m -> cm
  }

  ### TreeHeight in m
  if(TreeHeight %in% names(Data)){

    if (substr(TreeHeightUnit, 1, 2) == "mm" | substr(TreeHeightUnit, 1, 2) == "mi")
      Data[, c(TreeHeight) := .TreeHeight/1000] # mm -> m

    if (substr(TreeHeightUnit, 1, 1) == "m")
      Data[, c(TreeHeight) := .TreeHeight/100] # cm -> m


    if (substr(TreeHeightUnit, 1, 1) == "d")
      Data[, c(TreeHeight) := .TreeHeight/10] # dm -> m
  }

  ## Necessary columns creation from the existing

  ### IdTree (unique along Plot, SubPlot, TreeFieldNum)
  # Data[,  idTree := NULL]

  if(!IdTree %in% names(Data) & c(Plot, SubPlot, TreeFieldNum) %in% names(Data)){

    uniq_key <- unique(Data[, c(.Plot, .SubPlot, .TreeFieldNum)]) # , with = FALSE

    uniq_key[, IdTree := seq(1, nrow(uniq_key))]

    merge(Data, uniq_key)
  }

  ### POM ? (if pom is a code)

  ### PlotArea (not a column but a value)
  if(!PlotArea %in% names(Data) & is.numeric(PlotArea)){

    if(is.numeric(PlotArea)){ # if PlotArea is a (1) (numeric value) (cas à faire : 1 val par plot)
      Data[,  PlotArea := PlotArea]
    }
  }

  ### Genus Species (if ScientificName exists) (how to know the sep?)
  Data[, c("Genus", "Species") := tstrsplit(ScientificName, ".", fixed = TRUE)]

  ### ScientificName (if Genus & Species exist)
  Data[, ScientificName := paste(.Genus, .Species, sep = "_")]

    }, env)) # eval(substitute( END

  # Return in data.frame
  setDF(Data)
  return(Data)

}
