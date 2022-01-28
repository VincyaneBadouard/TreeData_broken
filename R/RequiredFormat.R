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
#' @details The argument keeping the value "none" indicates that the user does
#'   not have the requested information.
#'
#' @return Input inventory (data.frame) in the required package format.
#'
#' @export
#'
#' @importFrom data.table copy setDT setDF melt tstrsplit :=
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
#'   ScientificName = "none",
#'   VernName = "VernName",
#'   Family = "Family",
#'   Genus = "Genus",
#'   Species = "Species",
#'   CommercialSp = "CommercialSp",
#'   TreeHeight = "none",
#'   TreeHeightUnit = "none")
#'                }
#'
RequiredFormat <- function(
  Data,
  Plot = "none",
  SubPlot = "none",
  Time = "none",
  TreeFieldNum = "none",
  IdTree = "none",
  LifeStatus = "none",
  Size = "none",
  SizeUnit = "none",
  POM = "none",
  POMUnit = "none",
  PlotArea = "none",
  X = "none",
  Y = "none",
  ScientificName = "none",
  VernName = "none",
  Family = "none",
  Genus = "none",
  Species = "none",
  CommercialSp = "none",
  TreeHeight = "none",
  TreeHeightUnit = "none"
){

  #### Check arguments ####

  args <- c(Plot, SubPlot, Time, TreeFieldNum, IdTree, LifeStatus, # arguments VALUES
            Size, SizeUnit, POM, POMUnit, X, Y,
            ScientificName, VernName, Family, Genus, Species, CommercialSp,
            TreeHeight, TreeHeightUnit)
  # a value of NULL deletes the corresponding item of the list.


  argsCol <- c(Plot, SubPlot, Time, TreeFieldNum, IdTree, LifeStatus, # columns names arguments
               Size, POM, X, Y, ScientificName, VernName, Family, Genus,
               Species, CommercialSp, TreeHeight)
  argsUnit <-  c(SizeUnit, POMUnit, TreeHeightUnit) # units arguments

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

  ## The column name exists

  for(i in 1:length(argsCol))
    if(!argsCol[i] %in% names(Data) & argsCol[i] != "none")
      stop(paste0(argsCol[i]," is not a column name of your dataset"))

  # if the variable exists but the unit is not indicated
  if(SizeUnit == "none" & Size != "none")
    SizeUnit <- readline(cat("In what unit is your variable '", Size,"'?")) # question to the user


  if(POMUnit == "none" & POM != "none")
    POMUnit <- readline(cat("In what unit is your variable '", POM,"'?")) # question to the user. "code" is a possible answer


  if(TreeHeightUnit == "none" & TreeHeight != "none")
    TreeHeightUnit <- readline(cat("In what unit is your variable '", TreeHeight,"'?")) # question to the user

  ## Units

  if(length(argsUnit) > 0){
    for(i in 1:length(argsUnit))
      if(!argsUnit[i] %in% c("none", "code", "mm", "millimetre", "millimeter", "milimetro", "milimetrica",
                             "cm", "centimetre", "centimeter", "centimetro",
                             "dm", "decimetre", "decimeter", "decimetro",
                             "m", "metre", "meter", "metro")
      )
        stop("Please indicate units with 'mm' for millimetres, 'cm' for
          centimetres, 'dm' for decimetres and 'm' for metres")
  }

  #### Formatting ####

  # QUOI FAIRE AC LES ARGS = vecteurs ?

  # Environment
  # list with new names for the object in arguments (to avoid conflict and work with user variables names)
  env <- lapply(list(.Plot = Plot,
                     .SubPlot = SubPlot,
                     .Time = Time,    # if it's a vector as.name keeps only the first column name
                     .TreeFieldNum = TreeFieldNum,
                     .IdTree = IdTree,
                     .LifeStatus = LifeStatus,
                     .Size = Size,
                     .POM = POM,
                     .PlotArea = PlotArea,
                     .X = X,
                     .Y = Y,
                     .ScientificName = ScientificName,
                     .VernName = VernName,
                     .Family = Family,
                     .Genus = Genus,
                     .Species = Species,
                     .CommercialSp = CommercialSp,
                     .TreeHeight = TreeHeight
  ), as.name) # refer to R object by their name. Doesn't work if the value is NULL

  DataInput <- copy(Data) # input data copy

  ## data.frame to data.table
  setDT(Data) # with "set" "<-" is not necessary

  eval(substitute( # use of argument values as variables
    {

      ## Wide to long format (detect and reshape)
      # Detects arguments with multiple values

      # pata = list(a = 2, b = c(9,5,6)) # list
      # length(unlist(pata["b"])) # 3
      # length(pata$a) # 1
      # length(pata$b) # 3

      ColsList <- list(Plot=Plot, SubPlot=SubPlot, Time=Time, # arguments names and values
                       TreeFieldNum=TreeFieldNum, IdTree=IdTree,
                       LifeStatus=LifeStatus, Size=Size, POM=POM,
                       X=X, Y=Y, ScientificName=ScientificName, VernName=VernName,
                       Family=Family, Genus=Genus, Species=Species,
                       CommercialSp=CommercialSp, TreeHeight=TreeHeight)

      ArgsNames <- c("Plot", "SubPlot", "Time", "TreeFieldNum", "IdTree", # arguments names
                     "LifeStatus", "Size", "POM", "X", "Y", "ScientificName",
                     "VernName", "Family", "Genus", "Species", "CommercialSp", "TreeHeight")

      ColToTranspos_argname <- vector("character") # empty vectors

      for (N in 1:length(ArgsNames)) {

        if(length(unlist(ColsList[ ArgsNames[[N]] ])) > 1) # arg = multiple values
          ColToTranspos_argname <- c(ColToTranspos_argname, ArgsNames[[N]] )
      }

      # User col names in character (=argument values)
      ColToTranspos <- unlist(ColsList[ColToTranspos_argname], use.names = FALSE)
      ColToTranspos <- ColToTranspos[!ColToTranspos %in% "none"]

      if(length(ColToTranspos_argname) == 1){

        ValuesColName <- readline(cat("To which variable do the values in columns '", ColToTranspos,"' correspond?
1: Size\n
2: POM\n
3: TreeHeight\n")) # question to the user
        if(ValuesColName == "1") ValuesColName <- "Size"
        if(ValuesColName == "2") ValuesColName <- "POM"
        if(ValuesColName == "3") ValuesColName <- "TreeHeight"


        # Work only with 1 col to create from the wide format columns
        Data <- data.table::melt(Data,
                                 measure.vars = ColToTranspos, # cols to rows (arguments pour lesquels un vecteur est renseigné)
                                 variable.name = ColToTranspos_argname, # name of the new column (length=1) that contains the names of the transposed variables
                                 value.name = ValuesColName) # name of the new column that contains the values of the transposed variables

        if(ColToTranspos_argname == "Time"){
          Time <- ColToTranspos_argname
        }else{
          stop(cat("Create the cases where 'Time' is not the variable to transpose"))
        }

      } # ColToTranspos_argname == 1

      ## Class changing

      ### if it's a code
      # for(u in 1:length(argsUnit)){ # argsUnit: SizeUnit, POMUnit, TreeHeightUnit
      #   if(argsUnit[u] == "code"){
      #
      #   }
      # }

      ### as.character
      CharacVar <- c(Plot, SubPlot, TreeFieldNum, IdTree, ScientificName, VernName, Family, Genus, Species) # character variables
      CharacVar <- CharacVar[!CharacVar %in% "none"]

      Data[, (CharacVar) := lapply(.SD, as.character), .SDcols = CharacVar] # (CharacVar) to say that these are existing columns and not new ones to create

      ### as.numeric
      NumVar <- c(Time, Size, PlotArea, X, Y, TreeHeight) # numeric variables
      NumVar <- NumVar[!NumVar %in% "none"]

      Data[, (NumVar) := lapply(.SD, as.character), .SDcols = NumVar] # first as c haracter when the variable is in factor, to preserve writed information
      Data[, (NumVar) := lapply(.SD, as.numeric), .SDcols = NumVar] # () to say that these are existing columns and not new ones to create

      ### as.logical
      LogicVar <- c(LifeStatus, CommercialSp) # logical variables
      LogicVar <- LogicVar[!LogicVar %in% "none"]

      Data[, (LogicVar) := lapply(.SD, as.logical), .SDcols = LogicVar] # () to say that these are existing columns and not new ones to create

      ## Units changing
      ### Size in cm

      if(Size %in% names(Data)){

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

      if(!IdTree %in% names(Data) & all(c(Plot, SubPlot, TreeFieldNum) %in% names(Data))){

        uniq_key <- unique(Data[, c(Plot, SubPlot, TreeFieldNum)])

        uniq_key[, IdTree := seq(1, nrow(uniq_key))]

        merge(Data, uniq_key)

        IdTree <- "IdTree"
      }

      ### POM ? (if pom is a code)

      ### PlotArea (not a column but a value)
      if(!PlotArea %in% names(Data) & is.numeric(PlotArea)){

        if(length(PlotArea) == 1){ # if PlotArea is a (1) numeric value
          Data[,  PlotArea := PlotArea]
          PlotArea <- "PlotArea"

        }
        # if(length(PlotArea) > 1){ # cas à faire : c(1 val par plot)
        #   Data[,  PlotArea := PlotArea, by = Plot] # grouped
        #   Data[,  PlotArea := PlotArea, by = .(Plot)] # roup rows by values in specified column
        #
        #   Data[Plot == Plot1,  PlotArea := PlotArea1] # subset
        # }
      }

      ### Genus Species (if ScientificName exists) (detect or ask the sep?)


      if(!all(c(Genus, Species) %in% names(Data)) & ScientificName %in% names(Data)){ # or c(Genus, Species) == "none"

        # Ask the sep
        SfcnameSep <- readline(cat(
          "What is the separator (., _, , etc) between the genus and the species in '", ScientificName,"'?")) # question to the user

        Data[, c("Genus", "Species") := tstrsplit(ScientificName, SfcnameSep, fixed = TRUE)]

        Genus <- "Genus"
        Species <- "Species"
      }


      ### ScientificName (if Genus & Species exist)
      if(!ScientificName %in% names(Data) & all(c(Genus, Species) %in% names(Data))){ # or ScientificName == "none"
        Data[, ScientificName := paste(.Genus, .Species, sep = "_")]
      ScientificName <- "ScientificName"
}
    }, env)) # eval(substitute( END


  # Return in data.frame
  setDF(Data)
  return(Data)

}
