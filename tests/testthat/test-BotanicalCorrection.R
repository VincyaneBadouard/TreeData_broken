test_that("BotanicalCorrection", {


  # Create test data ------------------------------------------------------------------------------------------------------

  # Fabaceae Dicorynia guianensis (angelique) "a"
  # Lecythidaceae Eschweilera sagotiana (maho noir) "b"
  # Chrysobalanaceae Licania alba (koko) "c"
  # Fabaceae Eperua falcata (wapa) "d"
  Data <- data.table(Site = "Nowhere",
                     IdTree = c(rep("a", 4), rep("b", 4), rep("c", 4), rep("d", 4), rep("e", 1), rep("f", 1), rep("g", 1)), # 7 ind
                     Year = c(rep(c(2000:2003), 4), rep(2000, 3)) # 4 years each
  )
  Data <- Data[order(IdTree, Year)]
  Data[, Family := c(rep("Fabaceae", 4), rep("Lecythidaceae", 4), rep("Chrysobalanaceae", 4), rep("Fabaceae", 4), rep("Sapindaceae", 1), rep("Clusiaceae", 1), rep("Burseraceae", 1))]
  Data[, Genus := c(rep("Dicorynia", 4), rep("Eschweilera", 4), rep("Licania", 4), rep("Eperua", 4), rep("Indet.Sapindaceae", 1), rep("Tovomita", 1), rep("Protium", 1))]
  Data[, Species := c(rep("guianensis", 4), rep("sagotiana", 4), rep("alba", 4), rep("falcata", 4), rep("Indet.", 1), rep("sp.5-CAY", 1), rep("opacum_subsp.rabelianum", 1))]
  Data[, VernName := c(rep("angelique", 4), rep("maho noir", 4), rep("koko", 4), rep("wapa", 4), rep(NA, 3))]

  # Scientific-vernacular names match table (data.frame) : ScientificVernaTable
  ScientificVernaTable <- copy(Data)
  ScientificVernaTable <- unique(ScientificVernaTable[, list(Genus, Species, VernName)])

  # Create errors

  ## Missing value in  Family, ScientificName/Genus, species, VernName
  Data[IdTree %in% "d" & Year %in% 2000, ("Family") := NA_character_]
  Data[IdTree %in% "b" & Year %in% 2000, ("Genus") := NA_character_]
  Data[IdTree %in% "b", ("Family") := NA_character_]
  Data[IdTree %in% "c" & Year %in% 2000, ("Species") := NA_character_]
  Data[IdTree %in% "a" & Year %in% 2000, ("VernName") := NA_character_]
  Data[IdTree %in% "d" & Year %in% 2000, ("VernName") := NA_character_]




  ## Special characters
  Data[IdTree %in% "a", ("Family") := "Fabacé"] # good answer: "Fabaceae"
  Data[IdTree %in% "c" & Year %in% 2003, ("Genus") := "Licanï_a"] # good answer: "Licania"

  ## Variant botanical informations per IdTree
  Data[IdTree %in% "d" & Year %in% 2002, ("Species") := "grandi!flora"] # good answer: "falcata"

  ## Family name in the genus/species columns
  Data[IdTree %in% "b", ("Species") := "Lecythidaceae"] # good answer: "sagotiana"

  ## Family & Scientific names unmatch
  Data[IdTree %in% "c", ("Family") := "Lecythidaceae"] # good answer: "Chrysobalanaceae"

  ## Scientific & vernacular names unmatch
  Data[IdTree %in% "d" & Year %in% 2001, ("VernName") := "leaf"]  # good answer: "wapa"

  ## Old scientific name (A trouver)

  Data[, ScientificName := paste(Genus, Species)]

  # Create bad test data --------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(Data)

  # Check the function argument -------------------------------------------------------------------------------------------
  expect_error(BotanicalCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(BotanicalCorrection(Data, Source = TRUE),
               regexp = "must be NULL or a character vector")

  expect_error(BotanicalCorrection(Data, Source = "TRUE"),
               regexp = "should be one of")

  expect_error(BotanicalCorrection(Data, Source = "WFO", WFOData = NULL),
               regexp = "You must provide the 'WFOData' argument")

  expect_error(BotanicalCorrection(Data, Source = "TPL", DetectOnly = "TRUE"),
               regexp = "The 'DetectOnly' argument must be a logical")

  # Check the function work -----------------------------------------------------------------------------------------------
  options(warn = 2) # trace warning
  options(warn = 0) # when debug is over

  ## Detect Only: no correction, only comments (A FAIRE)----------------------------------------------------------------------------
  Rslt <- BotanicalCorrection(Data, Source = "TPL", DetectOnly = TRUE)

  # Correction
  Rslt <- BotanicalCorrection(Data, Source = "TPL")

  ## ScientificNameCor = GenusCor + SpeciesCor
  expect_true(all(Rslt$ScientificNameCor == paste(Rslt$GenusCor, Rslt$SpeciesCor)))

  ## No "aceae" in Genus or Species column --------------------------------------------------------------------------------
  expect_true(!any(grepl("aceae", Rslt$ScientificNameCor)))

  ## Family if Genus ------------------------------------------------------------------------------------------------------
  expect_true(all(!is.na(Rslt[!is.na(FamilyCor), GenusCor])))

  ## no special character in Genus and Family columns ---------------------------------------------------------------------
  expect_true(!any(grepl("[[:punct:]]", Rslt$GenusCor)))
  expect_true(!any(grepl("[[:punct:]]", Rslt$FamilyCor)))

  # Comment column ? (A FAIRE)
  # Source column ? (A FAIRE)
  # WFO ? (A FAIRE)

})

# Detect Only: no correction, only comments
# no "aceae" in Genus or Species column
# Family if Genus
# no special character in Genus and Family columns

# Comment column ?
# Source column ?
