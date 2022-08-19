#   - Check botanical identification (*BotanicalCorrection*)
#   - Check the life status evolution of the trees (*StatusCorrection*)
#   - Check diameter evolution of the trees (*DiameterCorrection*)
#   - Check recruitment (*RecruitmentCorrection*)

FullErrorProcessing <- function(

  Data,

  DetectOnly,

  ByStem = TRUE,

  # Botanical informations
  Source,
  WFOData = NULL,

  # Life status
  InvariantColumns = c("Site",
                       "Genus",
                       "Species",
                       "Family",
                       "ScientificName"),
  DeathConfirmation = 2,
  UseSize = FALSE,
  RemoveRBeforeAlive = FALSE,
  RemoveRAfterDeath = FALSE,

  # Taper
  DefaultHOM = 1.3,
  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB / (exp(- TaperParameter*(HOM - DefaultHOM))),

  # Diameter
  KeepMeas = c("MaxHOM", "MaxDate"),
  MaxDBH = 500,
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  Pioneers = NULL,
  PioneersGrowthThreshold = 7.5,

  TrustMeasSet = c("first", "last"),
  WhatToCorrect = c("POM change", "punctual", "shift"),
  CorrectionType = c("linear", "individual", "phylogenetic hierarchical"),

  DBHRange = 10,
  MinIndividualNbr = 5,
  Digits = 1L,

  DBHCorForDeadTrees = TRUE,

  # Recruitment
  MinDBH = 10
){

  #### General errors detection ####
  Data <- GeneralErrorsDetection(Data)

  #### Botanical informations ####

  Data <- BotanicalCorrection(Data = Data,
                              Source = Source,
                              WFOData = WFOData,
                              DetectOnly = DetectOnly)

  #### Life status ####

  Data <- StatusCorrection(Data,
                           InvariantColumns = InvariantColumns,
                           DeathConfirmation = DeathConfirmation,
                           UseSize = UseSize,
                           DetectOnly = DetectOnly,
                           RemoveRBeforeAlive = RemoveRBeforeAlive,
                           RemoveRAfterDeath = RemoveRAfterDeath)

  #### Taper ####

  Data <- TaperCorrection(Data,
                          DefaultHOM = DefaultHOM,

                          TaperParameter = TaperParameter,
                          TaperFormula = TaperFormula,

                          DetectOnly = DetectOnly)

  #### Diameter ####

  Data <- DiameterCorrection(Data,

                             KeepMeas = KeepMeas,

                             ByStem = ByStem,

                             DefaultHOM = DefaultHOM,
                             MaxDBH = MaxDBH,
                             PositiveGrowthThreshold = PositiveGrowthThreshold,
                             NegativeGrowthThreshold = NegativeGrowthThreshold,

                             Pioneers = Pioneers,
                             PioneersGrowthThreshold = PioneersGrowthThreshold,

                             TrustMeasSet = TrustMeasSet,
                             WhatToCorrect = WhatToCorrect,
                             CorrectionType = CorrectionType,

                             DBHRange = DBHRange,
                             MinIndividualNbr = MinIndividualNbr,
                             Digits = Digits,

                             DBHCorForDeadTrees = DBHCorForDeadTrees,

                             DetectOnly = DetectOnly)

  #### Recruitment ####

  Data <- RecruitmentCorrection(Data,
                                MinDBH = MinDBH,
                                PositiveGrowthThreshold = PositiveGrowthThreshold,
                                DetectOnly = DetectOnly
  )

  return(Data)

}
