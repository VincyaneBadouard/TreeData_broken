# library(ggplot2)
# ggplot(Rslt) +
#   aes(x = Year, y = LifeStatusCor) +
# Rslt[,LifeStatusCor := as.character(LifeStatusCor)]
# Rslt[,LifeStatus := as.character(LifeStatus)]
# Rslt[is.na(LifeStatusCor), LifeStatusCor := "NA"]
# Rslt[is.na(LifeStatus), LifeStatus := "NA"]
#
#
#
# ggplot(Rslt) +
#   aes(x = Year) +
#   geom_point(aes(y = LifeStatus), shape = "circle", size = 3.9, colour = "red") +
#   geom_line(aes(y = LifeStatus), colour = "red") +
#   geom_point(aes(y = LifeStatusCor), shape = "circle", size = 3.9, colour = "forestgreen") +
#   geom_line(aes(y = LifeStatusCor), colour = "forestgreen") +
#   theme_minimal() +
#   facet_wrap(vars(IdTree), scales = "free")
#
#
#
# ggplot(Rslt) +
#   aes(x = Year) +
#
#   # Initial
#   geom_point(aes(y = LifeStatus,
#                  color = ifelse(LifeStatus != LifeStatusCor, 'Initial', 'Conserved')),
#              shape = "circle", size = 3.9) +
#
#   geom_line(aes(y = LifeStatus, color = ifelse(LifeStatus != LifeStatusCor, 'Initial', 'Conserved'))) +
#
#
#   # Corrected
#   geom_line(aes(y = LifeStatusCor, color = ifelse(LifeStatus != LifeStatusCor, 'Corrected', 'Conserved'))) +
#   geom_point(aes(y = LifeStatusCor,
#                  color = ifelse(LifeStatus != LifeStatusCor | is.na(LifeStatus), 'Corrected', 'Conserved')),
#              shape = "circle", size = 3.9) +
#
#
#   # Colours
#   scale_colour_manual(name = "Status", values = c("Conserved" = "black",
#                                                   "Initial" = "red",
#                                                   "Corrected" = "forestgreen")) +
#   theme_minimal() +
#
#   # Titles
#   labs(
#     # title =  paste("IdStem: ",unique(Rslt$IdStem),""),
#     x = "Year", y = "LifeStatus")
#
#
