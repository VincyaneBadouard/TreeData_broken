# # Plot data form
# library(tidyverse)
# library(ggplot2)
#
# data = data.frame(year = 1982:2010)
#
# # sigmoid trajectory
# data$dbhc = 40/(exp(0.3*(1982-data$year) + 3) + 1)
#
# # 2 shifts: each corresponds to a 5 cm decrease
# data$dbh = data$dbhc - c(rep(0, 10), rep(5, 10), rep (10, 9))
#
#
# ggplot(data, aes(x=year)) +
#   geom_point(aes(y=dbh), col = 2)+
#   geom_point(aes(y=dbhc)) +
#   geom_vline(xintercept = c(1992, 2002), lty = 2) +
#
#   geom_segment(x = 1982, xend = 1992, y =  25, yend = 25,
#                arrow = arrow(ends = "both", length = unit(0.4, "cm")))+ # la fleche
#   annotate(geom = "text", x = 1986, y =  26, label = "sequence 1")+ # le texte
#
#   geom_segment(x = 1992, xend = 2002, y =  12, yend = 12,
#                arrow = arrow(ends = "both", length = unit(0.4, "cm")))+
#   annotate(geom = "text", x = 1996, y =  13, label = "sequence 2")+
#
#   geom_segment(x = 2002, xend = 2010, y =  20, yend = 20,
#                arrow = arrow(ends = "both", length = unit(0.4, "cm")))+
#   annotate(geom = "text", x = 2006, y =  21, label = "sequence 3") +
#   theme_classic()
#
# ggsave("illus_double_shift.pdf", height = 4, width = 6)
#
# # Test models
# # Paracou6 <- Guyafor2df(UID = "Vincyane.Badouard", PWD = "Vincyane973+", Driver = "SQL Server",
# #                                              WHERE = "Forest='Paracou' AND Plot='6'") # Data import
# load("D:/VSC Kourou/DATA/Guyafor_Bota.RData")
#
# BigInd <- Guyafor %>%
#   filter(Forest == "Paracou") %>%
#   filter(Plot == "6") %>%
#   group_by(idTree) %>%
#   filter(n() > 30) %>%
#   mutate(DBH = CircCorr/pi) %>%
#   # filter(min(DBH) < 20) %>%
#   filter(idTree == 100904) %>% # big tree: 100904, small tree: 100687, medium: 100818
#   collect()
#
# MedInd <- Guyafor %>%
#   filter(Forest == "Paracou") %>%
#   filter(Plot == "6") %>%
#   group_by(idTree) %>%
#   filter(n() > 30) %>%
#   mutate(DBH = CircCorr/pi) %>%
#   # filter(min(DBH) < 20) %>%
#   filter(idTree == 100818) %>% # big tree: 100904, small tree: 100687, medium: 100818
#   collect()
#
# SmallInd <- Guyafor %>%
#   filter(Forest == "Paracou") %>%
#   filter(Plot == "6") %>%
#   group_by(idTree) %>%
#   filter(n() > 30) %>%
#   mutate(DBH = Circ/pi) %>% # CircCorr
#   # filter(min(DBH) < 20) %>%
#   filter(idTree == 100824) %>% # big tree: 100904, small tree: 100687, medium: 100818
#   collect()
#
# ggplot(SmallInd, aes(CensusYear, DBH)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = F, aes(color = "linear")) +
#   geom_smooth(method = "lm", formula =y ~ poly(x, 2), se = F, aes(color = "poly2")) +
#   geom_smooth(method = "lm", formula =y ~ log(x), se = F, aes(color = "log"))
# # log et linear confondu
# # poly2
#
# ggplot(MedInd, aes(CensusYear, DBH)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = F, aes(color = "linear")) +
#   geom_smooth(method = "lm", formula =y ~ poly(x, 2), se = F, aes(color = "poly2")) +
#   geom_smooth(method = "lm", formula =y ~ log(x), se = F, aes(color = "log"))
#
# ggplot(BigInd, aes(CensusYear, DBH)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = F, aes(color = "linear")) +
#   geom_smooth(method = "lm", formula =y ~ poly(x, 2), se = F, aes(color = "poly2")) +
#   geom_smooth(method = "lm", formula =y ~ log(x), se = F, aes(color = "log"))
#
