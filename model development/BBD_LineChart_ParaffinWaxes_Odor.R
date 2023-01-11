###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
################### Box-Behnken Predicted vs. Observed ########################
###############################################################################

# Loading Packages

library(ggplot2)
library(ggpmisc)

# Creating dataset

bbd_data <- data.frame(
  observed = c(0.05679, 0.111667, 0.05456288, 0.0695383, 0.105074, 0.1244, 
               0.0580481, 0.0492514, 0.0918902, 0.0831786, 0.0539537, 0.0876625,
               0.0808236, 0.0825085, 0.0898495, 0.0641407, 0.0892545, 0.0922287),
  adjusted = c(0.0495423, 0.103849, 0.0542285, 0.07679, 0.105474, 0.124565, 
               0.0586143, 0.0570693, 0.0995423, 0.0826123, 0.0537878, 0.0800104,
               0.0831342, 0.0831342, 0.0831342, 0.0831342, 0.0831342, 0.0831342)
)

# Linea chart

ggplot(data = bbd_data, aes(x = observed, y = adjusted)) +
  geom_point(alpha = 0.3) +
  labs(x = "Observed Euclidean distance", y = "Adjusted Euclidean distance", title = "") +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_poly_eq(aes(label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")", sep = "")), 
               label.x.npc = "left", label.y.npc = "top",
               formula = y ~ x, parse = TRUE, size = 4, rr.digits = 4, 
               geom = "label_npc", label.size = 0)
