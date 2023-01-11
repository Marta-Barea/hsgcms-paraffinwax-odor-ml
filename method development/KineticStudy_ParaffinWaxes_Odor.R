###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################### Marta Barea-Sepúlveda ###############################
###############################################################################

###############################################################################
########################### Kinetic Study Barplot #############################
###############################################################################

# Loading Packages

library(ggplot2)

# Creating dataset

kinetic_data <- data.frame(
  time = c(5, 10, 15, 20, 25),
  response = c(0.12143609, 0.108597497, 0.124324297, 0.125576953, 0.105328579),
  sd = c(0.007003828, 0.003808987, 0.007509958, 0.017181075, 0.016703001),
  highlight = c("1", "0", "0", "0", "0")
  )

# Barplot

ggplot(kinetic_data) +
  geom_bar(aes(x = time, y = response, fill = highlight), 
           stat = "identity", 
           alpha = 0.7,
           width = 1.5) +
  scale_fill_manual(values = c( "1" = "#4169E1", "0" =  "#87CEEB"), guide = "none") +
  labs(x = "Incubation time (min)", y = "Euclidean distance", title = "") +
  geom_errorbar(aes(x = time, ymin = response-sd, ymax = response+sd), 
                width = 0.5, colour = "dark blue", alpha = 0.9, size = 0.5) 
 
