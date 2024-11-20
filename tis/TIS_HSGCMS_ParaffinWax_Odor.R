###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
######################## Total Ion Spectra (TIS) ##############################
###############################################################################

# Loading Packages

library(doParallel)
library(readxl)
library(data.table)
library(ggplot2)
library(ggtext)
library(rlang)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/HS-GC-MS/Desarrollo método TIS/Validación/Parafinas Macro/hsgcms_macrowax_dataset.xlsx", 
                      sheet = "pw_hsgcms_classification")

pw_ht <- as.data.frame(pw_data[,-c(1:4)])
pw_ht$Odor <- as.factor(pw_data$Odor)

# Total Ion Spectra (TIS) Plot

pw_ht_mean <- aggregate(.~ Odor, pw_ht, mean)

pw_ht_mean$Odor <- factor(pw_ht_mean$Odor,levels = c("None", "Slight", "Moderate", "Strong", "Very Strong"))

df <- reshape2::melt(pw_ht_mean, "Odor")

brk = c("None", "Slight", "Moderate", "Strong", "Very Strong")
cols = c("#87CEEB","#4682B4","#4169E1","#0F52BA","#000080")

element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
    element$family <- element$hi.family %||% element$family
  }
  NextMethod()
}

tis_plot <- ggplot(df, aes(x = variable, y = value, color = Odor)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.position = "none",
        axis.text = element_text(size = 6, hjust = .5, angle = 90),
        axis.title = element_text(size = 9, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_textbox_highlight(
          size = 10, face = "bold",
          fill = "white", box.color = "#363844", color = "#363844",
          halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(5, 0, 3, 0), margin = margin(0, 1, 3, 1),
          hi.labels = c("None", "Slight", "Moderate", "Strong", "Very Strong"),
          hi.fill = "#363844", hi.box.col = "black", hi.col = "white")) +
  labs(x = "m/z", y = "Normalized Intensity", title = "") +
  scale_color_manual(values = cols, breaks = brk) +
  facet_wrap(~ Odor, ncol = 5) +
  scale_x_discrete(limits = df$variable,
                   breaks = df$variable[seq(1, length(df$variable), by = 100)])

tis_plot

# Stop Parallelization

stopCluster(cl)