###############################################################################
####### Application of a system based on Vis-NIRS and Machine Learning ########
################### for the analysis of petroleum waxes #######################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
#################### Principal Components Analysis (PCA) ######################
###############################################################################

# Loading Packages

library(doParallel)
library(readxl)
library(prospectr)
library(cluster)
library(factoextra)
library(data.table)
library(egg)
library(ggplot2)
library(ggrepel)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/HS-GC-MS/Desarrollo método TIS/Validación/Parafinas Macro/hsgcms_macrowax_dataset.xlsx", 
                      sheet = "pw_hsgcms_classification")

pw_ht <- as.data.frame(pw_data[,-c(1:3)])
pw_ht$Odor <- as.factor(pw_data$ID)

# PCA

pw_pca <- prcomp(pw_ht[,-length(pw_ht)], scale = FALSE)

# Visualizing PCA results

pw_pca
summary(pw_pca)

# Visualizing eigenvalues (scree plot)

fviz_eig(pw_pca,
         xlab = "Principal Components (PCs)",
         ylab = "Explained Variance (%)",
         main = "",
         addlabels = TRUE,
         ggtheme = theme_minimal(),
         barcolor = "#4682B4",
         barfill = "#4682B4",
         linecolor = "#000000")

# Score plot for PC1 and PC2

scores_pca <- cbind.data.frame(predict(pw_pca),
                               group = pw_data$Odor)

m_labels <- as.matrix(pw_data[,-1])
rownames(m_labels) <- pw_data$ID

cols <- c("#000080","#0F52BA","#4169E1","#87CEEB","#4682B4")
cols

scatter_plot <- ggplot(scores_pca, aes(x = PC1, y = PC2, col = group, label = rownames(m_labels))) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = 0.5) +
  geom_point(aes(color = group), alpha = 1, size = 3.75) +
  geom_label_repel(aes(label = rownames(m_labels), color = group), 
                   size = 5,
                   max.overlaps = 50, 
                   point.padding = 0.2,
                   nudge_y = 0.005, 
                   nudge_x = 0.02, 
                   show.legend = F) +
  labs(x = "PC1 (59.1%)", y = "PC2 (22.8%)", title = "") + 
  scale_color_manual(values = cols) +
  scale_shape_manual(values = c(15, 17)) +
  guides(color = guide_legend(title = "Odor grade")) +
  theme(axis.title = element_text(size = 10, face = "bold"), 
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10)) 

scatter_plot

# Loadings plot

loadings <- cbind.data.frame(pw_pca$rotation[,c(1,2)])
setDT(loadings, keep.rownames = TRUE)[]

ld <- melt(loadings, "rn")

loadings_plot <- ggplot(ld, aes(x = rn, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 2) + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 15, hjust = 1, angle = 90),
        axis.title = element_text(size = 15, face = "bold")) +
  labs(x = "Wavelength (nm)", y = "Loadings PCs", title = "") +
  scale_x_discrete(limits = loadings$rn,
                   breaks = loadings$rn[seq(1, length(loadings$rn), by = 5)]) +
  scale_fill_manual(values = c("#4169E1", "#87CEEB")) 

loadings_plot

# Stop Parallelization

stopCluster(cl)