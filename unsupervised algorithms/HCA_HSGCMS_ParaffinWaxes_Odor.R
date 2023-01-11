###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
#################### Hierarchical Cluster Analysis (HCA) ######################
###############################################################################

# Loading Packages

library(doParallel)
library(readxl)
library(prospectr)
library(cluster)
library(purrr)
library(factoextra)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/HS-GC-MS/Desarrollo método TIS/Validación/Parafinas Macro/hsgcms_macrowax_dataset.xlsx", 
                      sheet = "pw_hsgcms_classification")

pw_ht <- as.matrix(pw_data[,-c(1:4)])
rownames(pw_ht) <- pw_data$ID

p <- pw_data[,-c(1:4)]
             
# Linkage methods to assess

m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")

#Compute coefficient

ac <- function(x) {
  agnes(pw_ht, method = x)$ac
}

# Print method and coefficient

map_dbl(m, ac)      

# Dissimilarity matrix

d <- dist(pw_ht, method = "euclidean")

# Hierarchical clustering 

hc1 <- hclust(d, method = "ward.D2")

#Generate colors from labels 

labels_cols_generator <- function(labels, order, colors = NULL) {
  result <- c()
  color_equivalence <- list()
  generator <- 1
  
  labels_sorted <- c()
  
  for (index in order) {
    labels_sorted <- c(labels_sorted, labels[[index]])
  }
  
  for (label in labels_sorted) {
    label = strsplit(label, '_')[[1]][1]
    if (is.null(color_equivalence[[label]])) {
      if (is.null(colors)) {
        color_equivalence[[label]] <- generator
      } else {
        color_equivalence[[label]] <- colors[[generator]]
      }
      generator = generator + 1
    }
    result <- c(result, color_equivalence[[label]])
  }
  
  result
}

labels_cols = labels_cols_generator(hc1$labels,
                                    hc1$order, 
                                    c("#000080","#0F52BA","#4169E1","#87CEEB","#4682B4"))

# Dendrogram

set.seed(5665)

dendrogram <- fviz_dend(x = hc1, 
                        show_labels = TRUE, 
                        cex = 0.7,
                        lwd = 0.6,
                        main = "",
                        xlab = "Samples",
                        ylab = "Height",
                        sub = "",
                        ggtheme = theme_classic(),
                        horiz = FALSE,
                        k = 2,
                        k_colors = c("#6082B6", "#A7C7E7"),
                        color_labels_by_k = TRUE,
                        label_cols = labels_cols,
                        type = "rectangle")

dendrogram

# Stop Parallelization

stopCluster(cl)