###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
##################### Random Forest (RF) Classifier ###########################
###############################################################################

# Loading Packages

library(doParallel)
library(readxl)
library(caret)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(rlang)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/HS-GC-MS/Desarrollo método TIS/Validación/Parafinas Macro/hsgcms_macrowax_dataset.xlsx", 
                      sheet = "pw_hsgcms_classification")

pw_ht <- as.data.frame(pw_data[,-c(1:3)])
pw_ht$Odor <- as.factor(pw_data$Odor)

# Data partition

set.seed(34221)

intrain <- createDataPartition(y = pw_ht$Odor, 
                               p = 0.7, 
                               list = FALSE)
pw_train <- pw_ht[intrain,]
pw_test <- pw_ht[-intrain,]

# Hyperparameters tuning and model training

set.seed(34221)

trctrl <- trainControl(method = "cv", number = 5)

rf_mtry <- expand.grid(.mtry = c(sqrt(ncol(pw_train[,-length(pw_train)]))))

ntrees <- c(seq(2,100,2))
params <- expand.grid(ntrees = ntrees)

store_maxnode <- vector("list", nrow(params))

set.seed(34221)

start_time_1 <- Sys.time()

for(i in 1:nrow(params)){
  ntree <- params[i,1]
  set.seed(34221)
  rf_model <- train(Odor ~., 
                    data = pw_train,
                    method = 'rf',
                    metric = 'Accuracy',
                    tuneGrid = rf_mtry,
                    trControl = trctrl,
                    ntree = ntree,
                    scale = FALSE)
  store_maxnode[[i]] <- rf_model
}

names(store_maxnode) <- paste("ntrees:", params$ntrees)

rf_results <- resamples(store_maxnode)
rf_results

lapply(store_maxnode, 
       function(x) x$results[x$results$Accuracy == max(x$results$Accuracy),])

total_time_1 <- Sys.time() - start_time_1
total_time_1

# Accuracy vs ntrees

rf_plot <- cbind.data.frame(Accuracy = colMeans(rf_results$values[,c(seq(2,100,2))]),
                            Ntrees = c(seq(2,100,2)))
rf_plot <- as.vector(rf_plot)
accuracy <- (rf_plot$Accuracy)*100
ntrees <- rf_plot$Ntrees

plot(x = ntrees,
     y = accuracy,
     type = "b", 
     pch = 19, 
     lty = 2,
     col = "#1F51FF",
     xlab = "Number of decision trees",
     ylab = "Accuracy (%) (5-Fold CV)")

# Final RF model

set.seed(34221)

start_time_2 <- Sys.time()

best_rf <- train(Odor ~., 
                 data = pw_train,
                 method = 'rf',
                 metric = 'Accuracy',
                 tuneGrid = rf_mtry,
                 trControl = trctrl,
                 ntree = 100,
                 scale = FALSE)

total_time_2 <- Sys.time() - start_time_2
total_time_2

best_rf
best_rf$finalModel

# Train set performance

training_error <- predict(best_rf, newdata = pw_train[,-length(pw_train)], type = "raw") 
cmatrix_training <- confusionMatrix(training_error,as.factor(pw_train$Odor))
cmatrix_training

#Test set performance

pred_model <- predict(best_rf, newdata = pw_test[,-length(pw_train)])

cmatrix <- table(prediction = pred_model, reference = pw_test$Odor)
cmatrix

prop.table(cmatrix)
round(prop.table(cmatrix,1)*100, 2)

cmatrix_test <- confusionMatrix(pred_model,as.factor(pw_test$Odor))
cmatrix_test

# Variable Importance

var_imp <- varImp(object = best_rf)

var_imp_1 <- var_imp[['importance']]

names <- rownames(var_imp_1)
rownames(var_imp_1) <- NULL

var_imp_2 <- cbind(names,var_imp_1)

varimp_plot <- plot(varImp(object = best_rf), 
                    top = 20,
                    col = c("#1F51FF"),
                    ylab = "m/z",
                    xlab = "Importance")

varimp_plot

sp_feat <- as.data.frame(var_imp_2[which(var_imp_2[,2] >= 70),])
sp_feat_1 <- sp_feat[,-2]

spfeatWithoutQuotes = c()

for (onespfeat in sp_feat_1) {
  onespfeat = str_replace_all(onespfeat, "`", "")
  spfeatWithoutQuotes = append(spfeatWithoutQuotes, onespfeat)
}

print(spfeatWithoutQuotes)

sp_selected <- which(names(pw_ht) %in% spfeatWithoutQuotes)
sp <- pw_ht[,c(sp_selected)]
sp$Odor <- pw_ht$Odor

# ANOVA for selected variables

spAsNumericColNames = lapply(spfeatWithoutQuotes, function (x) paste("mz", toString(x), sep = ""))

sp_1 <- sp %>% dplyr::select("Odor", everything())

sp_1$Odor <- factor(sp_1$Odor, levels = c("None", "Slight", "Moderate", "Strong", "Very Strong"))
colnames(sp_1)[2:ncol(sp_1)] <- spAsNumericColNames

formulae <- lapply(colnames(sp_1)[2:ncol(sp_1)], function(x) as.formula(paste0(x, " ~ Odor")))

res <- lapply(formulae, function(x) summary(aov(x, data = sp_1)))
names(res) <- format(formulae)
res

# Training a variable-reduced RF model

sp_selected_1 <- which(names(pw_train) %in% spfeatWithoutQuotes)
pw_train_1 <- pw_train[,c(sp_selected_1)]
pw_train_1$Odor <- pw_train$Odor

rf_mtry_1 <- expand.grid(.mtry = c(sqrt(ncol(pw_train_1[,-length(pw_train_1)]))))

set.seed(34221)

reduced_rf <- train(Odor ~., 
                    data = pw_train_1,
                    method = 'rf',
                    metric = 'Accuracy',
                    tuneGrid = rf_mtry_1,
                    trControl = trctrl,
                    ntree = 100)

reduced_rf
reduced_rf$finalModel

# Variable-reduced train set predictions

training_error_1 <- predict(reduced_rf, newdata = pw_train[,-length(pw_train_1)], type = "raw") 
cmatrix_training_1 <- confusionMatrix(training_error,as.factor(pw_train$Odor))
cmatrix_training_1

# Variable-reduced test set predictions

sp_selected_2 <- which(names(pw_test) %in% spfeatWithoutQuotes)
pw_test_1 <- pw_test[,c(sp_selected_2)]
pw_test_1$Odor <- pw_test$Odor

pred_model <- predict(reduced_rf, newdata = pw_test_1[,-length(pw_test_1)], type = "raw")

cmatrix <- table(prediction = pred_model, reference = pw_test_1$Odor)
cmatrix

prop.table(cmatrix)
round(prop.table(cmatrix,1)*100, 2)

cmatrix_test <- confusionMatrix(pred_model,as.factor(pw_test$Odor))
cmatrix_test

#Spectralprint

sp_mean <- aggregate(.~ Odor, sp, mean)

sp_nor <- as.data.frame(t(apply(sp_mean[2:9], 1, function(x) ((x/(max(x)))))))
sp_nor$Odor <- sp_mean$Odor

sp_nor$Odor <- factor(sp_nor$Odor,levels = c("None", "Slight", "Moderate", "Strong", "Very Strong"))

df <- reshape2::melt(sp_nor, "Odor")

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

ggplot(data = df, aes(x = variable, y = value, fill = Odor)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = cols, breaks = brk) +
  facet_wrap(~ Odor, ncol = 5) +
  xlab("m/z") + ylab("Normalized Intensity") +
  theme(legend.position = "none",
        axis.text = element_text(size = 8, hjust = .5, angle = 90),
        axis.title = element_text(size = 9, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_textbox_highlight(
          size = 10, face = "bold",
          fill = "white", box.color = "#363844", color = "#363844",
          halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
          padding = margin(5, 0, 3, 0), margin = margin(0, 1, 3, 1),
          hi.labels = c("None", "Slight", "Moderate", "Strong", "Very Strong"),
          hi.fill = "#363844", hi.box.col = "black", hi.col = "white")) 

# Stop Parallelization

stopCluster(cl)