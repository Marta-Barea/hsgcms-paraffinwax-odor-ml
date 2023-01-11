###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
############# Support Vector Machine (SVM) with Gaussian Kernel ###############
###############################################################################

# Loading Packages

library(readxl)
library(doParallel)
library(caret)
library(dplyr)
library(graphics)

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

trctrl_1 <- trainControl(method = "cv", number = 5)
gridradial_1 <- expand.grid(sigma = c(2^(seq(-10, 10, 0.5))), 
                            C = c(2^(seq(-10, 10, 0.5))))
set.seed(34221)

start_time_1 <- Sys.time()

svm_radial <- train(Odor ~., 
                    data = pw_train,
                    method = "svmRadial",
                    trControl= trctrl_1,
                    tuneGrid = gridradial_1,
                    metric = "Accuracy",
                    scale = FALSE)

total_time_1 <- Sys.time() - start_time_1
total_time_1

svm_radial
svm_radial$finalModel
filter(svm_radial[['results']], 
       C == svm_radial[["bestTune"]][["C"]], 
       sigma == svm_radial[["bestTune"]][["sigma"]])

# Final SVM model

set.seed(34221)

trctrl_2 <- trainControl(method = "cv", number = 5)
gridradial_2 <- expand.grid(sigma = svm_radial[["bestTune"]][["sigma"]], 
                            C = svm_radial[["bestTune"]][["C"]])
set.seed(34221)

start_time_2 <- Sys.time()

best_svm <- train(Odor ~.,                         
                  data = pw_train,
                  method = "svmRadial",
                  trControl= trctrl_2,
                  tuneGrid = gridradial_2,
                  metric = "Accuracy",
                  scale = FALSE)
best_svm

total_time_2 <- Sys.time() - start_time_2
total_time_2

saveRDS(best_svm, "~/Documents/GitHub/machine-learning_HSGCMS_ParaffinWax_Odor/App/SVM.rds")

# Train set predictions

training_error <- predict(best_svm, newdata = pw_train[,-length(pw_train)], type = "raw") 
cmatrix_training <- confusionMatrix(training_error,as.factor(pw_train$Odor))
cmatrix_training

# Test set predictions

pred_model <- predict(best_svm, newdata = pw_test[,-length(pw_test)])

cmatrix <- table(prediction = pred_model, reference = pw_test$Odor)
cmatrix

prop.table(cmatrix)
round(prop.table(cmatrix,1)*100, 2)

cmatrix_test <- confusionMatrix(pred_model,as.factor(pw_test$Odor))
cmatrix_test

# Contour plot of the SVM model

accuracy_gridsearch <- matrix((svm_radial[["results"]][["Accuracy"]] * 100), ncol = 41, nrow = 41)

kappa_gridsearch <- matrix(svm_radial[["results"]][["Kappa"]], ncol = 41, nrow = 41)

cost_expression <- expression(log[2] ~ C)
gamma_expression <- expression(log[2] ~ σ)

filled.contour(x = c(seq(-10,10,length.out = 41)),
               y = c(seq(-10,10,length.out = 41)),
               z = as.matrix(accuracy_gridsearch),
               color.palette = colorRampPalette(c("blue", "purple", "red", "orange")), 
               plot.title = title(main = "", sub = "", xlab = cost_expression, ylab = gamma_expression),
               plot.axes = {axis(1,seq(-10, 10, 1),cex.axis = 1,las = 2)
                 axis(2,seq(-10, 10, 1),cex.axis = 1,las = 2)})

filled.contour(x = c(seq(-10,10,length.out = 41)),
               y = c(seq(-10,10,length.out = 41)),
               z = as.matrix(kappa_gridsearch),
               color.palette = colorRampPalette(c("blue", "purple", "red", "orange")), 
               plot.title = title(main = "", sub = "", xlab = cost_expression, ylab = gamma_expression),
               plot.axes = {axis(1,seq(-10, 10, 1),cex.axis = 1,las = 2)
                 axis(2,seq(-10, 10, 1),cex.axis = 1,las = 2)})

# Stop Parallelization

stopCluster(cl)