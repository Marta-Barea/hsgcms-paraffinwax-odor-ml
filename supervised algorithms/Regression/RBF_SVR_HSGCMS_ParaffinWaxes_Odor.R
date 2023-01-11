###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
###################### Support Vector Regression (SVR) ########################
######################### Radial Basis Function (RBF) #########################
###############################################################################

# Loading Packages

library(readxl)
library(caret)
library(prospectr)
library(data.table)
library(MLmetrics)
library(dplyr)
library(graphics)
library(doParallel)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/HS-GC-MS/Desarrollo método TIS/Validación/Parafinas Macro/hsgcms_macrowax_dataset.xlsx", 
                      sheet = "pw_hsgcms_regression")

pw_ht <- as.data.frame(pw_data[,-c(1:3)])
pw_ht$Grade <- as.numeric(pw_data$Grade)

# Data slicing

set.seed(1345)
intrain <- createDataPartition(y = pw_ht$Grade, 
                               p = 0.7, 
                               list = FALSE)
pw_train <- pw_ht[intrain,]
pw_test <- pw_ht[-intrain,]

# Hyperparameters tuning and model training

set.seed(1345)

trctrl_1 <- trainControl(method = "cv", number = 5)

grid_radial_1 <- expand.grid(sigma = c(2^(seq(-10, 10, 0.5))), 
                             C = c(2^(seq(-10, 10, 0.5))))

start_time <- Sys.time()

svr_radial <- train(Grade ~., 
                    data = pw_train,
                    method = "svmRadial",
                    trControl= trctrl_1,
                    tuneGrid = grid_radial_1,
                    metric = "RMSE", 
                    scale = FALSE)

svr_radial

total_time <- Sys.time() - start_time
total_time

svr_radial$finalModel

subset(svr_radial$results, 
       svr_radial$results$C==svr_radial[["bestTune"]][["C"]] & 
         svr_radial$results$sigma==svr_radial[["bestTune"]][["sigma"]])

# Final SVR model

set.seed(1345)

trctrl_2 <- trainControl(method = "cv", number = 5)
grid_radial_2 <- expand.grid(sigma = svr_radial[["bestTune"]][["sigma"]], 
                             C = svr_radial[["bestTune"]][["C"]])

best_svr <- train(Grade ~., 
                  data = pw_train,
                  method = "svmRadial",
                  trControl= trctrl_2,
                  tuneGrid = grid_radial_2,
                  metric = "RMSE",
                  scale = FALSE)
best_svr

saveRDS(best_svr, "~/Documents/GitHub/machine-learning_HSGCMS_ParaffinWax_Odor/App/SVR.rds")

# Train set predictions

train_pred <- predict(best_svr, newdata = pw_train[,-length(pw_train)])

mse_train = MSE(pw_train$Grade, train_pred)
mae_train = MAE(pw_train$Grade, train_pred)
rmse_train = RMSE(pw_train$Grade, train_pred)
r2_train = caret::R2(pw_train$Grade, train_pred)

cat("MAE:", mae_train, "\n", "MSE:", mse_train, "\n", 
    "RMSE:", rmse_train, "\n", "R-squared:", r2_train)

# Test set predictions

test_pred <- predict(best_svr, newdata = pw_test[,-length(pw_test)])

mse_test = MSE(pw_test$Grade, test_pred)
mae_test = MAE(pw_test$Grade, test_pred)
rmse_test = RMSE(pw_test$Grade, test_pred)
r2_test = caret::R2(pw_test$Grade, test_pred)

cat("MAE:", mae_test, "\n", "MSE:", mse_test, "\n", 
    "RMSE:", rmse_test, "\n", "R-squared:", r2_test)

# Predictions' plot

train_pred_matrix <- data.frame(Real = c(pw_train$Grade),
                                Predicted = c(train_pred))

test_pred_matrix <- data.frame(Real = c(pw_test$Grade),
                               Predicted = c(test_pred))

train_group_labels <- rep("Train set", times = 54)
train_pred_matrix <- cbind(train_pred_matrix, Group = train_group_labels)

test_group_labels <- rep("Test set", times = 21)
test_pred_matrix <- cbind(test_pred_matrix, Group = test_group_labels)


df <- rbind(train_pred_matrix, test_pred_matrix)

group <- as.factor(df$Group)

plot(x = df$Real,
     y = df$Predicted,
     type = "p",
     pch = c(8, 7)[factor(group)],
     col = "black",
     main = "",
     xlab = "Real (%)",
     ylab = "Predicted (%)",
     xlim = c(0,100))
lines(0:100, 0:100, lwd = 1, lty = 1, col = "#1F51FF")
legend(x = 5, y = 95, c("Test set", "Training set"), cex = 0.8, pch = c(8, 7))

# Contour plot of the SVR model

rmse_gridsearch <- matrix((svr_radial[["results"]][["RMSE"]]), ncol = 41, nrow = 41)

cost_expression <- expression(log[2] ~ C)
gamma_expression <- expression(log[2] ~ σ)

filled.contour(x = c(seq(-10,10,length.out = 41)),
               y = c(seq(-10,10,length.out = 41)),
               z = as.matrix(rmse_gridsearch),
               color.palette = colorRampPalette(c("red", "orange", "purple", "blue")), 
               plot.title = title(main = "", sub = "", xlab = cost_expression, ylab = gamma_expression),
               plot.axes = {axis(1,seq(-10, 10, 1),cex.axis = 1,las = 2)
                 axis(2,seq(-10, 10, 1),cex.axis = 1,las = 2)})

# Stop Parallelization

stopCluster(cl)