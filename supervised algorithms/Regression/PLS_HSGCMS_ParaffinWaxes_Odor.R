###############################################################################
######## Development of a novel HS/GC-MS method with machine learning #########
###### for intelligent and automatic characterization and quantification ######
################## of the odor grade of paraffin waxes ########################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
######################## Partial Least Squares (PLS) ##########################
###############################################################################

# Loading Packages

library(readxl)
library(caret)
library(prospectr)
library(MLmetrics)
library(data.table)
library(doParallel)
library(stringr)
library(ggplot2)

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

# Hyperparameter tuning and model training

set.seed(1345)

trctrl <- trainControl(method = "cv", 
                       number = 5,
                       returnResamp = "final", 
                       verboseIter = FALSE, 
                       savePredictions = "final")

start_time <- Sys.time()

pls_model <- train(Grade ~., 
                   data = pw_train,
                   trControl = trctrl,
                   method = "pls", 
                   metric = "RMSE",
                   tuneLength = 20,
                   scale = FALSE) 
pls_model

total_time <- Sys.time() - start_time
total_time

summary(pls_model$finalModel)

ncomp_plot <- plot(pls_model,
                   xlab = "Number of components",
                   ylab = "RMSE (5-Fold CV)",
                   type = "b",
                   pch = 19,
                   col = "#1F51FF")
ncomp_plot

# Train set predictions

train_pred <- predict(pls_model, newdata = pw_train[,-length(pw_train)])

mse_train = MSE(pw_train$Grade, train_pred)
mae_train = MAE(pw_train$Grade, train_pred)
rmse_train = RMSE(pw_train$Grade, train_pred)
r2_train = caret::R2(pw_train$Grade, train_pred)

cat("MAE:", mae_train, "\n", "MSE:", mse_train, "\n", 
    "RMSE:", rmse_train, "\n", "R-squared:", r2_train)

# Test set predictions

test_pred <- predict(pls_model, newdata = pw_test[,-length(pw_test)])

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

# Variable Importance

var_imp <- varImp(object = pls_model)
var_imp_1 <- var_imp[['importance']]

names <- rownames(var_imp_1)
rownames(var_imp_1) <- NULL

var_imp_2 <- cbind(names,var_imp_1)

sp_feat <- as.data.frame(var_imp_2)
sp_feat_1 <- sp_feat[,-2]

spfeatWithoutQuotes = c()

for (onespfeat in sp_feat_1) {
  onespfeat = str_replace_all(onespfeat, "`", "")
  onespfeat = str_replace_all(onespfeat, r"(\\)", "")
  spfeatWithoutQuotes = append(spfeatWithoutQuotes, onespfeat)
}

print(spfeatWithoutQuotes)

var_imp_3 <- cbind(var_imp_2, Variable = spfeatWithoutQuotes)
var_imp_3 <- var_imp_3[,-1]

var_imp_plot <- ggplot(var_imp_3, aes(x = Variable, y = Overall)) +
  geom_bar(stat = "identity", fill = "#1F51FF") +
  labs(title = "", x = ("m/z"), y = "Relative Importance (%)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 10, face = "bold")) +
  scale_x_discrete(limits = var_imp_3$Variable,
                   breaks = var_imp_3$Variable[seq(1, length(var_imp_3$Variable), by = 5)]) 
var_imp_plot

# Stop Parallelization

stopCluster(cl)