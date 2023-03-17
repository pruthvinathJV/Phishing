install.packages("dplyr")
install.packages("ggplot")
install.packages("tidyr")
install.packages("rpart")

library(dplyr)
library(ggplot2)
library(tidyr)
library(rpart)

install.packages("data.table")
library(data.table)

install.packages("rpart.plot")
library(rpart.plot)

install.packages("magrittr")
library(magrittr)

trainX <- fread("/Users/pruthvinathjeripityvenkata/Desktop/UIC/Sem 3/IDS 572/HW/HW1/trainX.csv")
trainY <- fread("/Users/pruthvinathjeripityvenkata/Desktop/UIC/Sem 3/IDS 572/HW/HW1/trainY.csv")
train <- cbind(trainX, trainY)
as.data.frame(train)
colnames(train) <- c('r_mean', 'tex_mean', 'peri_mean', 'area_mean', 'smooth_mean',
                     'comp_mean', 'conc_mean', 'num_conc_mean', 'sym_mean', 'frac_mean',
                     'r_std', 'tex_std', 'peri_std', 'area_std', 'smooth_std',
                     'comp_std', 'conc_std', 'num_conc_std', 'sym_std', 'frac_std', 
                     'r_large', 'tex_large', 'peri_large', 'area_large', 'smooth_large',
                     'comp_large', 'conc_large', 'num_conc_large', 'sym_large', 'frac_large',
                     'Cancer')
summary(train)


install.packages("corrplot")
library(corrplot)
train.corr = cor(train)
corrplot(train.corr)


df_train <- train %>%
  select(-c(peri_mean, area_mean, r_large, peri_large, area_large, tex_large)) %>% 
  mutate(Cancer = factor(Cancer, levels = c(0, 1), labels = c('No', 'Yes'))) %>% 
  na.omit()



testX <- fread("/Users/pruthvinathjeripityvenkata/Desktop/UIC/Sem 3/IDS 572/HW/HW1/testX.csv")
testY <- fread("/Users/pruthvinathjeripityvenkata/Desktop/UIC/Sem 3/IDS 572/HW/HW1/testY.csv")
test <- cbind(testX, testY)
colnames(test) <- c('r_mean', 'tex_mean', 'peri_mean', 'area_mean', 'smooth_mean',
                     'comp_mean', 'conc_mean', 'num_conc_mean', 'sym_mean', 'frac_mean',
                     'r_std', 'tex_std', 'peri_std', 'area_std', 'smooth_std',
                     'comp_std', 'conc_std', 'num_conc_std', 'sym_std', 'frac_std', 
                     'r_large', 'tex_large', 'peri_large', 'area_large', 'smooth_large',
                     'comp_large', 'conc_large', 'num_conc_large', 'sym_large', 'frac_large',
                     'Cancer')
df_test <- subset(test, select = -c(peri_mean, area_mean, r_large, peri_large, area_large, tex_large) )
#mutate(df_train, Cancer_class = ifelse(Cancer == 0, 'No', 'Yes') )
# df_test <- subset(df_test, select = -c(Cancer) )
as.data.frame(df_train)

dim(df_train)
dim(df_test)
# prop.table(table(df_train$Cancer_class))
# prop.table(table(df_test$Cancer_class))

fit <- rpart(Cancer~., data = df_train, method = 'class', parms = list(split = "information"),control=rpart.control(minsplit = 6))
rpart.plot(fit, extra = 106)
print(fit)

install.packages("rpart.plot")
library(rpart.plot)
library(RColorBrewer)
install.packages("rattle")
library(rattle)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
ptree<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,
               main="Pruned Classification Tree")
rpart.rules(ptree)
predict_train <-predict(ptree, df_train, type = 'class')
table_mat_train <- table(df_train$Cancer, predict_train)
table_mat_train

accuracy_Train <- sum(diag(table_mat_train)) / sum(table_mat_train)
print(paste('Accuracy for train', accuracy_Train))

predict_test <-predict(ptree, df_test, type = 'class')
table_mat_test <- table(df_test$Cancer, predict_test)
table_mat_test

accuracy_Test <- sum(diag(table_mat_test)) / sum(table_mat_test)
print(paste('Accuracy for test', accuracy_Test))

df <- data.frame(imp = fit$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()
