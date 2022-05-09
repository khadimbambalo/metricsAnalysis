library(tidyverse)
library(caret)

# Systeme 1
## 75% des données
size <- floor(0.75 * nrow(systeme_1_bin))

## separation des donnees
set.seed(123)
train_ind <- sample(seq_len(nrow(systeme_1_bin)), size = size)
train = data.frame(systeme_1_bin[train_ind,3:14])
test_x = data.frame(systeme_1_bin[-train_ind,3:14])
test_y_mul = data.frame(systeme_1_bin[-train_ind,3])
names(test_y_mul) = names(systeme_1_bin[-train_ind,3])
#print(test_y)

# entrainement du modele
model_mul_1 <- glm( HT.Effort ~., data = train, family = binomial)
summary(model_mul_1)$coef

# prediction
prediction_mul_1 = as.vector(predict(model_mul_1, test_x, type = "response"))
prediction_mul_1[which(prediction_mul_1 >= 0.18)] = 1
prediction_mul_1[which(prediction_mul_1 < 0.18)] = 0

# Precision du modele
mean(prediction_mul_1 == test_y)

# Table de classification
table(prediction_mul_1, test_y_mul$`HT Effort`)



