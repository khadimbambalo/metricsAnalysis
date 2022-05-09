library(tidyverse)
library(caret)
# Systeme 2

# Variable predite
ht_effort2 <- data.frame(systeme_2_bin[,3])

# Variable WMPC
wmpc2 <- data.frame(systeme_2_bin[,14])

# Variable FOO
noo2 <- data.frame(systeme_2_bin[,12])

# Variable LOC
loc2 <- data.frame(systeme_2_bin[,10])

# Variable LCOM
lcom2 <- data.frame(systeme_2_bin[,4])


# Cas 1
## 75% des données
size <- floor(0.75 * nrow(systeme_2_bin))

## separation des donnees
set.seed(123)
train_ind <- sample(seq_len(nrow(systeme_2_bin)), size = size)
train_x_wmpc2 = data.frame(wmpc2[train_ind,])
names(train_x_wmpc2) <- names(wmpc2)
train_y = data.frame(ht_effort2[train_ind,])
names(train_y) <- names(ht_effort)
train <- data.frame(train_x_wmpc2, train_y)

test_x_wmpc2 = data.frame(wmpc2[-train_ind,])
names(test_x_wmpc2) <- names(wmpc2)
test_y = ht_effort2[-train_ind,]
names(test_y) <- names(ht_effort2)

# entrainement du modele
model_1_2 <- glm( HT.Effort ~ WMPC, family = binomial, data=train)
summary(model_1_2)$coef

# prediction
prediction_1_2 <- as.vector(predict(model_1_2, test_x, type = "response"))
prediction_1_2[which(prediction_1_2 >= 0.18)] = 1
prediction_1_2[which(prediction_1_2 < 0.18)] = 0

# Precision du modele
mean(prediction_1_2 == test_y)

# Table de classification
table(prediction_1_2, test_y)

# Cas 2
train_x_noo2 = data.frame(noo2[train_ind,])
names(train_x_noo2) <- names(noo2)
train_y = data.frame(ht_effort2[train_ind,])
names(train_y) <- names(ht_effort2)
train_2_2 <- data.frame(train_x_noo2, train_y)

test_x_noo2 = data.frame(noo2[-train_ind,])
names(test_x_noo2) <- names(noo2)
test_y = ht_effort2[-train_ind,]
names(test_y) <- names(ht_effort2)

# entrainement du modele
model_2_2 <- glm( HT.Effort ~ NOO, family = binomial, data=train_2_2)
summary(model_2_2)$coef

# prediction
prediction_2_2 <- as.vector(predict(model_2_2, test_x_noo2, type = "response"))
prediction_2_2[which(prediction_2_2 >= 0.18)] = 1
prediction_2_2[which(prediction_2_2 < 0.18)] = 0

# Precision du modele
mean(prediction_2_2 == test_y)

# Table de classification
table(prediction_2_2, test_y)


# Cas 3
train_x_loc2 = data.frame(loc2[train_ind,])
names(train_x_loc2) <- names(loc2)
train_y = data.frame(ht_effort2[train_ind,])
names(train_y) <- names(ht_effort2)
train_3_2 <- data.frame(train_x_loc2, train_y)

test_x_loc2 = data.frame(loc2[-train_ind,])
names(test_x_loc2) <- names(loc2)
test_y = data.frame(ht_effort2[-train_ind,])
names(test_y) <- names(ht_effort2)

# entrainement du modele
model_3_2 <- glm( HT.Effort ~ LOC, family = binomial, data=train_3_2)
summary(model_3_2)$coef

# prediction
prediction_3_2 <- as.vector(predict(model_3_2, test_x_loc2, type = "response"))
prediction_3_2[which(prediction_3_2 >= 0.18)] = 1
prediction_3_2[which(prediction_3_2 < 0.18)] = 0

# Precision du modele
mean(prediction_3_2 == test_y)

# Table de classification
table(prediction_3_2, test_y)



# Cas 4
train_x_lcom2 = data.frame(lcom2[train_ind,])
names(train_x_lcom2) <- names(lcom2)
train_y = data.frame(ht_effort2[train_ind,])
names(train_y) <- names(ht_effort2)
train_4_2 <- data.frame(train_x_lcom2, train_y)

test_x_lcom2 = data.frame(lcom2[-train_ind,])
names(test_x_lcom2) <- names(lcom2)
test_y = data.frame(ht_effort2[-train_ind,])
names(test_y) <- names(ht_effort2)

# entrainement du modele
model_4_2 <- glm( HT.Effort ~ LCOM, family = binomial, data=train_4_2)
summary(model_4_2)$coef

# prediction
prediction_4_2 <- as.vector(predict(model_4_2, test_x_lcom2, type = "response"))
prediction_4_2[which(prediction_4_2 >= 0.18)] = 1
prediction_4_2[which(prediction_4_2 < 0.18)] = 0

# Precision du modele
mean(prediction_4_2 == test_y)

# Table de classification
table(prediction_4_2, test_y$HT.Effort)




