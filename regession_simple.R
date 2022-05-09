library(tidyverse)
library(caret)
# Systeme 1

# Variable predite
ht_effort <- data.frame(systeme_1_bin[,3])

# Variable WMPC
wmpc <- data.frame(systeme_1_bin[,14])

# Variable FOO
foo <- data.frame(systeme_1_bin[,7])

# Variable LOC
loc <- data.frame(systeme_1_bin[,10])
#print(loc)

# Cas 1
## 75% des données
size <- floor(0.75 * nrow(systeme_1_bin))

## separation des donnees
set.seed(123)
train_ind <- sample(seq_len(nrow(systeme_1_bin)), size = size)
train_x_wmpc = data.frame(wmpc[train_ind,])
names(train_x_wmpc) <- names(wmpc)
train_y = data.frame(ht_effort[train_ind,])
names(train_y) <- names(ht_effort)
train <- data.frame(train_x_wmpc, train_y)

test_x_wmpc = data.frame(wmpc[-train_ind,])
names(test_x_wmpc) <- names(wmpc)
test_y = ht_effort[-train_ind,]
names(test_y) <- names(ht_effort)

# entrainement du modele
model_1 <- glm( HT.Effort ~ WMPC, family = binomial, data=train)
summary(model_1)$coef

# prediction
prediction_1 <- as.vector(predict(model_1, test_x, type = "response"))
prediction_1[which(prediction_1 >= 0.18)] = 1
prediction_1[which(prediction_1 < 0.18)] = 0

# Precision du modele
mean(prediction_1 == test_y)

# Table de classification
table(prediction_1, test_y)

# Cas 2
train_x_foo = data.frame(foo[train_ind,])
names(train_x_foo) <- names(foo)
train_y = data.frame(ht_effort[train_ind,])
names(train_y) <- names(ht_effort)
train_2 <- data.frame(train_x_foo, train_y)

test_x_foo = data.frame(foo[-train_ind,])
names(test_x_foo) <- names(foo)
test_y = ht_effort[-train_ind,]
names(test_y) <- names(ht_effort)

# entrainement du modele
model_2 <- glm( HT.Effort ~ FO, family = binomial, data=train_2)
summary(model_2)$coef

# prediction
prediction_2 <- as.vector(predict(model_2, test_x_foo, type = "response"))
prediction_2[which(prediction_2 >= 0.18)] = 1
prediction_2[which(prediction_2 < 0.18)] = 0

# Precision du modele
mean(prediction_2 == test_y)

# Table de classification
table(prediction_2, test_y)


# Cas 3
train_x_loc = data.frame(loc[train_ind,])
names(train_x_loc) <- names(loc)
train_y = data.frame(ht_effort[train_ind,])
names(train_y) <- names(ht_effort)
train_3 <- data.frame(train_x_loc, train_y)

test_x_loc = data.frame(loc[-train_ind,])
names(test_x_loc) <- names(loc)
test_y = data.frame(ht_effort[-train_ind,])
names(test_y) <- names(ht_effort)

# entrainement du modele
model_3 <- glm( HT.Effort ~ LOC, family = binomial, data=train_3)
summary(model_3)$coef

# prediction
prediction_3 <- as.vector(predict(model_3, test_x_loc, type = "response"))
prediction_3[which(prediction_3 >= 0.18)] = 1
prediction_3[which(prediction_3 < 0.18)] = 0

# Precision du modele
mean(prediction_3 == test_y)

# Table de classification
table(prediction_3, test_y$HT.Effort)




