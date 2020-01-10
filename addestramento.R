library(randomForest)
source("random_forest_training.R")

#FEATURES A MANO

feat_a_mano <- read.csv("features_a_mano.csv", sep=',')
labels <- factor(feat_a_mano$label,levels = c(1,2,3,4,5), labels = c("1", "2", "3","4","5"))

feat_a_mano$labels <-NULL
feat_a_mano$label <- labels

results_featsmano <- random_forest_training(feat_a_mano)

#FEATURES NN 128/64/132

feats64 <- read.csv("features/features_64.csv", sep=',', header=FALSE)
feats64$label <- labels

results_64 <- random_forest_training(feats64)

#FEATURES 64 NN + data aug

feats_nn_65 <- read.csv("features/aug_data_nn_65mila.csv", sep=',', header=FALSE)
feats_nn_65$label <- labels

results_nn_65 <- random_forest_training(feats_nn_65)

# FEATURES 64 NN + A MANO 

feats_nnmano <- read.csv("features/feat_nn_mano.csv", sep=',', header=FALSE)

cols <-  c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30",  "V31", "V32", "V33", "V34", "V35", "V36", "V37", "V38", "V39", "V40",  "V41", "V42", "V43", "V44", "V45", "V46", "V47", "V48", "V49", "V50",  "V51", "V52", "V53", "V54", "V55", "V56", "V57", "V58", "V59", "V60",  "V61", "V62", "V63", "V64","length","trend","seasonality","linearity","curvature","spikiness","e_acf1","stability","lumpiness","entropy","hurst","nonlinearity","alpha","beta","hwalpha","hwbeta","hwgamma","ur_pp","ur_kpss","y_acf1","diff1_acf1","diff2_acf1","y_acf5","diff1y_acf5","diff2_acf5","seas_acf1","sediff_acf1","sediff_seacf1","sediff_acf5","lmres_acf1","y_pacf5","diff1y_pacf5","diff2y_pacf5","label")
colnames(feats_nnmano) <- cols

labels <- factor(feats_nnmano_65$label,levels = c(1,2,3,4,5), labels = c("1", "2", "3","4","5"))
feats_nnmano$label <- NULL
feats_nnmano$label <- labels

results_nnmano <- random_forest_training(feats_nnmano)

# FEATURES 64 NN + A MANO + DATA AUG

feats_nnmano_aug <- read.csv("features/aug_data_nnmano_65mila.csv", sep=',', header=FALSE)

cols <-  c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30",  "V31", "V32", "V33", "V34", "V35", "V36", "V37", "V38", "V39", "V40",  "V41", "V42", "V43", "V44", "V45", "V46", "V47", "V48", "V49", "V50",  "V51", "V52", "V53", "V54", "V55", "V56", "V57", "V58", "V59", "V60",  "V61", "V62", "V63", "V64","length","trend","seasonality","linearity","curvature","spikiness","e_acf1","stability","lumpiness","entropy","hurst","nonlinearity","alpha","beta","hwalpha","hwbeta","hwgamma","ur_pp","ur_kpss","y_acf1","diff1_acf1","diff2_acf1","y_acf5","diff1y_acf5","diff2_acf5","seas_acf1","sediff_acf1","sediff_seacf1","sediff_acf5","lmres_acf1","y_pacf5","diff1y_pacf5","diff2y_pacf5","label")
colnames(feats_nnmano_aug) <- cols

labels <- factor(feats_nnmano_aug$label,levels = c(1,2,3,4,5), labels = c("1", "2", "3","4","5"))
feats_nnmano_aug$label <- NULL
feats_nnmano_aug$label <- labels

results_nnmano_aug <- random_forest_training(feats_nnmano_aug)

