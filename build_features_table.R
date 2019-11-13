library(M4comp2018)
library(tsfeatures)
library(urca)
library(seer)
#https://rdrr.io/github/thiyangt/seer/

labels <- read.csv('final_labels_smape.csv', sep=",",header = FALSE)

n_features <- 33 + 1

dimen <- 48000

feat_mat <- matrix(0,ncol=n_features, nrow=dimen)

colnames(feat_mat) <- c("length","trend","seasonality","linearity","curvature","spikiness","e_acf1","stability","lumpiness","entropy","hurst","nonlinearity","alpha","beta","hwalpha","hwbeta","hwgamma","ur_pp","ur_kpss","y_acf1","diff1_acf1","diff2_acf1","y_acf5","diff1y_acf5","diff2_acf5","seas_acf1","sediff_acf1","sediff_seacf1","sediff_acf5","lmres_acf1","y_pacf5","diff1y_pacf5","diff2y_pacf5","label")

for (i in 1:dimen) {
  
  ts <- monthly[[i]]$x
  feats <- tsfeatures(ts)
  
  feat_mat[i,1] <- length(ts) #length
  feat_mat[i,2] <- feats$trend #trend
  feat_mat[i,3] <- feats$seasonal_strength #seasonality
  feat_mat[i,4] <- feats$linearity #linearity
  feat_mat[i,5] <- feats$curvature #curvature
  feat_mat[i,6] <- feats$spike #spikiness
  feat_mat[i,7] <- feats$e_acf1 #e_acf1
  feat_mat[i,8] <- stability(ts) #stability
  feat_mat[i,9] <- lumpiness(ts) #lumpiness
  feat_mat[i,10] <- feats$entropy #entropy
  feat_mat[i,11] <- hurst(ts) #hurst
  feat_mat[i,12] <- nonlinearity(ts) #nonlinearity
  
  holt_param <- holt_parameters(ts)
  feat_mat[i,13] <- holt_param[1] #alpha
  feat_mat[i,14] <- holt_param[2] #betha
  
  hw_param <- hw_parameters(ts)
  feat_mat[i,15] <- hw_param[1] #alpha hw
  feat_mat[i,16] <- hw_param[2] #betha hw
  feat_mat[i,17] <- hw_param[3] #gamma hw
  
  feat_mat[i,18] <- ur.pp(ts)@teststat #test pp
  feat_mat[i,19] <- ur.kpss(ts)@teststat #test kpss
  
  feat_mat[i,20] <- feats$x_acf1 #??? #y_acf1
  feat_mat[i,21] <- feats$diff1_acf1 #diff1y_acf1
  feat_mat[i,22] <- feats$diff2_acf1 #diff2y_acf1
  
  acf5_param <- acf5(ts)
  feat_mat[i,23] <- acf5_param[1] #y_acf5
  feat_mat[i,24] <- acf5_param[2] #diff1y_acf5
  feat_mat[i,25] <- acf5_param[3] #diff2y_acf5
  
  feat_mat[i,26] <- feats$seas_acf1 #seas_acf1
  
  seas_diff <- acf_seasonalDiff(ts, feats$frequency ,13L) #monthly
  feat_mat[i,27] <- seas_diff[1]   #sediff_acf1
  feat_mat[i,28] <- seas_diff[2]  #sediff_seacf1
  feat_mat[i,29] <- seas_diff[3]  #sediff_acf5
  
  feat_mat[i,30] <- e_acf1(ts) #lmres_acf1
  
  pacf_param <- pacf_features(ts)
  feat_mat[i,31] <- pacf_param[1] #y_pacf5
  feat_mat[i,32] <- pacf_param[2] #diff1y_pacf5
  feat_mat[i,33] <- pacf_param[3] #diff2y_pacf5
  
  feat_mat[i,34] <- labels[i,1]
  
}

df_mat <- as.data.frame(feat_mat)

for (i in 1:dimen) {
  
  if(df_mat$seasonality[i] == 0){
    
    feat_mat[i,15] <- 0 #alpha hw
    feat_mat[i,16] <- 0 #betha hw
    feat_mat[i,17] <- 0#gamma hw
    feat_mat[i,26] <- 0 #seas_acf1
    
    feat_mat[i,27] <- 0   #sediff_acf1
    feat_mat[i,28] <- 0  #sediff_seacf1
    feat_mat[i,29] <- 0  #sediff_acf5
    
  }else{
    feat_mat[i,18] <- 0 #test pp
    feat_mat[i,19] <- 0 #test kpss
    
    feat_mat[i,30] <- 0  #sediff_acf5
  }
  
}


write.table(df_mat,file="features_table.csv",sep = ", ", row.names = FALSE)

