library(Rtsne)
library(ggbiplot)

s_df <- subset(df_mat, df_mat$seasonality!=0)

#write.table(s_df,"seas_features.csv",sep = ", ", row.names = FALSE)

labs <- s_df$label
feats <- s_df[,1:33]
feats$ur_kpss <- NULL
feats$ur_pp <- NULL
feats$sediff_acf5 <- NULL


l<-as.factor(labs)
## for plotting
colors = rainbow(length(unique(l)))
names(colors) = unique(l)

tsne <- Rtsne(feats, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500, check_duplicates=FALSE)

plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
text(tsne$Y, labels=l, col=colors[l])
