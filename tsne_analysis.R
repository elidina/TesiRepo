library(Rtsne)
library(ggbiplot)

features <- feat_mat[,-30]
df <- as.data.frame(features)
labs <- df$label

features <- feat_mat[,-34] #no label
features <- features[,-30]
df_feat <- as.data.frame(features)

l<-as.factor(labs)
## for plotting
colors = rainbow(length(unique(l)))
names(colors) = unique(l)

tsne <- Rtsne(df_feat, dims = 2, perplexity=10, verbose=TRUE, max_iter = 500, check_duplicates=FALSE)

plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
text(tsne$Y, labels=l, col=colors[l])
