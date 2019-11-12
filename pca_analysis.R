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

feat.pca <- prcomp(df_feat, center=TRUE, scale=TRUE)

g <- ggbiplot(feat.pca, obs.scale = 1, var.scale = 1, groups = l, ellipse = TRUE, circle = TRUE, choices = c(1,2))
print(g)
