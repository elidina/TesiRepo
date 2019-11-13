library(Rtsne)
library(ggbiplot)

s_df <- subset(df_mat, df_mat$seasonality!=0)

labs <- s_df$label
feats <- s_df[,1:33]
feats$ur_kpss <- NULL
feats$ur_pp <- NULL
feats$sediff_acf5 <- NULL


l<-as.factor(labs)
## for plotting
colors = rainbow(length(unique(l)))
names(colors) = unique(l)

feat.pca <- prcomp(feats, center=TRUE, scale=TRUE)

g <- ggbiplot(feat.pca, obs.scale = 1, var.scale = 1, groups = l, ellipse = TRUE, circle = TRUE, choices = c(1,2))
print(g)

components <- as.data.frame(feat.pca$rotation)
