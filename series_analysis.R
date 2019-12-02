library(M4comp2018)
library(muStat)

data(M4)
monthly <- Filter(function(l) l$period == "Monthly", M4)

dimen <- length(monthly)

m <- matrix(0,ncol=1,nrow = dimen)

#boh <- 0
#ind <- 0

for (i in 1:dimen) {
  
  series <- monthly[[i]]$x
  m[i,1] <- length(series)
  
  #if(length(series) < 500){
   # boh <- boh + length(series)
    #ind <- ind +1
  #}
}

res_mat <- matrix(0,ncol=4,nrow=1)
df_res <- as.data.frame(res_mat)
colnames(df_res) <- c("mean","STD","Max","Min")

df_res$mean[1] <- mean(m)
df_res$STD[1] <- stdev(m, unbiased = TRUE)
df_res$Max[1] <- max(m)
df_res$Min[1] <- min(m)

# mean      STD      Max Min
# 216.3002 137.4063 2794  42

plot(m, main = "Series Length", xlab = "Series", ylab = "Length")

#46597: numero di serie sotto i 500 valori
