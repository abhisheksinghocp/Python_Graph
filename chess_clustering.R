#https://towardsdatascience.com/clustering-on-mixed-type-data-8bbd0a2569c3
#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(sqldf)
library(reshape2)

set.seed(3456)

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/5_chess.csv", header=TRUE)
final_data_set$outcome <- NULL
colnames(final_data_set)

final_data_set$bkblk               <- as.factor( final_data_set$bkblk    )
final_data_set$katri               <- as.factor( final_data_set$katri    )
final_data_set$stlmt               <- as.factor( final_data_set$stlmt    )
final_data_set$bknwy               <- as.factor( final_data_set$bknwy    )
final_data_set$mulch               <- as.factor( final_data_set$mulch    )
final_data_set$thrsk               <- as.factor( final_data_set$thrsk    )
final_data_set$bkon8               <- as.factor( final_data_set$bkon8    )
final_data_set$qxmsq               <- as.factor( final_data_set$qxmsq    )
final_data_set$wkcti               <- as.factor( final_data_set$wkcti    )
final_data_set$bkona               <- as.factor( final_data_set$bkona    )
final_data_set$r2ar8               <- as.factor( final_data_set$r2ar8    )
final_data_set$wkna8               <- as.factor( final_data_set$wkna8    )
final_data_set$bkspr               <- as.factor( final_data_set$bkspr    )
final_data_set$reskd               <- as.factor( final_data_set$reskd    )
final_data_set$wknck               <- as.factor( final_data_set$wknck    )
final_data_set$bkxbq               <- as.factor( final_data_set$bkxbq    )
final_data_set$reskr               <- as.factor( final_data_set$reskr    )
final_data_set$wkovl               <- as.factor( final_data_set$wkovl    )
final_data_set$bkxcr               <- as.factor( final_data_set$bkxcr    )
final_data_set$rimmx               <- as.factor( final_data_set$rimmx    )
final_data_set$wkpos               <- as.factor( final_data_set$wkpos    )
final_data_set$bkxwp               <- as.factor( final_data_set$bkxwp    )
final_data_set$rkxwp               <- as.factor( final_data_set$rkxwp    )
final_data_set$wtoeg               <- as.factor( final_data_set$wtoeg    )
final_data_set$blxwp               <- as.factor( final_data_set$blxwp    )
final_data_set$rxmsq               <- as.factor( final_data_set$rxmsq    )
final_data_set$bxqsq               <- as.factor( final_data_set$bxqsq    )
final_data_set$simpl               <- as.factor( final_data_set$simpl    )
final_data_set$cntxt               <- as.factor( final_data_set$cntxt    )
final_data_set$skach               <- as.factor( final_data_set$skach    )
final_data_set$dsopp               <- as.factor( final_data_set$dsopp    )
final_data_set$skewr               <- as.factor( final_data_set$skewr    )
final_data_set$dwipd               <- as.factor( final_data_set$dwipd    )
final_data_set$skrxp               <- as.factor( final_data_set$skrxp    )
final_data_set$hdchk               <- as.factor( final_data_set$hdchk    )
final_data_set$spcop               <- as.factor( final_data_set$spcop    )

#str(final_data_set)

##################
# full data set
##################

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



full_sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  full_sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, full_sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, full_sil_width)


##########

# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))


#####################
# selected data set - 20
####################

final_data_set <- sqldf("select hdchk  ,
wkpos  ,
                        wkovl  ,
                        wknck  ,
                        wkna8  ,
                        wkcti  ,
                        thrsk  ,
                        skrxp  ,
                        rxmsq  ,
                        simpl  ,
                        skach  ,
                        mulch  ,
                        r2ar8  ,
                        reskr  ,
                        rkxwp  ,
                        wtoeg  ,
                        bkblk  ,
                        dwipd  ,
                        stlmt  ,
                        bknwy  
                        from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



sil_width_6 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width_6[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width_6,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width_6)


##########

# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))


#####################
# selected data set - 15
####################

final_data_set <- sqldf("select hdchk  ,
wkpos  ,
                        wkovl  ,
                        wknck  ,
                        wkna8  ,
                        wkcti  ,
                        thrsk  ,
                        skrxp  ,
                        rxmsq  ,
                        simpl  ,
                        skach  ,
                        mulch  ,
                        r2ar8  ,
                        reskr  ,
                        rkxwp  
                          from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



sil_width_5 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width_5[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width_5,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width_5)

##########

# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))

#####################
# selected data set - 10
####################

final_data_set <- sqldf("select hdchk  ,
wkpos  ,
                        wkovl  ,
                        wknck  ,
                        wkna8  ,
                        wkcti  ,
                        thrsk  ,
                        skrxp  ,
                        rxmsq  ,
                        simpl  
                           from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



sil_width_4 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width_4[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width_4,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width_4)


##########

# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))


#################################################################################################################
# RF
#################################################################################################################

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/5_chess.csv", header=TRUE)
final_data_set$outcome <- NULL

final_data_set$bkblk               <- as.factor( final_data_set$bkblk    )
final_data_set$katri               <- as.factor( final_data_set$katri    )
final_data_set$stlmt               <- as.factor( final_data_set$stlmt    )
final_data_set$bknwy               <- as.factor( final_data_set$bknwy    )
final_data_set$mulch               <- as.factor( final_data_set$mulch    )
final_data_set$thrsk               <- as.factor( final_data_set$thrsk    )
final_data_set$bkon8               <- as.factor( final_data_set$bkon8    )
final_data_set$qxmsq               <- as.factor( final_data_set$qxmsq    )
final_data_set$wkcti               <- as.factor( final_data_set$wkcti    )
final_data_set$bkona               <- as.factor( final_data_set$bkona    )
final_data_set$r2ar8               <- as.factor( final_data_set$r2ar8    )
final_data_set$wkna8               <- as.factor( final_data_set$wkna8    )
final_data_set$bkspr               <- as.factor( final_data_set$bkspr    )
final_data_set$reskd               <- as.factor( final_data_set$reskd    )
final_data_set$wknck               <- as.factor( final_data_set$wknck    )
final_data_set$bkxbq               <- as.factor( final_data_set$bkxbq    )
final_data_set$reskr               <- as.factor( final_data_set$reskr    )
final_data_set$wkovl               <- as.factor( final_data_set$wkovl    )
final_data_set$bkxcr               <- as.factor( final_data_set$bkxcr    )
final_data_set$rimmx               <- as.factor( final_data_set$rimmx    )
final_data_set$wkpos               <- as.factor( final_data_set$wkpos    )
final_data_set$bkxwp               <- as.factor( final_data_set$bkxwp    )
final_data_set$rkxwp               <- as.factor( final_data_set$rkxwp    )
final_data_set$wtoeg               <- as.factor( final_data_set$wtoeg    )
final_data_set$blxwp               <- as.factor( final_data_set$blxwp    )
final_data_set$rxmsq               <- as.factor( final_data_set$rxmsq    )
final_data_set$bxqsq               <- as.factor( final_data_set$bxqsq    )
final_data_set$simpl               <- as.factor( final_data_set$simpl    )
final_data_set$cntxt               <- as.factor( final_data_set$cntxt    )
final_data_set$skach               <- as.factor( final_data_set$skach    )
final_data_set$dsopp               <- as.factor( final_data_set$dsopp    )
final_data_set$skewr               <- as.factor( final_data_set$skewr    )
final_data_set$dwipd               <- as.factor( final_data_set$dwipd    )
final_data_set$skrxp               <- as.factor( final_data_set$skrxp    )
final_data_set$hdchk               <- as.factor( final_data_set$hdchk    )
final_data_set$spcop               <- as.factor( final_data_set$spcop    )

#str(final_data_set)
#colnames(final_data_set)

#####################
# selected data set - 20
####################

final_data_set <- sqldf ("select  rimmx,
wknck,
bxqsq,
katri,
wkpos,
wkna8,
bkxbq,
bkxwp,
bkxcr,
bkblk,
r2ar8,
wkovl,
mulch,
skrxp,
cntxt,
bkspr,
blxwp,
rxmsq,
bknwy,
wkcti
   from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



RF_sil_width_6 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  RF_sil_width_6[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, RF_sil_width_6,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, RF_sil_width_6)


# ##########
# 
# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))


#####################
# selected data set - 15
####################

final_data_set <- sqldf("select rimmx,
wknck,
bxqsq,
katri,
wkpos,
wkna8,
bkxbq,
bkxwp,
bkxcr,
bkblk,
r2ar8,
wkovl,
mulch,
skrxp,
cntxt from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



RF_sil_width_5 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  RF_sil_width_5[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, RF_sil_width_5,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, RF_sil_width_5)

##########

# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))

#####################
# selected data set - 10
####################

final_data_set <- sqldf("select rimmx,
wknck,
bxqsq,
katri,
wkpos,
wkna8,
bkxbq,
bkxwp,
bkxcr,
bkblk   from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



RF_sil_width_4 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  RF_sil_width_4[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, RF_sil_width_4,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, RF_sil_width_4)


##########

# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))


############################################################################################################
############################################################################################################

# ##########
# 
# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))
# 
# 
# 


plot(full_sil_width,col='dark green',lty=1,ylim=c(0.1,0.50),xlim=c(2,8), ylab="Silhouette Width", xlab="No of cluster" )
lines(full_sil_width, col="dark green",lty=1)

points(sil_width_6,col='red',pch="#")
lines(sil_width_6, col="red",lty=1)

points(sil_width_5,col='blue',pch="#")
lines(sil_width_5, col="blue",lty=1)

points(sil_width_4,col='dark red',pch="#")
lines(sil_width_4, col="dark red",lty=1)

##### RF

points(RF_sil_width_6,col='yellow',pch="X")
lines(RF_sil_width_6, col="yellow",lty=4)

points(RF_sil_width_5,col='green',pch="X")
lines(RF_sil_width_5, col="green",lty=4)

points(RF_sil_width_4,col='black',pch="X")
lines(RF_sil_width_4, col="black",lty=4)

# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(2,0.5,legend=c("Full data","PR Top 20","PR Top 15","PR Top 10","RF Top 20","RF Top 15","RF Top 10"), 
       col=c("dark green","red","blue","dark red","yellow","green","black"),
       pch=c("o","#","#","#","X","X","X"),lty=c(1,1,1,1,4,4,4),cex = 0.85)


# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
#legend(0,0.4,legend=c("Full data","PR Top 20","PR Top 15","PR Top 10"), col=c("dark green","red","blue","dark red"),
#       pch=c("o","#","#","#"),lty=c(1,1,1,1), ncol=1,cex = 0.85)

