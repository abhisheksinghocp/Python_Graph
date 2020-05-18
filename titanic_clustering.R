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

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/1_titanic.csv", header=TRUE)

final_data_set <- sqldf ("select  Pclass ,Sib_Spou , Parent_Child , Age_prect_level , Fare_prect_level , sex from final_data_set")

final_data_set$Pclass          <- as.factor(final_data_set$Pclass)
final_data_set$Sib_Spou         <- as.factor(final_data_set$Sib_Spou)
final_data_set$Parent_Child            <- as.factor(final_data_set$Parent_Child)
final_data_set$Age_prect_level               <- as.factor(final_data_set$Age_prect_level)
final_data_set$Fare_prect_level                <- as.factor(final_data_set$Fare_prect_level)
final_data_set$sex           <- as.factor(final_data_set$sex)

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

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#####################
# selected data set - 5
####################

final_data_set <- sqldf("select sex ,Pclass ,Age_prect_level,Fare_prect_level ,Parent_Child  from final_data_set")

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

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#####################
# selected data set - 4
####################

final_data_set <- sqldf("select sex ,Pclass ,Age_prect_level,Fare_prect_level  from final_data_set")

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

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#####################
# selected data set - 3
####################

final_data_set <- sqldf("select sex ,Pclass ,Age_prect_level  from final_data_set")

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

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



#################################################################################################################
# RF
#################################################################################################################
final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/1_titanic.csv", header=TRUE)

final_data_set <- sqldf ("select  Pclass ,Sib_Spou , Parent_Child , Age_prect_level , Fare_prect_level , sex from final_data_set")

final_data_set$Pclass          <- as.factor(final_data_set$Pclass)
final_data_set$Sib_Spou         <- as.factor(final_data_set$Sib_Spou)
final_data_set$Parent_Child            <- as.factor(final_data_set$Parent_Child)
final_data_set$Age_prect_level               <- as.factor(final_data_set$Age_prect_level)
final_data_set$Fare_prect_level                <- as.factor(final_data_set$Fare_prect_level)
final_data_set$sex           <- as.factor(final_data_set$sex)

#####################
# selected data set - 5
####################

final_data_set <- sqldf("select sex,
Pclass,
Fare_prect_level,
Age_prect_level,
Sib_Spou from final_data_set")

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
# selected data set - 4
####################

final_data_set <- sqldf("select sex,
Pclass,
Fare_prect_level,
Age_prect_level from final_data_set")

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
# selected data set - 3
####################

final_data_set <- sqldf("select sex,
Pclass,
Fare_prect_level from final_data_set")

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


plot(full_sil_width ,col='dark green',xlim=c(2,8),ylim=c(0.19,0.38), ylab="Silhouette Width", xlab="No of cluster")
lines(full_sil_width, col="dark green",lty=1)


points(sil_width_6,col='red',pch="#")
lines(sil_width_6, col="red",lty=1)

points(sil_width_5,col='blue',pch="#")
lines(sil_width_5, col="blue",lty=1)

#points(sil_width_4,col='dark red',pch="#")
#lines(sil_width_4, col="dark red",lty=1)


##### RF

points(RF_sil_width_6,col='yellow',pch="X")
lines(RF_sil_width_6, col="yellow",lty=4)

points(RF_sil_width_5,col='green',pch="X")
lines(RF_sil_width_5, col="green",lty=4)

#points(RF_sil_width_4,col='black',pch="X")
#lines(RF_sil_width_4, col="black",lty=4)


# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
legend(2,0.35,legend=c("Full data","PR Top 5","PR Top 4","RF Top 5","RF Top 4"), 
       col=c("dark green","red","blue","yellow","green"),
       pch=c("o","#","#","X","X"),lty=c(1,1,1,4,4),cex = 0.85)  #pch=c("o","#","X","+"), , ncol=1


# legend(0,0.5,legend=c("Full data","PR Top 5","PR Top 4","PR Top 3","RF Top 5","RF Top 4","RF Top 3"), 
#        col=c("dark green","red","blue","dark red","yellow","green","black"),
#        pch=c("o","#","#","#","X","X","X"),lty=c(1,1,1,1,4,4,4),cex = 0.85)  #pch=c("o","#","X","+"), , ncol=1


# 
# plot(full_sil_width,col='dark green',lty=1,ylim=c(0.19,0.52), ylab="Silhouette Width", xlab="No of cluster" )
# lines(full_sil_width, col="dark green",lty=1)
# 
# points(sil_width_6,col='red',pch="#")
# lines(sil_width_6, col="red",lty=2)
# 
# points(sil_width_5,col='blue',pch="X")
# lines(sil_width_5, col="blue",lty=3)
# 
# points(sil_width_4,col='dark red',pch="+")
# lines(sil_width_4, col="dark red",lty=4)
# 
# 
# # Adding a legend inside box at the location (2,40) in graph coordinates.
# # Note that the order of plots are maintained in the vectors of attributes.
# legend(1,0.5,legend=c("Full data","PR Top 5","PR Top 4","PR Top 3"), col=c("dark green","red","blue","dark red"),
#        pch=c("o","#","X","+"),lty=c(1,2,3,4), ncol=1)
# 
