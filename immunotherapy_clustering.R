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

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/6_Immunotherapy.csv", header=TRUE)
colnames(final_data_set)
final_data_set <- sqldf ("select  sex,age,Time,Number_of_Warts,Type,Area,induration_diameter from final_data_set")

final_data_set$sex               <- as.factor( final_data_set$sex    )
final_data_set$age               <- as.factor( final_data_set$age    )
final_data_set$Time               <- as.factor( final_data_set$Time    )
final_data_set$Number_of_Warts               <- as.factor( final_data_set$Number_of_Warts    )
final_data_set$Type               <- as.factor( final_data_set$Type    )
final_data_set$Area               <- as.factor( final_data_set$Area    )
final_data_set$induration_diameter               <- as.factor( final_data_set$induration_diameter    )

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
# 
# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))


#####################
# selected data set - 5
####################

final_data_set <- sqldf("select sex,
Type,
induration_diameter,
Number_of_Warts,
age from final_data_set")

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
# selected data set - 4
####################

final_data_set <- sqldf("select sex,
Type,
induration_diameter,
Number_of_Warts  from final_data_set")

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
# selected data set - 3
####################

final_data_set <- sqldf("select sex,
Type,
induration_diameter  from final_data_set")

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

#################################################################################################################
# RF
#################################################################################################################
final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/6_Immunotherapy.csv", header=TRUE)
colnames(final_data_set)
final_data_set <- sqldf ("select  sex,age,Time,Number_of_Warts,Type,Area,induration_diameter from final_data_set")

final_data_set$sex               <- as.factor( final_data_set$sex    )
final_data_set$age               <- as.factor( final_data_set$age    )
final_data_set$Time               <- as.factor( final_data_set$Time    )
final_data_set$Number_of_Warts               <- as.factor( final_data_set$Number_of_Warts    )
final_data_set$Type               <- as.factor( final_data_set$Type    )
final_data_set$Area               <- as.factor( final_data_set$Area    )
final_data_set$induration_diameter               <- as.factor( final_data_set$induration_diameter    )

#####################
# selected data set - 5
####################

final_data_set <- sqldf("select Time,
Number_of_Warts,
induration_diameter,
Area,
age from final_data_set")

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

final_data_set <- sqldf("select Time,
Number_of_Warts,
induration_diameter,
Area   from final_data_set")

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

final_data_set <- sqldf("select Time,
Number_of_Warts,
induration_diameter from final_data_set")

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
plot(full_sil_width,col='dark green',lty=1,ylim=c(0,0.4),xlim=c(0,8), ylab="Silhouette Width", xlab="No of cluster" )
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
legend(0,0.25,legend=c("Full data","PR Top 5","PR Top 4","PR Top 3","RF Top 5","RF Top 4","RF Top 3"), 
       col=c("dark green","red","blue","dark red","yellow","green","black"),
       pch=c("o","#","#","#","X","X","X"),lty=c(1,1,1,1,4,4,4),cex = 0.85)


