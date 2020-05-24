#https://towardsdatascience.com/clustering-on-mixed-type-data-8bbd0a2569c3
#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(caret)
library(sqldf)
library(reshape2)

set.seed(3456)

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/4_Metro_Interstate_Traffic_Volume.csv", header=TRUE)

colnames(final_data_set)

final_data_set$holiday               <- as.factor( final_data_set$holiday    )
final_data_set$weather_main               <- as.factor( final_data_set$weather_main    )
final_data_set$weather_description               <- as.factor( final_data_set$weather_description    )
final_data_set$Month               <- as.factor( final_data_set$Month    )
final_data_set$day               <- as.factor( final_data_set$day    )
final_data_set$Hour               <- as.factor( final_data_set$Hour    )
final_data_set$temp_level               <- as.factor( final_data_set$temp_level    )
final_data_set$clouds_all_level               <- as.factor( final_data_set$clouds_all_level    )

trainIndex <- createDataPartition(final_data_set$traffic_volume, p = .4, 
                                  list = FALSE, 
                                  times = 1)

train<-final_data_set[trainIndex,]
test<-final_data_set[-trainIndex,]

#final_data_set$traffic_volume <- NULL
train$traffic_volume <- NULL

final_data_set <- train

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
# plot(1:8, full_sil_width,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, full_sil_width)


##########

# tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))


#####################
# selected data set - 6
####################

final_data_set <- sqldf("select clouds_all_level,
                        temp_level,
                        holiday,
                        weather_main,
                        Month , hour   from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



sil_width_6 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width_6[i] <- pam_fit$silinfo$avg.width  
}
# plot(1:8, sil_width_6,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, sil_width_6)


##########

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

final_data_set <- sqldf("select clouds_all_level,
                        temp_level,
                        holiday,
                        weather_main,
                        Month   from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



sil_width_5 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width_5[i] <- pam_fit$silinfo$avg.width  
}
# plot(1:8, sil_width_5,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, sil_width_5)

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

final_data_set <- sqldf("select clouds_all_level,
                        temp_level,
                        holiday,
                        weather_main  from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



sil_width_4 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width_4[i] <- pam_fit$silinfo$avg.width  
}
# plot(1:8, sil_width_4,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, sil_width_4)


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

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/4_Metro_Interstate_Traffic_Volume.csv", header=TRUE)

colnames(final_data_set)

final_data_set$holiday               <- as.factor( final_data_set$holiday    )
final_data_set$weather_main               <- as.factor( final_data_set$weather_main    )
final_data_set$weather_description               <- as.factor( final_data_set$weather_description    )
final_data_set$Month               <- as.factor( final_data_set$Month    )
final_data_set$day               <- as.factor( final_data_set$day    )
final_data_set$Hour               <- as.factor( final_data_set$Hour    )
final_data_set$temp_level               <- as.factor( final_data_set$temp_level    )
final_data_set$clouds_all_level               <- as.factor( final_data_set$clouds_all_level    )

trainIndex <- createDataPartition(final_data_set$traffic_volume, p = .4, 
                                  list = FALSE, 
                                  times = 1)

train<-final_data_set[trainIndex,]
test<-final_data_set[-trainIndex,]

#final_data_set$traffic_volume <- NULL
train$traffic_volume <- NULL

final_data_set <- train
test <- NULL

#####################
# selected data set - 6
####################

final_data_set <- sqldf ("select  Hour,
                         day,
                         Month,
                         weather_description,
                         temp_level,
                         clouds_all_level
                         from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



RF_sil_width_6 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  RF_sil_width_6[i] <- pam_fit$silinfo$avg.width  
}
# plot(1:8, RF_sil_width_6,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, RF_sil_width_6)


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
# selected data set - 5
####################

final_data_set <- sqldf("select Hour,
                        day,
                        Month,
                        weather_description,
                        temp_level from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



RF_sil_width_5 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  RF_sil_width_5[i] <- pam_fit$silinfo$avg.width  
}
# plot(1:8, RF_sil_width_5,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, RF_sil_width_5)

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

final_data_set <- sqldf("select Hour,
                        day,
                        Month,
                        weather_description   from final_data_set")

#' Compute Gower distance
gower_dist <- daisy(final_data_set, metric = "gower")
gower_mat <- as.matrix(gower_dist)



RF_sil_width_4 <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  RF_sil_width_4[i] <- pam_fit$silinfo$avg.width  
}
# plot(1:8, RF_sil_width_4,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, RF_sil_width_4)


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


plot(full_sil_width,col='dark green',lty=1,ylim=c(0,0.50),xlim=c(2,8), ylab="Silhouette Width", xlab="No of cluster" )
lines(full_sil_width, col="dark green",lty=1)

abline(h = 0.20, col="purple",lty=2)

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
legend(2,0.5,legend=c("Full data","PR Top 6","PR Top 5","PR Top 4","RF Top 6","RF Top 5","RF Top 4","Reference"), 
       col=c("dark green","red","blue","dark red","yellow","green","black","purple"),
       pch=c("o","#","#","#","X","X","X",""),lty=c(1,1,1,1,4,4,4,2),cex = 0.85)


# Adding a legend inside box at the location (2,40) in graph coordinates.
# Note that the order of plots are maintained in the vectors of attributes.
#legend(0,0.4,legend=c("Full data","PR Top 20","PR Top 15","PR Top 10"), col=c("dark green","red","blue","dark red"),
#       pch=c("o","#","#","#"),lty=c(1,1,1,1), ncol=1,cex = 0.85)

