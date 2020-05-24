library(sqldf)
options(scipen=999)
set.seed(3456)
library("ggplot2")
library("cramer")
library("ModelMetrics")
library("MLmetrics")
library("kableExtra")
library("Information")
library("kableExtra")
library('caTools')
library('ROCR')
library('pROC')
library("randomForest")
library("InformationValue")
library('caret')
library('stringr')
#install.packages("rpart")
library("rpart")
seed = 3456

###################################################################################################################################
################# Titanic
###################################################################################################################################

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/1_titanic.csv", header=TRUE)

final_data_set$Survived              <- as.factor(final_data_set$Survived)
final_data_set$Pclass          <- as.factor(final_data_set$Pclass)
final_data_set$Sib_Spou         <- as.factor(final_data_set$Sib_Spou)
final_data_set$Parent_Child            <- as.factor(final_data_set$Parent_Child)
final_data_set$Age_prect_level               <- as.factor(final_data_set$Age_prect_level)
final_data_set$Fare_prect_level                <- as.factor(final_data_set$Fare_prect_level)
final_data_set$sex           <- as.factor(final_data_set$sex)

##############################################
########### Random Forest
##############################################
set.seed(3456)
fit.rf <- randomForest(Survived ~ Pclass + Sib_Spou + Parent_Child + Age_prect_level + Fare_prect_level + sex ,
                       data=final_data_set,proximity=TRUE, importance = TRUE,seed = seed) #

#print(fit.rf)

# ## Show "importance" of variables: higher value mean more important:
round(importance(fit.rf), 2)  

print('############################### Titanic RF result ###################')
#varImp(fit.rf)
varImpPlot(fit.rf,type=2)

print('##################################################################')

##############################################
########### Decision Tree  ###################
##############################################
set.seed(3456)

base_model <- rpart(Survived ~ Pclass + Sib_Spou
                    + Parent_Child + Age_prect_level + Fare_prect_level + sex
                    ,data = final_data_set
                    ,parms = list(split = "gini") #gini,information
                    ,control = rpart.control(cp = 0))

base_model$variable.importance


###################################################################################################################################
################# IRIS
###################################################################################################################################

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/2_iris.csv", header=TRUE)

colnames(final_data_set)[1] <- 'sepal_length' 
colnames(final_data_set)

#final_data_set$sepal_length              <- as.factor(final_data_set$sepal_length)
#final_data_set$sepal_width          <- as.factor(final_data_set$sepal_width)
#final_data_set$petal_length         <- as.factor(final_data_set$petal_length)
#final_data_set$petal_width            <- as.factor(final_data_set$petal_width)
final_data_set$species               <- as.factor(final_data_set$species)
str(final_data_set)

##############################################
########### Random Forest
##############################################
set.seed(3456)
fit.rf <- randomForest(species ~ sepal_length + sepal_width + petal_length + petal_width ,
                       data=final_data_set,proximity=TRUE, importance = TRUE) #

#print(fit.rf)

# ## Show "importance" of variables: higher value mean more important:
round(importance(fit.rf), 2)  

print('############################### Titanic RF result ###################')
#varImp(fit.rf)
varImpPlot(fit.rf,type=2)

print('##################################################################')

##############################################
########### Decision Tree  ###################
##############################################
set.seed(3456)

base_model <- rpart(species ~ sepal_length + sepal_width + petal_length + petal_width
                    ,data = final_data_set
                    ,parms = list(split = "gini") #gini,information
                    ,control = rpart.control(cp = 0))

base_model$variable.importance


###################################################################################################################################
################# brest cancer
###################################################################################################################################

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/3_dataR2.csv", header=TRUE)

#colnames(final_data_set)[1] <- 'sepal_length' 
colnames(final_data_set)

#final_data_set$sepal_length              <- as.factor(final_data_set$sepal_length)
#final_data_set$sepal_width          <- as.factor(final_data_set$sepal_width)
#final_data_set$petal_length         <- as.factor(final_data_set$petal_length)
#final_data_set$petal_width            <- as.factor(final_data_set$petal_width)
final_data_set$Classification               <- as.factor(final_data_set$Classification)
str(final_data_set)

##############################################
########### Random Forest
##############################################
set.seed(3456)
fit.rf <- randomForest(Classification ~ Age + BMI + Glucose + Insulin + HOMA + Leptin + Adiponectin +  Resistin + MCP.1,
                       data=final_data_set,proximity=TRUE, importance = TRUE) #

#print(fit.rf)

# ## Show "importance" of variables: higher value mean more important:
round(importance(fit.rf), 2)  

print('############################### Titanic RF result ###################')
#varImp(fit.rf)
varImpPlot(fit.rf,type=2)

print('##################################################################')

##############################################
########### Decision Tree  ###################
##############################################
set.seed(3456)

base_model <- rpart(Classification ~ Age + BMI + Glucose + Insulin + HOMA + Leptin + Adiponectin +  Resistin + MCP.1
                    ,data = final_data_set
                    ,parms = list(split = "gini") #gini,information
                    ,control = rpart.control(cp = 0))

base_model$variable.importance

################################################################################################################################
################# Chess
###################################################################################################################################
set.seed(3456)
final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/5_chess.csv", header=TRUE)

#colnames(final_data_set)[1] <- 'sepal_length' 
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
final_data_set$outcome             <- as.factor( final_data_set$outcome  )
str(final_data_set)

##############################################
########### Random Forest
##############################################
set.seed(3456)
fit.rf <- randomForest(outcome ~ bkblk   +
                         katri   +
                         stlmt   +
                         bknwy   +
                         mulch   +
                         thrsk   +
                         bkon8   +
                         qxmsq   +
                         wkcti   +
                         bkona   +
                         r2ar8   +
                         wkna8   +
                         bkspr   +
                         reskd   +
                         wknck   +
                         bkxbq   +
                         reskr   +
                         wkovl   +
                         bkxcr   +
                         rimmx   +
                         wkpos   +
                         bkxwp   +
                         rkxwp   +
                         wtoeg   +
                         blxwp   +
                         rxmsq   +
                         bxqsq   +
                         simpl   +
                         cntxt   +
                         skach   +
                         dsopp   +
                         skewr   +
                         dwipd   +
                         skrxp   +
                         hdchk   +
                         spcop   ,
                       data=final_data_set,proximity=TRUE, importance = TRUE) #

#print(fit.rf)

# ## Show "importance" of variables: higher value mean more important:
round(importance(fit.rf), 2)  

print('############################### Titanic RF result ###################')
#varImp(fit.rf)
varImpPlot(fit.rf,type=2)

print('##################################################################')

##############################################
########### Decision Tree  ###################
##############################################
set.seed(3456)

base_model <- rpart(outcome ~ bkblk   +
                      katri   +
                      stlmt   +
                      bknwy   +
                      mulch   +
                      thrsk   +
                      bkon8   +
                      qxmsq   +
                      wkcti   +
                      bkona   +
                      r2ar8   +
                      wkna8   +
                      bkspr   +
                      reskd   +
                      wknck   +
                      bkxbq   +
                      reskr   +
                      wkovl   +
                      bkxcr   +
                      rimmx   +
                      wkpos   +
                      bkxwp   +
                      rkxwp   +
                      wtoeg   +
                      blxwp   +
                      rxmsq   +
                      bxqsq   +
                      simpl   +
                      cntxt   +
                      skach   +
                      dsopp   +
                      skewr   +
                      dwipd   +
                      skrxp   +
                      hdchk   +
                      spcop
                    ,data = final_data_set
                    ,parms = list(split = "gini") #gini,information
                    ,control = rpart.control(cp = 0))

base_model$variable.importance


###################################################################################################################################
################# Immunotherapy
###################################################################################################################################

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/6_Immunotherapy.csv", header=TRUE)

#colnames(final_data_set)[1] <- 'sepal_length' 
colnames(final_data_set)

final_data_set$sex               <- as.factor( final_data_set$sex    )
final_data_set$age               <- as.factor( final_data_set$age    )
final_data_set$Time               <- as.factor( final_data_set$Time    )
final_data_set$Number_of_Warts               <- as.factor( final_data_set$Number_of_Warts    )
final_data_set$Type               <- as.factor( final_data_set$Type    )
final_data_set$Area               <- as.factor( final_data_set$Area    )
final_data_set$induration_diameter               <- as.factor( final_data_set$induration_diameter    )
final_data_set$Result_of_Treatment               <- as.factor( final_data_set$Result_of_Treatment    )

str(final_data_set)

##############################################
########### Random Forest
##############################################
set.seed(3456)
fit.rf <- randomForest(Result_of_Treatment ~ sex                  +
                         age                  +
                         Time                 +
                         Number_of_Warts      +
                         Type                 +
                         Area                 +
                         induration_diameter    ,
                       data=final_data_set,proximity=TRUE, importance = TRUE) #

#print(fit.rf)

# ## Show "importance" of variables: higher value mean more important:
round(importance(fit.rf), 2)  

print('############################### Titanic RF result ###################')
#varImp(fit.rf)
varImpPlot(fit.rf,type=2)

print('##################################################################')

##############################################
########### Decision Tree  ###################
##############################################

set.seed(3456)
base_model <- rpart(Result_of_Treatment ~ sex                  +
                      age                  +
                      Time                 +
                      Number_of_Warts      +
                      Type                 +
                      Area                 +
                      induration_diameter   
                    ,data = final_data_set
                    ,parms = list(split = "gini") #gini,information
                    ,control = rpart.control(cp = 0))

base_model$variable.importance


###################################################################################################################################
################# 4_Metro_Interstate_Traffic_Volume
###################################################################################################################################

final_data_set <- read.csv("C:/Users/abhissi/OneDrive - Microsoft/research/attachments/4_Metro_Interstate_Traffic_Volume.csv", header=TRUE)

#colnames(final_data_set)[1] <- 'sepal_length' 
colnames(final_data_set)

final_data_set$holiday               <- as.factor( final_data_set$holiday    )
final_data_set$weather_main               <- as.factor( final_data_set$weather_main    )
final_data_set$weather_description               <- as.factor( final_data_set$weather_description    )
final_data_set$Month               <- as.factor( final_data_set$Month    )
final_data_set$day               <- as.factor( final_data_set$day    )
final_data_set$Hour               <- as.factor( final_data_set$Hour    )
final_data_set$temp_level               <- as.factor( final_data_set$temp_level    )
final_data_set$clouds_all_level               <- as.factor( final_data_set$clouds_all_level    )

str(final_data_set)

trainIndex <- createDataPartition(final_data_set$traffic_volume, p = .4, 
                                  list = FALSE, 
                                  times = 1)

train<-final_data_set[trainIndex,]
test<-final_data_set[-trainIndex,]
final_data_set <- train
##############################################
########### Random Forest
##############################################
set.seed(3456)
fit.rf <- randomForest(traffic_volume ~ holiday                 +
                         weather_main            +
                         weather_description     +
                         Month                   +
                         day                     +
                         Hour                    +
                         temp_level              +
                         clouds_all_level            ,
                       data=final_data_set,proximity=TRUE, importance = TRUE) #

#print(fit.rf)

# ## Show "importance" of variables: higher value mean more important:
round(importance(fit.rf), 2)  

print('############################### Titanic RF result ###################')
#varImp(fit.rf)
varImpPlot(fit.rf,type=2)

print('##################################################################')

##############################################
########### Decision Tree  ###################
##############################################
set.seed(3456)

base_model <- rpart(traffic_volume ~ holiday                 +
                      weather_main            +
                      weather_description     +
                      Month                   +
                      day                     +
                      Hour                    +
                      temp_level              +
                      clouds_all_level    
                    ,data = final_data_set
                    ,parms = list(split = "gini") #gini,information
                    ,control = rpart.control(cp = 0))

base_model$variable.importance

