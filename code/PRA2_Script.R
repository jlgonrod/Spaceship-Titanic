# Package names
packages <- c("tidyr", "dplyr","ggplot2", "keras","reshape2","tidyverse",
              "caret","ROCR", "knitr", 'nortest', "bestNormalize","corrplot",
              "arules")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Set seed for the repeatability
set.seed(15463)

# Load the dataframe in the object called df
df <- read.csv("~/MASTER CIENCIA DE DATOS/Tipologia y ciclo de vida de los datos/Practicas/Práctica2/Ejercicio/Repositorio/data/raw_data/train.csv",
               colClasses=c("HomePlanet"="factor",
                            "CryoSleep"="logical",
                            "Destination"="factor",
                            "VIP"="logical",
                            "Transported"="logical"))

# Transform the field Age into integer
df$Age <- as.integer(df$Age)

# The structure of the dataframe is shown.
str(df)

# The variable Name is dropped because it doesn't apport relevant information.
df <- select(df, -Name)

# 5 new variables are created based on two preexisting variables.
df <- df %>%
  mutate(PassengerGroup=
           as.character(sapply(strsplit(PassengerId,"_"), `[`, 1))) %>%
  mutate(PassengerNumInGroup=
           as.factor(sapply(strsplit(PassengerId,"_"), `[`, 2))) %>%
  mutate(CabinPlatform = 
           as.factor(sapply(strsplit(Cabin,"/"), `[`, 1))) %>%
  mutate(CabinNumber = 
           as.integer(sapply(strsplit(Cabin,"/"), `[`, 2))) %>%
  mutate(CabinSide = 
           as.factor(sapply(strsplit(Cabin,"/"), `[`, 3)))

# Cabin is dropped because gives the same info than other variables.
df <- select(df, -Cabin)

# A summary od the dates is showed
summary(df)

# The empty level name from values of Homeplanet and Destination are replaced 
# with the value Unknown.
levels(df$HomePlanet) <- c("Unknown", "Earth", "Europa", "Mars")
levels(df$Destination) <- c("Unknown", "55 Cancri e", "PSO J318.5-22",
                            "TRAPPIST-1e")

# Check the amount of NA's values in each column.
sapply(df, function(x) sum(length(which(is.na(x)))))

# The rows with NA values in Age, CryoSleep, CabinPlatform, CabinName 
# and CabinSide are dropped.
df <- subset(df, !is.na(df$Age) & 
               !is.na(df$CryoSleep) &
               !is.na(df$CabinPlatform) & 
               !is.na(df$CabinNumber) & 
               !is.na(df$CabinSide), 
             
             select = colnames(df))

# The lack of value in the field VIP entail the row elimination
df <- subset(df, !is.na(df$VIP), select = colnames(df))

# the elimination of all the null regristers and it's implications are studied.
dfGr <- na.omit(df)
dfGr %>%
  group_by(dfGr$VIP) %>%
  summarize(across(c(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck),
                   mean,
                   na.rm = TRUE))

# Based on the previous analysis, the NA values are asiigned the mean base on
# the VIP or no VIP as the mean of those groups for that variable.

roomServiceVIP = mean(df[df$VIP == TRUE,]$RoomService, na.rm = TRUE)
roomServiceNVIP = mean(df[df$VIP == FALSE,]$RoomService, na.rm = TRUE)

foodCourtVIP = mean(df[df$VIP == TRUE,]$FoodCourt, na.rm = TRUE)
foodCourtNVIP = mean(df[df$VIP == FALSE,]$FoodCourt, na.rm = TRUE)

shoppingMallVIP = mean(df[df$VIP == TRUE,]$ShoppingMall, na.rm = TRUE)
shoppingMallNVIP = mean(df[df$VIP == FALSE,]$ShoppingMall, na.rm = TRUE)

spaVIP = mean(df[df$VIP == TRUE,]$Spa, na.rm = TRUE)
spaNVIP = mean(df[df$VIP == FALSE,]$Spa, na.rm = TRUE)

vrdeckVIP = mean(df[df$VIP == TRUE,]$VRDeck, na.rm = TRUE)
vrdeckNVIP = mean(df[df$VIP == FALSE,]$VRDeck, na.rm = TRUE)

df$RoomService[is.na(df$RoomService) & df$VIP == TRUE] <- roomServiceVIP
df$RoomService[is.na(df$RoomService) & df$VIP == FALSE] <- roomServiceNVIP

df$FoodCourt[is.na(df$FoodCourt) & df$VIP == TRUE] <- foodCourtVIP
df$FoodCourt[is.na(df$FoodCourt) & df$VIP == FALSE] <- foodCourtNVIP

df$ShoppingMall[is.na(df$ShoppingMall) & df$VIP == TRUE] <- shoppingMallVIP
df$ShoppingMall[is.na(df$ShoppingMall) & df$VIP == FALSE] <- shoppingMallNVIP

df$Spa[is.na(df$Spa) & df$VIP == TRUE] <- spaVIP
df$Spa[is.na(df$Spa) & df$VIP == FALSE] <- spaNVIP

df$VRDeck[is.na(df$VRDeck) & df$VIP == TRUE] <- vrdeckVIP
df$VRDeck[is.na(df$VRDeck) & df$VIP == FALSE] <- vrdeckNVIP

# Na's values are checkede again. Now there are 0.
sapply(df, function(x) sum(length(which(is.na(x)))))

# The boxplot is ploted to detect the outliers.
boxplotCols <- c("RoomService","FoodCourt","ShoppingMall","Spa","VRDeck")
boxplot(select(df, boxplotCols), col = rainbow(length(boxplotCols)))

# The amount of register for each value of transported is ploted
ggplot(data = df) + geom_bar(mapping = aes(x = Transported, fill = Transported))

# It's also shown numerically
df %>%
  count(Transported)

# A new field with the total amount spended is created.
df_exp <- df
df_exp$TotalExpenses <- df_exp$RoomService +
  df_exp$FoodCourt + 
  df_exp$ShoppingMall + 
  df_exp$Spa + 
  df_exp$VRDeck
df_exp <- select(df_exp, c("HomePlanet", 
                           "CryoSleep", 
                           "Destination",
                           "Age",
                           "VIP",
                           "PassengerGroup",
                           "CabinPlatform",
                           "CabinSide",
                           "TotalExpenses",
                           "Transported"))

# The realiton between the vatiable display and TotalExpenses is ploted
ggplot(data = df_exp, 
       mapping = 
         aes(x = TotalExpenses)) + 
  geom_freqpoly(mapping = aes(colour = Transported), binwidth = 100)

# TotalExpenses is discretized
table(discretize(df_exp$TotalExpenses, "cluster" ))

hist(df_exp$TotalExpenses,
     main="Distribución de Gasto por Pasajero",
     xlab="Gasto", 
     ylab="Pasajeros",
     col = "ivory")
abline(v=discretize(df_exp$TotalExpenses, method="cluster", onlycuts=TRUE),
       col="red")

df_exp$TotalExpenses_KM <- discretize(df_exp$TotalExpenses, "cluster" )

# The relation between TotalExpenses y Transported is ploted
ggplot(data = df_exp) +
  geom_bar(mapping = aes(x = Transported, fill = TotalExpenses_KM), 
           position = "fill")

# The relation between the variables Tranported y CabinSide is studied
ggplot(data = df_exp) + geom_count(mapping = aes(x = Transported, 
                                                 y = CabinSide))

# The relation between the variables Tranported y CabinPlatform is studied
df_exp %>%
  count(Transported, CabinPlatform) %>%
  ggplot(mapping = aes(x = Transported, y = CabinPlatform)) +
  geom_tile(mapping = aes(fill = n))

# The relation between the variables Tranported y CryoSleep is studied
df_exp %>%
  count(Transported, CryoSleep) %>%
  ggplot(mapping = aes(x = Transported, y = CryoSleep)) + 
  geom_tile(mapping = aes(fill = n))

# The normality of RoomService is checked
par(mfrow=c(1,2))

hist(df$RoomService)
qqnorm(df$RoomService, main="Q-Q RoomService")
qqline(df$RoomService,col=2)

ad.test(df$RoomService)

# The normality of FoodCourt is checked
par(mfrow=c(1,2))

hist(df$FoodCourt)
qqnorm(df$FoodCourt, main="Q-Q FoodCourt")
qqline(df$FoodCourt,col=2)

ad.test(df$FoodCourt)

# The normality of ShoppingMall is checked
par(mfrow=c(1,2))

hist(df$ShoppingMall)
qqnorm(df$ShoppingMall, main="Q-Q ShoppingMall")
qqline(df$ShoppingMall,col=2)

ad.test(df$ShoppingMall)


# The normality of Spa is checked
par(mfrow=c(1,2))

hist(df$Spa)
qqnorm(df$Spa, main="Q-Q Spa")
qqline(df$Spa,col=2)

ad.test(df$Spa)

# The normality of VRDeck is checked
par(mfrow=c(1,2))

hist(df$VRDeck)
qqnorm(df$VRDeck, main="Q-Q VRDeck")
qqline(df$VRDeck,col=2)

ad.test(df$VRDeck)

# The normality of Age is checked

par(mfrow=c(1,2))

hist(df$Age)
qqnorm(df$Age, main="Q-Q Age")
qqline(df$Age,col=2)

ad.test(df$Age)

# The homocedasticity of RoomService is checked
fligner.test(RoomService ~Transported, data=df)
boxplot(RoomService~Transported, data = df, 
        main="Comprobación de homocedasticidad para RoomService", 
        ylim=c(0,2000))

# The homocedasticity of FoodCourt is checked
fligner.test(FoodCourt ~Transported, data=df)
boxplot(FoodCourt~Transported, data = df, 
        main="Comprobación de homocedasticidad para FoodCourt", 
        ylim=c(0,500))

# The homocedasticity of ShoppingMall is checked
fligner.test(ShoppingMall ~Transported, data=df)
boxplot(ShoppingMall~Transported, data = df, 
        main="Comprobación de homocedasticidad para ShoppingMall",
        ylim=c(0,300))

# The homocedasticity of Spa is checked
fligner.test(Spa ~Transported, data=df)
boxplot(Spa~Transported, data = df, 
        main="Comprobación de homocedasticidad para Spa", 
        ylim=c(0,1500))

# The homocedasticity of VRDeck is checked
fligner.test(VRDeck ~Transported, data=df)
boxplot(VRDeck~Transported, data = df, 
        main="Comprobación de homocedasticidad para VRDeck", ylim=c(0,1200))

# The homocedasticity of Age is checked
fligner.test(Age ~ Transported, data=df)
boxplot(Age~Transported, data = df, 
        main="Comprobación de homocedasticidad para Age")

# CORRELATION ANALYSIS

# All the vars are transformed into numeric and sotred in a new df.
df_cor <- df[,2:17]
df_cor <- df_cor[,-12:-13]
df_cor$HomePlanet <- unclass(df_cor$HomePlanet)
df_cor$Destination <- unclass(df_cor$Destination)
df_cor$VIP <- unclass(df_cor$VIP)
df_cor$Transported <- unclass(df_cor$Transported)
df_cor$CabinPlatform <- unclass(df_cor$HomePlanet)
df_cor$CabinSide <- unclass(df_cor$HomePlanet)

df_cor$CryoSleep[df_cor$CryoSleep==TRUE] = 1
df_cor$CryoSleep[df_cor$CryoSleep==FALSE] = 0

df_cor$VIP[df_cor$VIP==TRUE] = 1
df_cor$VIP[df_cor$VIP==FALSE] = 0

df_cor$Transported[df_cor$Transported==TRUE] = 1
df_cor$Transported[df_cor$Transported==FALSE] = 0

head(df_cor)

df_cor[] <- lapply(df_cor, function(x) as.numeric(as.character(x)))
head(df_cor)

# the correlation is studied with the Sperman method
res<- cor(df_cor, method = "spearman", use = "complete.obs")
round(res, 2)

# The correlation is plotted
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# HYPOTHESIS CONTRAT
# It's calculated the total amount spent and sotred in a variable
df <- df %>%
  mutate(amountSpent = RoomService+FoodCourt+ShoppingMall+Spa+VRDeck)
kable(cbind("amountSpent"=head(df)$amountSpent), align = 'c')

# There're two subvectors with the amountSpent separated by group
amountSpenteTransported <- filter(df, Transported==TRUE)$amountSpent

amountSpenteNOTransported <- filter(df, Transported==FALSE)$amountSpent

# The test is runed with the wilcox method
wilcox.test(amountSpenteNOTransported, amountSpenteTransported)

#LOGARITHMIC REGRESSION MODEL

# The data partition is created, 70% to train and 30% to validate
df_raw <- df

Index <- createDataPartition(df_raw$Transported, p=0.7, list=FALSE,times=1)

df <- df_raw[Index,]
df_validation <- df_raw[-Index,]

# The variables implemented are selected
df1 <- select(df, -c(PassengerId, PassengerGroup, HomePlanet, Destination, VIP,
                     PassengerNumInGroup, CabinPlatform, Age, amountSpent))

# The model is trained. And the summary plotted
model_log <- glm(Transported ~ ., data = df1, family = binomial(link='logit'))
summary(model_log)

# The model is used to predict
fitted.results <- predict(model_log, 
                          newdata = select(df_validation, c(CryoSleep,
                                                            RoomService,
                                                            FoodCourt,
                                                            ShoppingMall,
                                                            Spa,VRDeck,
                                                            CabinNumber,
                                                            CabinSide))
                          ,type = "response")

fitted.results_FACTOR <- ifelse(fitted.results > 0.5,TRUE,FALSE)

resultado_comprobacion <- cbind("ID"=df_validation$PassengerId, 
                                "Transported"=df_validation$Transported,
                                "Prediction"=fitted.results_FACTOR)

df_validation_info <- as.data.frame(resultado_comprobacion)
kable(head(df_validation_info),align='c', row.names=FALSE)

# The model is evaluated with the confussion Matrix
df_validation_info$Transported <- as.factor(df_validation_info$Transported)
df_validation_info$Prediction <- as.factor(df_validation_info$Prediction)

confusionMatrix(df_validation_info$Transported, df_validation_info$Prediction)

# The ROC curve is plotted and the area under the curve is calculated

pr <- prediction(fitted.results, df_validation$Transported)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


