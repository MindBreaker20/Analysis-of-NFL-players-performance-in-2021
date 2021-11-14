#Libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(e1071)
library(purrr)
library(MASS)
library(cluster) 
library(magrittr)
library(dplyr)
library(ggpubr)

#The creation of data frame called data_nfl filled with data from xlsx file 
data_nfl <- as.data.frame(read_excel("data_NFL.xlsx")) 

#--------------------------------------Preliminary analysis of used variables------------------------------------------
#The search of outliers with help of boxplot visualizations
par(mfrow=c(3,2))
boxplot(data_nfl$age,
        main="Age variable box plot",
        xlab="Age of players in years",
        col="palegreen",
        freq=FALSE)

boxplot(data_nfl$won,
     main="won variable box plot",
     xlab="Number of won matches",
     col="yellow",
     freq=FALSE
)

boxplot(data_nfl$lost,
     main="Lost variable box plot",
     xlab="Number of lost matches",
     col="salmon",
     freq=FALSE
)

boxplot(data_nfl$ypa,
     main="Ypa variable box plot",
     xlab="Average profit per trial",
     col="lightskyblue1",
     freq=FALSE
)

boxplot(data_nfl$int,
     main="Int variable box plot",
     xlab="Number of opponents' passes intercepted",
     col="orange",
     freq=FALSE
)

boxplot(data_nfl$td,
     main="Box plot of the td variable",
     xlab="The number of attempts by a player",
     col="purple",
     freq=FALSE
)

#Basic variable statistics
summary(data_nfl)

#skewness
skewness(data_nfl$age)
skewness(data_nfl$won)
skewness(data_nfl$lost)
skewness(data_nfl$ypa)
skewness(data_nfl$int)
skewness(data_nfl$td)

#kurtosis
kurtosis(data_nfl$age)
kurtosis(data_nfl$won)
kurtosis(data_nfl$lost)
kurtosis(data_nfl$ypa)
kurtosis(data_nfl$int)
kurtosis(data_nfl$td)

#coefficient of variation
cv_age = sd(data_nfl$age)/mean(data_nfl$age)
print(cv_age)
cv_won = sd(data_nfl$won)/mean(data_nfl$won)
print(cv_won)
cv_lost = sd(data_nfl$lost)/mean(data_nfl$lost)
print(cv_lost)
cv_ypa = sd(data_nfl$ypa)/mean(data_nfl$ypa)
print(cv_ypa)
cv_int = sd(data_nfl$int)/mean(data_nfl$int)
print(cv_int)
cv_td = sd(data_nfl$td)/mean(data_nfl$td)
print(cv_td)

#correlation matrix
cor_matrix <- cor(data_nfl[, -1])
round(cor_matrix, 2)

#---------------------------------------------HELLWIG METHOD----------------------------------------------------
#setting optimal value
optimal_value = 27 #mean
data_h <- data_nfl #the creation of new data frame for data transformation during Hellwig method implementation

#multiplying variable lost by -1
data_h[4] = data_h[4] * (-1)

#changing variables into simulants
l1 = as.numeric(length(data_h[,2])) #obtained data fram'se length
for(j in 1:l1){
  k = as.numeric(data_h[j, 2])
  if( k == optimal_value){ #implementation of given equation for data transformation
    k = 1
  }
  else if(k < optimal_value){
    k = - 1/(k - optimal_value - 1)
  }
  else{
    k = 1/(k - optimal_value + 1)
  }
  data_h[j, 2] = k
}

#standarization + template establishment
w1 = as.numeric(length(data_h[1,])) #the width of obtained data frame

for(j in 2:w1){
  m = mean(data_h[, j])
  s = sd(data_h[, j])
  for(l in 1:l1){
    k = as.numeric(data_h[l, j])
    k = (k - m)/s
    data_h[l, j] = k
  }
}

#templates - searching max value
templates <- c() #empty vector for future values

for(j in 2:w1){
  z = j - 1
  k = max(data_h[, j])
  templates[z] <- k
}

#calculation of assets distances from established template
for(j in 2:w1){
  for(l in 1:l1){
    z = j - 1
    k = as.numeric(data_h[l, j])
    k = (k - templates[z])^2
    data_h[l, j] = k
  }
}

dist<- c()
for(j in 1:l1){
  s = 0.0
  for(l in 2:w1){
    k = as.numeric(data_h[j, l])
    s = s + k
  }
  s = sqrt(s)
  dist[j] <- s
}
data_h["distance"] <- dist

#max possible distance calculation
calc_mean = mean(data_h$distance)
calc_sd = sd(data_h$distance)
d0 = calc_mean + 2 * calc_sd

#value of measure
#creation of new data frame
hellwig = data.frame(data_h$player)

h <- c() #empty vector for future Hellwig values

for(j in 1:l1){
  k = as.numeric(data_h[j, 7])
  k = 1 - (k / d0)
  h[j] = k
}

#A cretion of final comparison
hellwig["Hellwig"] <- h
names(hellwig) <- c("Player", "Hellwig")
hellwig %>% arrange(desc(Hellwig), .by_group = TRUE)

#-------------------------------------------------STANDARDIZED SUM METHOD------------------------------------------------------
#optimal value configuration
optimal_value = 27 #mean
data_ssm <- data_nfl #a copy of NFL data frame

#age variable multiplicitation by -1
data_ssm[4] = data_ssm[4] * (-1)

#changing variables into simulants
#variable zawartosc.alk
for(j in 1:l1){
  k = as.numeric(data_ssm[j, 2])
  if( k == optimal_value){
    k = 1
  }
  else if(k < optimal_value){
    k = - 1/(k - optimal_value - 1)
  }
  else{
    k = 1/(k - optimal_value + 1)
  }
  data_ssm[j, 2] = k
}

#standarization + template establishment
for(j in 2:w1){
  m = mean(data_ssm[, j])
  s = sd(data_ssm[, j])
  for(l in 1:l1){
    k = as.numeric(data_ssm[l, j])
    k = (k - m)/s
    data_ssm[l, j] = k
  }
}

#sum by rows
data_ssm$s_rang <- rowSums( data_ssm[, 2:7])

w <- c() #empty vector for future indicator values
for(j in 1:l1){
  s = as.numeric(data_ssm[j,8])
  max_diff = max(data_ssm$s_rang) - min(data_ssm$s_rang)
  s = (s - min(data_ssm$s_rang))/max_diff
  w[j] = s
}

data_ssm$indicator <- w

standardized_sums = data.frame(data_ssm$player)
names(standardized_sums) <- "Player"
standardized_sums["indicator"]<- w
standardized_sums%>% arrange(desc(indicator),group = TRUE)

#---------------------------------------------------CLUSTER ANALYSIS--------------------------------------------------------
data2 = data_nfl
rownames(data2) <- data2$player
data2 <- subset(data2, select = -c(1))
data2 = scale(data2)
#--------------------------------------------------K-MEANS----------------------------------------------------------------
set.seed(123)

wss <- function(k) {
  kmeans(data2, k = 3, nstart = 10 )$tot.withinss
}

k.values <- 1:15

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Liczba klastrów K",
     ylab="Ca³kowita suma kwadratów wewn¹trz klastrów")

results <- kmeans(data2,centers=3)

par(mfrow=c(1,1))
clusplot(data2,results$cluster,color=TRUE,shade=TRUE,labels=2,lines = 0,
         main="Grouping of NFL players using the k-means method at k = 3",xlab="Dim1",
         ylab="Dim2")

#Clusters creation for future statistics
#cluster 1
players1 <- c("Zach Wilson", "Davis Mills", "Jared Goff", "Trevor Lawrence", "Justin Fields", "Sam Darnold", "Mac Jones", 
              "Jalen Hurts", "Daniel Jones", "Tua Tagovailoa")
age1 <- c(22, 23, 27, 22, 22, 24, 23, 23, 24, 23)
won1 <- c(1, 0, 0, 1, 2, 3, 3, 2, 2, 1)
lost1 <- c(5, 5, 7, 5, 3, 4, 4, 5, 5, 3)
ypa1 <- c(6.5, 6.1, 6.5, 6.8, 6.2, 6.8, 7.2, 7.1, 7.2, 7.1)
int1 <- c(9, 7, 6, 8, 6, 8, 6, 4, 4, 4)
td1 <- c(4, 5, 8, 7, 2, 7, 9, 10, 5, 7)
cluster1 <- data.frame(players1, age1, won1, lost1, ypa1, int1, td1)
summary(cluster1)

#cluster 2
players2 <- c("Geno Smith", "Jimmy Garoppolo", "Teddy Bridgewater", "Baker Mayfield", "Carson Wentz", "Ryan Tannehill",
              "Matt Ryan", "Kirk Cousins", "Russell Wilson")
age2 <- c(31, 30, 28, 26, 28, 33, 36, 33, 32)
won2 <- c(0, 2, 3, 3, 3, 5, 3, 3, 2)
lost2 <- c(2, 3, 4, 3, 4, 2, 3, 3, 3)
ypa2 <- c(7.1, 7.6, 7.4, 8.5, 7.7, 7.6, 6.8, 7.4, 9.6)
int2 <- c(1, 4, 5, 3, 1, 5, 4, 2, 1)
td2 <- c(3, 3, 12, 6, 11, 7, 12, 13, 10)
cluster2 <- data.frame(players2, age2, won2, lost2, ypa2, int2, td2)
summary(cluster2)

#cluster 3
players3 <- c("Patrick Mahomes", "Justin Herbert", "Lamar Jackson", "Joe Burrow", "Josh Allen", "Jameis Winston",
              "Derek Carr", "Dak Prescott", "Kyler Murray", "Matthew Stafford")
age3 <- c(26, 23, 24, 24, 25, 27, 30, 28, 24, 33)
won3 <- c(3, 4, 5, 5, 4, 4, 5, 5, 7, 6)
lost3 <- c(4, 2, 2, 2, 2, 2, 2, 1, 0, 1)
ypa3 <- c(7.6, 7.2, 8.6, 9.2, 7.5, 7.4, 8.5, 8.4, 9.0, 9.0)
int3 <- c(9, 4, 5, 8, 3, 3, 5, 4, 7, 4)
td3 <- c(18, 14, 10, 17, 15, 13, 12, 16, 17, 19)
cluster3 <- data.frame(players3, age3, won3, lost3, ypa3, int3, td3)
summary(cluster3)

#-------------------------------------------------K-MEDOID----------------------------------------------------------------
set.seed(123)
kmed <- pam(data2, k = 3)
clusplot(data2,kmed$cluster,color=TRUE,shade=TRUE,labels=2,lines = 0,
         main="Grouping of NFL players using the k-medoid method at k = 3",xlab="Dim1",
         ylab="Dim2")

#--------------------------------------------------WARD---------------------------------------------------------------
# Dissimilarity matrix
d <- dist(data2, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "ward.D" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1, main = "Grouping by Ward's method at k = 3")
#-------------------------------------------------CLASSICA METRIC MDS------------------------------------------------------
data1 = data_nfl
rownames(data1) <- data1$player
data1 <- subset(data1, select = -c(1))

# Cmpute MDS
mds <- data1 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(data1),
          size = 1,
          repel = TRUE)

clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(data1),
          color = "groups",
          palette = "jco",
          main = "Classic multidimensional scaling",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

#-------------------------------------------------------SAMMON-------------------------------------------------------
mds <- data1 %>%
  dist() %>%          
  sammon() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(data1),
          size = 1,
          repel = TRUE)

clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(data1),
          color = "groups",
          palette = "jco",
          main = "Sammon's scaling method",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)