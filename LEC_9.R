#-----Section 01-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
London.dis <- read.csv("09_London_districts.csv", stringsAsFactors = FALSE)
head(London.dis)
str(London.dis)

# check for missing data
apply(London.dis, MARGIN = 2, FUN = function(x) sum(is.na(x)))
install.packages("Amelia")
library(Amelia)
missmap(London.dis, col = c("black", "grey"), legend = FALSE)
#London.dis <- na.omit(London.dis)

# select a dependent variable and independent variables
London.dis2 <- data.frame(London.dis$Life_Male,  London.dis$Dom_Build, London.dis$Smoking, London.dis$Obese,
                          London.dis$Episodes, London.dis$Benefits, London.dis$Crime)
colnames(London.dis2) <- c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime")

#-----Section 02-------------------------------------------

# correlation matrix

# Correlations among numeric variables in
cor.matrix <- cor(London.dis2, use = "pairwise.complete.obs", method = "spearman")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

# rename rows and columns
#dimnames(cor.df) <- list(c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"),
#                         c("Life_Male","Dom_Build", "Smoking", "Obese", "Episodes", "Benefits", "Crime"))
round(cor.df, 2)

#-----Section 03-------------------------------------------
install.packages("psych")
library(psych)

pairs.panels(London.dis2, method = "spearman", hist.col = "grey", col = "blue", main = "Spearman")
install.packages("corrgram")
library(corrgram)
# corrgram works best with Pearson correlation
corrgram(London.dis2, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="London variables")


#-----Section 04-------------------------------------------

#cor.test(x, y,
#	alternative = c("two.sided", "less", "greater"),
#	method = c("pearson", "kendall", "spearman"),
#	exact = NULL, conf.level = 0.95, continuity = FALSE, ...)

# test correlation of dependent variable with all independent variables
cor.test(London.dis$Life_Male, London.dis$Dom_Build, method = "spearman")
cor.test(London.dis$Life_Male, London.dis$Smoking, method = "spearman")
cor.test(London.dis$Life_Male, London.dis$Obese, method = "spearman")
cor.test(London.dis$Life_Male, London.dis$Episodes, method = "spearman")
cor.test(London.dis$Life_Male, London.dis$Benefits, method = "spearman")
cor.test(London.dis$Life_Male, London.dis$Crime, method = "spearman")

# looking at internal correlations between three variables
cor.test(London.dis$Smoking,London.dis$Benefits, method = "spearman")
cor.test(London.dis$Smoking,London.dis$Crime, method = "spearman")
cor.test(London.dis$Benefits,London.dis$Crime, method = "spearman")

#-----Section 05-------------------------------------------

#partial correlation
install.packages("ppcor")
library(ppcor)

#calculate partial correlation using Pearson and then Spearman
pcor.test(London.dis$Life_Male, London.dis$Benefits, London.dis$Smoking)
pcor.test(London.dis$Life_Male, London.dis$Smoking, London.dis$Benefits)
pcor.test(London.dis$Life_Male, London.dis$Benefits, London.dis$Smoking, method="spearman")
pcor.test(London.dis$Life_Male, London.dis$Smoking, London.dis$Benefits, method="spearman")

#-----Section 06-------------------------------------------

# select variables by excluding those not required; the %in% operator means 'matching'
myvars <- names(London.dis) %in% c("Code", "Name", "Age0_19","Age20_64", "Age65plus",
                                   "Deprivation", "Life_Male", "Life_Female")

# the ! operator means NOT
London.dis3 <- London.dis[!myvars]
str(London.dis3)
rm(myvars)

#-----Section 07-------------------------------------------

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(London.dis3))

# Determine Number of Factors to Extract
install.packages("nFactors")
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(London.dis3))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

#-----Section 08-------------------------------------------

# Varimax Rotated Principal Components
# retaining 'nFactors' components
install.packages("GPArotation")
library(GPArotation)

# principal() uses a data frame or matrix of correlations
fit <- principal(London.dis3, nfactors=4, rotate="varimax")
fit

#-----Section 09-------------------------------------------

# weed out further variables after first factor analysis
myvars <- names(London.dis3) %in% c("Binge_Drink", "Benefits")
London.dis3 <- London.dis3[!myvars]
str(London.dis3)
rm(myvars)

# get eigenvalues
ev <- eigen(cor(London.dis3))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
fit <- principal(London.dis3, nfactors=4, rotate="varimax")
fit

#-----Section 10-------------------------------------------

attach(London.dis3)

# Select the variables after factor analysis for use in clustering

# Is crime the best for component PC3?
boxplot(Crime, NonDom_Build, Dom_Gardens,
        names=c("Crime", "NonDom_Build", "Dom_Gardens"))
boxplot(Greenspace, Dom_Build,
        names=c("Greenspace", "Dom_Build"))

myvars <- c("Dom_Build", "Smoking", "NonDom_Build", "Obese")
London.dis3 <- London.dis[myvars]
str(London.dis3)
rm(myvars)

# Prepare Data
London.dis3 <- na.omit(London.dis3) # listwise deletion of missing
boxplot(London.dis3) # visualise the variables
# scale to 0-1
London.dis3 <- apply(London.dis3, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
summary(London.dis3)
boxplot(London.dis3)  # visualise the variables after scaling

#-----Section 11-------------------------------------------

# Ward Hierarchical Clustering
d <- dist(London.dis3, method = "euclidean") # distance matrix

set.seed(12345)
fit <- hclust(d, method="ward.D")
# plot(fit) # display dendogram
plot (fit, labels = London.dis$Name)
# draw dendogram with red borders around the 4 clusters
groups <- cutree(fit, k=4) # cut tree into 4 clusters
rect.hclust(fit, k=4, border="green")

# assign cluster number to borough name
mydata1 <- data.frame(London.dis$Code)
names(mydata1)[1] <- "Code"
mydata1 <- within (mydata1, Name <- London.dis$Name)
mydata1 <- within (mydata1, cluster <- groups)

mydata1
write.csv(mydata1,file.choose())

#-----Section 12-------------------------------------------

# K-Means Cluster Analysis
set.seed(12345)
fit <- kmeans(London.dis3, 4) # 4 cluster solution

#visualise clusters
install.packages("cluster")
install.packages("fpc")
library(cluster)
library(fpc)

clusplot(London.dis3, fit$cluster)       # from library(cluster)
plotcluster(London.dis3, fit$cluster)    # from library(fpc)

# get cluster means
aggregate(London.dis3, by=list(fit$cluster),FUN = mean)

# append cluster assignment
mydata2 <- data.frame(London.dis$Code)
names(mydata2)[1] <- "Code"
mydata2 <- within (mydata2, Name <- London.dis$Name)
mydata2 <- data.frame(mydata2, fit$cluster)

mydata2
write.csv(mydata2,file.choose())

#-----Section 13-------------------------------------------

library(sf)
library(tmap)
library(dplyr)

tmap_mode("plot")

london.polygon <- st_read("london_polygon.shp", quiet = TRUE)
thames <- st_read("thames.shp", quiet = TRUE)

london.jn <- left_join(london.polygon, mydata1, by = c("CENSUS_COD" = "Code"))
st_geometry_type(london.jn)
st_geometry(london.jn)

tm_shape(london.jn)+
  tm_fill("cluster", 
          palette = "Oranges", 
          legend.is.portrait = TRUE) +
  tm_shape(thames) +
  tm_fill(col = "lightblue") +
  tm_layout(title = "London Clusters",
            title.position = c("right","top"),
            title.color = "grey65",
            title.size = 1.5,
            legend.height = 0.35, 
            legend.width = 0.35,
            legend.outside = FALSE,
            legend.position = c("right", "bottom"),
            frame = TRUE)

#-----Section 14-------------------------------------------

detach(London.dis3)

#-----Section 15-------------------------------------------
# using different data types (numeric, ordinal, categorical)

# read in data from csv file
alcohol <- read.csv("SHG_Alcohol.csv", stringsAsFactors = FALSE)
str(alcohol)

#------------------------------------------------
# Prepare Data

# remove ID field
alc <- as.data.frame(alcohol[-1])

# listwise deletion of missing data
alc <- na.omit(alc)

# ordinal data
cols1 <- c("Vill_SocProb1", "Vill_SocProb2", "Vill_SocProb3", "Vill_SocProb4",
           "Vill_SocProb5", "Vill_SocProb6")
alc[cols1] <- lapply(alc[cols1], ordered, levels = c(6, 5, 4, 3, 2, 1), labels = c("6", "5", "4", "3", "2", "1"))
str(alc)

# factors
cols2 <- c("SHG_ProbSolve", "SHG_AlcSolve1", "SHG_AlcSolve2", "SHG_AlcSolve3",
           "SHG_AlcSolve4", "SHG_AlcSolve5", "SHG_AlcSolve6", "SHG_AlcSolve7",
           "SHG_AlcProb1", "SHG_AlcProb2", "SHG_AlcProb3", "SHG_AlcProb4",
           "SHG_AlcProb5", "SHG_AlcProb6", "SHG_AlcProb7")
alc[cols2] <- lapply(alc[cols2], factor, levels = c(0, 1), labels = c("No", "Yes"))
str(alc)

#------------------------------------------------
# heterogeneous correlations
install.packages("polycor")
library(polycor)

alc.cor <- hetcor(alc)
alc.cor$type
round(alc.cor$correlations, 2)
library(corrgram)
# corrgram works best with Pearson correlation
corrgram(alc.cor$correlations, order=FALSE)

#------------------------------------------------
# (Hierarchical) Agglomerative Clustering using Gower dissimilarity

g.dist <- daisy(alc, metric = "gower") # dissimilarity matrix

set.seed(12345)
fit <- hclust(g.dist, method="complete")
plot (fit, labels = FALSE, main = "Agglomerative complete linkages")
groups <- cutree(fit, k=4) # cut tree into 4 clusters
# draw dendrogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red")

# assign cluster to each record
alc <- within (alc, cluster <- groups)
alc$cluster <- as.factor(alc$cluster)

write.csv(alc, "alcohol_cluster.csv")

#------------------------------------------------
# Inspect clusters

# boxplot numeric variable
boxplot(alc$SHG_time ~ alc$cluster)

# crosstable for categorical
install.packages("gmodels")
library(gmodels)
xtab <- CrossTable(alc$cluster, alc$SHG_ProbSolve, digits = 2, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#-----Section 16-------------------------------------------

# remove all variables from the environment
rm(list=ls())
