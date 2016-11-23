library(ggplot2)

#Read in Orange dataset from R into data.frame
df <- data.frame(Orange)
#get summary of Orange dataset
summary(df)
#get structure of each columns
str(df$Tree)
str(df$age)
str(df$circumference)


#Calculate the mean and the median of the trunk 
#circumferences for different size of the trees. (Tree)

#aggregate data.frame by Tree and compute mean circumference
circum.mean <- aggregate(df$circumference,by=list(df$Tree),FUN=mean)
colnames(circum.mean) <- c("Tree","Avg Circumference")
knitr::kable(circum.mean)

#aggregate data.frame by Tree and compute median circumference
circum.median <- aggregate(df$circumference,by=list(df$Tree),FUN=median)
colnames(circum.median) <- c("Tree","Med. Circumference")
knitr::kable(circum.median)


#Make a scatter plot of the trunk circumferences against the age of the tree. 
#Use different plotting symbols for different size of trees.**


p <- ggplot(df) + geom_point(aes(y=df$circumference,x=df$age,colour=df$Tree)) +
  scale_colour_hue(l=80, c=150)
p + labs(title="Scatter Plot \n Age vs Circumference by Tree",x="Age",y="Circumference",
         colour="Tree")

p <- ggplot(df,aes(y=df$circumference,x=df$age,colour=df$Tree)) + geom_point() +
  geom_line(size=1,alpha=0.8) + scale_colour_hue(h=c(180, 270))
p + labs(title="Line Plot: \n Age vs Circumference by Tree",x="Age",y="Circumference",
         colour="Tree")

#Display the trunk circumferences on a comparative boxplot against tree. 
#order the boxplots in the increasing order of maximum diameter.

circum.max <- aggregate(df$circumference,by=list(df$Tree),FUN=max)#aggregate for max circum
colnames(circum.max) <- c("Tree","Max Circum.") #rename columns
knitr::kable(circum.max) #knit table

factor(df$Tree,c("3","1","5","2","4")) #reorder the boxplot for max circum. by tree
p<-ggplot(df,aes(x=Tree,y=circumference))+ geom_boxplot(aes(fill=Tree))# ggplot: boxplot 
p + labs(title="Box Plot: Trunk Circumference", y="Circumference",x="Tree")



### Question 4
#1.)	First, download “Temp” data set. Find the difference between the 
#maximum and the minimum monthly average temperatures for each country and 
#report/visualize top 20 countries with the maximum differences for the period since 1900.

#Load csv
temp <- read.csv("Data/Temp.csv",header=TRUE)
# Remove any columns with "NA"
temp1 <- temp[!(is.na(temp$Monthly.AverageTemp)),]
#Return first 5 columns
head(temp1)
#Dimensions of the dataset
dim(temp1)

temp2 <- subset(temp1, as.Date(Date) >= "1900-01-01")
head(temp2)

#Aggregate for max and min average temps
temp.max <- aggregate(temp1["Monthly.AverageTemp"],by=temp1["Country"],FUN=max)
temp.min <- aggregate(temp1["Monthly.AverageTemp"],by=temp1["Country"],FUN=min,na.rm=TRUE)

#Create new data.frame to join the two aggregated list
data <- data.frame(temp.max,temp.min)
#Drop extra Country column
data$Country.1 <- NULL
head(data)
#Rename column
colnames(data)<-c("Country","Max Avg. Temp","Min Avg. Temp")
head(data)
#Take difference between max and min avg. temp columns
data$Diff <- data$'Max Avg. Temp' - data$'Min Avg. Temp'

#Sort the dataframe by decreasing Diff
data <-data[order(data$Diff,data$Country,decreasing = TRUE),]
head(data)
#Subset the data to only take the first 20 columns with highest temp diff.
data.sub <- data[1:20,] 

#####Needs Changing.....
#plot Country vs Temp Diff
p<- ggplot(data.sub,aes(Country,fill=Diff))+geom_bar() + coord_flip()
p + labs(title="Country vs Change in Temperature",
         x="Maximum - Minimum Avg. Monthly Temp",
         y = "Country")

