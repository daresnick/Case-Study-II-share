#Load R Packages
library(ggplot2)
library(tseries)

###############
##Question 2##
##############

#Questions below correspond to Air Products & Chemicals, Inc. stock (symbol = `ADP`):
  
#1.) Download the data.

#SNPdatahist <- get.hist.quote('^gspc',quote="Close")
SNPdata <- get.hist.quote('adp',quote="Close")
plot(SNPdata,col="red",main="Stock: Air Product & Chemicals, Inc.",xlab="Index",ylab="Closing Price")

#2.) Calculate log returns.

SNPret <- log(lag(SNPdata)) - log(SNPdata)
SNPret <- SNPret[!(is.na(SNPret)),]
  
#3.) Calculate volatility measure.
SNPvol <- sd(SNPret) * sqrt(250) * 100
SNPvol

#4.) Calculate volatility over entire length of series for various three different decay factors.

## volatility
get
Vol <- function(d, logrets)
{
  var = 0
  lam = 0
  varlist <- c()
  for (r in logrets) {
    lam = lam*(1 - 1/d) + 1
    var = (1 - 1/lam)*var + (1/lam)*r^2
    varlist <- c(varlist, var)
  }
  sqrt(varlist)
}

volest <- Vol(10,SNPret)
volest2 <- Vol(30,SNPret)
volest3 <- Vol(100,SNPret)

#5.) Plot the results, overlaying the volatility curves on the data, just as was done in the S&P example.

plot(volest,type="l")
lines(volest2,type="l",col="red")
lines(volest3, type = "l", col="blue")

###############
##Question 3##
##############

#The built-in data set called Orange in R is about the growth of orange trees. 
#Explore the data:

#Read in Orange dataset from R into data.frame
df <- data.frame(Orange)
head(df)

print("Summary of Orange dataset")
summary(df)#get summary of Orange dataset

#get structure of each columns
print("Structure of data.frame")
str(df$age)
str(df$circumference)

#Convert tree to character
df$Tree <- as.character(df$Tree)
str(df$Tree)

#Take a look at the data
explore.data <- function(data,x,n){
  title <-  paste0(n," vs Tree")
  p <- ggplot(df) + geom_point(aes(y = age, x = Tree, colour = Tree, shape = Tree), 
                               size = 3) + scale_colour_hue(l = 80, c = 150)
  p + labs(title = title, x = "Tree type", y = "Age", colour = "Tree") +
    theme(plot.title=element_text(hjust=0.5))
}

#Call explore.data function to plot age vs tree
x <- df$age
name <- "Age"
explore.data(df,x,name)


#Call explore.data function to plot circum vs tree
x <- df$circumference
name <- "Circumference"
explore.data(df,x,name)

#a) Calculate the mean and the median of the trunk circumferences for different size 
#of the trees. (Tree)

#aggregate data.frame by Tree and compute mean circumference
circum.mean <- aggregate(df$circumference,by=list(df$Tree),FUN=mean)
colnames(circum.mean) <- c("Tree","Avg_Circumference")
circum.mean

#aggregate data.frame by Tree and compute median circumference
circum.median <- aggregate(df$circumference,by=list(df$Tree),FUN=median)
colnames(circum.median) <- c("Tree","Med_Circumference")
circum.median

#b) Make a scatter plot of the trunk circumferences against the age of the tree. 
#Use different plotting symbols for different size of trees.

p <- ggplot(df) + geom_point(aes(y=circumference,x=age,colour=Tree, shape = Tree),size=3.0) + scale_colour_hue(l=80, c=150)
p + labs(title="Age vs Circumference by Tree",x="Age",y="Circumference",colour="Tree") +
  theme(plot.title = element_text(hjust = .5)) 

p <- ggplot(df,aes(y=circumference,x=age,colour=Tree)) + geom_point(aes(shape = Tree),size=3.0) +
  geom_line(size=1,alpha=0.8) + scale_colour_hue(l=80, c=150)
p + labs(title="Age vs Circumference by Tree",x="Age",y="Circumference",
         colour="Tree") +theme(plot.title = element_text(hjust = .5)) 

#c) Display the trunk circumferences on a comparative boxplot against tree.
#Order the boxplots in the increasing order of maximum diameter.

circum.max <- aggregate(df$circumference,by=list(df$Tree),FUN=max)#aggregate for max circum
colnames(circum.max) <- c("Tree","Max Circum.") #rename columns
circum.max #knit table

df$Tree <- factor(df$Tree,c("3","1","5","2","4")) #reorder the boxplot for max circum. by tree
p<-ggplot(df,aes(x=Tree,y=circumference))+ geom_boxplot(aes(fill=Tree))# ggplot: boxplot 
p + labs(title="Box Plot: Trunk Circumference", y="Circumference",x="Tree") + 
  theme(plot.title=element_text(hjust=.5))


###############
##Question 4##
##############

#i.) First, download “Temp” data set. Find the difference between the maximum and the 
#minimum monthly average temperatures for each country and report/visualize top 20 
#countries with the maximum differences for the period since 1900.

#Create new data.frame to join the two aggregated list'
tempraw <- read.csv("Data/Temp.csv",header=TRUE)
temp <- tempraw
head(temp)

#Preprocessing the Data:

#Need to make Date column into a character in order to use grepl to extract out other date format
temp$Date <- as.character(temp$Date)

#Deletes all the dates below 1900 because all of those dates are in a different format with "-" and not "/"
temp <- temp[!grepl("-",temp$Date),]

# Remove any columns with "NA" just to be careful
temp1 <- temp[!(is.na(temp$Date)),]

#Make Country column a character
temp1$Country <- as.character(temp1$Country)

#return all the rows (i.e. margin=1) with NA 
row.with.na <- apply(temp, 1, function(x) {any(is.na(x))})

#Sum all of the rows containing NA
sprintf("Number of Rows Deleted that contained NA's: %s ",sum(row.with.na))

#Remove the Rows with NA's
temp1 <- temp[!row.with.na,]

#Aggregate for max and min average temps
temp.max <- aggregate(temp1["Monthly.AverageTemp"],by=temp1["Country"],FUN=max)
temp.min <- aggregate(temp1["Monthly.AverageTemp"],by=temp1["Country"],FUN=min,na.rm=TRUE)

#Create new data.frame to join the two aggregated list
data <- data.frame(temp.max,temp.min)

#Drop extra Country column
data$Country.1 <- NULL

#Rename column
colnames(data)<-c("Country","Max Avg. Temp","Min Avg. Temp")

#Take difference between max and min avg. temp columns
data$Diff <- data$'Max Avg. Temp' - data$'Min Avg. Temp'

#Sort the dataframe by decreasing Diff
data <-data[order(data$Diff,data$Country,decreasing = TRUE),]
head(data,10)


#Subset the data to only take the first 20 columns with highest temp diff.
data.sub <- data[1:20,] 
#plot Country vs Temp Diff
p<- ggplot(data.sub,aes(Country,Diff,fill=Diff))+geom_bar(stat='identity') +
  scale_fill_gradientn(colours=c("dodgerblue1","darkblue","firebrick1"),values = scale(c(35,40,45))) 
p + labs(title="Country vs Change in Temperature", x="Country",
         y = "High-Low Avg. Monthly Temp Diff") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.x=element_text(angle=60,hjust=1)) + 
  coord_cartesian(ylim=c(30,50))


#(ii) Select a subset of data called 'UStemp' where US land temperatures from 01/01/1990
#in Temp data. Use UStemp dataset to answer the followings.

#subset to data for only country = "U.S."
temp.usa <- subset(temp1,temp1$Country == "United States")
head(temp.usa)

#Find the row in which date equals 1/1/90
which(temp.usa$Date == "1/1/90")
#delete all of the obs with date before 1990
temp.usa <- temp.usa[-c(1:1080),]
#change the Date column to date object
temp.usa$Date <- as.Date(temp.usa$Date, format="%m/%d/%y")

#a) Create a new column to display the monthly average land 
#temperatures in Fahrenheit (°F).**

#Convert Temp from celsius to fahrenheit:
temp.usa$Temp_F <- ((temp.usa$Monthly.AverageTemp * (9/5)) + 32)
head(temp.usa["Temp_F"])

#b) Calculate the average land temperature by year and plot it. The original file 
#has the average land temperature by month.
 
#Average Land Temperatue by Year:
temp.usa$year <- substr(temp.usa$Date,1,4)
df.temp.usa <- do.call(data.frame,aggregate(Temp_F ~ year,data=temp.usa,FUN=mean))
df.temp.usa$year <- as.numeric(as.character(df.temp.usa$year))
#str(df.temp.usa$year)

#plot USA yearly avgerage temp
p <- ggplot(df.temp.usa) + geom_line(aes(x=year,y=Temp_F),stat='identity',lwd=1.0,colour="blue") 
p + labs(title="USA Yearly Avg. Temperature" , x="Year", y="Temperature (F)") + 
  theme_bw() +  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_text(angle=60,hjust=1),legend.position = "none") +
  theme(plot.title=element_text(hjust=0.5)) 

#c) Calculate the one year difference of average land temperature by year and provide 
#the maximum difference (value) with corresponding two years.

temp.usa.year.diff <- temp.usa$Temp_F[1:23]-temp.usa$Temp_F[2:24]

temp.usa.year.diff.year<- c("1990-1991", "1991-1992", "1992-1993", "1993-1994", 
                            "1994-1995", "1995-1996", "1996-1997", "1997-1998", "1998-1999", "1999-2000", "2000-2001", 
                            "2001-2002", "2002-2003", "2003-2004", "2004-2005", "2005-2006", "2006-2007", "2007-2008", 
                            "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013")

temp.usa.ydiff <- data.frame(temp.usa.year.diff.year, temp.usa.year.diff)
colnames(temp.usa.ydiff) <- c("Years","AvgTempDiff")

temp.usa.ydiff$Years2 <- as.integer(temp.usa.ydiff$Years)

p <- ggplot(temp.usa.ydiff) + geom_point(aes(x=Years,y=AvgTempDiff), size = 3, colour="Red")
p + labs(title="Yearly Avg. Difference Temperature") +
  theme_bw() +  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_text(angle=60,hjust=1),legend.position = "none") +
  theme(plot.title=element_text(hjust=0.5)) + geom_line(aes(x=Years2,y=AvgTempDiff),colour="Blue",lwd=1)

#(iii) Download 'CityTemp' data set. Find the difference between the maximum and the minimum temperatures for 
#each major city and report/visualize top 20 cities with maximum differences for the period since 1900. 

citytempraw <- read.csv("./Data/CityTemp.csv",header=TRUE)
citytemp <- citytempraw
head(citytemp)

#Preprocessing the Data:

#Convert the Date column into a character in order to use grepl to extract out other date format
citytemp$Date <- as.character(citytemp$Date)
#Delete all dates below 1900 because all of those dates are in a different format with "-" and not "/"
citytemp <- citytemp[!grepl("-",citytemp$Date),]

row.with.na <- apply(citytemp,1, function(x){any(is.na(x))})
sprintf("Number of rows deleted with NA's: %s",sum(row.with.na))
citytemp1<-citytemp[!row.with.na,]

#Identify which columns are strings
cols = c(4,5,6,7);    
# convert these columns to characters using the apply function
citytemp1[,cols] = apply(citytemp1[,cols], 2, function(x) as.character(x))
#test if worked correctly
str(citytemp1$City)

#Aggregate for max and min average temps
citytemp.max <- aggregate(citytemp1["Monthly.AverageTemp"],by=citytemp1["City"],FUN=max)
citytemp.min <- aggregate(citytemp1["Monthly.AverageTemp"],by=citytemp1["City"],FUN=min,na.rm=TRUE)

#Create new data.frame to join the two aggregated list
citydata <- data.frame(citytemp.max,citytemp.min)

#Drop extra Country column
citydata$City.1 <- NULL

#Rename column
colnames(citydata)<-c("City","Max Avg. Temp","Min Avg. Temp")

#Take difference between max and min avg. temp columns
citydata$Diff <- citydata$'Max Avg. Temp' - citydata$'Min Avg. Temp'

#Sort the dataframe by decreasing Diff
citydata <-citydata[order(citydata$Diff,citydata$City,decreasing = TRUE),]
head(citydata, 10)

#Subset the data to only take the first 20 columns with highest temp diff.
citydata.sub <- citydata[1:20,]

p <- ggplot(citydata.sub) + geom_point(aes(x=City,y=Diff), size = 3, colour="Red")
p + labs(title="Difference per City") +
  theme(axis.text.x=element_text(angle=60,hjust=1),legend.position = "none") +
  theme(plot.title=element_text(hjust=0.5))

#(iv) Compare the two graphs in (i) and (iii)
p4 <- ggplot() + geom_point(data=data.sub, aes(x=data.sub$Country, y=data.sub$Diff), color='Blue3', size = 3) +
  geom_point(data=citydata.sub, aes(x=citydata.sub$City, y=citydata.sub$Diff), color='Red2', size = 3)
p4 + labs(title="Max Difference for Top 20 Countries/Cities", 
          x = "Countries (Blue) and Cities (Red)", y = "Max Difference") +
  theme(title=element_text(size=14), axis.title=element_text(size=14), 
          axis.text.x=element_text(angle=60,hjust=1),legend.position = "none") +
  theme(plot.title=element_text(hjust=0.5))

#Extra Exploratory Analysis
hilow <- function(df,x,name){
  p <- ggplot(data=df, aes(x,Diff,fill=Diff)) + geom_bar(stat='identity') + 
    coord_cartesian(ylim=c(30,50)) + scale_fill_gradient(low = "blue", high = "red")
  p + labs(title="High-Low Average Temp Difference", 
           x=name,y="Average Temperature Difference") +  
    theme(axis.text.x=element_text(angle=60,hjust=1)) + 
    theme(plot.title=element_text(hjust=0.5)) 
}

# Difference in Average Temp Per Country
country_var <- data.sub$Country
hilow(data.sub,country_var,name="Country")

# Difference in Average Temp Per Country
city_var <- citydata.sub$City
hilow(data.sub,city_var,name="City")
