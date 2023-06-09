AllometryData <- read.csv('G1_Allometry.csv')
AllometryData
View(AllometryData)

nrow(AllometryData)# number of rows 
ncol(AllometryData)  #n.of cols
rownames(AllometryData)
colnames(AllometryData) # name to cols

str(AllometryData)
summary(AllometryData)  # to get summary statistical of data mean,mode....

head(AllometryData) # first 6 rows 
tail(AllometryData)  # last six rows 

#Select specific data.
Per.type <- Allomentry[ 2, 3 ]
Per.type
#specific row and col
Setinfo <- Allomentry[ c(1:10), c("diameter", "leafarea")]
Setinfo
# can get first 25 row
Setinfo <- head(Allomentry,25)
Setinfo


Allomentry <- data.frame(AllometryData) # convert data to data frame 
Allomentry
Result <- data.frame(Allomentry[Allomentry$species=='PIPO',])# get specific part in data 
Result
Result2 <- data.frame(Allomentry[Allomentry$height,])
Result2
test <- as.numeric(Allomentry$height) #to make height  numeric only 
test

complete.cases(Allomentry)  # to get NA places by get False in it 

Allomentry[!complete.cases(Allomentry),]  # to get Rows in  missing data 

#filter Data
Allomentry 
#to get each species data 
filter1<-  Allomentry[Allomentry$species =='PIPO',]
filter2<-  Allomentry[Allomentry$species =='PSME',]             
filter3<-  Allomentry[Allomentry$species =='PIMO',]            

filter1
filter2
filter3

#extract data from data frame
filter4 <- Allomentry[Allomentry$species=='PIPO', c("diameter","leafarea")]
filter4
# to convert data into numeric and get rid of comma in numbers 

Allomentry$height
Allomentry$height <- gsub(",",".",Allomentry$height)
Allomentry$height
Allomentry$height <- as.numeric(Allomentry$height)

#logical operator
is.na(Allomentry)    # get true result for Na value otherwise get false 
any(is.na.data.frame(Allomentry))  #get result True 

# Function to data
Allomentry$height
NewAllomentry <- Allomentry[,]
NewAllomentry

Z<- mean(Allomentry$height)    #to get the mean of data height
Z

calheight <- function(height)
{
  return(height>82)
}
# new names of col
colnames(Allomentry) <- (c(Species="Types",diameter="Lenght of diam"))
colnames(Allomentry)   

#DEAL with missing data
missing_values <- sum(is.na(Allomentry))
missing_values
  

# remove Na value
Allomentry<- na.omit(Allomentry)
Allomentry

# get data without duplicate
Allomentry <- unique(Allomentry)
Allomentry
   
# Statistical functions to data 
mean$height <- mean(Allomentry$height)
mean$Height 

meadian$height <- meadian(Allomentry$height)
mode$height <- mode(Allomentry$height)

#Use to predict missing data (ITT)

install.packages("mice")
library(mice)
data("nhanses2")
mydata<- nhenses2
pre.imputation <- mice(mydata,
                       m=5,                               #create five columns and give values to missing 
                       mthd=c("","pmm","logreg","polyreg"),
                       maxit=20)                           #max of iteration to dataset to get more infos and increase accurency of algorthim 

print(pre.imputation$imp)
print(pre.imputation)
mynewdata <- complete(pre.imputation,5)              #to apply predict value to missing 
length(mydata)

# Use tidyr to complete missing 

install.pavkage("tidyr")
library(tidyr)
data("Allomentry")
clean<- drop_na(Allomentry)  
x<- mean(clean$branchmass)
clean$NEW <- as.factor(ifelse(clean$branchmaa>x,"BIG BRANCH","SMALLBRANCH"))
clean$NEW

# To draw scatter plot
library(ggplot2)
data("Allomentry")
draw1 <- ggplot(Allomentry)
draw1 <- ggplot(Allomentry, aes(x=species, y=height))
draw1
draw1 + geom_point()


# vsulize data with histogram
Allomentry <- read.csv("Allomentry.csv")
draw_hist <- ggplot(Allomentry ,aes(height))
draw_hist
draw_hist + geom_histogram()
draw_hist + geom_histogram(binwidth = 5)
draw_hist + geom_histogram(color="red")
draw_hist + geom_histogram(fill="blue")
draw_hist + geom_histogram(alpha = 0.5)
draw_hist + geom_histogram() + ggtitle("Height")
draw_hist + geom_histogram() + labs(x="count", y="species")


#Bar Chart to how data
mydata <- read.csv("G1_Allomentry.csv")
draw_sc <- ggplot(mydata, aes(species,leafarea))
draw_sc + geom_point(aes(color=Region)) + labs(x="Type", y="Area of Leafs")
draw_sc + geom_point(aes(color=Region)) + stat_smooth(se=FALSE)









