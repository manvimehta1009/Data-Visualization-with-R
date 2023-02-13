#Data Visualizations 
#(Working directory has been set to the folder where the csv files have been stored)

##Loading the data
toyota.df<-read.csv("ToyotaCorolla.csv",header=TRUE)
##View(toyota.df)

#Printing the list of variables and checking the dimension of the df
##names(toyota.df)
##dim(toyota.df)

#Part a- Data exploration and visualization 
library(ggplot2)
library(GGally)

toyotacorrelation <- subset(toyota.df, select = -c(Model, Fuel_Type, Color))
ggcorr(toyotacorrelation, hjust = 1)
cor(toyotacorrelation, use = "complete.obs")

#Part b (i)- Creating dummy variables for fuel type and colour

#Using model.matrix to convert Fuel_Type into a dummy variable 
fuel_dummy<-model.matrix(~0+Fuel_Type,data=toyota.df)
#converting it into a data frame
fuel_dummy<- as.data.frame(fuel_dummy)
#checking the names and position of the dummy variables in the matrix
t(t(names(fuel_dummy)))
#Dropping the last dummy variable column 
fuel_dummy<-fuel_dummy[,-3]
#View(fuel_dummy)

#Using model.matrix to convert color into a dummy variable
color_dummy<-model.matrix(~0+ Color,data=toyota.df)
#converting it into a data frame
color_dummy<- as.data.frame(color_dummy)
#checking the names and position of the dummy variables in the matrix
t(t(names(color_dummy)))
#Dropping the last dummy variable column 
color_dummy<-color_dummy[,-10]
#View(color_dummy)

#Factoring in the dummy variables to the toyota dataframe
##Removing the original fuel_type and color columns and adding the dummy variable columns
toyota.df<-toyota.df[,c(1:7,9,10,12:39)]
toyota.df<-cbind(toyota.df,fuel_dummy,color_dummy)
##View(toyota.df)
#dim(toyota.df) #for checking the total number of variables after adding the dummy variables

#Part b(ii) #Creating partitions

#using set.seed to get the same partitions when re-running the code
set.seed(1)

#partitioning into training-50%, validation-30% and test-20%
train.rows<-sample(rownames(toyota.df),dim(toyota.df)[1]*0.5)
valid.rows<-sample(setdiff(rownames(toyota.df), train.rows),dim(toyota.df)[1]*0.3)
test.rows<-setdiff(rownames(toyota.df), union(train.rows, valid.rows))

#Creating dataframes for all the three data types
train.data <- toyota.df[train.rows, ]
valid.data <- toyota.df[valid.rows, ]
test.data <- toyota.df[test.rows, ]


#Problem 3.2

#Loading data
ridingmowers.df<-read.csv("RidingMowers.csv")
##View(ridingmowers.df)

#Loading libraries
library(scales)
library(ggplot2)

#Plotting the graph with required specifications
ggplot(ridingmowers.df) + #to access the data frame for the plot
  geom_jitter(aes(x = Income , y = Lot_Size , colour = Ownership))+ #Specifications
  ggtitle("Income vs. Lot Size")+ #Main title
  theme(plot.title = element_text(hjust = 0.5,size=12))+ #Centre aligning 
  xlab("Income")+ylab("Lot Size") #Labels for x and y axis

 
#Problem 3.3
#Part a-Barchart
#Loading data
laptopsales.df<-read.csv("LaptopSalesJanuary2008.csv")
View(laptopsales.df)

#Plotting the barchart
store.avg <- aggregate(laptopsales.df$Retail.Price ~ laptopsales.df$Store.Postcode, data = laptopsales.df, mean)
store.avg
ggplot(store.avg)+
  geom_col(aes(x=store.avg$`laptopsales.df$Store.Postcode`,y=store.avg$`laptopsales.df$Retail.Price`))+
  theme(axis.text.x = element_text(angle = 90)) + 
  coord_cartesian(ylim=c(475, 500))+
  ggtitle(("Barchart of Store Postcode vs.Average Retail Price"))



#Part b-Side by side boxplots
## side-by-side boxplots
ggplot(laptopsales.df) +
  geom_boxplot(aes(x = Store.Postcode, y =Retail.Price ))+
  xlab("Store Postcode")+ylab("Retail Price")+
  theme(axis.text.x = element_text(hjust = 0.5,angle = 90))+
  ggtitle(("Boxplot of Store Postcode vs. Retail Price"))+
  theme(plot.title = element_text(hjust = 0.5,size=12))

  
#Case Study-Movie Theater Releases

#Loading data
moviesdf<-read.csv("Movies2016.csv")
View(moviesdf)



#Plotting required histograms
hist(moviesdf$Opening.Gross.Sales....millions.)
hist(moviesdf$Total.Gross.Sales....millions.)
moviesdf$Number.of.Theaters <- as.numeric((moviesdf$Number.of.Theaters) ,NA.rm=TRUE) # Convert one variable to numeric
class(moviesdf$Number.of.Theaters)
hist(moviesdf$Number.of.Theaters)
hist(moviesdf$Weeks.in.Release)

#Plotting scatter plots

#Opening Gross Sales vs. Total Gross Sales
ggplot(moviesdf) + #to access the dataframe for the plot
  geom_jitter(aes(x = Opening.Gross.Sales....millions., y=Total.Gross.Sales....millions.))+ #Specifications
  ggtitle("Opening Gross Sales vs. Total Gross Sales")+ #Main title
  theme(plot.title = element_text(hjust = 0.5,size=12))+ #Centre aligning 
  xlab("Opening Gross Sales")+ylab("Total Gross Sales")


#Number of Theaters vs. Total Gross Sales
ggplot(moviesdf) + #to access the dataframe for the plot
  geom_jitter(aes(x = Number.of.Theaters, y=Total.Gross.Sales....millions.))+ #Specifications
  ggtitle("Number of Theaters vs. Total Gross Sales")+ #Main title
  theme(plot.title = element_text(hjust = 0.5,size=12))+ #Centre aligning 
  xlab("Number of Theaters")+ylab("Total Gross Sales")


#Number of Weeks in Release vs. Total Gross Sales
ggplot(moviesdf) + #to access the dataframe for the plot
  geom_jitter(aes(x = Weeks.in.Release, y=Total.Gross.Sales....millions.))+ #Specifications
  ggtitle("Number of weeks in release vs. Total Gross Sales")+ #Main title
  theme(plot.title = element_text(hjust = 0.5,size=12))+ #Centre aligning 
  xlab("Number of Weeks in Release")+ylab("Total Gross Sales")


