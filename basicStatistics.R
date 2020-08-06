#-------------------------------------------------------------------------------
# Coursera- Guided project to calculate descriptive statistical metrics on a dataset
# and create a data quality report file.
#-------------------------------------------------------------------------------

# Task 1- Load and view dataset in R studio.
data<-read.csv("data.csv",stringsAsFactor=FALSE)
View(data)
#-------------------------------------------------------------------------------
# Task 2- Calculate measure of frequency metrics.
length(data$Transmission.Type)
length(unique(data$Transmission.Type))
freq<-table(data$Transmission.Type)
freq<-sort(freq,decreasing = TRUE)
freq

#-------------------------------------------------------------------------------
# Task 3- Calculate measure of Central Tendency Metrics.
mean(data$Engine.HP)      #There are missing values.
mean(data$Engine.HP,na.rm = TRUE)     #Missing values are removed.
median(data$Engine.HP,na.rm = TRUE)
#In R there is no inbuilt function to calculate mode.
uniqueval<-unique(data$Engine.HP)
uniqueval
uniqueval[which.max(tabulate(match(data$Engine.HP,uniqueval)))]

#-------------------------------------------------------------------------------
# Task 4- Calculate measure of Dispersion Metrics.
min(data$Engine.HP)
min(data$Engine.HP,na.rm = TRUE)
max(data$Engine.HP,na.rm = TRUE)
range(data$Engine.HP,na.rm = TRUE)
var(data$Engine.HP,na.rm = TRUE)
sd(data$Engine.HP,na.rm = TRUE)

#-------------------------------------------------------------------------------
# Task 5- Calculate additional data quality metrics.
class(data$Engine.HP)
sum(is.na(data$Transmission.Type))
sum(is.na(data$Number.of.Doors))

#-------------------------------------------------------------------------------
# Task 6- Calculate Descriptive Statistics on all column.
apply(data,MARGIN = 2,length)
sapply(data, function(x) min(x,na.rm=TRUE))
quality_data<-function(df=NULL){
  if(is.null(df)) print("Please pass a non-empty data frame")
  summary_table<-do.call(data.frame,list(
    Min=sapply(df,function(x) min(x,na.rm=TRUE)),
    Max=sapply(df,function(x) max(x,na.rm=TRUE)),
    Mean=sapply(df,function(x) mean(x,na.rm=TRUE)),
    SD=sapply(df,function(x) sd(x,na.rm=TRUE)),
    Total=apply(df,2,length),
    NULLS=sapply(df,function(x) sum(is.na(x))),
    unique=sapply(df,function(x) length(unique(x))),
    DataType=sapply(df,class)              
  ))
  nums<-vapply(summary_table,is.numeric,FUN.VALUE = logical(1))
  summary_table[,nums]<-round(summary_table[,nums],digits = 3)
  return(summary_table)
  
}
#-------------------------------------------------------------------------------
# Task 7- Generate a Data Quality Report File
df_quality<-quality_data(data)
df_quality<-cbind(Columns=rownames(df_quality),
                  data.frame(df_quality,row.names = NULL))
write.csv(df_quality,"Data Quality Report Final.csv",row.names = FALSE)

#-------------------------------------------------------------------------------
# END OF PROJECT
#-------------------------------------------------------------------------------