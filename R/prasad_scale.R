#' Realtime BG Prasad Socioeconomic Status Scale
#'
#' This function measures the realtime socioeconomic status according to BG Prasad Scale.
#'
#' @param income A number, Family Income per month in Rupees
#'
#' @return BG Prasad Socioeconomic Class.
#' @author Biswarup Pramanik
#' @details
#' This function measures the realtime socioeconomic status according to BG Prasad Scale based on income parameter given to it.
#' NOTE: You will be asked to enter the latest Consumer Price Index-Industrial Worker (CPI-IW) value
#' available from the website "http://labourbureaunew.gov.in/LBO_indnum.htm"
#' @export

prasad_scale<- function(income)
{

#Consumer Price Index
cpi<- readline("Enter the latest Consumer Price Index-Industrial Worker (CPI-IW) value  :  ")
cpi<- as.numeric(unlist(strsplit(cpi, ",")))

#Load data
df<-data.frame(income)
scale<-read.csv(file=system.file("data", "Prasad_Scale.csv", package = "socioeconomicR"),header=TRUE)

#Warning Messages
if(any(df$income=="")) warning("Check Income")

#Income
df$income<-ifelse(df$income>=(0.228259*100*cpi),10000,df$income)
df$income<-ifelse(df$income<=((0.228259*100*cpi)-1) & df$income>=(0.228259*50*cpi),5000,df$income)
df$income<-ifelse(df$income<=((0.228259*50*cpi)-1) & df$income>=(0.228259*30*cpi),2500,df$income)
df$income<-ifelse(df$income<=((0.228259*30*cpi)-1) & df$income>=(0.228259*15*cpi),1500,df$income)
df$income<-ifelse(df$income<(0.228259*15*cpi),500,df$income)

#Prasad Class
df$prasad_class<-scale$prasad_class[match(df$income,scale$income)]

return(df$prasad_class);
}