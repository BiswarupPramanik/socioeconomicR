#' Realtime Kuppuswamy Socioeconomic Status Scale
#'
#' This function measures the realtime socioeconomic status according to Kuppuswamy Scale.
#'
#' @param occupation A character,Occupation of Head of the Family, Possible values are - "professional","doctor","advocate","engineer","architect","director","manager",
#' "senior administrator","reader","professor","newspaper editor","college principal","architect","bank manager","semi professional","high school teacher",
#' "college lecturer","junior administrator","junior medical practitioner","arithmetic skill job","clerk","accountant","typist","elementary school teacher",
#' "farm owner","shopkeeper","salesman","insurance agent","news journalist","skilled worker","driver","telephone operator","mason","carpenter","mechanic",
#' "semi skilled worker","factory labourer","car cleaner","petty shopkeeper","unskilled worker","domestic servant","peon","watchman","unemployed"
#' @param education A character,Education of Head of the Family, Possible values are - "profession","honors","post-graduation","MA","MSc","PhD","MEd","MBBS","BE","BArch",
#' "graduate","BA","BSc","BEd","intermediate","diploma","XII pass","12 pass","10+2 pass","+2 pass","high school cerificate","X pass","10 pass","XI pass","11 pass","10+1 pass","+1 pass",
#' "middle school certificate","IX pass ","9 pass","VIII pass","8 pass","primary school certificate","VII pass","7 pass","VI pass","6 pass","V pass","5 pass","IV pass","4 pass",
#' "III pass","3 pass","II pass","2 pass","I pass","1 pass","illiterate"
#' @param income A number, Family Income per month in Rupees
#'
#' @param 
#' @return Kuppuswamy Socioeconomic Status Class.
#' @author Biswarup Pramanik
#' @details
#' This function measures the realtime socioeconomic status according to Kuppuswamy Scale based on three parameters given to it (occupation,education,income).
#' NOTE: You will be asked to enter the latest Consumer Price Index-Industrial Worker (CPI-IW) value available from the website "http://labourbureaunew.gov.in/LBO_indnum.htm"
#' @export

kuppuswamy_scale<- function(occupation,education,income)
{

#Consumer Price Index
cpi<- readline("Enter the latest Consumer Price Index-Industrial Worker (CPI-IW) value  :  ")
cpi<- as.numeric(unlist(strsplit(cpi, ",")))

#Load data
df<-data.frame(occupation,education,income)
scale<-read.csv(file=system.file("data", "Kuppuswamy_Scale.csv", package = "socioeconomicR"),header=TRUE)

#Warning Messages
if(any(df$occupation=="")) warning("Check Occupation")
if(any(df$education=="")) warning("Check Education")
if(any(df$income=="")) warning("Check Income")

#Occupation
df$occupation<-as.character(df$occupation)
df$occupation_score<-scale$occupation_score[match(df$occupation,scale$occupation)]

#Education
df$education<-as.character(df$education)
df$education_score<-scale$education_score[match(df$education,scale$education)]

#Income
df$income<-ifelse(df$income>=(0.077114527027027*2000*cpi),100000,df$income)
df$income<-ifelse(df$income<=((0.077114527027027*2000*cpi)-1) & df$income>=(0.077114527027027*1000*cpi),40000,df$income)
df$income<-ifelse(df$income<=((0.077114527027027*1000*cpi)-1) & df$income>=(0.077114527027027*750*cpi),20000,df$income)
df$income<-ifelse(df$income<=((0.077114527027027*750*cpi)-1) & df$income>=(0.077114527027027*500*cpi),15000,df$income)
df$income<-ifelse(df$income<=((0.077114527027027*500*cpi)-1) & df$income>=(0.077114527027027*300*cpi),8000,df$income)
df$income<-ifelse(df$income<=((0.077114527027027*300*cpi)-1) & df$income>=((0.077114527027027*100*cpi)+1),5000,df$income)
df$income<-ifelse(df$income<=(0.077114527027027*100*cpi),1000,df$income)

df$income_score<-scale$income_score[match(df$income,scale$income)]


#Kuppuswamy Score
df$kuppuswamy_score<-(df$occupation_score+df$education_score+df$income_score)

df$kuppuswamy_score<-ifelse(df$kuppuswamy_score<=29 & df$kuppuswamy_score>=26,28,df$kuppuswamy_score)
df$kuppuswamy_score<-ifelse(df$kuppuswamy_score<=25 & df$kuppuswamy_score>=16,20,df$kuppuswamy_score)
df$kuppuswamy_score<-ifelse(df$kuppuswamy_score<=15 & df$kuppuswamy_score>=11,13,df$kuppuswamy_score)
df$kuppuswamy_score<-ifelse(df$kuppuswamy_score<=10 & df$kuppuswamy_score>=5,7,df$kuppuswamy_score)
df$kuppuswamy_score<-ifelse(df$kuppuswamy_score<5,4,df$kuppuswamy_score)


#Kuppuswamy Class
df$kuppuswamy_class<-scale$kuppuswamy_class[match(df$kuppuswamy_score,scale$kuppuswamy_score)]

return(df$kuppuswamy_class);
}