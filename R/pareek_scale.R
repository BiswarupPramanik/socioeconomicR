#' Udai Pareek Socioeconomic Status Scale
#'
#' This function measures the socioeconomic status according to Udai Pareek Scale.
#'
#' @param caste A character, Caste of the family, Possible values are - "schedule caste","lower caste","artisan caste","agriculture caste","prestige caste","dominant caste"
#' @param occupation A character, Occupation of head of family, Possible values are - "none","laborer","caste occupation","business","independent profession","cultivation","service"
#' @param education A character,Education of head of family, Possible values are - "illiterate","can read only","can read and write","primary","middle","high school","graduate","above graduate"
#' @param land A number, Land owned in acres
#' @param social.participation A character, Social participation of family, Possible values are - "none","member of one organization","member of more than one organization",
#' "office holder in such a organization","wide public leader"
#' @param house A character, House type, Possible values are - "no house","hut","kutcha house","mixed house","pucca house","mansion"
#' @param farm.power A number, Number of draught animals
#' @param material.possessions A character, Material possesions, Possible values are - "bullock cart","cycle","radio","chair","mobile phone","television","refrigerator"
#' @param family.type A character, Family type, Possible values are "single","joint","extended","size up to 5","any other distinctive features"
#'
#' @return Udai Pareek Socioeconomic Status Class.
#' @author Biswarup Pramanik
#' @details
#' This function measures the socioeconomic status according to Udai Pareek Scale based on three parameters given to it (occupation,education,income).
#' @export

pareek_scale<- function(caste,occupation,education,land,social.participation,house,farm.power,material.possessions,family.type)
{

#Load data
df<-data.frame(caste,occupation,education,land,social.participation,house,farm.power,material.possessions,family.type)
scale<-read.csv(file=system.file("data", "Pareek_Scale.csv", package = "socioeconomicR"),header=TRUE)

#Caste
df$caste_score<-scale$caste_score[match(df$caste,scale$caste)]

#Occupation
df$occupation_score<-scale$occupation_score[match(df$occupation,scale$occupation)]

#Education
df$education_score<-scale$education_score[match(df$education,scale$education)]

#Land
df$land<-ifelse(df$land<=0,0,df$land)
df$land<-ifelse(df$land<1,0.5,df$land)
df$land<-ifelse(df$land<5 & df$land>=1,3,df$land)
df$land<-ifelse(df$land<10 & df$land>=5,7,df$land)
df$land<-ifelse(df$land<15 & df$land>=10,12,df$land)
df$land<-ifelse(df$land<20 & df$land>=15,17,df$land)
df$land<-ifelse(df$land>=20,22,df$land)

df$land_score<-scale$land_score[match(df$land,scale$land)]

#Social Participation
df$social.participation_score<-scale$social.participation_score[match(df$social.participation,scale$social.participation)]

#House
df$house_score<-scale$house_score[match(df$house,scale$house)]

#Farm Power
df$farm.power<-ifelse(df$farm.power<=0,0,df$farm.power)
df$farm.power<-ifelse(df$farm.power>=1 & df$farm.power<=2,1,df$farm.power)
df$farm.power<-ifelse(df$farm.power>=3 & df$farm.power<=4,3,df$farm.power)
df$farm.power<-ifelse(df$farm.power>=5 & df$farm.power<=6,5,df$farm.power)

df$farm.power_score<-scale$farm.power_score[match(df$farm.power,scale$farm.power)]

#Material Possessions
df$material.possessions_score<-scale$material.possessions_score[match(df$material.possessions,scale$material.possessions)]

#Family Type
df$family.type_score<-scale$family.type_score[match(df$family.type,scale$family.type)]


#Pareek Score
df$pareek_score<-(df$caste_score+df$occupation_score+df$education_score+df$land_score+df$social.participation_score+df$house_score+df$farm.power_score+df$material.possessions_score+df$family.type_score)

df$pareek_score<-ifelse(df$pareek_score>=43,50,df$pareek_score)
df$pareek_score<-ifelse(df$pareek_score<=42 & df$pareek_score>=33,40,df$pareek_score)
df$pareek_score<-ifelse(df$pareek_score<=32 & df$pareek_score>=24,30,df$pareek_score)
df$pareek_score<-ifelse(df$pareek_score<=23 & df$pareek_score>=13,20,df$pareek_score)
df$pareek_score<-ifelse(df$pareek_score<13,10,df$pareek_score)


#Pareek Class
df$pareek_class<-scale$pareek_class[match(df$pareek_score,scale$pareek_score)]

}