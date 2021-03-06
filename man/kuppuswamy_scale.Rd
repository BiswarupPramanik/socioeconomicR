% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kuppuswamy_scale.R
\name{kuppuswamy_scale}
\alias{kuppuswamy_scale}
\title{Realtime Kuppuswamy Socioeconomic Status Scale}
\usage{
kuppuswamy_scale(occupation, education, income)
}
\arguments{
\item{occupation}{A character,Occupation of Head of the Family, Possible values are - "professional","doctor","advocate","engineer","architect","director","manager",
"senior administrator","reader","professor","newspaper editor","college principal","architect","bank manager","semi professional","high school teacher",
"college lecturer","junior administrator","junior medical practitioner","arithmetic skill job","clerk","accountant","typist","elementary school teacher",
"farm owner","shopkeeper","salesman","insurance agent","news journalist","skilled worker","driver","telephone operator","mason","carpenter","mechanic",
"semi skilled worker","factory labourer","car cleaner","petty shopkeeper","unskilled worker","domestic servant","peon","watchman","unemployed"}

\item{education}{A character,Education of Head of the Family, Possible values are - "profession","honors","post-graduation","MA","MSc","PhD","MEd","MBBS","BE","BArch",
"graduate","BA","BSc","BEd","intermediate","diploma","XII pass","12 pass","10+2 pass","+2 pass","high school cerificate","X pass","10 pass","XI pass","11 pass","10+1 pass","+1 pass",
"middle school certificate","IX pass ","9 pass","VIII pass","8 pass","primary school certificate","VII pass","7 pass","VI pass","6 pass","V pass","5 pass","IV pass","4 pass",
"III pass","3 pass","II pass","2 pass","I pass","1 pass","illiterate"}

\item{income}{A number, Family Income per month in Rupees}
}
\value{
Kuppuswamy Socioeconomic Status Class.
}
\description{
This function measures the realtime socioeconomic status according to Kuppuswamy Scale.
}
\details{
This function measures the realtime socioeconomic status according to Kuppuswamy Scale based on three parameters given to it (occupation,education,income).
NOTE: You will be asked to enter the latest Consumer Price Index-Industrial Worker (CPI-IW) value available from the website "http://labourbureaunew.gov.in/LBO_indnum.htm"
}
\author{
Biswarup Pramanik
}
