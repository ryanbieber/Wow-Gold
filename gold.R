#wowgoldmap
library(ggplot2)
library(scales)
library(zoo)
library(readstata13)
library(haven)
library(forecast)
library(prophet)

wowgold <-read.dta13("wowgold.dta", convert.dates = TRUE)

#looking at some summary of the file
summary(wowgold)

#getting date into R format from stata
wowgold$time<- as.POSIXct(wowgold$date/1000, origin = "1960-01-10")
wowgold$rtime <- as.Date(wowgold$time)

##setting gold to log10 as it increases exponentially
wowgold$gtousd <-log10(wowgold$usdtog)

##adding release dates for expansions
wowgold$releasedatecata <- as.Date("2010-12-10 12:00:00")
wowgold$releasedatemop <- as.Date( "2012-09-25 12:00:00")
wowgold$releasedatedra <- as.Date( "2014-11-13 12:00:00")
wowgold$releasedatelegi <- as.Date( "2016-08-30 12:00:00")
wowgold$releasedatebfa <- as.Date( "2018-08-13 12:00:00")
wowgold$releasedategold <- as.Date("2015-04-07 12:00:00")


xpacs <- ggplot(wowgold, aes(x=rtime, y=gtousd)) + 
  geom_line(aes(group=region, color = region),  linetype=1)+
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%Y-%m"))+
  theme( axis.text.x = element_text(angle = 90, hjust = 1, size =5))+
  labs(x = "Date", y="Gold to USD in Log", title="Gold to USD in World of Warcraft" ) +
  geom_vline(data = wowgold, aes(xintercept = as.numeric(releasedatecata)))+
  geom_vline(data = wowgold, aes(xintercept = as.numeric(releasedatemop)))+
  geom_vline(data = wowgold, aes(xintercept = as.numeric(releasedatedra)))+
  geom_vline(data = wowgold, aes(xintercept = as.numeric(releasedatelegi)))+
  geom_vline(data = wowgold, aes(xintercept = as.numeric(releasedatebfa)))+
  geom_vline(data = wowgold, aes(xintercept = as.numeric(releasedategold)))+
  geom_text(aes(x = releasedatecata, y = 3.5, label = "Cata"))+
  geom_text(aes(x = releasedatemop, y = 3.7, label = "Mop"))+
  geom_text(aes(x = releasedatedra, y = 3.6, label = "Draenor"))+
  geom_text(aes(x = releasedatelegi, y = 2.7, label = "Legion"))+
  geom_text(aes(x = releasedatebfa, y = 3.5, label = "BFA"))+
  geom_text(aes(x = releasedategold, y = 4.3, label = "Token"))
xpacs

#WOwToken
## looking at prices when they added a buying gold functionality
wowtokenstata <- read.dta13("wowtokenstata.dta")
wowtokenstata$gtousd <- wowtokenstata$buyprice/20

#adding patch variables
wowtokenstata$sixonetwo <- as.Date("2015-03-24 12:00:00")
wowtokenstata$sixtwo <- as.Date( "2015-06-22 12:00:00")
wowtokenstata$sixtwotwo <- as.Date( "2015-09-01 12:00:00")
wowtokenstata$sixtwothree <- as.Date( "2015-10-17 12:00:00")
wowtokenstata$sixtwofour <- as.Date( "2016-03-22 12:00:00")
wowtokenstata$sevenzerothree <- as.Date("2016-07-19 12:00:00")
wowtokenstata$sevenone <- as.Date("2016-10-25 12:00:00")
wowtokenstata$sevenonefive <- as.Date( "2017-01-10 12:00:00")
wowtokenstata$seventwo <- as.Date( "2017-03-28 12:00:00")
wowtokenstata$seventwofive <- as.Date( "2017-06-13 12:00:00")
wowtokenstata$seventhree <- as.Date( "2017-08-29 12:00:00")
wowtokenstata$seventhreetwo <- as.Date("2017-10-24 12:00:00")
wowtokenstata$seventhreefive <- as.Date("2018-01-16 12:00:00")
wowtokenstata$eightzeroone <- as.Date( "2018-07-17 12:00:00")
wowtokenstata$eightone <- as.Date( "2018-12-11 12:00:00")
wowtokenstata$eightonefive <- as.Date( "2019-03-12 12:00:00")
wowtokenstata$releasedatelegi <- as.Date( "2016-08-30 12:00:00")
wowtokenstata$releasedatebfa <- as.Date( "2018-08-13 12:00:00")


wowtokenplot<- ggplot(wowtokenstata, aes(x=date, y=gtousd)) + 
  geom_line(aes(group=region, color = region),  linetype=1)+
  labs(x = "Date", y="Gold to USD", title="Gold to USD in World of Warcraft" ) +
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(releasedatelegi)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(releasedatebfa)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(sixonetwo)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(sixtwo)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(sixtwotwo)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(sixtwothree)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(sixtwofour)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(sevenone)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(seventwofive)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(seventhree)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(seventhreetwo)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(seventhreefive)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(eightzeroone)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(eightone)))+
  geom_vline(data = wowtokenstata, aes(xintercept = as.numeric(eightonefive)))+
  geom_text(aes(x = releasedatelegi, y = 20000, label = "Legion"))+
  geom_text(aes(x = releasedatebfa, y = 2000, label = "BFA"))+
  geom_text(aes(x = sixonetwo, y = 16000, label = "6.1.2"))+
  geom_text(aes(x = sixtwo, y = 6000, label = "6.2"))+
  geom_text(aes(x = sixtwotwo, y = 12000, label = "6.2.2"))+
  geom_text(aes(x = sixtwothree, y = 5000, label = "6.2.3"))+
  geom_text(aes(x = sixtwofour, y = 13000, label = "6.2.4"))+
  geom_text(aes(x = sevenone, y = 200, label = "7.1"))+
  geom_text(aes(x = seventwofive, y = 23000, label = "7.2.5"))+
  geom_text(aes(x = seventhree, y = 5000, label = "7.3"))+
  geom_text(aes(x = seventhreetwo, y = 25000, label = "7.3.2"))+
  geom_text(aes(x = seventhreefive, y = 7000, label = "7.3.5"))+
  geom_text(aes(x = eightzeroone, y = 28000, label = "8.0.1"))+
  geom_text(aes(x = eightone, y = 2000, label = "8.1"))+
  geom_text(aes(x = eightonefive, y = 17000, label = "8.1.5"))
wowtokenplot



  