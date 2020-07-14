library(knitr)
#library(svglite)
#knitr::opts_chunk$set(dev = "svglite", fig.ext = ".svg")
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE)
options(width=100)
library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(RMariaDB)
library(data.table)
library(magrittr)
library(tidytext)
library(tidyverse)
library(textdata)
library(glue)
library(stringr)
library(RColorBrewer)
library(plyr)
require(scales)



db <- dbConnect(RMariaDB::MariaDB(), user='###', password="####", dbname='###', host='####',interactive_timeout=864000)
dbListTables(db)

query <- "SELECT * FROM recent_isolamento WHERE sentiment IS NOT NULL AND temp_excluded = 0
	AND created_at BETWEEN '2020-03-25 00:00:00' AND '2020-04-19 23:59:00'"
rsQuery <- dbSendQuery(db, query)
tweets<-dbFetch(rsQuery)

#tweets$full_text <- as.array(tweets$full_text)

## Cloudy With a Chance of Lots of Words

nohandles <- str_replace_all(tweets$full_text, "@\\w+", "")
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("portuguese"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("isolamento","quarentena","social","pra","tá","sobr","vai","covid","covid19",'#coronavirus','coronavírus', '#covid19', '#covid_19', 'pandemia', 'pra', 'coronavirus', 'covid19','q','vai','tá','aqui'))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)



# did not do this stemming
wordCorpus <- tm_map(wordCorpus, stemDocument)
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
wordcloud(words = wordCorpus, scale=c(8,0.1), max.words=500, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

tdm <- TermDocumentMatrix(wordCorpus)
tdm
inspect(tdm[12880:12890, 270:280])

## All the Feels
tweets$full_text <- as.character(tweets$full_text)
mySentiment <- get_nrc_sentiment(tweets$full_text, language = "portuguese")
head(mySentiment)
tweets <- cbind(tweets, mySentiment)


sentimentTotals <- data.frame(colSums(tweets[,c(11:20)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
#sentimento <- c("raiva","antecipação","nojo","medo","alegria","tristeza","surpresa", "confiança","negativo")
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        theme(legend.position = "top", legend.spacing.x = unit(0.5, 'cm'),
              axis.text.x = element_text(angle=90, hjust=1)) +
        xlab("Sentiments") + ylab("Total") + ggtitle("Sum of all tweets scores")


#### time
tweets$timestamp <- with_tz(ymd_hms(tweets$created_at), "America/Sao_Paulo")
#newdat <- dat[!is.na(dat$Factor), ]

posnegtime <- tweets %>% 
        group_by(timestamp = cut(timestamp, breaks="24 hours")) %>%
        summarise(negative = mean(negative),
                  positive = mean(positive)) %>% melt
names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])

ggplot(data = posnegtime, aes(x = as.Date(timestamp), y = meanvalue, group = sentiment)) +
        geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
        geom_point(size = 0.5) +
        ylim(0, NA) + 
        scale_colour_manual(values = c("springgreen4", "firebrick3")) +
        theme(legend.title=element_blank(), axis.title.x = element_blank()) +
        scale_x_date(breaks = date_breaks("1 week"), 
                     labels = date_format("%d-%m")) +
        ylab("Average sentiment score") + 
        ggtitle("Sentiment Over Time")

# linha do tempo da semana
tweets$weekday <- wday(tweets$timestamp, label = TRUE)
tweets$weekday <- forcats::fct_explicit_na(tweets$weekday)

weeklysentiment <- tweets %>% group_by(weekday) %>% 
        summarise(anger = mean(anger), 
                  anticipation = mean(anticipation), 
                  disgust = mean(disgust), 
                  fear = mean(fear), 
                  joy = mean(joy), 
                  sadness = mean(sadness), 
                  surprise = mean(surprise), 
                  trust = mean(trust)) %>% melt
names(weeklysentiment) <- c("weekday", "sentiment", "meanvalue")

ggplot(data = weeklysentiment, aes(x = weekday, y = meanvalue, group = sentiment)) +
        geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
        geom_point(size = 0.5) +
        ylim(0, 0.6) +
        theme(legend.title=element_blank(), axis.title.x = element_blank()) +
        ylab("Escore médio de sentimento") + 
        ggtitle("Sentimento durante a semana")


#### dia do mês
#tweets$monthday <- format(as.Date(tweets$created_at), "%d-%m")

tweets$monthday <- as.Date(tweets$created_at)
dates <- unique(sort(tweets$monthday))
tweets$monthday <- factor(tweets$monthday, labels = dates,  ordered = T)


monthdaySentiment <- tweets %>% group_by(monthday) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% reshape2::melt()
names(monthdaySentiment) <- c("monthDay", "sentiment", "meanvalue")
monthdaySentiment$monthDay <- as.Date(monthdaySentiment$monthDay)


ggplot(data = monthdaySentiment, aes(x = monthDay, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment During the Year") +
  scale_x_date(labels = date_format("%b-%d"), breaks = date_breaks("1 day"))

# linha do tempo do mes
tweets$month <- month(tweets$timestamp, label = TRUE)
monthlysentiment <- tweets %>% group_by(month) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% melt
names(monthlysentiment) <- c("month", "sentiment", "meanvalue")

ggplot(data = monthlysentiment, aes(x = month, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Average sentiment score") + 
  ggtitle("Sentiment During the Year")



####################


##### Data Visualizaiton by Weekdays

# Split the column date.time into two columns: date amd time

#separate value time from the dataframe and store it
pickup_time <- format(as.POSIXct(with_tz(ymd_hms(tweets$created_at), "America/Sao_Paulo")), format="%H:%M:%S")

#separate the value data from dataframe and store it
pickup_date <- format(as.POSIXct(with_tz(ymd_hms(tweets$created_at), "America/Sao_Paulo")), format="%m/%d/%Y")

##create another column Time to the end of the column fill column with the data separeted from Data.Time
tweets$Time <- pickup_time

#create another column Date to the end of the column fill column with the data separeted from Data.Time
tweets$Date <- pickup_date


###########Add another column in which dates are converted into weekdays###########


#create another column weekdays to the end of the column fill column with the data converted from the column Date
tweets$Weekday <- weekdays(as.Date(tweets$created_at,format="%m/%d/%Y"))

#############Produce Simple Statistics

#count the frequency of the value in column Weekday
pickup_frequency<- as.data.frame(table(tweets$Weekday))

#rename the header so that it's more conveninient to do step 4 visualization
names(pickup_frequency)<- c("Weekday","Pickups")

#reorder the table according to Weekdays, so that when it comes to data visualization the weekdays will be in the right order
pickup_frequency$Weekday<- factor(pickup_frequency$Weekday, levels=c("domingo","segunda-feira","terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado"))
pickup_frequency<-pickup_frequency[order(pickup_frequency$Weekday),]

#show the weekday with most pickups along with the frequency
pickup_frequency[which.max(pickup_frequency$Pickups),]

#show the weekday with least pickups along with the frequency
pickup_frequency[which.min(pickup_frequency$Pickups),]

#Visualization by Weekdays 1
ggplot(data=pickup_frequency, aes(x = Weekday, y=Pickups, fill=Weekday))+
  geom_bar(stat = "identity")+ggtitle(label = "Weekday Pickup Comparison")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Weekdays")+ylab("Number of Pickups")

#Visualization by Weekdays 2

#add column Month to the master data frame
tweets$Month<-months(as.POSIXct(tweets$Date, format="%m/%d/%Y"))
#subset the data so that the size is smaller, and it is easier to plot
weekly_trend <- subset(tweets, select = c(Weekday, Month))

#count the pickups by two cololumns: weekday and month
weekly_trend<- ddply(weekly_trend, .(weekly_trend$Weekday, weekly_trend$Month), nrow)

#change the column name of the new data frame
names(weekly_trend) <- c("Weekday", "Month","Pickups")

#reorder the data frame according to two columns: weekday and month
weekly_trend$Weekday <- factor(weekly_trend$Weekday,levels=c("domingo","segunda-feira","terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado")) 
weekly_trend$Month<- factor(weekly_trend$Month, levels = c("março", "abril", "maio", "junho","julho","agosto"))
weekly_trend<-weekly_trend[with(weekly_trend, order(Month, Weekday)),]

#plot the data
ggplot(weekly_trend,aes(Weekday, Pickups)) + 
  geom_bar(aes(fill = Month),stat = "identity",position = "dodge")+
  scale_fill_brewer(palette = "Accent") +
  ggtitle(label = "Weekday Pickup Each Month")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Weekdays")+ylab("Number of Pickups")

###############Data Visualization by Months 1

#Count the frequency of each month
mpickups<-as.data.frame(table(tweets$Month))
names(mpickups)<- c("Month","Pickups")

#reorder the table accordiing to mouth so that it is easier to plot in the next step
mpickups$Month<- factor(mpickups$Month, levels = c("março", "abril", "maio", "junho","julho","agosto"))
mpickups<-mpickups[order(mpickups$Month),]

#Plot
ggplot(mpickups, aes(x=Month, y=Pickups, group=1))+
  geom_point(color="dark green")+geom_line(color="orange")+
  ggtitle(label = "Trend over the Months by Month")+theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+
  ylab("Number of Pickups")

###############Data Visualization by Months 2
ggplot(weekly_trend,aes(Month, Pickups)) + 
  geom_bar(aes(fill = Weekday),stat = "identity",position = "dodge")+
  scale_fill_brewer(palette = "Set2") +
  ggtitle(label = "Monthly Trend by Weekday")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Month")+
  ylab("Number of Pickups")

###############Data Visualization by Months 3
#subset the data so that the size is smaller, and it is easier to plot
monthly_trend <- subset(tweets, select = c(Date, Month))

#count the pickups by two cololumns: weekday and month
monthly_trend<- ddply(monthly_trend, .(monthly_trend$Date, monthly_trend$Month), nrow)

#change the column name of the new data frame
names(monthly_trend) <- c("Date", "Month","Pickups")

#Change the data type of the date column into date so that it will be easier to add breaks in x axis later in plotting
monthly_trend$Date <- as.character.Date(monthly_trend$Date)
monthly_trend$Date <-as.Date(monthly_trend$Date, format = "%m/%d/%Y")

ggplot(monthly_trend, aes(Date, Pickups))+geom_line(aes(color=Month))+
  geom_smooth(method = 'loess',color="red")+
  scale_x_date(breaks = date_breaks("9 days"))+
  ggtitle(label = "Trend over the Months by Date")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),axis.text.x = element_text(angle=90))+
  ylab("Number of Pickups")

###############Visualization by Time of the Day 1
# Select the data needed for this task
daily_trend <- subset(tweets, select = c(Time, Month))

#Change the time format to simply showing the hour so that it will be easier for regrouping the file and plotting (if name of x axis is too long, it will not be clear and pretty)
H<-format(as.POSIXct(strptime(daily_trend$Time, "%H:%M:%S", tz="")), format="%H")
daily_trend$Time <- H

#convert the time column into class time
daily_trend$Time <- as.character.Date(daily_trend$Time, format="%H")

#count the pickups by two cololumns: time and month
daily_trend<- ddply(daily_trend, .(daily_trend$Time, daily_trend$Month), nrow)
names(daily_trend)<- c("Hour","Month","Pickups")

# plot the data - bar graph
ggplot(daily_trend, aes(Hour, Pickups, fill=Month))+
  geom_bar(stat = "identity")+
  ggtitle(label = "Trend Over Time of the Day")+theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+
  xlab("Hour")+
  ylab("Number of Pickups")

###############Visualization by Time of the Day 2

#line graph
ggplot(daily_trend, aes(Hour, Pickups, group=Month))+
  geom_line(aes(color=Month))+
  ggtitle(label = "Trend Over Time of the Day")+
  theme_minimal()+theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+
  xlab("Hour")+
  ylab("Number of Pickups")