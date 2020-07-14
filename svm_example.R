
#get nrc sentiment
library(textreg)
mysentiment_df<-data.frame(mySentiment)
head(mysentiment_df)
dim(mysentiment_df)

labels=list("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
labels

#get document matrix

dtm_covid_data <- DocumentTermMatrix(wordCorpus)
dtm_covid_data
covid_data_corpus <- removeSparseTerms(dtm_covid_data, 0.99)
covid_data_corpus
dtm_covid_matrix <- as.matrix(covid_data_corpus)

#make the dataset
dtm_covid_matrix <- cbind(dtm_covid_matrix, max.col(mysentiment_df[,1:10]))
colnames(dtm_covid_matrix)[ncol(dtm_covid_matrix)] <- "sentiment"
covid_data_set<-as.data.frame(dtm_covid_matrix)
dim(covid_data_set)
covid_data_set[1,]

#wordcloud
library("wordcloud")
covid_data_cloud_corpus <- VCorpus(VectorSource(tweets$full_text))
wordcloud(covid_data_cloud_corpus,min.freq = 2, scale=c(8,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)

#barchart
sentiment_score<-table(covid_data_set$sentiment)
sentiment_score
Sentimentscores_covid<-as.data.frame(sentiment_score)
Sentimentscores_covid
Sentimentscores_covid$Var1<-as.vector(unlist(labels))
Sentimentscores_covid
names(Sentimentscores_covid)<-c("Sentiment","Score")
library(ggplot2)
ggplot(data=Sentimentscores_covid,aes(x=Sentiment,y=Score))+geom_bar(aes(fill=Sentiment),stat="identity")+theme(legend.position="none")+xlab("Sentiments")+ylab("scores")+ggtitle("Sentimental analysis of people on Corona")

#training data testing data splitting
trainObs <- sample(nrow(covid_data_set), .8 * nrow(covid_data_set), replace = FALSE)
head(trainObs)
training_set <- covid_data_set[trainObs,]

testObs <- sample(nrow(covid_data_set), .2 * nrow(covid_data_set), replace = FALSE)
head(testObs)
testing_set <- covid_data_set[testObs,]

#svm radial kernal
library(e1071) 
svm_radial_covid_model<-svm(sentiment ~., data=training_set,method="C-classification", kernal="radial",gamma=0.1, cost=10)
svm_radial_covid_model

svm_radial_covid_result <- predict(svm_radial_covid_model, newdata = testing_set)

check_accuracy_radial <- as.data.frame(cbind(prediction = svm_radial_covid_result, sentiment = testing_set$sentiment))
head(check_accuracy_radial)
check_accuracy_radial$prediction<-round(check_accuracy_radial$prediction,digits = 0)
head(check_accuracy_radial)

library(caret)
test_radial<-check_accuracy_radial$sentiment
svm_radial_covid_confusion_matrix<-confusionMatrix(factor(check_accuracy_radial$prediction,min(test_radial):max(test_radial)),factor(check_accuracy_radial$sentiment,min(test_radial):max(test_radial)))
svm_radial_covid_confusion_matrix

svm_radial_covid_confusion_matrix$byClass

Precision_radial<-svm_radial_covid_confusion_matrix$byClass[,"Precision"]
Recall_radial<-svm_radial_covid_confusion_matrix$byClass[,"Recall"]
F1_radial<-svm_radial_covid_confusion_matrix$byClass[,"F1"]
Accuracy_radial<-svm_radial_covid_confusion_matrix$byClass[,"Balanced Accuracy"]

svm_radial_covid_confusion_matrix$overall
Model_Accuracy_radial<-svm_radial_covid_confusion_matrix$overall["Accuracy"]

#radial
confusion_mat_radial<-data.frame(svm_radial_covid_confusion_matrix$table)
confusion_mat_radial

ggplot(data =  confusion_mat_radial, mapping = aes(x = Reference, y = Prediction)) +     geom_tile(aes(fill = Freq), colour = "white") + geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) + scale_fill_gradient(low = "cyan", high = "red") +theme_bw() + theme(legend.position = "none")
