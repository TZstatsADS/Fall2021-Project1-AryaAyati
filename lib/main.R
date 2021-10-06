# Arya Ayati Project 1
#hypothesis: The sentiment of a school should not change over time since a school
  #usually has a train of thought that doesn't usually evolve so much to change the
  #overall sentiment
library(dplyr)
library(tidyverse)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(Rcpp)
library(textclean)
library(ggalt)
library(ggplot2)
library(gridExtra)

setwd("..")
data.raw = read.csv('data/philosophy_data.csv')

colnames(data.raw)
unique(data.raw$author)
unique(data.raw$school)
summary(data.raw$original_publication_date)
unique(data.raw$title)

data.raw$sentence_replaced = replace_non_ascii(data.raw$sentence_lowered, replacement = "", remove.nonconverted = TRUE)

sentenceCorpus <- Corpus(VectorSource(data.raw$sentence_replaced))
sentenceCorpus<-tm_map(sentenceCorpus, removeWords, stopwords("english"))
sentenceCorpus<-tm_map(sentenceCorpus, removeWords, character(0))
sentenceCorpus<-tm_map(sentenceCorpus, removePunctuation)
sentenceCorpus<-tm_map(sentenceCorpus, stripWhitespace)

tdm <- TermDocumentMatrix(sentenceCorpus)
tdm
tdm = removeSparseTerms(tdm, 0.99)
tdm
tdm.tidy = tidytext::tidy(tdm)
tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))

png("figs/Base_Cleaned_WC.png", units="in", width=8, height=5, res=600)
wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
          scale=c(7,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))
dev.off()

#need to remove sparse terms first to use weighting
tdm.TFIDF <- TermDocumentMatrix(sentenceCorpus, 
                                control = list(weighting = weightTfIdf))
tdm.TFIDF
tdm.TFIDF = removeSparseTerms(tdm.TFIDF, 0.99)
tdm.TFIDF
tdm.tidyTFIDF = tidy(tdm.TFIDF)
tdm.overallTFIDF=summarise(group_by(tdm.tidyTFIDF, term), sum(count))

png("figs/TFIDF_overall_WC.png", units="in", width=8, height=5, res=600)
wordcloud(tdm.overallTFIDF$term, tdm.overallTFIDF$`sum(count)`,
          scale=c(7,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          use.r.layout=T,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))
dev.off()

data.activeYears = data.raw %>%
  select(c(1:3,6)) %>%
  group_by(title, school, original_publication_date) %>%
  summarise() %>%
  group_by(school, original_publication_date) %>%
  summarise(count = n()) %>%
  mutate(count = as.factor(count))

png("figs/PubTime_All.png", units="in", width=8, height=5, res=600)
ggplot(data.activeYears, aes(x = original_publication_date, y = school,
                      size = count,
                      color = count))+
  geom_point(alpha = 0.7)+
  labs(size = "Publications", color = "Publications")+
  ggtitle("Publications Per School Over Time")+
  xlab("Year")+
  ylab("School")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#filter out schools with only one or two publications
data.activeYearsgtt = data.activeYears %>%
  group_by(school) %>%
  filter(n() > 2)


png("figs/PubTime_GTT.png", units="in", width=8, height=5, res=600)
windows.options(width=10, height=8)
ggplot(data.activeYearsgtt, aes(x = original_publication_date, y = school,
                                size = count,
                                color = count))+
  geom_point(alpha = 0.7)+
  labs(size = "Publications", color = "Publications")+
  ggtitle("Publications Per School Over Time")+
  xlab("Year")+
  ylab("School")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#Sentiment Analysis per school here
data.gttSchools = unique(data.activeYearsgtt$school)
data.gttSenti = data.raw %>%
  filter(school %in% data.gttSchools)
# loop over schools using below function definition

#Start Function definition with sch as parameter
PerSchoolSentimentAnalysis <- function(sch, data.gttSenti) {
  schooldata.raw = data.gttSenti[data.gttSenti$school==sch,]
  
  years = unique(schooldata.raw$original_publication_date)
  schooldata.Senti = data.frame(matrix(ncol = 5, nrow = 0))
  colnames(schooldata.Senti) <- c("year", "negative", "positive", "sentiment", "netSenti")
  
  for (i in years){
    #per publication sentiment
    schooldata.temptxt = schooldata.raw %>%
      filter(original_publication_date == i)
    schooldata.yrTokens = data_frame(tokens = schooldata.temptxt$tokenized_txt) %>% 
      unnest_tokens(word, tokens)
  #reference https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
    schooldata.tmpSenti = schooldata.yrTokens %>% 
      inner_join(get_sentiments(bing)) %>% 
      count(sentiment) %>% 
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive / (positive + negative), netSenti = positive-negative)
    schooldata.yrSenti = cbind(data_frame(year = i), schooldata.tmpSenti)
    schooldata.Senti = rbind(schooldata.Senti, schooldata.yrSenti)
  }
  percLM = lm(sentiment~year, data = schooldata.Senti)
  netLM = lm(netSenti~year, data = schooldata.Senti)
  
  percPlot = ggplot(data = schooldata.Senti, aes(x=year, y=sentiment)) + 
    geom_point(color='blue') +
    geom_smooth(method = "lm", se = TRUE, formula = y~x)+
    labs(subtitle = paste("Adj R2 = ",signif(summary(percLM)$adj.r.squared, 5),
                       "Intercept =",signif(percLM$coef[[1]],5 ),
                       " Slope =",signif(percLM$coef[[2]], 5),
                       " P =",signif(summary(percLM)$coef[2,4], 5)))+
    ggtitle(paste("Sentiment by Year for", str_to_title(sch)))+
    xlab("Year")+
    ylab("Positive Sentiment Percent")+
    theme(plot.title = element_text(hjust = 0.5))
  netPlot = ggplot(data = schooldata.Senti, aes(x=year, y=netSenti)) + 
    geom_point(color='blue') +
    geom_smooth(method = "lm", se = TRUE, formula = y~x)+
    labs(subtitle = paste("Adj R2 = ",signif(summary(netLM)$adj.r.squared, 5),
                          "Intercept =",signif(netLM$coef[[1]],5 ),
                          " Slope =",signif(netLM$coef[[2]], 5),
                          " P =",signif(summary(netLM)$coef[2,4], 5)))+
    ggtitle(paste("Sentiment by Year for", str_to_title(sch)))+
    xlab("Year")+
    ylab("Net Positive Sentiment")+
    theme(plot.title = element_text(hjust = 0.5))
  
  png(paste("figs/", sch, "_plots.png", sep=""), units="in", width=12, height=5, res=600)
  grid.arrange(percPlot, netPlot, ncol=2)
  dev.off()
  
  #Insert plots here in markdown
  
  #save regression summaries for later
  schooldata.yrlm = cbind(data_frame(school = sch),
                          data_frame(list(percLM)),
                          data_frame(list(netLM)))
  colnames(schooldata.yrlm) <- c("school", "percentLM", "netLM")
  return(schooldata.yrlm)
}

data.schoolLM = data.frame(matrix(ncol = 3, nrow = 0))
colnames(data.schoolLM) <- c("school", "percentLM", "netLM")
# data.schoolLM = rbind(data.schoolLM,
#                       PerSchoolSentimentAnalysis('empiricism', data.gttSenti))

for (school in data.gttSchools) {
  data.schoolLM = rbind(data.schoolLM,
                        PerSchoolSentimentAnalysis(school, data.gttSenti))
}







