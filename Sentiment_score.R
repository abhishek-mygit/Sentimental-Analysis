install.packages("twitteR")
install.packages("ROAuth")
install.packages("magrittr")
install.packages("RInside")
install.packages("Rcpp")
library("twitteR")
library("ROAuth")

#RESTART R session!

#httr
install.packages(c("devtools", "rjson", "bit64", "httr"))

# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions
cred <- OAuthFactory$new(consumerKey='Enter_Consumer_or_API_Key_here',
                         consumerSecret='Enter_Consumer_or_API_Secret_here',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")


#save for later use for Windows
save(cred, file="twitter authentication.Rdata")


load("twitter authentication.Rdata")
#registerTwitterOAuth(cred)

api_key <- "Enter_Consumer_or_API_Key_here"
api_secret <- "Enter_Consumer_or_API_Secret_here"
access_token <- "Enter_Access_Token_here"
access_token_secret <- "Enter_Access_Token_Secret_here"

setup_twitter_oauth(consumer_key="Enter_Consumer_or_API_Key_here",consumer_secret ="Enter_Consumer_or_API_Secret_here",access_token="Enter_Access_Token_here",access_secret ="Enter_Access_Token_Secret_here" )


search.string <- "Jallikattu"
no.of.tweets <- 50

#tweets <- searchTwitter(search.string, n=no.of.tweets, cainfo="cacert.pem", lang="en")
tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
tweets


install.packages("ggplot2")
install.packages("plyr")
library(plyr)

processeddata = laply(tweets, function(t) t$getText())

# Read in dictionary of positive and negative works
goodwords = scan('./opinion-lexicon-English/positive-words.txt',what='character',comment.char=';')
badwords = scan('./opinion-lexicon-English/negative-words.txt',what='character',comment.char=';')


# Add a few most-used Twitter phrases specific to desired topic 
good_text = c(goodwords, 'Jallikattu', 'Support jallikattu', 'ban peta', '#Jallikattu', 'alanganallur', 'maadu','TN','Farmers','care','Save','A1','A2','milk')
bad_text = c(badwords, 'ban jallikattu', 'support peta', 'hurting','torturing','hurting cows','alcohol','liquor','pain','drugs','dope','tame','fight','bullfight')






 ##############  Hu and Liu Opinion Lexicon Sentiment Analysis  ###################

score.sentiment = function(sentences, good_text, bad_text, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply"
 
 scores = laply(sentences, function(sentence, good_text, bad_text) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '',sentence)
    sentence = gsub('[[:cntrl:]]', '',sentence)
    sentence = gsub('\\d+','',sentence)
    #to remove emojis
    sentence <- iconv(sentence,'UTF-8','ASCII')
    sentence = tolower(sentence)        
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, good_text, bad_text, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


# Call the function and return a data frame
sentanalysis <- score.sentiment(processeddata, good_text, bad_text, .progress='text')

library(ggplot2)

sentanalysis$positive <- as.numeric(sentanalysis$score >0)
sentanalysis$negative <- as.numeric(sentanalysis$score <0)
sentanalysis$neutral <- as.numeric(sentanalysis$score == 0)

sentanalysis$polarity <- ifelse(sentanalysis$score>0,"positive",ifelse(sentanalysis$score<0,"negative",ifelse(sentanalysis$score==0,"Neutral",0)))

qplot(factor(polarity),data=sentanalysis,geom="bar",fill=factor(polarity))+xlab("Sentiment Score"),ylab("No. of users"),ggtitle("Analysis of Users Sentiments")

qplot(factor(score),data=sentanalysis,geom="bar",fill=factor(score))+xlab("Sentiment Score"),ylab("No. of users"),ggtitle("Sentiment Scores & Analysis")







 #############  Emoticon Analysis  ###############

#### Score Tweets ####

sentiment.file <- "./AFINN-111.txt"


#Initialize sentiment dictionary

df.sentiments <- read.table(sentiment.file,header=F,sep="\t",quote="",col.names=c("term","score"))

df.sentiments$term <- gsub("[^[:alnum:]]", " ",df.sentiments$term)


ScoreTerm <- function(term){
  df.sentiments[match(term,df.sentiments[,"term"]),"score"]
  }

ScoreText <- function(text){
  text <- tolower(gsub("[^[:alnum:]]", " ",text))
  text <- do.call(c,strsplit(text," "))
  text <- text[text!=""]
  length(text)
  scores <- ScoreTerm(text)
  scores[is.na(scores)] <- 0
  sum(scores)
 }

tweet.scores <- sapply(tweets,ScoreText)


#### Extract the Emoticon(s) ####

emoticon.list <- c("\\:\\-\\)","\\:\\)","\\=\\)",
                    
                    "\\:\\-D","\\:D","8\\-D","8D","x\\-D","xD","X\\-D","XD",
                    
                    "\\:\\-\\(","\\:\\(",
                    
                    "\\:\\-\\|","\\:\\|",
                    
                    "\\:\\'\\-\\(","\\:\\'\\(\\)",
                    
                    "\\:\\'\\-\\)","\\:\\'\\)",
                    
                    "\\:\\-o","\\:\\-O","\\:o","\\:O",
                    
                    "o_O","o\\.O","O_o","O\\.o",
                    
                    "\\:\\*","\\;\\-\\)","\\;\\)",
                    
                    "\\%\\-\\)",
                    
                    "\\<3","\\<\\/3" )


# Take care that none is a substring of another. You can't have ":)" and ":))"; or ">:-)" and ":-)", etc.
# If you do want them, you'll need to change the matching code somewhat to make sure it only matches the longest one or something


FindMatches <- function(emoticon,tweets){
#This will return a vector of elements from <tweets> which contain <emoticon>
  grep(emoticon,tweets)
}

emoticon.score.distributions <- sapply(emoticon.list, function(e){
    containing.indices <- FindMatches(e,tweets)
    score.dist <- tweet.scores[containing.indices]
    as.numeric(score.dist)
  })


##### Post-processing The results #####

## Mean,sd
emoticon.sentiment.means <- sapply(emoticon.score.distributions,mean)
emoticon.sentiment.sds   <- sapply(emoticon.score.distributions,sd)

## Plot
clean.scores <- emoticon.score.distributions[sapply(emoticon.score.distributions,length)!=0]

emoticon.tags <-do.call( c,sapply( 1:length(clean.scores), function(x) rep( names(clean.scores[x]),length(clean.scores[[x]]) ) ) )

allscores <- do.call(c, clean.scores)
df.scores<-data.frame(score=as.numeric(allscores),emoticon=emoticon.tags)
row.names(df.scores)<- 1:nrow(df.scores)


library(ggplot2)

p <- ggplot(data=df.scores,aes(
  x=score,
  y=reorder(emoticon,score,mean),
  color=emoticon,
  group=emoticon
)) 


p.nonzero <- ggplot(data=df.scores[df.scores$score!=0,],aes(
  x=score,y=reorder(emoticon,score,mean),
  color=emoticon,
  group=emoticon
)) 

p + geom_point(size=2,alpha=0.6, position = position_jitter(height = 0.2)) +
    geom_errorbarh(xintercept = mean,height=0.6, size=1,aes(xmax=..x..,xmin=..x..),color="black") +
    theme(legend.position="none") +  xlab("Score") + ylab("Emoticon") +  
  scale_y_discrete(labels=function(s) { gsub('\\\\','',s) } #To remove the \\s from the emoticons
 ) 





 ############### NAIVE BAYES ALGORITHM ################

install.packages('devtools')
library('devtools')
install_github('mananshah99/sentR')
library('sentR')
install.packages('nlp')
library('nlp')
install.packages('slam')
library('slam')
install.packages('tm')
library('tm')

positive <- good_text
negative <- bad_text

naivebayesalgr <- classify.naivebayes(tweets)
naivebayesalgr

# GRAPH PLOT - Frequency dependent Naive-Bayes

# Classify Emotion

class_emo = classify.naivebayes(processeddata)
# get emotion best fit
emotion = class_emo[,4]
#Substitute NA's by 'unknown'
emotion[is.na(emotion)] = 'unknown'

sent_df = data.frame(text=processeddata,emotion=emotion,stringsAsFactors=FALSE) #dataframe with results

sent_df= within(sent_df,emotion <- factor(emotion,levels=names(sort(table(emotion),decreasing=TRUE)))) #sort dataframe

sent_df

ggplot(sent_df, aes(x=emotion)) + 
	geom_bar(aes(y=..count.., fill=emotion)) + 
	scale_fill_brewer(palette='Dark2') + 
	labs(x='emotion categories', y='number of tweets') +
	ggtitle('Sentiment Analysis of Tweets') +
	theme(plot.title=element_text(size=12,face='bold'))



