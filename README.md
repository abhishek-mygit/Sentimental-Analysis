# Sentimental Analysis


Objective
------------
A Program (in R programming language) that retrieves tweets from Twitter (using twitteR package) on any generic topic and categorizes them into tweets that are in favour of or against by analyzing the lexicons and emoticons in them.


Programming Language
---------------------
R-3.3.3


Software/Platform
---------------------
RStudio


API used for Retrieval of Tweets
-----------------------------------
twitteR API


Packages Needed
----------------------
1) twitteR
2) ROAuth
3) devtools
4) rjson
5) bit64
6) httr
7) ggplot2
8) magrittr
9) RInside
10) Rcpp
11) plyr
12) stringr
13) sentR (Github-mananshah99/sentR)
14) nlp
15) slam
16) tm


Algorithms Used
--------------------------
1) Hu and Liu Opinion Lexicon 
   This algorithm takes the retrieved tweets and compares word by word with a list of words/emotions that are are already classified into positive and negative by standard conferences with which it assigns scores and polarity based on the matches and deep comparisons.

2) Emoticon sensing and analysis
   This algorithm classifies the tweets into positive or negative based on the emoticons used in them.

3) Naïve-Bayes Algorithm
   This algorithm at first cleanses the tweets by converting to lowercase and by removing the punctuations, emojis, user details, taggings, references, etc., and then classify them based on Naive-Bayes Classifier.


Results
--------------------
Emoticon sensing is the best among all algorithms, as nowadays emoticons are widely used in tweets and also perfectly describe whether a tweet is positive or negative.Naïve-Bayes is the next best algorithm as it categorizes and then compares to assign positive or negative label to the sentiments. Hu and Liu Opinion Lexicon is the next best as it searches and mines the tweets with the matched texts and also assigns polarity.

