### Text preprocess in R ###

################################################################

rm(list=ls()) #clear workspace
getwd() #check working dictionary

### Load packages
library(tm) #package for text mining
library(stringr) #


################################################################
# Step 1: Load the dataset
## This example will use 100 Facebook posts as example for text preprocessing. Please download the file named 'Text preprocessing_example.csv' and put it in the R working directory. 
## The dataset has two column: one is Facebok posts (Message), while another is content topic. 
data <- read.csv(file="Github/Text preprocessing_example.csv", header=T)
data$Message[1:3] #check the posts. 
### We can see these posts often contain lots of words and other characters like urls. Before we conduct content analysis, we need to simplify these texts.

# Step 2: transform textual data into a corpus
## Next, we need to transform the text file into a corpus for preprocessing
corpus <- Corpus(VectorSource(data$Message)) #make sure your put the text feature in 'Corpus' function (use 'data$Message' instead of 'data')

# Step 3 preprocessing
## The main idea of text preprocessing is to simplify our textual for further analysis. In principle, there have several steps for preprocessing including remove stopwords, lowercasing, remove numbers and Punctuation. 
# It is important to remember that the order and choice of these steps will influence your final textual datase. See: Denny, M. J., & Spirling, A. (2018). Text preprocessing for unsupervised learning: Why it matters, when it misleads, and what to do about it. Political Analysis, 26(2), 168-189.

## 3.1. These are common steps:
corpus <- tm_map(corpus, removeWords, stopwords("english")) #Removing Stopwords
corpus <- tm_map(corpus, content_transformer(removeNumbers)) #Removing Numbers
corpus <- tm_map(corpus,  content_transformer(tolower)) #Lowercase
## Note: if your text file has urls, mentions@, and hashtags#, then it is a good idea to remove these characters before removing Punctuation. Otherwise, these special characters may not be removed from your corpus and will influence your analysis.


## 3.2. Usually social media posts contain urls, mentions '@', hashtags and other special characters. Thus, we also need to remove these content. To do so, we can define several functions:
removeURL <- function(x) { gsub('http\\S+\\s*',"" , x) } #Removing urls
removeMentions <- function(x) { gsub("@\\w+", "", x) } #Removing mentions
removeHashTags  <- function(x) { gsub('#\\S+', "", x) } #Removing hashtags
removeRT <- function(x) { gsub('\\b+RT', "", x) } #\#Removing RT in tweets

## Apply these functions to the corpus:
corpus <- tm_map(corpus,removeURL) 
corpus <- tm_map(corpus,removeMentions) 
corpus <- tm_map(corpus,removeHashTags) 
corpus <- tm_map(corpus,removeControl) 
corpus <- tm_map(corpus,removeRT)  

## 3.3. Now we can remove other characters:
corpus <- tm_map(corpus, content_transformer(removePunctuation)) #Removing Punctuation
corpus <- tm_map(corpus, content_transformer(stripWhitespace)) #Removing whitespaces
corpus<- tm_map(corpus, content_transformer(stemDocument), language="english") #Stemming

# Step 4: check the results
## Finally let's check the result by comparing the original texts with the preprocess texts
data$Message[[5]]
as.character(corpus_clean[[5]])

# If the results are not good, we can change the order of thesteps or add more steps. 
# If the results are acceptable, then we can convert the corpus to a Document-Term Matrix for quantitative text analysis and text mining.
