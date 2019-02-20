### Text preprocess in R ###

################################################################
### Install packages
rm(list=ls()) #clearing workspace
library(tm) #package for text mining
library(stringr)

################################################################
## Load the dataset
### This example will use 100 Facebook posts as example for text preprocessing. 
### The dataset has two column: one is Facebok posts (Message), while another is content topic. 
data <- read.csv(file="Github/Text preprocessing_example.csv", header=T)
data$Message[1:3] #check the posts. 
### We can see these posts often contain lots of words and other characters like urls. Before we conduct content analysis, we need to simplify these texts.

# Next, we need to transform the text file into a corpus for preprocessing
corpus <- Corpus(VectorSource(data$Message)) #make sure your put the text feature in 'Corpus' function (use 'data$Message' instead of 'data')

# The main idea of text preprocessing is to simplify our textual for further analysis. In principle, there have several steps for preprocessing including remove stopwords, lowercasing, remove numbers and Punctuation. 
# It is important to remember that the order and choice of these steps will influence your final textual datase. See: Denny, M. J., & Spirling, A. (2018). Text preprocessing for unsupervised learning: Why it matters, when it misleads, and what to do about it. Political Analysis, 26(2), 168-189.

# These are common steps:
corpus_clean <- tm_map(corpus, tolower) #convert to lowercase 
corpus_clean <- tm_map(myCorpus, removeWords, stopwords("english")) #remove stopwords
stopwords("english") #check stopwords
corpus_clean <- tm_map(myCorpus, removePunctuation) #remove punctuation
corpus_clean <- tm_map(myCorpus, removeNumbers) #remove numbers
corpus_clean <- tm_map(corpus_clean, stemDocument) #stemming


# Usually social media posts contain urls, mentions '@', hastags and other special characters. Thus, we also need to remove these content. To do so, we can define a function:
Preprocess_posts <- function(X) {
  gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", X)
  gsub("http\\w+", "", X)
  gsub("http:", "", X) 
  gsub("http[[:alnum:]]*", "", X)
  gsub("[ \t]{2,}", "", X)
  gsub("^\\s+|\\s+$", "", X)
  gsub('[[:cntrl:]]', '', X) 
  gsub("@\\w+", "", X) 
  gsub('#\\S+', '', X)
  sub("\\s*\\B#\\w+(?:\\s*#\\w+)*\\s*$", "", X) 
  gsub('\\b+RT', '', X) 
  gsub(' +',' ', X) 
  gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", X)
}

corpus_clean <- tm_map(corpus_clean, Preprocess_posts) #apply the function to our corpus

#Finally let's check the result by comparing the original texts with the preprocess texts
data$Message[[5]]
as.character(corpus_clean[[5]])

# If the results are not good, we can change the steps of preprocessing or add more steps. 
