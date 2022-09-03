library(ggplot2)
library(tm)
library(stringi)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(slam)
library(data.table)
library(NLP)
library(RWeka)


# download file, if not present already
if( ! file.exists("Coursera-SwiftKey.zip") ){
  src_zip_file <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(src_zip_file, destfile = "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}

# look at the source files
zip_files <- unzip("Coursera-SwiftKey.zip", list = T)
zip_files$Date <- NULL
zip_files$Language <- substr(zip_files$Name, 7, 8)
zip_files$Length_in_Mb <- zip_files$Length/(1024^2)
zip_files <- zip_files[zip_files$Length>0,]


# only load twitter
zip_files <- zip_files[ grep("en_US", zip_files$Name),  ]


## Looking for the data


#Read en_US.blogs.txt, en_US.news.txt, en_US.twitter.txt.

blogsURL <- file("en_US.blogs.txt", open="rb") 
blogs <- readLines(blogsURL, encoding = "UTF-8", skipNul=TRUE)

newsURL <- file("en_US.news.txt", open = "rb")
news <- readLines(newsURL, encoding = "UTF-8", skipNul=TRUE)

twitterURL <- file("en_US.twitter.txt", open = "rb")
twitter <- readLines(twitterURL, encoding = "UTF-8", skipNul=TRUE)



## Some data statistics

## Size of Files
S1 <- file.info("en_US.blogs.txt")$size / 1024^2
S2 <- file.info("en_US.news.txt")$size  / 1024^2  
S3 <- file.info("en_US.twitter.txt")$size / 1024^2 

## Number of lines
N1 <- length(blogs) 
N2 <- length(news)  
N3 <- length(twitter) 

## Counting the Words
C1 <- sum(stri_count_words(blogs)) 
C2 <- sum(stri_count_words(news))  
C3 <- sum(stri_count_words(twitter)) 

## The length of the longest line in any of the three en_US data sets
M1 <- max(nchar(blogs)) 
M2 <- max(nchar(news))  
M3 <- max(nchar(twitter)) 
```


```{r  }
resume <- data.frame(
        Name = c("Blogs","News","Twitter"),
        Size = c(S1, S2, S3),
        Nember_of_lines = c(N1, N2, N3),
        Nember_of_words = c(C1, C2, C3),
        max_size_Line = c(M1, M2, M3)
)
resume



## Data sampling



set.seed( 1984 ) 
dspl.blogs  <- sample(blogs,  5000,  replace = TRUE)
set.seed( 1984 ) 
dspl.news <- sample(news, 5000,  replace = TRUE)
set.seed( 1984 )
dspl.tweets <- sample(twitter ,   5000 ,  replace = TRUE)

# blending texts together
dspl <- c(dspl.blogs, dspl.tweets, dspl.news)



##  Corpus and cleaning the data

# text mining on sampled data
corpus <- VCorpus(VectorSource(dspl))

# switch encoding: convert character vector from UTF-8 to ASCII
corpus <- tm_map(corpus, function(x) iconv(x, 'UTF-8', 'ASCII', sub="byte"))
corpus <- tm_map(corpus, tolower, lazy = TRUE) 
corpus <- tm_map(corpus,removePunctuation, preserve_intra_word_dashes=TRUE)
corpus <- tm_map(corpus, removeNumbers)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
corpus <- tm_map(corpus, removeURL)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# assign TEXT flag
corpus <- tm_map(corpus, PlainTextDocument) 

for (i in 1:10){
print(corpus[[i]]$content)
}

## Saving the final corpus
saveRDS(corpus, file = "./finalcorpus.RData")


## Reading final Corpus as data.frame

final_corpus <- readRDS("./finalCorpus.RData")

finalCorpus <-data.frame(text=unlist(sapply(final_corpus,`[`, "content")),stringsAsFactors = FALSE)


## Tokenization
###  Top 20 3-grams in each file
We'll create tri-grams from each data source and plot the top 20 tri-gram. 

```{r  }
library(NLP)
library(RWeka)
file_top_3grams <- data.frame(FileName=character(), TriGram=character(), Count=integer())
# Collecting top 20 3-grams
  token_delim <- " \\t\\r\\n.!?,;\"()"
  tritoken <- NGramTokenizer(dspl, Weka_control(min=3,max=3, delimiters = token_delim))
  tri <- as.data.frame(table(tritoken)) 
  names(tri) <- c("TriGram","Count")
  tri <- tri[order(-tri$Count),]  
  top_tri_words <- cbind(FileName = dspl, tri[1:20,])
  
  file_top_3grams = rbind(file_top_3grams,top_tri_words)

```

```{r  }
ggplot(file_top_3grams,aes(x=reorder(TriGram,Count),y=Count, fill = FileName)) +
  geom_bar(stat='identity') +
  coord_flip() + labs(y='TriGram',x='Count', title = "Top 20 3-grams in each file") + 
  facet_wrap(~ FileName, scales = "free") +
  theme(legend.position="none")



## Tokenizer function to get unigrams
### gettinh unigram

unigram <- NGramTokenizer(finalCorpus, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
unigram <- data.frame(table(unigram))
unigram <- unigram[order(unigram$Freq,decreasing = TRUE),]
names(unigram) <- c("word1", "freq")
unigram$word1 <- as.character(unigram$word1)

write.csv(unigram[unigram$freq > 1,],"unigram.csv",row.names=F)
unigram <- read.csv("unigram.csv",stringsAsFactors = F)
saveRDS(unigram, file = "unigram.RData")
head(unigram)


### Unigram Plot
unigram <- readRDS("unigram.RData")
p1 <- ggplot(data=unigram[1:10,], aes(x = word1, y = freq))
p2 <- p1 + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequently Words")
p3 <- p2 + geom_text(data = unigram[1:10,], aes(x = word1, y = freq, label = freq), hjust=-1, position = "identity")
p3


### Obtaining the biGram

# Tokenizer function to get bigrams
bigram <- NGramTokenizer(finalCorpus, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
bigram <- data.frame(table(bigram))
bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]
names(bigram) <- c("words","freq")
head(bigram)


bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

## saving files 
write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")


### Obtaining the triGrams


# Tokenizer function to get trigrams
trigram <- NGramTokenizer(finalCorpus, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
trigram <- data.frame(table(trigram))
trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]
names(trigram) <- c("words","freq")
head(trigram)



trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
# trigram$words <- NULL
trigram <- data.frame(word1 = trigram$one,word2 = trigram$two, 
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)
# saving files
write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")


### Obtaining the quadGrams

# Tokenizer function to get quadgrams
quadgram <- NGramTokenizer(finalCorpus, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
quadgram <- data.frame(table(quadgram))
quadgram <- quadgram[order(quadgram$Freq,decreasing = TRUE),]
names(quadgram) <- c("words","freq")
head(quadgram)


quadgram$words <- as.character(quadgram$words)
str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))
# quadgram$words <- NULL
quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, stringsAsFactors=FALSE)
# saving files
write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")








