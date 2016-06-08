#' @import ("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")

#' Retrieve a vector of all the words in all the comments of the thread passed to getWords. 
#' 
#' @param url Link to desired Reddit comment thread 
#' @return A vector of all the words in the comment thread
#' @examples getWords("https://www.reddit.com/r/funny/comments/4mno3n/pure_chaos/")
#' @export

getWords <- function(url) {
  content <- reddit_content(url)
  comments <- content$comment
  comments <- tolower(comments)
  comments <- removePunctuation(comments)
  comments <- removeNumbers(comments)
  comments <- removeWords(comments, stopwords("english"))
  comments <- stripWhitespace(comments)
  comments <- unlist(str_split(comments, " "))
  }

#' Retrieve a corpus of all comments of the thread passed to getComments
#' 
#' @param url Link to desired Reddit comment thread 
#' @return A corpus of all the comments in the thread 
#' @examples getWords("https://www.reddit.com/r/funny/comments/4mno3n/pure_chaos/")
#' @export

getComments <- function(url) {
  content <- reddit_content(url)
  comments <- content$comment
  corpus <- Corpus(VectorSource(url))
  
  corpus <- tm_map(corpus, content_transformer(tolower), lazy=T) #lower case conversion
  corpus <- tm_map(corpus, removePunctuation, lazy=T)
  corpus <- tm_map(corpus, removeNumbers, lazy=T) #remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("english"), lazy=T) #remove stop words
  corpus <- tm_map(corpus, stripWhitespace, lazy=T) #strip whitespace
  corpus <- tm_map(corpus, stemDocument, lazy=T)
}

#' Retrieve a vector of all the words in all the comments of the thread passed to getWords. 
#' 
#' @param words A vector of all the words in the comment thread 
#' @return A pretty wordcloud 
#' @examples getWords(words_vector) 
#' @export

posWordCloud <- function(words) {
  lex <- read.csv("inquirerbasic.csv", stringsAsFactors=FALSE)
  lex$Entry = gsub("#1", "", lex$Entry)
  lex = lex[!grepl("#", lex$Entry), ]
  pos.lex = tolower(lex$Entry[lex$Positiv != ""])
  
  
  pos.comments = words[words %in% pos.lex]

  wordcloud(pos.comments, min.freq = mean(as.numeric(table(pos.comments))), colors = brewer.pal(9, "Blues")[6:9])
  }

#' Retrieve a vector of all the words in all the comments of the thread passed to getWords. 
#' 
#' @param words A vector of all the words in the comment thread 
#' @return A pretty wordcloud 
#' @examples getWords(words_vector) 
#' @export

negWordCloud <- function(words) {
  
  lex <- read.csv("inquirerbasic.csv", stringsAsFactors=FALSE)
  lex$Entry = gsub("#1", "", lex$Entry)
  lex = lex[!grepl("#", lex$Entry), ]
  
  neg.lex = tolower(lex$Entry[lex$Negativ != ""])
  
  neg.comments = words[words %in% neg.lex]
  
  wordcloud(neg.comments, scale = c(4, .5), min.freq = mean(as.numeric(table(neg.comments))), colors = brewer.pal(9, "Reds")[6:9])
}

#' Retrieve the difference between the sum total of positive and negative words in the thread
#' 
#' @param comments A corpus of all the comments in the thread
#' @return the difference between positive and negative comments, where a positive result indicates more positive words and vice-versa
#' @examples all_feelings(comment_corpus)
#' @export

all_feelings <- function(comments) {
  
  positive <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
  negative <- sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Negativ"))
  
  margin <- positive - negative # negative score means more negative than positive
  print(sum(margin))
}

