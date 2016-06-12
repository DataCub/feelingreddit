.onLoad <- function(libname, pkgname) {install.packages("tm.lexicon.GeneralInquirer", 
                                                        repos="http://datacube.wu.ac.at", type="source")}

#' Retrieve a vector of all the words in all the comments of the thread passed to getWords. 
#' 
#' @param url Link to desired Reddit comment thread 
#' @return A vector of all the words in the comment thread
#' @examples getWords("https://www.reddit.com/r/funny/comments/4mno3n/pure_chaos/")
#' @export

getWords <- function(url) {
  content <- RedditExtractoR::reddit_content(url)
  comments <- content$comment
  comments <- tm::removePunctuation(comments)
  comments <- tolower(comments)
  comments <- tm::removeNumbers(comments)
  comments <- stringr::str_replace_all(comments,"[^[:graph:]]", " ") 
  comments <- tm::removeWords(comments, tm::stopwords("english"))
  comments <- tm::stripWhitespace(comments)
  comments <- unlist(stringr::str_split(comments, " "))
}

#' Retrieve a corpus of all comments of the thread passed to getComments
#' 
#' @param url Link to desired Reddit comment thread 
#' @return A corpus of all the comments in the thread 
#' @examples getWords("https://www.reddit.com/r/funny/comments/4mno3n/pure_chaos/")
#' @export

getComments <- function(url) {
  content <- RedditExtractoR::reddit_content(url)
  comments <- content$comment
  corpus <- tm::Corpus(tm::VectorSource(url))
  
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower), lazy=T) #lower case conversion
  corpus <- tm::tm_map(corpus, tm::removePunctuation, lazy=T)
  corpus <- tm::tm_map(corpus, tm::removeNumbers, lazy=T) #remove numbers
  comments <- stringr::str_replace_all(comments,"[^[:graph:]]", " ") #does something with graphical chr
  corpus <- tm::tm_map(corpus, tm::removeWords, stopwords("english"), lazy=T) #remove stop words
  corpus <- tm::tm_map(corpus, tm::stripWhitespace, lazy=T) #strip whitespace
  corpus <- tm::tm_map(corpus, tm::stemDocument, lazy=T)
}

#' Retrieve a wordcloud of the most used positive words in all the comments of the thread passed to posWordCloud
#' Ensure you install tm.lexicon.GeneralInquirer for this by running:
#' install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
#' 
#' @param words A vector of all the words in the comment thread 
#' @return A pretty wordcloud 
#' @examples getWords(words_vector) 
#' @export

posWordCloud <- function(words) {
  lex <- lexicon
  lex$Entry = gsub("#1", "", lex$Entry)
  lex = lex[!grepl("#", lex$Entry), ]
  pos.lex = tolower(lex$Entry[lex$Positiv != ""])
  
  pos.comments = words[words %in% pos.lex]

  wordcloud::wordcloud(pos.comments, min.freq = mean(as.numeric(table(pos.comments))), colors = RColorBrewer::brewer.pal(9, "Blues")[6:9])
  }

#' Retrieve a wordcloud of the most used negative words in all the comments of the thread passed to negWordCloud
#' Ensure you install tm.lexicon.GeneralInquirer for this by running:
#' install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
#' 
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
  
  wordcloud::wordcloud(neg.comments, scale = c(4, .5), min.freq = mean(as.numeric(table(neg.comments))), colors = brewer.pal(9, "Reds")[6:9])
}

#' Retrieve the difference between the sum total of positive and negative words in the thread
#' Ensure you install tm.lexicon.GeneralInquirer for this by running:
#' install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
#' 
#' @param comments A corpus of all the comments in the thread
#' @return the difference between positive and negative comments, where a positive result indicates more positive words and vice-versa
#' @examples all_feelings(comment_corpus)
#' @export

all_feelings <- function(comments) {
  
  positive <- sapply(comments, tm::tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
  negative <- sapply(comments, tm::tm_term_score, terms_in_General_Inquirer_categories("Negativ"))
  
  margin <- positive - negative # negative score means more negative than positive
  print(sum(margin))
}

