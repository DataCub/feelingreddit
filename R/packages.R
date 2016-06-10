.onLoad <- function(libname, pkgname) {install.packages("tm.lexicon.GeneralInquirer", 
                                                        repos="http://datacube.wu.ac.at", type="source")}
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(RedditExtractoR)
library(stringr)
