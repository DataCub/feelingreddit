% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{all_feelings}
\alias{all_feelings}
\title{Retrieve the difference between the sum total of positive and negative words in the thread
Ensure you install tm.lexicon.GeneralInquirer for this by running:
install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")}
\usage{
all_feelings(comments)
}
\arguments{
\item{comments}{A corpus of all the comments in the thread}
}
\value{
the difference between positive and negative comments, where a positive result indicates more positive words and vice-versa
}
\description{
Retrieve the difference between the sum total of positive and negative words in the thread
Ensure you install tm.lexicon.GeneralInquirer for this by running:
install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
}
\examples{
all_feelings(comment_corpus)
}

