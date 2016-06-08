# feelingreddit
Analyze reddit comments for sentiment analysis

This package contains some functions to evaluate the collective sentiment of a given Reddit post. Essentially, the comments to a post can be read in, and by determining the "positivity" of the words in the comments, values are assigned, and a numerical measure of the sentiment is produced. 
Furthermore, two other functions posWordCloud and negWordCloud plot a wordcloud, which is a compact visual representation that indicates the most frequent positive and negative words in the comments, respectively.

While rudimentary, the functions in this package may be used in the development of more sophisticated algorithms for analyzing reddit posts, or for simply formulating educated guesses at the Reddit community's overarching opinions on certain topics.

The more advanced functions of this package require the installation of tm.lexicon.GeneralInquirer, which can be installed with the following commmand: 

install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")