# install.packages("rJava")
# install.packages("Rwordseg", repos="http://R-Forge.R-project.org")

# library("Rwordseg", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.1")
# .libPaths()

library(Rwordseg)
messages = c("你愛她","她愛你")
segmentCN(messages)
segmentCN(messages,returnType = "tm")

library(tm)
tmWordsVec = segmentCN(messages,returnType = "tm")
corpus <- Corpus(VectorSource(tmWordsVec))
tdm = TermDocumentMatrix(corpus,control = list(removePunctuation = FALSE,
                                               stopwords = FALSE))
as.matrix(tdm)

?TermDocumentMatrix
?Corpus
