# library(RPTT)
# listPageUrls = getListPageUrls("Gossiping")[1:5]
# postUrls = unlist(lapply(listPageUrls,getPostUrls))
# postData = lapply(postUrls, function(url) try(getPostData(url),TRUE))
# error_idx = which(sapply(postData, function(x) class(x)) == "try-error")
# data_idx = which(sapply(postData, function(x) class(x)) != "try-error")
# data = postData[data_idx]
# postDf = data.frame(do.call(rbind,lapply(postData[data_idx],function(xx) xx$postData )))
# pushDf = do.call(rbind,lapply(postData[data_idx],function(xx) xx$pushDf))

unlist(postDf$Title)[1]
# install.packages("jiebaR")
library(jiebaR)
messages = unlist(postDf$Title)
mixseg = worker()

segRes = lapply(messages,function(msg) mixseg <= msg)
paste(segRes[[1]],collapse = " ")


library(tm)
# install.packages("tm")
tmWordsVec = sapply(segRes,function(ws) paste(ws,collapse = " "))
corpus <- Corpus(VectorSource(tmWordsVec))
tdm = TermDocumentMatrix(corpus,control = list(wordLengths = c(1, Inf)))
inspect(tdm)
