
rm(list=ls(all.names = TRUE))

source(paste0(getwd(),"/PttCrawler/packIntoFunction.R"))

listPageUrls = getListPageUrls("Gossiping")[1:5]
listPageUrls

postUrls = unlist(lapply(listPageUrls,getPostUrls))
postUrls

getPostData("https://www.ptt.cc/bbs/Gossiping/M.1431338763.A.1BF.html")


getPostData(postUrls[2])
postData = lapply(postUrls[3:5],getPostData)

postDf = data.frame(do.call(rbind,lapply(postData,function(xx) xx$postData)))
pushDf = do.call(rbind,lapply(postData,function(xx) xx$pushDf))


postData = lapply(postUrls,getPostData)
postData = lapply(postUrls, function(url) try(getPostData(url),TRUE))
sapply(postData, function(x) class(x))
sapply(postData, function(x) class(x)) != "try-error"
error_idx = which(sapply(postData, function(x) class(x)) == "try-error")
data_idx = which(sapply(postData, function(x) class(x)) != "try-error")

data = postData[data_idx]
postDf = data.frame(do.call(rbind,lapply(postData[data_idx],function(xx) xx$postData )))
View(postDf)

postDf$Title
