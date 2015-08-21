＃ http://www.bjt.name/upload/pdf/Text%20Mining%20in%20R.pdf



RTrack.title = '2015台灣資料科學愛好者年會 R 語言資料分析上手課程 '
RTrack.content = "面對龐大與多元的資料浪潮，資料科學世代已經興起，近年來國外企業應用 R 語言進行資料分析已經相當成熟，包含 Google、Facebook、Agoda、Intel、Pfizer、Bank of America 等企業，都採用 R 語言進行資料分析。國際知名的 KDnuggets 論壇最新統計，R 語言已經連續三年獲得資料科學家採用資料分析語言第一名的殊榮。

在眾多的資料分析工具中，R語言在統計分析上最為知名，除了具備物件導向的開發環境、擁有強大的資料視覺化能力、更有支援跨平台與免費等優勢，R 語言專注在資料分析上的特性，使它已經成為目前資料分析和繪圖的主流軟體之一。

本年會的「R 語言資料分析上手課程」將帶領有志成為資料科學家的朋友們，透過R語言進入資料分析的殿堂。課程首先介紹 R 的基礎語法，接著引導學員們從真實生活中發想問題，針對問題去進行資料收集、萃取、清理，最後以資料視覺化簡單呈現資料，完成分析資料的第一步。課程中將由來自R 社群的講師們聯手出擊。諸位講師均有豐富的R 語言使用經驗，也有R 語言套件的開發經驗，相信能以深入淺出的方式，向各位學員介紹R 這個處理資料最方便的工具之一。

「資料分析上手課程」除了基礎內容之外，課程中也精心安排 R 語言應用的閃電秀，讓學員能認識 R 語言在各個領域中是如何被應用的。最後再利用公開資料，讓學員們自我練習資料分析的基本功：收集資料、整理資料及資料視覺化。為使得課程進行順利，也請學員詳讀課前須知，並完成各項環境準備。"

library(jiebaR)
library(tm)
library(slam)
# sudo apt-get install gsl-bin libgsl0-dev
# install.packages("topicmodels")
# archlinux: gsl
library(topicmodels)
library(wordcloud)
# gfortran
library(igraph)

# JAVA PATH??
# library(Rwordseg)
# require(rJava)

system(". ~/.bashrc")

mixseg = worker()

File = "speaker"
Lines <- readLines(File)
allstring <- do.call(paste,as.list(Lines))
speaker <- strsplit(allstring,"-----",fixed=TRUE)
cutter <- function(msg){
  filter_words = c("的","在","與","及","等","是","the","and","in","a","at","he","is","of")
  return(filter_segment(mixseg <= msg,filter_words))
  }

segRes = lapply(speaker[[1]],cutter)
paste(segRes[[1]],collapse = " ")
tmWordsVec = sapply(segRes,function(ws) paste(ws,collapse = " "))
corpus <- Corpus(VectorSource(tmWordsVec))
tdm = TermDocumentMatrix(corpus,control = list(wordLengths = c(1, Inf)))

# ref: http://computational-communication.com/post/wen-ben-wa-jue/2013-09-27-topic-modeling-of-song-peom
wordcorpus = corpus
dtm1 <- DocumentTermMatrix(wordcorpus,
                           control = list(
                             wordLengths=c(1, Inf), # to allow long words
                             #bounds = list(global = c(5,Inf)), # each term appears in at least 5 docs
                             removeNumbers = TRUE, 
                             # removePunctuation  = list(preserve_intra_word_dashes = FALSE),
                             weighting = weightTf, 
                             encoding = "UTF-8")
)

colnames(dtm1)
findFreqTerms(dtm1, 10) # 看一下高频词


m <- as.matrix(dtm1)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
# par(mar = rep(2, 4))
# png(paste(getwd(), "/wordcloud50_",  ".png", sep = ''), 
#     width=10, height=10, 
#     units="in", res=700)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word,d$freq, scale=c(6,0.5), min.freq=mean(d$freq),
          max.words=100, random.order=FALSE, rot.per=.01, colors=pal2)
#dev.off()

dtm = dtm1
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1=term_tfidf >= quantile(term_tfidf, 0.5)       # second quantile, ie. median
dtm <- dtm[,l1]
dtm = dtm[row_sums(dtm)>0, ]; dim(dtm) # 2246 6210
summary(col_sums(dtm))


fold_num = 10
kv_num =  seq(2,24)
seed_num = 2015
try_num = 1

smp<-function(cross=fold_num,n,seed)
{
  set.seed(seed)
  dd=list()
  aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)
  for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]
  return(dd)
}

selectK<-function(dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp) # change 60 to 15
{
  per_ctm=NULL
  log_ctm=NULL
  for (k in kv)
  {
    per=NULL
    loglik=NULL
    for (i in 1:try_num)  #only run for 3 replications# 
    {
      cat("R is running for", "topic", k, "fold", i,
          as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")),"\n")
      te=sp[[i]]
      tr=setdiff(1:dtm$nrow, te) # setdiff(nrow(dtm),te)  ## fix here when restart r session
      
      # VEM = LDA(dtm[tr, ], k = k, control = list(seed = SEED)),
      # VEM_fixed = LDA(dtm[tr,], k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
      
      #       CTM = CTM(dtm[tr,], k = k, 
      #                 control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))  
      #       
      Gibbs = LDA(dtm[tr,], k = k, method = "Gibbs",
                  control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
      
      per=c(per,perplexity(Gibbs,newdata=dtm[te,]))
      loglik=c(loglik,logLik(Gibbs,newdata=dtm[te,]))
    }
    per_ctm=rbind(per_ctm,per)
    log_ctm=rbind(log_ctm,loglik)
  }
  return(list(perplex=per_ctm,loglik=log_ctm))
}

sp=smp(n=dtm$nrow, seed=seed_num) # n = nrow(dtm)

system.time((ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)))

## plot the perplexity

m_per=apply(ctmK[[1]],1,mean)
m_log=apply(ctmK[[2]],1,mean)

k=c(kv_num)
df = ctmK[[1]]  # perplexity matrix
logLik = ctmK[[2]]  # perplexity matrix

write.csv(data.frame(k, df, logLik), paste(getwd(), "/Perplexity2_","gibbs5_100", ".csv", sep = ""))

# save the figure
png(paste(getwd(), "/Perplexity2_",try_num, "_gibbs5_100",".png", sep = ''), 
    width=5, height=5, 
    units="in", res=700)


matplot(k, df, type = c("b"), xlab = "Number of topics", 
        ylab = "Perplexity", pch=1:try_num,col = 1, main = '')       
legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num) 

dev.off()

png(paste(getwd(), "/LogLikelihood2_", "gibbs5_100",".png", sep = ''), 
    width=5, height=5, 
    units="in", res=700)
matplot(k, logLik, type = c("b"), xlab = "Number of topics", 
        ylab = "Log-Likelihood", pch=1:try_num,col = 1, main = '')       
legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num) 
dev.off()



# 'Refer to http://cos.name/2013/08/something_about_weibo/'
k = 20
SEED <- 2015
jss_TM2 <- list(
  VEM = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs = LDA(dtm, k = k, method = "Gibbs", 
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
  CTM = CTM(dtm, k = k, 
            control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))) )   
save(jss_TM2, file = paste(getwd(), "/jss_TM2.Rdata", sep = ""))
save(jss_TM, file = paste(getwd(), "/jss_TM1.Rdata", sep = ""))

termsForSave1<- terms(jss_TM2[["VEM"]], 10)
termsForSave2<- terms(jss_TM2[["VEM_fixed"]], 10)
termsForSave3<- terms(jss_TM2[["Gibbs"]], 10)
termsForSave4<- terms(jss_TM2[["CTM"]], 10)

write.csv(as.data.frame(t(termsForSave1)), 
          paste(getwd(), "/topic-document_", "_VEM_", k, "_2.csv", sep=""),
          fileEncoding = "UTF-8")

write.csv(as.data.frame(t(termsForSave2)), 
          paste(getwd(), "/topic-document_", "_VEM_fixed_", k, "_2.csv", sep=""),
          fileEncoding = "UTF-8")

write.csv(as.data.frame(t(termsForSave3)), 
          paste(getwd(), "/topic-document_", "_Gibbs_", k, "_2.csv", sep=""),
          fileEncoding = "UTF-8")
write.csv(as.data.frame(t(termsForSave4)), 
          paste(getwd(), "/topic-document_", "_CTM_", k, "_2.csv", sep=""),
          fileEncoding = "UTF-8")


#'topic graphs'
tfs = as.data.frame(termsForSave3, stringsAsFactors = F); tfs[,1]
tfs = as.data.frame(termsForSave4, stringsAsFactors = F); tfs[,1]


adjacent_list = lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1]) 
edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic = unlist(lapply(1:10, function(i) rep(i, 9)))
edgelist$topic = topic
g <-graph.data.frame(edgelist,directed=T )
l<-layout.fruchterman.reingold(g)
# edge.color="black"
nodesize = centralization.degree(g)$res 
V(g)$size = log( centralization.degree(g)$res )

nodeLabel = V(g)$name
E(g)$color =  unlist(lapply(sample(colors()[26:137], 10), function(i) rep(i, 9))); unique(E(g)$color)

# 保存图片格式
# png(  paste(getwd(), "/topic_graph_gibbs.png", sep=""), width=5, height=5, units="in", res=700)
      
plot(g, vertex.label= nodeLabel,  edge.curved=TRUE, vertex.label.cex =1.25,  edge.arrow.size=0.2, layout=l)
    
# 结束保存图片
dev.off()

###################################################################333
# 關鍵字
keyworker = worker("keywords")
keyword_dsc = vector_keywords(cutter['2015台灣資料科學愛好者年會 R 語言資料分析上手課程 '],keyworker)
# Q: 數字代表意思？

# 詞性
tagger = worker("tag")
tagger <= RTrack.title

qseg$type = "mp" ### 重设模型参数的同时，重新启动引擎。
qseg$type        ### 下次重新启动包是将使用现在的参数，构建模型。
quick_worker$detect = T ### 临时修改，对下次重新启动包时，没有影响。
get_qsegmodel()         ### 获得当前快速模式的默认参数

show_dictpath()     ### 显示词典路径
edit_dict("user")   ### 编辑用户词典


library(tm)
xx<-c(
  "进入", "一个", "平衡", "时代", "现在", "是", "住宅", "价格上涨",
  "太快", "政府", "采用", "政策", "方式", "调控", "这些", "资金",
  "就", "有", "往", "商业地产", "走", "的", "趋势", "因为", "商业地产",
  "把", "自己", "划分", "到", "这", "一类", "去", "从", "职业",
  "来说", "我", "可能", "是", "设计师", "医生", "老师", "记者",
  "那", "我", "就", "做", "一个", "好", "的", "记者", "好", "的",
  "医生", "这是", "社会", "上", "需要", "的", "现在", "这个", "时代",
  "确实", "是", "一个", "特别", "好", "的", "时代", "也", "是")

# length(xx)
corpus = Corpus(VectorSource(xx))
# default 會挑出關鍵字？
dtm_psy = TermDocumentMatrix(corpus)
tdm = DocumentTermMatrix(corpus,control = list(wordLengths = c(1, Inf)))
tdm = DocumentTermMatrix(corpus)
inspect(tdm)
inspect(dtm_psy)




library(tm)
corpus = Corpus(VectorSource(cutter[RTrack.content]))
wordcorpus <- corpus
dtm1 <- DocumentTermMatrix(wordcorpus,
                           control = list(
                             wordLengths=c(1, Inf), # to allow long words
                             #bounds = list(global = c(5,Inf)), # each term appears in at least 5 docs
                             removeNumbers = TRUE, 
                             # removePunctuation  = list(preserve_intra_word_dashes = FALSE),
                             weighting = weightTf, 
                             encoding = "UTF-8")
)

colnames(dtm1)
findFreqTerms(dtm1, 5) # 看一下高频词





















