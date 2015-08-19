

RTrack.title = '2015台灣資料科學愛好者年會 R 語言資料分析上手課程 '
RTrack.content = "面對龐大與多元的資料浪潮，資料科學世代已經興起，近年來國外企業應用 R 語言進行資料分析已經相當成熟，包含 Google、Facebook、Agoda、Intel、Pfizer、Bank of America 等企業，都採用 R 語言進行資料分析。國際知名的 KDnuggets 論壇最新統計，R 語言已經連續三年獲得資料科學家採用資料分析語言第一名的殊榮。

在眾多的資料分析工具中，R語言在統計分析上最為知名，除了具備物件導向的開發環境、擁有強大的資料視覺化能力、更有支援跨平台與免費等優勢，R 語言專注在資料分析上的特性，使它已經成為目前資料分析和繪圖的主流軟體之一。

本年會的「R 語言資料分析上手課程」將帶領有志成為資料科學家的朋友們，透過R語言進入資料分析的殿堂。課程首先介紹 R 的基礎語法，接著引導學員們從真實生活中發想問題，針對問題去進行資料收集、萃取、清理，最後以資料視覺化簡單呈現資料，完成分析資料的第一步。課程中將由來自R 社群的講師們聯手出擊。諸位講師均有豐富的R 語言使用經驗，也有R 語言套件的開發經驗，相信能以深入淺出的方式，向各位學員介紹R 這個處理資料最方便的工具之一。

「資料分析上手課程」除了基礎內容之外，課程中也精心安排 R 語言應用的閃電秀，讓學員能認識 R 語言在各個領域中是如何被應用的。最後再利用公開資料，讓學員們自我練習資料分析的基本功：收集資料、整理資料及資料視覺化。為使得課程進行順利，也請學員詳讀課前須知，並完成各項環境準備。"

#install.packages("devtools")
#install.packages("stringi")
#install.packages("pbapply")
#install.packages("Rcpp")
#install.packages("RcppProgress")
library(stringi)
library(pbapply)
library(Rcpp)
library(RcppProgress)
install_github("qinwf/cidian")

library(jiebaR)

cutter = worker()

cutter[RTrack.title]
cutter[RTrack.content]

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


## 抓議程講者（未爬）
# messages = unlist(allData)
# segRes = lapply(messages,function(msg) mixseg <= msg)
# paste(segRes[[1]],collapse = " ")
# tmWordsVec = sapply(segRes,function(ws) paste(ws,collapse = " "))
# corpus <- Corpus(VectorSource(tmWordsVec))
# tdm = TermDocumentMatrix(corpus,control = list(wordLengths = c(1, Inf)))


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





















