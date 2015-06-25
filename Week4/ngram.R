# ngram<- function(sentence, n){
#   chunk <- c()
#   for(i in 1 : (nchar(sentence)-n+1)){
#     chunk <- append(chunk, substr(sentence, i , i+n-1))
#   }
#   return(chunk)
# }


ngram <- function(sentence, n) {
  lapply(1 : (nchar(sentence)-n+1), function(i){
    substr(sentence, i, i+n-1)
  })
} 


ngram_split <- function(sentence, n) {
  do.call(rbind,lapply(1 : (nchar(sentence)-n+1), function(i){
    unlist(strsplit(substr(sentence, i, i+n-1),,split=""))
  }))
} 
