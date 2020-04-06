df2 <- readRDS("./Data/bigram.RDS")
df3 <- readRDS("./Data/trigram.RDS")
df4 <- readRDS("./Data/quadgram.RDS")
bigram <- df2[df2$freq > 1,]
trigram <- df3[df3$freq > 1,]
quadgram <- df4[df4$freq > 1,]
bigram_2 <- df2[df2$freq == 1,]; rm(df2)
trigram_2 <- df3[df3$freq == 1,]; rm(df3)
quadgram_2 <- df4[df4$freq == 1,]; rm(df4)

cleanup<-function(x){
  x <- tolower(x)
  # remove URLs
  x <- gsub("http\\S*", "", x)
  # get rid of mentions / emails
  x <- gsub("@\\S*", "", x)
  # Get instances of ’ and replace with an ascii '
  x <- gsub("\xe2\x80\x99", "'", x, perl=TRUE)
  x <- gsub("\u0027|\u0060|\u0091|\u0092|\u0093|\u0094|\u2019|\u009d", "'", x, perl=TRUE)
  # Strip out all punctuation except "'" 
  x <- gsub("[^\'\\w]", " ", x, perl = TRUE)
  # Strip out multiple spaces
  x <- gsub("[' ']{2,}",' ', x)
  # strip out white space at start and end of string
  x <- gsub("^\\s*", "", x)
  x <- gsub("\\s*$", "", x)
  return(x)
}

pred <- function(sentence=NULL, num){
  if (length(sentence) > 0){
    sentence <- cleanup(sentence)
    value <- NA
    for (i in 1:num) {
      sen <- unlist(strsplit(sentence,' '))
      if(length(sen)>=3){
        pred_4 <- grep(paste0("^", sen[length(sen)-2]," ", sen[length(sen)-1]," ", sen[length(sen)]), quadgram$ngrams)
        value <- quadgram$ngrams[pred_4[1]]
        value <- unlist(strsplit(value, " "))[4]
      }
      if(length(sen)==2){
        pred_3 <- grep(paste0("^", sen[length(sen)-1]," ", sen[length(sen)]), trigram$ngrams)
        value <- trigram$ngrams[pred_3[1]]
        value <- unlist(strsplit(value, " "))[3]
      }
      if(length(sen)==1){
        pred_2 <- grep(paste0("^", sen, " "), bigram$ngrams)
        value <- bigram$ngrams[pred_2[1]]
        value <- unlist(strsplit(value, " "))[2]
      }
      if (!is.na(value)) {sentence <- paste(sentence, value)}
      else {
        if(length(sen)>=3){
          pred_4 <- grep(paste0("^", sen[length(sen)-2]," ", sen[length(sen)-1]," ", sen[length(sen)]), quadgram_2$ngrams)
          value <- quadgram_2$ngrams[pred_4[1]]
          value <- unlist(strsplit(value, " "))[4]
        }
        if(length(sen)==2){
          pred_3 <- grep(paste0("^", sen[length(sen)-1]," ", sen[length(sen)]), trigram_2$ngrams)
          value <- trigram_2$ngrams[pred_3[1]]
          value <- unlist(strsplit(value, " "))[3]
        }
        if(length(sen)==1){
          pred_2 <- grep(paste0("^", sen, " "), bigram_2$ngrams)
          value <- bigram_2$ngrams[pred_2[1]]
          value <- unlist(strsplit(value, " "))[2]
        }
        if (!is.na(value)) {sentence <- paste(sentence, value)}
        else {
          sentence <- "Sample too small for your prediction"}
      }
    }
    return(sentence)
  }
  else {return("Please enter text to begin")}
}
