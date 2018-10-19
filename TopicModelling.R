#topic modelling

require(tidytext)
require(tm)
require(sentimentr)
require(topicmodels)
data(stop_words)
extra_words = c("call", "caller", "called", "sample", "said", "stated", "counselor", "primary", "secondary")


#function to tokenise the text column and remove stop words
TidyText = function(dat){
  
  # log = 1:length(column_text)
  # #column_text = as.character(column_text)
  # 
  # #creating bigrams to index negation
  # if(ngrams == 2){
  #   dd = data.frame(log, column_text, stringsAsFactors = FALSE)%>%
  #     unnest_tokens(., output = ngram, input = column_text, 
  #                   token = "ngrams",
  #                   n = ngrams)%>%
  #     separate(ngram, c("word1", "word"), sep = " ")%>%
  #     filter(word1 == "not")%>%
  #     select(one_of(c("log", "word")))
  #     
  #   
  # }else{
  #   #else use unigrams and return with stop words removed
  #   dd = data.frame(log, column_text, stringsAsFactors = FALSE)%>%
  #     unnest_tokens(., output = word, input = column_text)%>%
  #     anti_join(stop_words)%>%
  #     count(log, word)
  # }
  
  dat%>%
    select(body)%>%
    mutate(log = 1:nrow(.))%>%
    unnest_tokens(., output = word, input = body)%>%
    anti_join(stop_words)%>%
    filter(!word %in% extra_words)%>%
    group_by(log)%>%
    mutate(body = paste0(word, collapse = " "))%>%
    select(one_of(c("log", "body")))%>%
    distinct()%>%
    ungroup()%>%
    return()
}

#sentiment analysis function using sentimentr package
GetSentiment = function(column_text){
  
  get_sentences(column_text)%>%
    sentiment_by()%>%
    as_tibble()%>%
    return()
  
}

#function to compute LDA topic model
TidyLDA = function(dat, k=10){
  
  corp = dat%>%
    filter(body != "")%>%
    TidyText()%>%
    select(body)%>%
    VectorSource()%>%
    Corpus()
  
  #dict = corp
  corp = tm_map(corp, stemDocument)
  #corp = tm_map(corp, stemCompletion, dictionary=dict, type = "first")
  dtm = corp%>%
    DocumentTermMatrix()
  
  lda = LDA(dtm, k, control = list(seed = 1234))
  tidy(lda, matrix = "beta")%>%
    mutate(beta = ifelse(beta > quantile(beta, c(0.8))[1], beta, 0))%>%
    group_by(term)%>%
    mutate(m = mean(beta))%>%
    arrange(-m, -beta, term)%>%
    nest()%>%
    slice(1:10)%>%
    unnest()%>%
    mutate(term = factor(term),
           topic = factor(topic))%>%
    select(-m)%>%
    return()
  
}
