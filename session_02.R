# libraries 
library(quanteda)
library(lexicon)

texts <- c("System change not climate change","The Current State of the Climate")
dfmat <- texts %>%
  tokens() %>%
  dfm()
dfmat

# remove stopwords
dfmat <- texts %>%
  tokens() %>%
  tokens_remove(pattern=stopwords("en")) %>%
  dfm()
dfmat

# ngram / unigram
dfmat <- texts %>%
  tokens() %>%
  tokens_ngrams(2) %>%
  dfm()
dfmat

# ngram / trigram
dfmat <- texts %>%
  tokens() %>%
  tokens_ngrams(3) %>%
  dfm()
dfmat

# stemming
dfmat <- texts %>%
  tokens() %>%
  tokens_wordstem() %>%
  dfm()
dfmat

# Lemmatizing 
texts_2 <- c("I am","you are", "she is")
dfmat_2 <- texts_2 %>%
  tokens() %>%
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
  dfm()
dfmat_2


dfmat_2 <- texts_2 %>%
  tokens() %>%
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
  dfm() %>%
  dfm_tfidf()
dfmat_2

# Text from exercise 
texts_1 <- c("Once upon a time there was land haunted by smog",
             "The people have suffered and could not breathe except the days the factories turned off.")
dfmat_1 <- texts_1 %>%
  tokens() %>%
  dfm()
dfmat_1

dfmat_1 <- texts_1 %>%
  tokens() %>%
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
  dfm() %>%
  dfm_tfidf()
dfmat_1

# Classification

texts <- c(
  "Poverty and inequality implications of carbon pricing",
  "Optimizing and Comparing Topic Models is Simple",
  "How to stop cities and companies causing planetary harm",
  "Contextualized Document Embeddings Improve Topic Coherence",
  "Optimal carbon taxation and horizontal equity"
)
dfmat <- texts %>%
  tokens() %>%
  tokens_wordstem() %>%
  dfm() %>%
  dfm_tfidf()
pred <- (-1 + dfmat[,"document"]*0.5 + dfmat[,"topic"]*3)@x
pred

# exercise #2  
# round 1
texts_3 <- c(
  "Singles in your area", 
  "one time offer 50% off",
  "important information attached",
  "updated document per specifications",
  "read before 12PM 09/15"
)
dfmat_3 <- texts_3 %>%
  tokens() %>%
  #tokens_wordstem() %>%
  dfm() %>%
  dfm_tfidf()
pred_2 <- (1 + dfmat_3[,"singles"]*1.5 + dfmat_3[,"off"]*1.5 + dfmat_3[,"important"]*2)@x
pred_2

#round 2
texts_4 <- c(
  "Singles in your area", 
  "one time offer 50% off",
  "important information attached",
  "updated document per specifications",
  "read before 12PM 09/15",
  "time off request",
  "important offer attached"
)
dfmat_4 <- texts_4 %>%
  tokens() %>%
  #tokens_wordstem() %>%
  dfm() %>%
  dfm_tfidf()
pred_3 <- (1 + dfmat_4[,"singles"]*1.5 + dfmat_4[,"off"]*1.5 + dfmat_4[,"important"]*2 + dfmat_4[,"attached"]*3)@x
pred_3

#round 3
texts_5 <- c(
  "Singles in your area", 
  "one time offer 50% off",
  "important information attached",
  "updated document per specifications",
  "read before 12PM 09/15",
  "time off request",
  "important offer attached",
  "Help support Sierra Club",
  "mindfulness",
  "a major win for employees",
  "TFS Wednesday Report"
  
)
dfmat_5 <- texts_5 %>%
  tokens() %>%
  #tokens_wordstem() %>%
  dfm() %>%
  dfm_tfidf()
pred_4 <- (1 + dfmat_5[,"singles"]*1.5 + 
             dfmat_5[,"off"]*1.5 + 
             dfmat_5[,"important"]*2 + 
             dfmat_5[,"attached"]*3 +
             dfmat16_5[,"help"]*1)@x
pred_4
