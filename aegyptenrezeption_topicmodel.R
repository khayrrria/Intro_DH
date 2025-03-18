# run "gutenbergr_downloads" script first!

setwd("C:/Dateien/Studium/Master_AEG_Bachelor_DH/2023_WiSe_DH_Einfuehrung-DH/Aegyptenrezeption_Topic_Model/")
options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)
library(dplyr)
library(stringr)

#######################################

# text preprocessing

###
# additional trimming by keeping nouns only (because just removing the stopwords with quanteda did not lead to informative topics)
library(udpipe)
m_eng <- udpipe::udpipe_download_model(language = "english-ewt")
m_eng <- udpipe_load_model(file = m_eng$file_model)

# part of speech tagging
text_vector <- as.character(egyptLit_corpus$text)
pos_paragraphs <- udpipe::udpipe_annotate(m_eng, x = text_vector, doc_id = egyptLit_corpus$paragraph_id)  
pos_paragraphs <- as.data.frame(pos_paragraphs)

# filtering nouns only
noun_rows <- pos_paragraphs %>%
  filter(pos_paragraphs$upos %in% c("NOUN"))

noun_rows %>%
  group_by(doc_id) %>% 
  summarise(paragraphs_lemma = str_c(lemma, collapse = " "))

doc_ids <- data.frame(doc_id = unique(pos_paragraphs$doc_id))
nouns_each_paragraph <- noun_rows %>% group_by(doc_id) %>% 
  summarise(paragraphs_lemma = str_c(lemma, collapse = " "))
nouns_each_paragraph <- left_join(doc_ids, nouns_each_paragraph, by = "doc_id")

# cleaning up the textdata
paragraphs_lowercase <- tolower(nouns_each_paragraph$paragraphs_lemma)
paragraphs_no_sc <- str_replace_all(paragraphs_lowercase, "[[:punct:]]", " ")
paragraphs_no_sc <- str_replace_all(paragraphs_lowercase, "_", " ")
paragraphs_no_sc <- str_replace_all(paragraphs_lowercase, "-", "")
paragraphs_no_sc_and_num <- str_replace_all(paragraphs_no_sc, "\\d", "")
paragraphs_cleaned <- str_squish(paragraphs_no_sc_and_num)

egyptLit_filtered <- nouns_each_paragraph %>% 
  mutate(paragraphs_cleaned = paragraphs_cleaned) %>%
  filter(paragraphs_cleaned != "") %>%
  mutate(paragraph_id = row_number())

###
# building a corpus of lemmata
lemma_corpus <- corpus(egyptLit_filtered$paragraphs_cleaned, docnames = egyptLit_filtered$paragraph_id)

lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")
stopwords_extended <- readLines("stopwords_en.txt",
                                encoding = "UTF-8")

corpus_tokens <- lemma_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)

###
# collocations
egLit_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,
                                                               min_count = 25)
egLit_collocations <- egLit_collocations[1:250, ]
corpus_tokens <- tokens_compound(corpus_tokens, egLit_collocations)

#######################################

# model calculation: Document-Term Matrix

DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

# [check model for 'uninformative' domain specific terms and remove if needed!]
# --> some texts use old english pronouns, which are not included in the stopword list
old_pronouns <- c("thee", "thou", "thy", "thine")
man_woman <- c("man", "woman")
DTM <- DTM[, !(colnames(DTM) %in% old_pronouns)]
DTM <- DTM[, !(colnames(DTM) %in% man_woman)]

# empty documents exist (even with lower min_docfreq) --> they need to be removed
library(Matrix)
non_empty_docs <- rowSums(DTM) > 0
DTM <- DTM[non_empty_docs, ]

# number of topics [--> check different values!]
K <- 20

# compute the LDA model, inference via n iterations of Gibbs sampling [--> check different values!]
topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 500,
  seed = 1,
  verbose = 25,
  alpha = 0.02))

######################################################

# checking results
tmResult <- posterior(topicModel)
beta <- tmResult$terms # K distributions over ncol(DTM) terms
theta <- tmResult$topics # nDocs(DTM) distributions over K topics

terms(topicModel, 10)
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")
# [a lot of uninformative words --> parameters need to be changed & words need to be filtered out!]

# visualization through LDAvis
library(LDAvis)
library("tsne")
library(servr)
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)

#########################################################
# topic proportion over time

library(reshape2)
library(ggplot2)
library(pals)

###
# the filtered dataset needs to get back the metadata about publishing years
egyptLit_filtered$paragraph_id <- as.character(egyptLit_filtered$paragraph_id)

egyptLit_filtered <- left_join(
  egyptLit_filtered, 
  egyptLit_corpus[, c("paragraph_id", "year")], 
  by = "paragraph_id")

# summarising years into centuries for better visualization
egyptLit_filtered$century<- paste0(substr(egyptLit_filtered$year, 0, 2), "00")

# since theta has been filtered down and does not match the length of the dataframes anymore, the metadata needs to be appended directly to theta

theta_with_century <- as.data.frame(theta)
theta_with_century$paragraph_id <- rownames(theta_with_century)

theta_with_century <- left_join(theta_with_century, egyptLit_filtered[, c("paragraph_id", "century")], by = "paragraph_id")

###

# get mean topic proportions per century
topic_proportion_per_century <- aggregate(theta,
                                         by = list(century = theta_with_century$century), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_century)[2:(K+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_century, id.vars = "century")
# plot topic proportions per year as bar plot
require(pals)
ggplot(vizDataFrame,
       aes(x=century, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "century") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

