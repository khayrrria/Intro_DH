library(gutenbergr)
library(dplyr)

#############################################################

# downloading books through their ID

# 38 books total, <https://www.gutenberg.org/ebooks/results/> search parameters: Subject = egypt, Language = English, LoC Class = PR (07.03.2025)
# 4 different versions of Shakespeare's "Antony and Cleopatra", 2 versions of Doyle's "Tragedy of the 'Korosko'" --> only one version was included here
# new total: 34 books

book_IDs <- c(61478, 33876, 33610, 52343, 52342, 15721, 15242, 5079, 59716, 21768, 2062, 61116, 2769, 2722, 66095, 72906, 22224, 7031, 6308, 60123, 23994, 5164, 37965, 72901, 57297, 58944, 40108, 2268, 3329, 3710, 3718, 60185, 34978, 9799)

books_dataframe <- gutenberg_download((book_IDs), 
                            mirror = (gutenberg_get_mirror(verbose = TRUE)),
                            strip = TRUE,
                            meta_fields = c("title", "author"))

# the books have been stored in the dataframe with separate entries for each line, so the text needs to be merged
booksMerged_dataframe <- books_dataframe %>%
  group_by(gutenberg_id, author, title) %>% 
  summarise(text = paste(text, collapse = " "), .groups = "drop")

# one book mistakenly got two entries because it has two authors, so they need to be combined
booksMerged_dataframe <- booksMerged_dataframe %>%
  group_by(gutenberg_id, title, text) %>%  
  summarise(author = paste(author, collapse = " "), .groups = "drop")


# adding publishing year (excluding 8 books that could not be downloaded)
years = c(1916, 1898, 1909, 1909, 1920, 1922, 1897, 1912, 1898, 1677, 1910, 1889, 1899, 1921, 1853, 1918, 1897, 1922, 1913, 1910, 1623, 1898, 1860, 1861, 1921, 1914)

booksMerged_dataframe <- booksMerged_dataframe %>%
  arrange(author, gutenberg_id) # the alphabetical order of authors is the order of "years"

booksMerged_dataframe$year <- years

###############################################################
# manually downloading 8 books that could not be downloaded through gutenbergr...

setwd("C:/Dateien/Studium/Master_AEG_Bachelor_DH/2023_WiSe_DH_Einfuehrung-DH/Aegyptenrezeption_Topic_Model/missingbooks")

missing_books <- data.frame(gutenberg_id = c(61478, 61116, 66095, 72906, 60123, 72901, 40108, 60185),
                            title = c("The Court of the King, and Other Studies", "Pharos and Pharillon","Queen of the Dawn: A Love Tale of Old Egypt", "The way of the spirit", "The Wonderful Year", "By the gods beloved", "Tales of Secret Egypt", "Bedouin Love"),
                            text = c(
                              paste(readLines("61478.txt", encoding = "UTF-8"), collapse = " "),
                              paste(readLines("61116.txt", encoding = "UTF-8"), collapse = " "), 
                              paste(readLines("66095.txt", encoding = "UTF-8"), collapse = " "), 
                              paste(readLines("72906.txt", encoding = "UTF-8"), collapse = " "), 
                              paste(readLines("60123.txt", encoding = "UTF-8"), collapse = " "), 
                              paste(readLines("72901.txt", encoding = "UTF-8"), collapse = " "), 
                              paste(readLines("40108.txt", encoding = "UTF-8"), collapse = " "), 
                              paste(readLines("60185.txt", encoding = "UTF-8"), collapse = " ")),
                            author = c("Benson, Margaret", "Forster, E. M.", "Haggard, H. Rider", "Haggard, H. Rider", "Locke, William John", "Baroness Orczy, Emmuska", "Rohmer, Sax", "Weigall, Arthur E. P. Brome"), 
                            year = c(1913, 1923, 1925, 1906, 1915, 1921, 1919, 1922)
                            )

# merging the dataframes
booksMerged_dataframe <- rbind(booksMerged_dataframe, missing_books)

################################################################

# each text needs to be split into several parts, since a topic model can not be made from whole books

########################
# AI generated function
library(stringr)

split_into_sentences <- function(text, sentences_per_chunk = 10) {
  sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))  # Split text at sentence boundaries
  chunked <- split(sentences, ceiling(seq_along(sentences) / sentences_per_chunk))  # Group sentences into chunks
  paragraphs <- sapply(chunked, paste, collapse = " ")  # Recombine into paragraph-like segments
  return(paragraphs)
}
########################

# applying the function to the dataframe

egyptLit_corpus <- data.frame()

for (i in 1:nrow(booksMerged_dataframe)) {
  paragraphs <- split_into_sentences(booksMerged_dataframe$text[i], sentences_per_chunk=10)
  
  # creating a temporary dataframe for storing the paragraphs from each iteration
  temp_dataframe <- data.frame(gutenberg_id = booksMerged_dataframe$gutenberg_id[i],
                               author = booksMerged_dataframe$author[i],
                               title = booksMerged_dataframe$title[i],
                               year = booksMerged_dataframe$year[i],
                               text = paragraphs)
  
  egyptLit_corpus <- rbind(egyptLit_corpus, temp_dataframe)
}

# the entries need unique paragraph IDs to preprocess the corpus later
egyptLit_corpus <- egyptLit_corpus %>%
  mutate(paragraph_id = paste0(row_number()))

# next up: run "aegyptenrezeption_topicmodel" script!

