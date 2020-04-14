#WORDCLOUD ANALYSIS

#1. Read Library & Data
library(tidytext)
library(textclean)
library(wordcloud)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(stopwords)
library(readr)
library(readtext)
library(readxl)

#blokA
#Text Joining using Full join verb with dplyr
blokA12 <- blokA1 %>% full_join(blokA2, by = c("text","text"))
blokA123 <- blokA12 %>% full_join(blokA3, by = c("text","text"))
#Text Cleaning
#1. dari data frame rubah dulu ke vector
blokAtext <- as.vector(blokA123$text,mode="any")
is.vector(blokAtext)

head(blokAtext)

blokAtext %>% 
  str_to_lower() %>%
  replace_contraction() %>%
  replace_word_elongation() %>%
  strip() %>%
  head()

#Tokenize & Remove Stoprwords
stopwordsbahasa <- read_excel('C:/Users/Aulia Muthia/Downloads/SEMESTER 8/SIP/stopwords/stopwords.xlsx',col_names = TRUE)

blokAwords <- enframe(blokAtext,value = "text", name = NULL) %>%
  unnest_tokens(text,text) %>%
  count(text,sort=TRUE) %>% 
  anti_join(stopwordsbahasa,by = c("text"="stopwords"))

#Create Wordcloud
blokAwords%>%
  with(
    wordcloud(
      words = text,
      freq = n,
      max.words = 500,
      random.order = FALSE,
      colors = brewer.pal (name = "Dark2", 8) 
    )
  )



#blokM
blokM12 <- blokM1 %>% full_join(blokM2, by = c("text","text"))
blokM123 <- blokM12 %>% full_join(blokM3, by = c("text","text"))
#Text Cleaning
#1. dari data frame rubah dulu ke vector
blokMtext <- as.vector(blokM123$text,mode="any")
is.vector(blokMtext)

head(blokMtext)

blokMtext %>% 
  str_to_lower() %>%
  replace_contraction() %>%
  replace_word_elongation() %>%
  strip() %>%
  head()

#Tokenize & Remove Stoprwords
blokMwords <- enframe(blokMtext,value = "text", name = NULL) %>%
  unnest_tokens(text,text) %>%
  count(text,sort=TRUE) %>% 
  anti_join(stopwordsbahasa,by = c("text"="stopwords"))

#Create Wordcloud
blokMwords%>%
  with(
    wordcloud(
      words = text,
      freq = n,
      max.words = 500,
      random.order = FALSE,
      colors = brewer.pal (name = "Dark2", 5) 
    )
  )

#cipete
cipete12 <- cipete1 %>% full_join(cipete2, by = c("text","text"))
cipete123 <- cipete12 %>% full_join(cipete3, by = c("text","text"))

#fatmawati
fatmawati12 <- fatmawati1 %>% full_join(fatmawati2, by = c("text","text"))
fatmawati123 <- fatmawati12 %>% full_join(fatmawati3, by = c("text","text"))

#lebakbulus
lebakbulus12 <- lebakbulus1 %>% full_join(lebakbulus2, by = c("text","text"))
lebakbulus123 <- lebakbulus12 %>% full_join(lebakbulus3, by = c("text","text"))

#hnawi
hnawi12 <- hnawi1 %>% full_join(hnawi2, by = c("text","text"))
hnawi123 <- hnawi12 %>% full_join(hnawi3, by = c("text","text"))


