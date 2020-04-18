## “Persepsi Masyarakat Terhadap Pembatasan Transportasi Jakarta // dengan bantuan Wordcloud & Sentiment Analysis menggunakan metode Text Mining pada Software R”

Oleh:
Raden Akmal Fauzi 	15146026
Alfira Azzahra 	15416066
Rizka Fitria T.	15416080
Aulia Muthia	15416101
Dellaneira Syifaa	15416102


### Background

  Transportasi menjadi elemen dasar infrastruktur yang berpengaruh pada pola pengembangan perkotaan. Problem transportasi dan dampak turunannya diprediksi akan menjadi masalah akut khususnya di negara berkembang, dimana populasi kota tumbuh dengan cepat dan kebutuhan terhadap kendaraan bermotor meingkat dengan tajam (World Bank, 1977). Munculnya virus corona yang menyebabkan penyakit Covid-19 menimbulkan berbagai kebijakan untuk mengurangi penyebaran virus tersebut. Kebijakan-kebijakan yang terbentuk menghimbau untuk menghindari segala bentuk kegiatan berkumpul dan pentingnya menjaga jarak tiap individu. Salah satunya yaitu kebijakan pembatasan waktu layanan dan jumlah penumpang angkutan umum sejak 23 Februari 2020. Dinas Perhubungan Provinsi DKI Jakarta menyatakan telah berkoordinasi dengan Kementerian Perhubungan dan PT Kereta Commuter Indonesia (KCI) untuk menyepakati penyesuaian layanan transportasi umum di Jakarta untuk moda transportasi MRT, LRT, Transjakarta, dan KRL Jabodetabek. 

  Kebijakan pembatasan moda transportasi yang menyebabkan pengurangan jam operasional, rute layanan, dan pembatasan jumlah penumpang akan berdampak pada pengguna transportasi publik. Pasalnya, transportasi publik yang biasanya digunakan untuk bermobilisasi dari dan/atau ke rumah, kantor, sekolah, dan sarana pelayanan umum lainnya akan berkurang supply-nya dalam memenuhi permintaan masyarakat.
  
  Penelitian ini dilakukan untuk memberikan gambaran mengenai efek dari kebijakan pembatasan transportasi akibat wabah Covid-19 terhadap masyarakat di wilayah terdampak.
  
  Untuk mengetahui efek yang ditimbulkan dari kebijakan pembatasan transportasi, akan dilakukan analisis terhadap tanggapan masyarakat menggunakan wordcloud analysis dan sentiment analysis dengan metode Text Mining melalui software R. 
  
  Data yang digunakan dalam penelitian ini yaitu Crawling Data Media Sosial Twitter selama satu minggu, sejak 6 April 2020 sampai 14 April 2020 untuk melihat persepsi masyarakat terkait pembatasan transportasi umum di DKI Jakarta karena adanya kebijakan pembatasan transportasi.
  "Search Operator" pada R termasuk keyword yang berhubungan dengan kondisi transportasi DKI Jakarta, antara lain: 
@DishubDKI_JKT, @PT_Transjakarta, @keretaapikita, @CommuterLine, @KAI121, @mrtjakarta, @lrtjkt, @bptj151, @CurhatKRL, @InfoKRL,,, @ComLineJKT, "ojol", "gojek", "angkot", "angkutan umum", "ojek", "permenhub"
  
  Data yang didapat sebanyak 87751 tweet yang berasal dari 71202 unique screenName.

```markdown
# This script performs the following steps:
#   Step 1: Step 1: Install packages, load required library packages, and set working directories
#   Step 2: Establish access to Twitter API
#   Step 3: Retrieving tweets
#   Step 4: Cleaning tweets
#   Step 5: Analyzing tweets - patterns and frequency of words (Wordcloud Analysis)
#   Step 6: Analyzing tweets - sentiments 
#   This script only includes step 1, 3, 4, 5, and 6. 
################################################################################################# 



**Step 1: Read Library & Data**
library(tidytext)
library(textclean)
library(wordcloud)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(stopwords)
library(readxl)

###############################################################

##################
**Step 3: Retrieving tweets**
##################

#The search operator include any keyword that represent Transportation condition in DKI Jakarta, 
#Such as: 1. @DishubDKI_JKT
#         2. @PT_Transjakarta
#         3. @keretaapikita
#         4. @CommuterLine
#         5. @KAI121
#         6. @mrtjakarta
#         7. @lrtjkt
#         8. @bptj151
#         9. @CurhatKRL
#         10. @InfoKRL
#         11.@ComLineJKT
#         and other words that related to transportation in Jakarta, such as:
#         12. "ojol", "gojek", "angkot", "angkutan umum", "ojek", "permenhub"

setup_twitter_oauth(consumer_key ="",
                    consumer_secret = "",
                    access_token = "",
                    access_secret = "")
transportasidata <- searchTwitteR("keywords",
                                  n=50000,
                                  since='2020-04-06',
                                  until='2020-04-14',
                                  retryOnRateLimit =1)
transportasidata <- twListToDF(transportasidata)

##################
**Step 4: Text Cleaning**
##################

#1. Convert transportasidata to vector from dataframe
transportasidatatext <- as.vector(transportasidata$transportasidata,mode="any")
is.vector(transportasidatatext)

head(transportasidatatext)

transportasidatatext %>% 
  str_to_lower() %>% # a function from stringr package to convert characters to lowercase 
  replace_contraction() %>% # replace contraction with both words
  replace_word_elongation() %>% # replace word elongation with shortened form
  strip() %>% #remove all non words character
  head(30)

##################
**Step 5: Analyzing tweets - patterns and frequency of words (Wordcloud Analysis)**
##################

**Tokenize & Remove Stoprwords**

#We will build our list of stop words that are suitable for indonesian tweets
#we will use the list of stopword developed by ...
stopwordsbahasa <- read_excel('C:/Users/Aulia Muthia/Downloads/SEMESTER 8/SIP/stopwords/stopwords.xlsx',
                              col_names = TRUE)

transportasidatawords <- enframe(transportasidatatext,value = "transportasidata", name = NULL) %>% 
                        #to convert dataframe to vector
                        unnest_tokens(transportasidata,transportasidata) %>% 
                        #tokenization to transform all the words from text to one data
                        count(transportasidata,sort=TRUE) %>% 
                        anti_join(stopwordsbahasa,by = c("transportasidata"="stopwords")) #remove stopwords
                        #stopwords are words that do not have any additional meaning to the core concept words

head(transportasidatawords,10)

**Create Wordcloud with wordcloud package in R**

transportasidatawords%>% #recall the data that we will used
  with(
    wordcloud(
      words = transportasidata, #column within our data that we want to visualize
      freq = n, #column within our data that contains frequency of words
      max.words = 125, #count of words limit that we want to visualize in wordcloud 
      random.order = FALSE, #to visualize the data based on the frequency
      colors = brewer.pal (name = "Dark2", 50) #color palette
    )
  )

##################
**Step 6: Analyzing tweets - sentiments analysis**
##################

# load and scan the words into R
pos.words <- scan("C:/Users/Aulia Muthia/Downloads/positive.csv", what = 'character')
neg.words <- scan("C:/Users/Aulia Muthia/Downloads/negative.csv", what = 'character')

# Create a function for sentiment analysis
sentiment.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.transportasidata = data.frame(score=scores, text=sentences)
  return(scores.transportasidata)
}

#Apply sentiments function to the tweets
result <- sentiment.score(transportasidata$transportasidata,pos.words,neg.words)

# View the summary of the results 
summary(result$score)
hist(result$score, col = "yellow", main = 'Score of tweets', ylab = 'Count of tweets')

count(result$score)

qplot(result$score,xlab = "Score of tweets")

write.csv(result, 'C:/Users/Aulia Muthia/Downloads/SEMESTER 8/SIP/Data/Acc Transportasi/scoringtransport.csv',row.names = FALSE)


```
### Interpretation 
### 1. Wordcloud Analysis

  Terlihat bahwa secara umum penggunaan kata yang sering muncul berkaitan dengan ojek online terkait dengan kebijakan PSBB dalam wabah Covid-19 ini adalah kata "ojol", "ojek", "penumpang" "commuterline", "angkot" , "driver". Hal tersebut mengindikasikan bahwa adanya obrolan masif terkait transportasi yang dilakukan oleh pengguna Twitter. Selain itu, kata yang banyak diperbincangkan seperti "gojekindonesia", "pemerintah" "orderan", "gabisa", "corona", "melarang", "pembatasan" dapat terjadi disebabkan masyarakat yang memperbincangkan dampak negatif dari adanya kebijakan pembatasan transportasi tersebut, khususnya jika ojek online dilarang untuk mengambil penumpang.

### 2. Sentiment Analysis

  Dari hasil sentiment analysis, ditemukan bahwa rata-rata scoring adalah - 0.5733154. Sedangkan, modus dari penilaian adalah nilai 0 dan -1. Terlihat pula bahwa hasil scoring dari masing masing text cenderung lebih banyak tweets yang scorenya negatif daripada yang positif, artinya banyak tweet yang memiliki makna yg buruk di dalam konteks tweetnya terkait transportasi disaat PSBB dan wabah Covid 19 ini.


