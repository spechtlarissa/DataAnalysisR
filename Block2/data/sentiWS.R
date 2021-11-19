pacman::p_load(here, tidyverse, readxl, writexl, tm, wordcloud, syuzhet, kableExtra, stringr, topicmodels, ggrepel, tidytext)
negativ <- read.delim("C:/Users/Larissa/Desktop/DataScienceR/Block2/data/SentiWSN.txt", header=FALSE)

positiv<- read.delim("C:/Users/Larissa/Desktop/DataScienceR/Block2/data/SentiWSP.txt", header=FALSE)


# zuerst separieren wir die Wörter in V1
neg1 <- negativ %>%
  select(V1, V2) %>% #wir brauchen nur diese beiden Spalten
  mutate(V1 = as.character(V1)) %>%  #benötigt für den nächsten Schritt
  mutate(V1 = sub("\\|.*","\\", V1)) %>%
  mutate(V1 = tolower(V1)) %>% #bereinigt ohne den Anhang nach "|"
  `colnames<-`(c("word", "sentiment")) #Spalten werden umbenannt
# nun separieren wir die Wörter in V2
einzel_negativ <- strsplit(as.character(negativ$V3), split = ",") #die aufgelisteten Wörter werden getrennt
neg2 <- data.frame(V1 = rep(negativ$V2, sapply(einzel_negativ, length)), V3 = unlist(einzel_negativ)) %>% #und mit den Werten in V2 wieder zusammengefügt
  `colnames<-`(c("sentiment", "word")) #Spalten werden umbenannt

# zuerst separieren wir die Wörter in V1
pos1 <- positiv %>%
  select(V1, V2) %>% #wir brauchen nur diese beiden Spalten
  mutate(V1 = as.character(V1)) %>%  #benötigt für den nächsten Schritt
  mutate(V1 = sub("\\|.*","\\", V1)) %>%
  mutate(V1 = tolower(V1)) %>% #bereinigt ohne den Anhang nach "|"
  `colnames<-`(c("word", "sentiment")) #Spalten werden umbenannt
# nun separieren wir die Wörter in V2
einzel_positiv <- strsplit(as.character(positiv$V3), split = ",") #die aufgelisteten Wörter werden getrennt
pos2 <- data.frame(V1 = rep(positiv$V2, sapply(einzel_positiv, length)), V3 = unlist(einzel_positiv)) %>% #und mit den Werten in V2 wieder zusammengefügt
  `colnames<-`(c("sentiment", "word")) #Spalten werden umbenannt (Achtung, andere Reihenfolge)

# c) gemeinsames Lexikon aus den vier Dataframes
SentiWS_df <- rbind(neg1 %>%
                      mutate(Polarity = "negative"),
                    neg2%>%
                      mutate(Polarity = "negative"),
                    pos1 %>%
                      mutate(Polarity = "positive"), 
                    pos2 %>%
                      mutate(Polarity = "positive") %>%
                mutate("word" = as.character(word)))  

                
SentiWS_df <- SentiWS_df[!duplicated(SentiWS_df$word),] 
