library(tm)
library(udpipe)
library(sentimentr)
library(SentimentAnalysis)
library(tidyverse)
library(ggthemes)
library(networkD3)
library(plotly)
library(pins)
library(downloadthis)

## Fetch saved tweets
climate <- pin_get("climate")

####### Sentiment and Emotion Analyses #######

## Compute sentiment
climate_sentences <- get_sentences(climate$text)
climate_sentiments <- sentiment(climate_sentences)

## Select the first 18000 rows
climate_sentiments <- climate_sentiments[1:18000,]

## Rename first column
colnames(climate_sentiments)[1] <- "Tweets"

## Visualize sentiment score
ggscore <- climate_sentiments |> 
  ggplot()+aes(x = Tweets,y = sentiment)+geom_line(size = 0.8,color = "blue")+theme_hc()+
  geom_point(size = 0.8,color = "blue")+labs(title = "Sentiment Analysis of Tweets about Climate Change",
                                             subtitle = "Positive:6759, Negative: 5755, Neutral: 5486",
                                             caption = "Source: Twitter API",
                                             y = "Sentiment Score"
  )
ggplotly(ggscore)

## Emotions analysis
emotions <- emotion_by(climate$text)  

## Aggregate emotions
emotion_sum <- subset(
  aggregate(
    emotion_count ~ emotion_type,emotions,sum
  ),
  emotion_count > 0
)

## Create new emotions data frame
top_emotions <- data.frame(
  Emotions = c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust"),
  Count = c(13505,10184,4241,27142,8319,8328,8588,15942)
)

## Visualize emotions
colors <- c("#270082","#F1D00A","#00B4D8","#FF1818",
            "#5463FF","#533E85","#86C6F4","#D82148")
emo_pie <- top_emotions |> 
  plot_ly(
    labels = ~Emotions,values = ~Count,type = "pie",
    textposition = "inside",
    textinfo = "label+percent",
    insidetextfont = list(color = "#FFFFFF"),
    hoverinfo = "text",
    text = ~paste("Emotion Count:",Count),
    marker = list(colors = colors,
                  line = list(color = "#FFFFFF",width = 1)),
    showlegend = FALSE) |> 
  layout(title = "Count of Emotions Expressed in Tweets about Climate Change",
         xaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE))

emo_pie

###### Text Analyses #######

## Import stopwords
mystopwords <- readLines("stopwords.txt")

## Clean tweets with custom built function
cleanText<-function(text){
  a<-gsub("-","",text)
  b<-gsub("/","",a)
  c<-gsub("_","",b)
  d<-gsub("&","",c)
  e<-gsub("%","",d)
  f<-gsub("???","",e)
  g<-gsub("http[^[:space:]]*","",f)
  h<-gsub("\\d","",g)
  i<-gsub("[[:cntrl:]]","",h)
  j<-gsub("@\\S+","",i)
  
  
  
  return(j)
  
}

climate_clean <- cleanText(climate$text)

## Convert to corpus
climate_corp <- Corpus(VectorSource(climate_clean))

climate_corp <- climate_corp |> 
  tm_map(removePunctuation) |> 
  tm_map(removeNumbers) |> 
  tm_map(stripWhitespace) |> 
  tm_map(tolower) |> 
  tm_map(removeWords,stopwords("english")) |> 
  tm_map(removeWords,mystopwords)

tmd <- TermDocumentMatrix(climate_corp)
a <- as.matrix(tmd)
s <- sort(rowSums(a),decreasing = TRUE)
d <- data.frame(Words = names(s),Frequency = s)

d  <- d |> head(10)

df <- data.frame(
  Word = reorder(d$Words,+d$Frequency),
  Frequency = d$Frequency
)

## Plot the most frequently occurring words
ggdf <- df |> 
  ggplot()+aes(x = Word,y = Frequency)+
  geom_bar(stat = "identity",fill = "#58D68D")+coord_flip()+
  scale_y_continuous(expand = c(0,0))+theme(axis.ticks = element_blank(),
                                            axis.title = element_text(size = 12,face = "bold"),
                                            axis.text = element_text(size = 10,face = "bold"),
                                            plot.title = element_text(size = 15,face = "bold"))+
  labs(title = "Most Frequently Occurring Words in Climate Change Tweets",
       y = "Frequency Count")+theme_hc()

ggplotly(ggdf)

## Find most relevant bi-grams

climate_clean <- cleanText(climate$text)

## Load text annotation model
udmodel_english <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

## Annotate text
climate_text <- udpipe_annotate(udmodel_english,climate_clean)
climate_df <- data.frame(climate_text)

## Apply RAKE algorithm to extract bigrams
climate_bigrams <- keywords_rake(climate_df,term = "lemma",group = "doc_id",
                                 relevant = climate_df$upos %in% c("NOUN","ADJ"))

## Read from csv
climate_bi <- read.csv("climate_bigrams.csv",encoding = "UTF-8")

## Visualize data
climate_bi_df <- climate_bi |> head(20)

climate_bi2 <- data.frame(
  Keywords = reorder(climate_bi_df$keyword,+climate_bi_df$rake),
  RAKE = climate_bi_df$rake
)

## Plot bigrams
ggbi <- climate_bi2 |> 
  ggplot()+aes(x = Keywords,y = RAKE)+geom_bar(stat = "identity",fill = "#5463FF")+
  theme(axis.text = element_text(size = 10,face = "bold"),
        axis.title = element_text(size = 12,face = "bold"),
        plot.title = element_text(size = 15,face = "bold"),
        axis.ticks = element_blank())+
  labs(title = "Top Twenty Bi-grams in Climate Change Tweets",
       y = "RAKE Score")+
  coord_flip()+scale_y_continuous(expand = c(0,0))+theme_hc()
ggplotly(ggbi)

## Create dendrogram of co-occurring words
## Cluster dendrogram
dtm <- TermDocumentMatrix(climate_corp,
                          control = list(minWordLength = c(1,Inf)))

## Remove sparse terms
tc <- removeSparseTerms(dtm,sparse = 0.99)

as <- as.matrix(tc)

## Calculate distance
distance <- dist(scale(as))

## Convert to hierarchical clustering object
hc <- hclust(distance,method = "ward.D")

dendroNetwork(hc,zoom = TRUE,treeOrientation = "vertical",width = 1300,height = 650,nodeColour = "#3498db",
              nodeStroke = "#3498db",textColour = "black",linkType = "elbow",textRotate = 90,fontSize = 15)