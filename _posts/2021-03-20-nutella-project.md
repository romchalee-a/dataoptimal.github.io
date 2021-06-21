---
title: "Nutella Project - R (Text Analytics)"
date: 2021-03-20
tags: [R, Text Analytics, business report]
header:
  image: "/images/The-walking-dead.jpg"
excerpt: "Investigated customer’s survey using four NLP frameworks to develop sentiment analysis and analyze words relationship and provided four main actionable recommendations to refine targeting customer, improve marketing campaign, product line and increase exposure on social media
"
mathjax: "true"
---

#Part 1 Business Insight Report

##Introduction

Nutella Project aimed to analyze how people purchase Nutella and to drive actionable recommendations for future marketing approaches. The project initiated by designing a survey about morning routines, how people get to work, and how their perfect days look like. In the end, the survey would ask if the participant would buy Nutella or not. The survey was run across 34 people of diverse backgrounds. For the overview finding of this survey, Nutella indeed is a very likable product, 74% of respondents said that they would buy Nutella.

##Text Analytics Frameworks

This project was applied the R programming language to conducted text analytics along with analyzing word frequency, word relationship, and sentiment analysis. Frameworks applied in this analysis were consist of Correlogram, Sentiment Analysis, N-gram, and Pairwise Correlation.

###1.	Correlogram

Goal: to find keywords for business failure and success
Insights:
-	business failure keywords: Hungry, cooking, bar 
-	business success keyword: cheat, morning 

###2.	Sentiment analysis

Goal: to find specific keywords of business success with underlining sentiment 
Insights:
-	words associated with joy: perfect, beach, sunny 
-	words associated with sadness: bad, late, feeling

###3.	N-gram
Goal: to find phrases that stand out for the business success group
Insights: Outstanding phrases
-	Chocolate, greasy, stuff, hamburgers, fries 
-	Glazed, banana, whipped 

###4.	Pairwise Correlation/ Correlation Network
Goal: to find high correlated words for the business success group
Insights: keywords with high correlation
-	Cereal milk 
-	Pizza chocolate  

##Recommendations

Based on the result of the frameworks, they are leading to the final four recommendations for Nutella.

###1.	Refine target audience

Nutella should refine their target audience not to target people who are more likely to be into cooking at home and like to go to bars but instead target people who are into cheat meals like gym addicts or target people who are busy young adults by recommending Nutella as a fast and easy snack.

###2.	Introducing new product lines and increase exposure on social media**

-	Nutella could expand its product line with more distinct flavor combinations such as Nutella Cereal.  
-	Increase exposure on social media by leveraging trendy food hashtags #pizzanutella #nutelladay #pizzanutellabanana 
-	Introducing a different recipe with Nutella for the tiny jars collection (QR code recipe)

###3.	Revamping old campaigns

Nutella should avoid using words with sad sentiment in any future marketing campaign. Instead, Nutella can adopt joyful keywords in campaigns to revamp and further develop the old campaigns ‘wake up with Nutella campaign’ & ‘morning campaign’ to include a narrative story of a perfect “sunny day” to spend time on the “beach” and having a taste of sweet chocolate and hazelnut.

###4.	Tapping into themed/ seasonal celebrations for greater benefits

With a phrase like ‘chocolate, greasy, hamburgers, and fries’ we recommend a partnership with (any) fast-food chain to cross-sell by including Nutella to their menu. For example, Nutella can partner with McDonald's by adding Nutella to a happy meal or providing a new food pair like fries with Nutella dip. With a phrase like ‘glazed banana whipped’, we recommend launching a Valentine’s Day-themed campaign appealing towards the sentiment of joy and celebration with a bold theme for example Nutella can launch a new packaging design with the tag of ‘Bae-up with Nutella’ and ‘Breakup with Nutella’.

The present for this project can be watched via this link: [Nutella Project Presentation](https://www.youtube.com/watch?v=vQFuouPmwMA)

#Part 2: R Code

##R Programming: Frameworks Analysis

###PREPARE DATA - TOKENIZATION
```
#LOAD ALL LIBRARIES
library(textreadr)
library(pdftools)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
data(stop_words)       	      #create dummy object to remove stop words later 
library(scales) 	            #correlogram 
library(tm)		                #dtm and corpus 
library(Matrix)		            #dtm -- putting the data in a sparse matrix
library(textdata) 	          #sentiment analysis
library(tidyr)		            #sentiment analysis
library(reshape2)             #shape the sentiment
library(igraph)               #n-gram plot
library(ggraph)               #n-gram plot
library(widyr)                #correlation between word
library(quanteda)           	#Naive Bayes
library(quanteda.textmodels)  #Naive Bayes
library(RColorBrewer)         #Naive Bayes


##############################################
# LOAD DATA 
##############################################

#load path to file
setwd("/Users/admin/Desktop/Text_Analytics/Nutella_Team_11/Team 11 - Survey Responses") 
nm <- list.files(path="/Users/admin/Desktop/Text_Analytics/Nutella_Team_11/Team 11 - Survey Responses")

#read the data to make sure all is imported 
my_data <- read_document(file=nm[1])                #read 1st file, line 1-7 for answers from q1-7         
my_data_together <- paste(my_data, collapse = " ")  #1st file: group all 7 answers like a monologue 

#bind to display all file name in 1 column 
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " "))) 

#turn .txt files into DF 
mydf <- data_frame(line=1:34, text=my_txt_text)
View(mydf)

##############################################
# PREPARE DATA - TOKENIZATION
##############################################

#group data by line number; line = id for survey response 
og_nutella <- mydf %>%
  group_by(line) %>%
  ungroup()

#add more to stop_words --> customize stop words 
custom_stop_words <- tribble(
  #column names should match stop_words
  ~word,  ~lexicon,
  #add words to custom stop words
  "um", "CUSTOM",
  "yeah",  "CUSTOM",
  "ooh", "CUSTOM",
  "oh", "CUSTOM"
)

#bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)

#tidy data;one token per row (removing stop words)
tidy_nutella <- og_nutella %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words2) #dictionary including customized stop words 

#counting frequencies for tokens
tidy_nutella %>%
  count(word, sort=TRUE)
```
###FRAMEWORK 1: CORRELATION
```
##############################################
# FRAMEWORK 1: CORRELATION (corr between success groups and failed groups) 
# goal: find keywords that separate the two groups 
##############################################

#BIZ SUCCESS ONLY
df_success <- mydf[c(1,3,4,5,6,7,8,10,11,12,13,16,17,18,19,20,21,22,23,26,27,28,30,33,33,34),]
#tidy data - tokenize, remove customized stop words 
tidy_success <- df_success %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)
#count frequencies 
tidy_success %>%
  count(word, sort=TRUE)

#BIZ FAILURE ONLY
df_failure <- mydf[c(2,9,14,15,24,25,29,31),]
#tidy data - tokenize, remove customized stop words 
tidy_fail <- df_failure %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)
#count frequencies 
tidy_fail %>%
  count(word, sort=TRUE)

#combine both data and prepare for correlation framework  
frequency <- bind_rows(mutate(tidy_success, author= "Biz Success"),
                       mutate(tidy_fail, author= "Biz Failure"))%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n /sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Biz Success`)

#CORRELOGRAM
correlogram <- ggplot(frequency, aes(x=proportion, y=`Biz Failure`, 
                      color = abs(`Biz Failure`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=4)+
  theme(legend.position = "none")+
  labs(y= "Biz Failure", x=NULL)
print(correlogram)

#CORRELATION TEST
corr_compare <- cor.test(data=frequency[frequency$author == "Biz Success",],
         ~proportion + `Biz Failure`)
print(corr_compare)
```
###FRAMEWORK 2: SENTIMENT ANALYSIS
```
##############################################
# FRAMEWORK 2: SENTIMENT ANALYSIS
#goal: find specific keywords with underlining sentiment for BIZ SUCCESS GROUP ONLY 
##############################################

#prepare data 
og_nutella_success <- df_success %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) %>%
  count(line, word, sort=TRUE) %>%
  ungroup()

#load sentiment dictionaries 
afinn <- get_sentiments("afinn") #Negative vs positive sentiment
nrc <- get_sentiments("nrc")     #emotions
bing <- get_sentiments("bing")   #binary

#bind all 3 dictionaries  
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

#WORDS THAT BRING NEGATIVE & POSITIVE SENTIMENTS WITH WEIGHTS
#afinn-negative --> words to avoid for business successs
afinn <- sentiments %>%
  filter(lexicon == "afinn")

neg_weight <- og_nutella_success %>%
  inner_join(afinn) %>%
  mutate(tokenfreqsentiment = n*value) %>%
  arrange(desc(-tokenfreqsentiment))

#afinn-positive --> words to focus for business sucess  
pos_weight <- og_nutella_success %>%
  inner_join(afinn) %>%
  mutate(tokenfreqsentiment = n*value) %>%
  arrange(desc(tokenfreqsentiment))

#print top 3 results 
head(neg_weight,3)
head(pos_weight,3)

#WORDS THAT BRING JOY & SADNESS 
#nrc-sadness --> words to avoid for biz success 
nrc <- get_sentiments("nrc") %>%  
  filter(sentiment == "sadness")

nrc_sad <- og_nutella_success %>%
  inner_join(nrc) %>%  			
  count(word, sort=T)

#nrc-joy --> words to focus for biz success 
nrc <- get_sentiments("nrc") %>%  
  filter(sentiment == "joy")

nrc_joy <- og_nutella_success %>%
  inner_join(nrc) %>%  			
  count(word, sort=T)

#print top 3 results 
head(nrc_sad,3)
head(nrc_joy,3)
```
###FRAMEWORK 3: N-GRAM
```
##############################################
# FRAMEWORK 3: N-GRAM  
#goal: find phrases that stood out for BIZ SUCCESS GROUP ONLY 
##############################################

#create trigram 
trigram <- df_success %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram))%>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word) %>%
  filter(!word3 %in% stop_words2$word) 

#count trigram 
trigram_counts <- trigram %>%
  count(word1, word2, sort = TRUE)

#create matrix to draw trigram network
trigram_graph <- trigram_counts %>%
  graph_from_data_frame()

#visualize trigram network
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
nutella_trigram <- ggraph(trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightsalmon2", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
print(nutella_trigram)
```
###FRAMEWORK 4: PAIRWISE CORRELATION/ CORR NETWORK
```
#prepare dataset
my_tidy_df <- df_success %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words2$word)

#filter least common words 
word_cors <- my_tidy_df %>%
  group_by(word) %>%
  filter(n() >= 4) %>%
  pairwise_cor(word, line, sort=TRUE) #check corr based on how often words appear in the same survey 


#PLOT CORRELATION NETWORK (for corr above 0.5)
corr_network <- word_cors %>%
  filter(correlation >.5) %>%  
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightsalmon1", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()
print(corr_network)
```
###R PROGRAMMING: SHINY DASHBOARD
```
## app.R ##
library(shiny)
library(shinydashboard)
library(DT)

#install.packages("shiny")
#install.packages("shinydashboard")

ui <- dashboardPage( skin = "black",
        dashboardHeader(title = "Nutella Analysis"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("General", tabName = "general", icon = icon("dashboard")),
            menuItem("Token Frequency", tabName = "frequency", icon = icon("wave-square")),
            menuItem("Phrase Analysis", tabName = "correlogram", icon = icon("chart-bar")), #th
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("smile"))
          )
        ),
  dashboardBody(
    tabItems(
      
      #general tab content
      tabItem( tabName = "general",h2("Survey Overview"),
               
               fluidRow(
               valueBox(34, subtitle = "Survey Participants", icon = icon("users")),
               valueBox(26, subtitle = "People that want to buy Nutella", icon = icon("thumbs-up")),
               valueBox(round((26 / 34)*100, 2), subtitle = "Nutella Acceptance (%)", icon = icon("check"),color= "green")
               ),
               fluidRow(
                 
               )
      ),
      
      # First tab content
      tabItem(tabName = "frequency",  h2("Token Frequency in Responses"),
        # Boxes need to be put in a row (or column)
        fluidRow(
          column(width = 8,
        box(title = "Histogram", status = "primary", solidHeader = TRUE, collapsible = T,
            plotly::plotlyOutput('Frequency_Tokens'), width = 12),
        box(title = "Frequency Table", status = "primary", solidHeader = T, width = 12,
            tableOutput('Frequency_Token'), collapsible = T)
          ),
        column(width = 4,
        box(
          title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
          sliderInput('keywords', 'Number of Keywords', 1, 20, 10))
        )
        )
        
        
      ),
  
      # Second tab content
      tabItem(tabName = "correlogram",
              h2("Phrase Analysis"),
        
              fluidRow( 
        box(title = "Correlogram", status="primary", solidHeader = TRUE, collapsible = TRUE,
            plotly::plotlyOutput('Correlogram'), width = 8),
        box(title = "Explanation", status = "warning", width = 4,
            "The keywords over the diagonal line are used significantly more in 
            answers of people that do not want to buy nutella.
            The keywords underneith the diagonal line are used by people that like to buy Nutella.")),
        
              fluidRow(
        box(title = "Trigrams",status= "primary", solidHeader = TRUE, width = 8, collapsible = TRUE,
            plotOutput('Trigram')),
        box(title = "Explanation", status = "warning", width = 4,
            "Interesting phrases that stand out are: 1. Chocolare, greasy, stuff, 
            Hamburgers and fries. 2. Glazed, banana, whipped ")),
        
              fluidRow(
        box(title = "Correlation Network", status = "primary", solidHeader = T, width = 8,
            plotOutput('Corr_Network'), collapsible = TRUE),
        box(title= "Controls", status="warning", solidHeader=T, width = 4,
            sliderInput('corr', "Minimum Correlation", 0, 0.7, 0.5))
              )
        
      ),
  
      # Third tab content
      tabItem(tabName = "sentiment",
            h2("Sentiment Analysis"),
          
            fluidRow(
              box(title = "Frecuency Sentiment", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  tableOutput('Frequency_Sentiment')),
              
              box(
                title = "Controls", status = "warning", solidHeader = TRUE,
                selectInput("positive_negative", "Select Sentiment", c("Positive", "Negative")))
                
            ),
            
            fluidRow(
              box(title = "Sentiment Words", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  tableOutput('Word_Sentiment')),
              
              box(
                title = "Controls", status = "warning", solidHeader = TRUE,
                selectInput("joy_sad", "Show top words that bring", c("Joy", "Sadness")))
            ) 
          
      )
    )
  )
)

server <- function(input, output) {
  frequency_tokens <- function (){
    tidy_nutella %>%
    count(word, sort=TRUE) %>%
    head(input$keywords) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col()
    }
 
  output$Frequency_Tokens <- plotly::renderPlotly({frequency_tokens()})
  
  frequency_token <-function(){
    tidy_nutella %>%
    count(word, sort=TRUE) %>%
    head(input$keywords)
  }
  output$Frequency_Token <- renderTable({frequency_token()})
  
  correlogram <- function() {
    ggplot(frequency, aes(x=proportion, y=`Biz Failure`, 
                          color = abs(`Biz Failure`- proportion)))+
      geom_abline(color="grey40", lty=2)+
      geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
      geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
      scale_x_log10(labels = percent_format())+
      scale_y_log10(labels= percent_format())+
      scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
      facet_wrap(~author, ncol=4)+
      theme(legend.position = "none")+
      labs(y= "Biz Failure", x=NULL)
  }
  output$Correlogram <-plotly::renderPlotly({correlogram()})
  
  trigram <- function(){
    ggraph(trigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightsalmon2", size = 3) +
      geom_node_text(aes(label = name, size = input$size), vjust = 1, hjust = 1) +
      theme_void()
    
  }
  output$Trigram <- renderPlot({trigram()})
  
  corr_network <- function(){
    word_cors %>%
      filter(correlation >input$corr) %>%  
      graph_from_data_frame() %>%
      ggraph(layout = "fr")+
      geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
      geom_node_point(color = "lightsalmon1", size= 6)+
      geom_node_text(aes(label=name), repel=T)+
      theme_void()
  }
  output$Corr_Network <- renderPlot({corr_network()})
  
  sentiment_table <- function() {
    if (input$positive_negative == "Positive"){
      og_nutella_success %>%
        inner_join(afinn) %>%
        mutate(tokenfreqsentiment = n*value) %>%
        arrange(desc(tokenfreqsentiment)) %>%
        head(10)
    }
    else {
      og_nutella_success %>%
        inner_join(afinn) %>%
        mutate(tokenfreqsentiment = n*value) %>%
        arrange(desc(-tokenfreqsentiment)) %>%
        head(10)
    }
  }
  output$Frequency_Sentiment <-renderTable({sentiment_table()})
  
  joy_sadness <-function(){
    if (input$joy_sad == "Joy") {
      nrc <- get_sentiments("nrc") %>%  
        filter(sentiment == "joy")
      
      og_nutella_success %>%
        inner_join(nrc) %>%  			
        count(word, sort=T) %>%
        head(5)
    }
    else {
      nrc <- get_sentiments("nrc") %>%  
        filter(sentiment == "sadness")
      
      og_nutella_success %>%
        inner_join(nrc) %>%  			
        count(word, sort=T) %>%
        head(5)
    }
  }
  output$Word_Sentiment <- renderTable({joy_sadness()})
  
}

shinyApp(ui, server)
```
