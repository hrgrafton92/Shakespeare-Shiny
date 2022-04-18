library(shiny)
library(tidyverse)
#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(shinythemes)
library(tidytext)
library(hrbrthemes)
library(ggthemes)
library(ggsci)
library(cowplot)
library(bslib)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

#testing function runs properly
#getFreq("romeo",TRUE)


# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("simplex"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      # task2: add in the inputs in the sidebarPanel
      selectInput("book_choice","Choose A Book",books),
      checkboxInput("stop_words","Remove Stop Words",value=TRUE),
      actionButton("run","Rerun"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("maxwords","Max # of Words",min=10,max=200,value=100,step=10),
      sliderInput("largewords","Size of Largest Words",min=1,max=8,value=4),
      sliderInput("smallwords","Size of Smallest Words",min=.1,max=4,value=.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput("minwords","Minimum Words for Counts Chart",min=10,max=100,value=25),
      sliderInput("fontsize","Word Size for Counts Chart",min=8,max=30,value=14)
    ),
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    mainPanel(
      tabsetPanel(
        # task6: and modify your figure heights
        tabPanel("Word Cloud",plotOutput("cloud",height = "600px",width="600px")),
        tabPanel("Word Count",plotOutput("freq",height = "600px",width="600px"))
      )
    )
    
  ),
)
server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$run, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book_choice,input$stop_words) # ... = replace with the two inputs from Task 2
    })
  })
    output$cloud <- renderPlot({
      v <- freq()
      pal <- brewer.pal(8,"Dark2")
      v %>% 
        with(
          wordcloud(
            word, 
            n, 
            scale = c(input$largewords,input$smallwords),
            random.order = FALSE, 
            max.words = input$maxwords, 
            colors=pal))
  })
    output$freq <- renderPlot({
      v <- freq()
      v %>% filter(n > input$minwords)%>%
        ggplot(aes(reorder(word,n),n))+
        geom_bar(stat='identity')+
        coord_flip()+
        theme_pander()+
        theme(text = element_text(size = 8),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y = element_text(size = input$fontsize),
              axis.text.x = element_text(size = input$fontsize))
    })
}


shinyApp(ui = ui, server = server)
