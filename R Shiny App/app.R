library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

load("quora.Rdata")
load("stack_exchange.Rdata")
load("stack_overflow.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Network of question-and-answer websites on topics in varied fields"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset",
                  choices = c("Quora", "Stack-exchange", "Stack-overflow", "All"),
                  selected = "Quora"),
      
      checkboxGroupInput("topic",
                         "Choose which Topic",
                         choices = c("technology", "movie", "health", "food", "science", "music", "visiting and travel", "sports", "fashion and lifestyle", "politics"),
                         selected = c("technology", "movie", "health", "food", "science", "music", "visiting and travel", "sports", "fashion and lifestyle", "politics")),
      
      checkboxInput("iit","IIT"),
      conditionalPanel(condition = "input.iit == true", 
                       checkboxGroupInput("topic1", "IIT related questions", choices = c("IIT Kanpur statistics", "IIT Bombay statistics"))),
      
      checkboxInput("Coding","coding"),
      conditionalPanel(condition = "input.Coding == true", 
                       checkboxGroupInput("topic2", "Coding questions", choices = c("python", "c++", "java", "fortran", "matlab"))),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset and Summary", h4("Summary of the chosen dataset"), verbatimTextOutput("summary"), h4("Most followed/Viewed quesion (Choose single topic to view):"), textOutput("top"), tableOutput("table")), 
        tabPanel("Plot", plotOutput("plot1"), plotlyOutput("plot2"), plotOutput("plot3")),
        tabPanel("Comparative Study", plotOutput("plot4"), plotOutput("plot5"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
      datasetInput <- reactive({
      switch(input$dataset,
             "Quora" = subset(quora, quora$topic %in% c(input$topic, input$topic1, input$topic2)),
             "Stack-exchange" = subset(stack_exchange, stack_exchange$topic %in% c(input$topic, input$topic1, input$topic2)),
             "Stack-overflow" = subset(stack_overflow, stack_overflow$topic %in% c(input$topic, input$topic1, input$topic2)),
             "All" = "Please select individually to view the data" 
            )
      })

      output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
      })
      
      output$top <- renderText({
        
        if(input$dataset == "Quora")
        {
          if(length(c(input$topic, input$topic1, input$topic2)) > 1)
          {
            print("Please select single topic to view the most followed question")
          }
          else
          {
            paste("on", c(input$topic, input$topic1, input$topic2), "in Quora is",  na.omit(quora$questions[quora$followers == max(quora$followers[quora$topic %in% c(input$topic, input$topic1, input$topic2)])]))
          }
        }
        else if(input$dataset == "Stack-exchange")
        {  
          if(length(c(input$topic, input$topic1, input$topic2)) > 1)
          {
            print("Please select single topic to view the most followed question")
          }
          else
          {
          paste("on", c(input$topic, input$topic2), "in Stack-exchange is",  na.omit(stack_exchange$questions[stack_exchange$views == max(stack_exchange$views[stack_exchange$topic %in% c(input$topic, input$topic2)])]))
          }
        }
        else if(input$dataset == "Stack-overflow")
        {
          if(length(c(input$topic, input$topic1, input$topic2)) > 1)
          {
            print("Please select single topic to view the most followed question")
          }
          else
          {
          paste("on", c(input$topic, input$topic2), "in Stack-overflow is",  na.omit(stack_overflow$questions[stack_overflow$views == max(stack_overflow$views[stack_overflow$topic %in% c(input$topic, input$topic2)])]))
          }
        }
       })
      
      output$table <- renderTable({
        head(datasetInput(), n = input$obs)
      })
      
      output$plot1 <- renderPlot({
        if(input$dataset == 'Quora')
        {
          par(mfrow = c(1,2))
          plot(quora$followers, quora$answers, xlab = "Number of followers", ylab = "Number of answers", main = "Scatterplot of answers vs followers", pch = 16)
          abline(lm(quora$answers ~ quora$followers))
          topic <- as.factor(quora$topic)
          plot(quora$followers, quora$answers, xlab = "Number of followers", ylab = "Number of answers", main = "Scatterplot of answers vs followers (Topicwise)", col = topic, pch = 16)
          topic1 <- character()
          for(i in 1:length(levels(topic)))
          {
            level <- levels(topic)[i]
            foo <- subset(quora, quora$topic == level)
            abline(lm(foo$answers ~ foo$followers), col = i)
            if(coefficients(lm(foo$answers ~ foo$followers))[2] <= 0)
            {
              topic1 <- c(topic1, levels(topic)[i])
            }
          }
          topic1 <- as.factor(topic1)
          legend("topleft", col = topic1, lty = 2,
                 legend = topic1,
                 title = "Topics with Negative slope")
        }
        else if(input$dataset == 'Stack-exchange')
        {
          topic <- unique(stack_exchange$topic[stack_exchange$topic %in% c(input$topic, input$topic1, input$topic2)])
          avg_views <- numeric(length = length(topic))
          avg_votes <- numeric(length = length(topic))
          avg_answers <- numeric(length = length(topic))
          for(i in 1:length(topic))  
          {
            avg_views[i] <- mean(na.omit(stack_exchange[stack_exchange$topic %in% topic[i],]$views))
            avg_votes[i] <- mean(na.omit(stack_exchange[stack_exchange$topic %in% topic[i],]$votes))
            avg_answers[i] <- mean(na.omit(stack_exchange[stack_exchange$topic %in% topic[i],]$answers))
          }
          data <- data.frame(Topic = rep(topic, 3), Name = c(rep("ln (avg_views)", length(topic)), rep("avg_votes", length(topic)), rep("avg_answers", length(topic))), Value = c(log(avg_views), avg_votes, avg_answers))  
          ggplot(data, aes(x = Topic, y = Value, group = Name)) +
            geom_line(aes(color = Name)) +
            geom_point(aes(color = Name)) +
            labs(x = 'Topics',
                 y = 'Average Numbers',
                 title = 'Average number of answers, votes and views per topic') +
            theme(axis.text.x = element_text(angle = 60, hjust = 1),  axis.title = element_text(face = "bold"), plot.title = element_text(size = 20, face = "bold"))
        }
        else if(input$dataset == "Stack-overflow")
        {
          topic <- unique(stack_overflow$topic[stack_overflow$topic %in% c(input$topic, input$topic1, input$topic2)])
          avg_views <- numeric(length = length(topic))
          avg_votes <- numeric(length = length(topic))
          avg_answers <- numeric(length = length(topic))
          for(i in 1:length(topic))  
          {
            avg_views[i] <- mean(na.omit(stack_overflow[stack_overflow$topic %in% topic[i],]$views))
            avg_votes[i] <- mean(na.omit(stack_overflow[stack_overflow$topic %in% topic[i],]$votes))
            avg_answers[i] <- mean(na.omit(stack_overflow[stack_overflow$topic %in% topic[i],]$answers))
          }
          data <- data.frame(Topic = rep(topic, 3), Name = c(rep("ln (avg_views)", length(topic)), rep("ln (avg_votes)", length(topic)), rep("avg_answers", length(topic))), Value = c(log(avg_views), log(avg_votes), avg_answers))  
          ggplot(data, aes(x = Topic, y = Value, group = Name)) +
            geom_line(aes(color = Name)) +
            geom_point(aes(color = Name)) +
            labs(x = 'Topics',
                 y = 'Average Numbers',
                 title = 'Average number of answers, votes and views per topic') +
            theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title = element_text(face = "bold"), plot.title = element_text(size = 20, face = "bold"))
        }
      })

      output$plot2 <- renderPlotly({ 
        
        if(input$dataset == 'Quora')
        {
          topic <- unique(quora$topic[quora$topic %in% c(input$topic, input$topic1, input$topic2)])
          total_followers <- numeric(length = length(topic))
          for(i in 1:length(topic))  
          {
            total_followers[i] <- sum(quora[quora$topic %in% topic[i],]$followers)
          }
          dat1 <- data.frame(topic, total_followers)
          dat1$topic <- factor(dat1$topic, levels = dat1$topic) 
          plot_ly(dat1, labels = ~topic, values = ~total_followers, type = 'pie',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = '#FFFFFF'),
                         hoverinfo = 'text',
                         text = ~paste(total_followers, 'total followers'),
                         marker = list(line = list(color = '#FFFFFF', width = 1)),
                         #The 'pull' attribute can also be used to create space between the sectors
                         showlegend = TRUE) %>% layout(title = "Quora question followers by topics",
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
        else if(input$dataset == 'Stack-exchange')
        {
          topic <- unique(stack_exchange$topic[stack_exchange$topic %in% c(input$topic, input$topic1, input$topic2)])
          total_views <- numeric(length = length(topic))
          for(i in 1:length(topic))  
          {
            total_views[i] <- sum(na.omit(stack_exchange[stack_exchange$topic %in% topic[i],]$views))
          }
          dat1 <- data.frame(topic, total_views)
          dat1$topic <- factor(dat1$topic, levels = dat1$topic) 
          plot_ly(dat1, labels = ~topic, values = ~total_views, type = 'pie',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste(total_views, 'total views'),
                  marker = list(line = list(color = '#FFFFFF', width = 1)),
                  #The 'pull' attribute can also be used to create space between the sectors
                  showlegend = TRUE) %>% layout(title = "Stack-exchange question views by topics",
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
        else if(input$dataset == "Stack-overflow")
        {
          topic <- unique(stack_overflow$topic[stack_overflow$topic %in% c(input$topic, input$topic1, input$topic2)])
          total_views <- numeric(length = length(topic))
          for(i in 1:length(topic))  
          {
            total_views[i] <- sum(na.omit(stack_overflow[stack_overflow$topic %in% topic[i],]$views))
          }
          dat1 <- data.frame(topic, total_views)
          dat1$topic <- factor(dat1$topic, levels = dat1$topic) 
          plot_ly(dat1, labels = ~topic, values = ~total_views, type = 'pie',
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste(total_views, 'total views'),
                  marker = list(line = list(color = '#FFFFFF', width = 1)),
                  #The 'pull' attribute can also be used to create space between the sectors
                  showlegend = TRUE) %>% layout(title = "Stack-overflow question views by topics",
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
      })
      
      output$plot3 <- renderPlot({ 
        
        if(input$dataset == 'Quora')
        {
          topic <- unique(quora$topic[quora$topic %in% c(input$topic, input$topic1, input$topic2)])
          dat2 <- quora[quora$topic %in% topic,]
          g <- ggplot(dat2, aes(topic, followers))
          g + geom_boxplot(varwidth=T, fill="plum") + 
            theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
            labs(title="Box plot", 
                 subtitle="Number of followers grouped by Topic",
                 x="Topic",
                 y="Number of followers")
        }
        else if(input$dataset == 'Stack-exchange')
        {
          topic <- unique(stack_exchange$topic[stack_exchange$topic %in% c(input$topic, input$topic1, input$topic2)])
          dat2 <- stack_exchange[stack_exchange$topic %in% topic,]
          g <- ggplot(dat2, aes(topic, views))
          g + geom_boxplot(varwidth=T, fill="plum") + 
            theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
            labs(title="Box plot", 
                 subtitle="Number of views grouped by Topic",
                 x="Topic",
                 y="Number of views")
        }
        else if(input$dataset == "Stack-overflow")
        {
          topic <- unique(stack_overflow$topic[stack_overflow$topic %in% c(input$topic, input$topic1, input$topic2)])
          dat2 <- stack_overflow[stack_overflow$topic %in% topic,]
          g <- ggplot(dat2, aes(topic, views))
          g + geom_boxplot(varwidth=T, fill="plum") + 
            theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
            labs(title="Box plot", 
                 subtitle="Number of views grouped by Topic",
                 x="Topic",
                 y="Number of views")
        }
      })
      
      output$plot4 <- renderPlot({ 
          
          if(input$dataset == "All")
          {
            rate_quora <- na.omit(quora$answers/quora$followers)
            rate_stack_exchange <- stack_exchange$answers/stack_exchange$votes
            rate_stack_exchange <- rate_stack_exchange[!is.na(rate_stack_exchange) & !is.infinite(rate_stack_exchange)]
            rate_stack_overflow <- na.omit(stack_overflow$answers/stack_overflow$votes)
            rate_stack_overflow <- rate_stack_overflow[!is.na(rate_stack_overflow) & !is.infinite(rate_stack_overflow)]
            avg1 <- mean(rate_quora)
            avg2 <- mean(rate_stack_exchange)
            avg3 <- mean(rate_stack_overflow)
            barplot(c(avg1, avg2, avg3), names.arg = c("Quora", "Stack-exchange", "Stack-overflow"), ylim = 0:1, xlab = "Different platforms", ylab = "average number of answer per follower", main = 'Comparison between different platforms', col = "grey")
          }
       })
      
      output$plot5 <- renderPlot({
        
        if(input$dataset == "All")
        {
          topic <- unique(stack_exchange$topic[stack_exchange$topic %in% c(input$topic, input$topic1, input$topic2)])
          rate_quora<- numeric(length = length(topic))
          rate_stack_exchange <- numeric(length = length(topic))
          rate_stack_overflow <- numeric(length = length(topic))
          for(i in 1:length(topic))
          {
            rate_quora[i] <- mean(na.omit((quora[quora$topic %in% topic[i],]$answers)/(quora[quora$topic %in% topic[i],]$followers)))
            vec.exchange <- ((stack_exchange[stack_exchange$topic %in% topic[i],]$answers)/(stack_exchange[stack_exchange$topic %in% topic[i],]$votes))
            rate_stack_exchange[i] <- mean(vec.exchange[!is.na(vec.exchange) & !is.infinite(vec.exchange)])
            vec.overflow <- ((stack_overflow[stack_overflow$topic %in% topic[i],]$answers)/(stack_overflow[stack_overflow$topic %in% topic[i],]$votes))
            rate_stack_overflow[i] <- mean(vec.overflow[!is.na(vec.overflow) & !is.infinite(vec.overflow)])
          }
          data <- data.frame(Topics = rep(topic, 3), Platforms = c(rep("Quora", length(topic)), rep("Stack-exchange", length(topic)), rep("Stack-overflow", length(topic))), Value = c(rate_quora, rate_stack_exchange, rate_stack_overflow))  
        
          ggplot(data, aes(fill=Platforms, y=Value, x=Topics)) + 
            geom_bar(position="dodge", stat="identity") +
            labs(y = "Average number of answer per follower",
                 title = 'Grouped barchart',
                 subtitle = 'Comparison between different platforms (Topicwise)') +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        }
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
