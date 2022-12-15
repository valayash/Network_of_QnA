library(tidyverse)
library(rvest)
library(stringr)

# Data Scraping from Quora
# highly popular topics on Quora

topic <- c("technology", "movie", "health", "food", "science", "music", "Visiting%20and%20travel", "sports", "fashion%20and%20lifestyle", "politics", "python", "c++", "java", "fortran", "matlab", "IIT%20kanpur%20statistics", "IIT%20bombay%20statistics")

# creating an empty data frame of specific dimension
mat <- matrix(ncol = 4, nrow = 170)
quora <- data.frame(mat)

# first 10 questions, number of followers, number of answers of each question for each topic
for(i in 1:length(topic))
{
  print(i) #checking purpose 
  
  html_topic <- read_html(paste("https://www.quora.com/search?q=", topic[i], "&type=question", sep = "")) #reading the source page

  text <- html_topic %>% html_elements("script") %>% html_text( )

  # matching pattern from the string
  question_pattern <- str_match_all(text, "https://www.quora.com/........................................................................................................................................................................................................................................................")
                                                                 
  # location of the ending of a question
  loc <- str_locate(question_pattern[[18]], "ch=1")[,1]
  
  # saving first 10 questions is a character vector Qs
  Qs = character(length = length(question_pattern[[18]]))
  for(j in 1:length(question_pattern[[18]]))
  {
    Qs[j] <- gsub("-", " ", substr(question_pattern[[18]][j], 23, loc[j]-1))
  }
  
  # removing the answers included in the set of questions Qs
  for(l in 1:length(Qs))
  {
    if(is.na(str_locate(Qs[l], "/answer/")[,1]))
    {
      Qs = Qs
    }
    else{ Qs = Qs[-l] }
  }
  Qs = na.omit(Qs)
  Qs = Qs[1:10]
  # number of answers for each question
  answer = character(length = length(Qs))
  for(j in 1:length(Qs))
  {
    print(j)
    html_answer <- read_html(paste("https://www.quora.com/", gsub(" ", "-", substr(Qs[j], 1, nchar(Qs[j])-1)), sep = ""))
  
    text_ans <- html_answer %>% html_elements("meta") %>% html_attr("content")
  
    location <- str_locate(text_ans[[9]], ":")[1,1]
  
    answer[j] <- substr(text_ans[[9]], 14, location-2)
    
    if(is.na(answer[j])){ answer[j] = 0 }
    else if(nchar(answer[j]) == 0){ answer[j] = 1}
  }
  answer <- as.numeric(answer)
  
  # number of followers for each question
  followercount <- str_match_all(text, "followerCount................")

  loc <- str_locate(followercount[[18]], ",")[,1]

  follower = character(length = length(question_pattern[[18]]))
  for(k in 1:length(question_pattern[[18]]))
  {
    follower[k] <- substr(followercount[[18]][k], 17, loc[k]-1)
  }
  follower <- as.numeric(follower)
  
  quora[((10*i)-9) : (10*i),] = data.frame(questions = na.omit(Qs), Subject = topic[i], num_follower = na.omit(follower), num_answer = answer)
}
colnames(quora) <- c("questions", "topic", "followers", "answers")
quora[61:70, 2] <- rep("visiting and travel", 10) 
quora[81:90, 2] <- rep("fashion and lifestyle", 10) 
quora[151:160, 2] <- rep("IIT Kanpur statistics", 10)
quora[161:170, 2] <- rep("IIT Bombay statistics", 10)
save(quora, file = "quora.Rdata")


# Data Scraping from Stack-overflow

topic <- c("technology", "movie", "health", "food", "science", "music", "Visiting+and+travel", "sports", "fashion+and+lifestyle", "politics", "python", "c++", "java", "fortran", "matlab")

# creating an empty list of data frames
mat1 <- matrix(ncol = 7, nrow = 225)
stack_overflow <- data.frame(mat1)


for(i in 1:length(topic))
{
  print(i) #checking whether the loop is running
  
  if(i <= 10)
  {
    html <- read_html(paste("https://stackoverflow.com/search?q=", topic[i], sep = "")) #reading the source page
  }
  else
  {
    html <- read_html(paste("https://stackoverflow.com/questions/tagged/", topic[i], "?tab=Frequent", sep = ""))
  }
  questions <- html %>% html_elements(".s-post-summary--content-title a") %>% html_text()
  
  questions_url <- html %>% html_elements(".s-post-summary--content-title a") %>% html_attr("href") #link of each question
  
  views <- numeric(length = length(questions))
  votes <- numeric(length = length(questions))
  answers <- numeric(length = length(questions))
  asked_year <- numeric(length = length(questions)) 
  modified_year <- numeric(length = length(questions))
  
  # number of votes, answers and views per question
  for(j in 1:length(questions_url))
  {
    print(j)
    
    html1 <- read_html(paste("https://stackoverflow.com", questions_url[j], sep = ""))
    
    qs.view <-  (html1 %>% html_elements(".flex--item.ws-nowrap.mb8") %>% html_attr("title"))[3]
    
    views[j] <- as.numeric(gsub(",", "", substr(qs.view, 8, nchar(qs.view)-6)))
    
    votes[j] <- as.numeric((html1 %>% html_elements(".js-vote-count") %>% html_text())[1])
    
    answers[j] <- as.numeric((html1 %>% html_elements("h2") %>% html_attr("data-answercount"))[1])
    
    asked_year[j] <- as.numeric(substr((html1 %>% html_elements(".flex--item.ws-nowrap.mr16.mb8") %>% html_attr("title"))[1], 1, 4))
    
    modified_year[j] <-  as.numeric(substr((html1 %>% html_elements(".s-link.s-link__inherit") %>% html_attr("title"))[2], 1, 4))
  }
  stack_overflow[((length(questions)*i)-(length(questions)-1)) : (length(questions)*i),] <- data.frame(questions, rep(topic[i], length(questions)), votes, answers, views, asked_year, modified_year)
}
colnames(stack_overflow) <- c("questions", "topic", "votes", "answers", "views", "asked year", "modified year")
stack_overflow[91:105, 2] <- rep("visiting and travel", 15) 
stack_overflow[121:135, 2] <- rep("fashion and lifestyle", 15) 
save(stack_overflow, file = "stack_overflow.Rdata")




# Data Scraping from Stack-exchange

topic <- c("technology", "movie", "health", "food", "science", "music", "Visiting+and+travel", "sports", "fashion+and+lifestyle", "politics", "python", "c++", "java", "fortran", "matlab")

# creating an empty list of data frames
mat2 <- matrix(ncol = 7, nrow = 225)
stack_exchange <- data.frame(mat2)


for(i in 1:length(topic))
{
  print(i)
  
  html <- read_html(paste("https://stackexchange.com/search?q=", topic[i], sep = ""))
  
  questions <- html %>% html_elements(".summary span a") %>% html_text()
  
  questions_url <- html %>% html_elements(".result-link a") %>% html_attr("href")
  
  views <- numeric(length = length(questions))
  votes <- numeric(length = length(questions))
  answers <- numeric(length = length(questions))
  asked_year <- numeric(length = length(questions)) 
  modified_year <- numeric(length = length(questions))
  
  for(j in 1:length(questions_url))
  {
    print(j)
    
    html1 <- read_html(questions_url[j])
    
    qs.view <-  (html1 %>% html_elements(".flex--item.ws-nowrap.mb8") %>% html_attr("title"))[3]
    
    views[j] <- as.numeric(gsub(",", "", substr(qs.view, 8, nchar(qs.view)-6)))
    
    votes[j] <- as.numeric((html1 %>% html_elements(".js-vote-count") %>% html_text())[1])
    
    answers[j] <- as.numeric((html1 %>% html_elements("h2") %>% html_attr("data-answercount"))[1])
    
    asked_year[j] <- as.numeric(substr((html1 %>% html_elements(".flex--item.ws-nowrap.mr16.mb8") %>% html_attr("title"))[1], 1, 4))
    
    modified_year[j] <-  as.numeric(substr((html1 %>% html_elements(".s-link.s-link__inherit") %>% html_attr("title"))[2], 1, 4))
  }
  stack_exchange[((length(questions)*i)-(length(questions)-1)) : (length(questions)*i),] <- data.frame(questions, rep(topic[i], length(questions)), votes, answers, views, asked_year, modified_year)
}
colnames(stack_exchange) <- c("questions", "topic", "votes", "answers", "views", "asked_year", "modified_year")
stack_exchange[91:105, 2] <- rep("visiting and travel", 15) 
stack_exchange[121:135, 2] <- rep("fashion and lifestyle", 15) 
save(stack_exchange, file = "stack_exchange.Rdata")
 
