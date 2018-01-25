jeopardyparser <- function(link, oldresults=NULL){
  require(XML)
  require(tidyverse)
  jeopardy <- readLines("http://www.j-archive.com/showgame.php?game_id=5882")


    #Getting Clues
  clues <- 1:length(jeopardy)
  n <- 1
  for (i in 1:length(jeopardy)) {
   if (grepl("div onmouseover", jeopardy[i])) {
      clues[n] <- jeopardy[i]
      n <- n+1
    }
    if (grepl("td class=\\\"clue\\\"", jeopardy[i]) & grepl("td class=\\\"clue\\\"", jeopardy[i+2])){
      clues[n] <- "Clue not read"
      n <- n+1
    }
  }
  clues <- clues[1:n-1]
  clues <- data.frame(clues)
    #Tidy the Data
  clues1 <- separate(clues, clues, into=c("garbage1", "clues"), sep="correct_response") %>%
    separate(clues, into=c("clues", "answer"), sep="onmouseout") %>%
    separate(answer, into=c("garbage2", "answer"), sep="_stuck', '") %>%
    separate(answer, into=c("answer", "garbage3"), sep="onclick")
  clues1$clues <- gsub("&amp;", "and", clues1$clues)
  clues1$clues <- gsub("&(.*?);", "", clues1$clues)
  clues1$clues <- gsub("')\"", "", clues1$clues)
  clues1$clues <- gsub("\\\\", "", clues1$clues)
  clues1$clues <- gsub("br /", "", clues1$clues)
  clues1 <- separate(clues1, clues, into=c("clues", "garbage4"),sep="/")
  for(i in 1:length(clues1[[1]])) {
    if (clues1[[1]][i]=="Clue not read") {
      clues1$garbage4[i] <- "clue not read"
    }
  }
  for(i in 1:length(clues1[[1]])) {if (clues1$garbage4[i]=="i") {clues1$clues[i] <- substring(clues1$clues[i], 2)}}
  
  clues1$answer <- gsub("&amp;", "and", clues1$answer)
  clues1$answer <- gsub("&(.*?);", "", clues1$answer)
  clues1$answer <- gsub("')\"", "", clues1$answer)
  clues1$answer <- gsub("\\\\", "", clues1$answer)
  clues1$answer <- gsub("br /", "", clues1$answer)
  
  clues2 <- select(clues1, -garbage1, -garbage2, -garbage3, -garbage4)
  
    #Getting Categories
  
  categories <- 1:length(jeopardy)
  n <- 1
  for (i in 1:length(jeopardy)){
    if (grepl("category_name", jeopardy[i])) {
      categories[n] <- jeopardy[i]
      n <- n+1
    }
  }
  categories <- categories[1:n-1]
  
  categoriesdf <- data.frame(categories) %>%
    separate(categories, into=c("garbage1", "categories"), sep="name\\\">") %>%
    separate(categories, into=c("categories", "garbage2"), sep="</td>") %>%
    select(-garbage1, -garbage2)

  categoriesdf[[1]] <- gsub("<(.*?)>", "", categoriesdf[[1]])

  firstround <- rep(categoriesdf[[1]][1:6], 5)
  secondround <- rep(categoriesdf[[1]][7:12], 5)
  categorieslist <- c(firstround, secondround, categoriesdf[[1]][13])
  
  values <- vector()
  values[1:6] <- 200
  values[7:12] <- 400
  values[13:18] <- 600
  values[19:24] <- 800
  values[25:30] <- 1000
  values[31:36] <- 400
  values[37:42] <- 800
  values[43:48] <- 1200
  values[49:54] <- 1600
  values[55:60] <- 2000
  values[61] <- NA
  
  result <- data.frame(categorieslist, values, clues2)

  if (!is.null(oldresults)) {result <- rbind(tempresult, oldresults)}  #This combines them if you already have a sheet.
  return(result)
}


jarchivelink <- "http://www.j-archive.com/showgame.php?game_id=5882"

test <- jeopardyparser(link=jarchivelink)

for (i in 1:2) {if (i==1) {
  oldresults <- jeopardyparser(paste("http://www.j-archive.com/showgame.php?game_id=",i, sep=""))
  }
  if (i!=1) {oldresults <- jeopardyparser(paste("http://www.j-archive.com/showgame.php?game_id=",i, sep=""), oldresults = oldresults)}
  Sys.sleep(5)}  #The sleep seems to be needed to download again.

