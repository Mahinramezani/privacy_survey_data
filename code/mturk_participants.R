library(tidyverse)

qualtrics  <- read.csv("data/MTURK_02_20_qualtrics.csv")
qualtrics <- qualtrics[-c(1:2),]

q_before <- qualtrics[which(qualtrics$bid == ''),]
q_before <- q_before[c("pid", "StartDate", "EndDate", "IPAddress", "Duration..in.seconds.", 
                       "Finished", "UserLanguage", "Q1", "Q30", "Q31", "Q32", "Q33", "Q53", "Q54")]
          
q_return <- qualtrics[which(qualtrics$bid != ''),]
q_return <- q_return[c("pid", "bid", "StartDate", "EndDate", "Duration..in.seconds.", "Q81",
                       "Q11_1", "Q12_1", "Q14_1", "Q56_1", "Q15_1", "Q66_1", "Q17_1",
                       "Q67_1", "Q69_1", "Q75_1", "Q68_1", "Q70_1", "Q71_1", "Q72_1",
                       "Q36_1", "Q43", "Q44", "Q45", "Q47", "Q48", "Q49", "Q50", "Q51",
                       "Q35", "Q27", "Q28", "Q29", "Finished", "UserLanguage")]

agree <- q_before[which(q_before$Q1 == 'I Agree'),]
disagree <- q_before[which(q_before$Q1 == 'I Disagree'),]

with_bid <- agree[which(agree$pid %in% q_return$bid),]
without_bid <- agree[which(!(agree$pid %in% q_return$bid)),]


######################################################################################
#                       conjointly
######################################################################################

conjointly <- read.csv("data/MTURK_02_20_Respondents.csv")
conjointly <- conjointly[c("pid..", "Participant.ID", "Included.in.analysis.", "Status", 
                           "Time.of.opening.survey", "Length.of.interview", "Device",
                           "Location..City", "Location..Region", "Location..Postcode", "Location..Country")]

conjointly <- conjointly[which(conjointly$pid.. %in% agree$pid),]
x <- conjointly[which(duplicated(conjointly$pid..) == TRUE),]

c_complete <- conjointly[which(conjointly$Status == 'Completed survey'),] 
c1 <- conjointly[which(conjointly$Status == 'Incomplete survey'),] 
c2 <- conjointly[which(conjointly$Status == 'Opened survey link, but did not complete survey'),] 
c3 <- conjointly[which(conjointly$Status == 'Completed, but marked as a low quality response'),] 

c4 <- c_complete[which(!(c_complete$pid.. %in% with_bid$pid)),]  #complete conjointly but don't have bid

c5 <- c2[which((c2$pid.. %in% with_bid$pid)),]   # c2 with bid
c6 <- c3[which((c3$pid.. %in% with_bid$pid)),]  # c3 with bid
#a <- qualtrics[which(qualtrics$pid == 1595158994),]

#c7 <- c_complete[which((c_complete$pid.. %in% without_bid$pid)),]  # c3 with bid

c8 <- c_complete[which(c_complete$pid.. %in% with_bid$pid),]
c8 %>% write.csv("data/MTURK_02_20_conj_complete.csv")

######################################################################################
#                       clean complete data
######################################################################################

conj <- read.csv("data/MTURK_02_20_conj_complete.csv")
conj <- conj[,-1]
raw_responses <- read.csv("data/MTURK_02_20_raw_responses.csv")
raw_responses <- raw_responses[which(raw_responses$ID %in% conj$Participant.ID),]
conj <- conj %>%
  left_join(raw_responses, by = c("Participant.ID" = "ID"))
names(conj)[1] <- "pid"

qualt <- with_bid[which(with_bid$pid %in% conj$pid),]
return_data <- q_return[which(q_return$pid %in% conj$pid),]
q <- qualt %>% 
  left_join(return_data, by = "pid") 

q$pid <- as.numeric(as.character(q$pid))

df <- q %>%
  left_join(conj, by = "pid")



df <- df %>% mutate(total_time_minute = 
                      (as.numeric(as.character(Duration..in.seconds..x)) + 
                      as.numeric(as.character(Duration..in.seconds..y))) / 60 +
                      Length.of.interview,
                    conjointly_time_minute = Length.of.interview,
                    faq_time_minute = as.numeric(as.character(Duration..in.seconds..y)) / 60)


a <- df[c("total_time_minute", "conjointly_time_minute", "faq_time_minute")]


df %>% write.csv("result/mturk_data_round1.csv")
a %>% write.csv("result/mturk_data_round1_time.csv")



######################################################################################
#                       all data
######################################################################################

qualtrics  <- read.csv("data/MTURK_02_20_qualtrics.csv")
qualtrics <- qualtrics[-c(1:2),]

q_before <- qualtrics[which(qualtrics$bid == ''),]
q_before <- q_before[c("pid", "StartDate", "EndDate", "IPAddress", "Duration..in.seconds.", 
                       "Finished", "UserLanguage", "Q1", "Q30", "Q31", "Q32", "Q33", "Q53", "Q54")]

q_return <- qualtrics[which(qualtrics$bid != ''),]
q_return <- q_return[c("pid", "bid", "StartDate", "EndDate", "Duration..in.seconds.", "Q81",
                       "Q11_1", "Q12_1", "Q14_1", "Q56_1", "Q15_1", "Q66_1", "Q17_1",
                       "Q67_1", "Q69_1", "Q75_1", "Q68_1", "Q70_1", "Q71_1", "Q72_1",
                       "Q36_1", "Q43", "Q44", "Q45", "Q47", "Q48", "Q49", "Q50", "Q51",
                       "Q35", "Q27", "Q28", "Q29", "Finished", "UserLanguage")]


conjointly <- read.csv("data/MTURK_02_20_Respondents.csv")
conjointly <- conjointly[c("pid..", "Participant.ID", "Included.in.analysis.", "Status", 
                           "Time.of.opening.survey", "Length.of.interview", "Device",
                           "Location..City", "Location..Region", "Location..Postcode", "Location..Country")]

conjointly <- conjointly[which(conjointly$pid.. %in% q_before$pid),]
x <- conjointly[which(duplicated(conjointly$pid..) == TRUE),]

raw_responses <- read.csv("data/MTURK_02_20_raw_responses.csv")
raw_responses <- raw_responses[which(raw_responses$ID %in% conjointly$Participant.ID),]
conj <- conjointly %>%
  left_join(raw_responses, by = c("Participant.ID" = "ID"))
names(conj)[1] <- "pid"

qualt <- q_before[which(q_before$pid %in% conj$pid),]
return_data <- q_return[which(q_return$pid %in% conj$pid),]
q <- qualt %>% 
  left_join(return_data, by = "pid") 

q$pid <- as.numeric(as.character(q$pid))

df <- q %>%
  left_join(conj, by = "pid")



df <- df %>% mutate(total_time_minute = 
                      (as.numeric(as.character(Duration..in.seconds..x)) + 
                         as.numeric(as.character(Duration..in.seconds..y))) / 60 +
                      Length.of.interview,
                    conjointly_time_minute = Length.of.interview,
                    faq_time_minute = as.numeric(as.character(Duration..in.seconds..y)) / 60)

a <- df[c("total_time_minute", "conjointly_time_minute", "faq_time_minute")]

df %>% write.csv("result/mturk_data_round1_all.csv")
