library(tidyverse)

qualtrics  <- read.csv("data/Dynata_02_12_qualtrics.csv")
qualtrics <- qualtrics[-c(1:2),]

q_before <- qualtrics[which(qualtrics$bid == ''),]
q_before <- q_before[c("pid", "psid", "StartDate", "EndDate", "IPAddress", "Duration..in.seconds.", 
                       "Finished", "UserLanguage", "Q1", "Q30", "Q31", "Q32", "Q33", "Q53", "Q54")]
          
q_return <- qualtrics[which(qualtrics$bid != ''),]
q_return <- q_return[c("pid", "bid", "psid", "StartDate", "EndDate", "Duration..in.seconds.", "Q77",
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

conjointly <- read.csv("data/Dynata_02_12_Respondents.csv")
conjointly <- conjointly[c("pid..", "Participant.ID", "Included.in.analysis.", "Status", 
                           "Time.of.opening.survey", "Length.of.interview", "Device",
                           "Location..City", "Location..Region", "Location..Postcode", "Location..Country")]

conjointly <- conjointly[which(conjointly$pid.. %in% agree$pid),]
x <- conjointly[which(duplicated(conjointly$pid..) == TRUE),]
#z <- conjointly[which(conjointly$pid.. == 1707774377),]  ##1382089647, 1595158994, 1644795689, 1502609979, 1704404045, 1569754338, 1627269087, 1131905905, 1450702811, 1568421418, 
                                                          ##1511105397, 1503559022, 1704951863, 1704937364, 1589174296, 1399868891, 1707774377  

 

conjointly <- conjointly[which(!(conjointly$Participant.ID %in% 
                                   c(29978435, 29978455, 29978452, 29978462, 29980263, 29972943, 30238467, 
                                     30311266, 30311269, 30311209, 30311272, 30311236, 30311258, 30311279, 
                                     30311275, 30311270, 30311278, 30287631, 30287639, 30262754, 30246387, 
                                     30250363, 30311015, 30437867, 30437869, 30437866, 30437863, 30466232, 
                                     30640166, 30607699, 30524011, 30500793, 30450950, 31221789, 31222018))),]
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
c8 %>% write.csv("data/Dynata_02_12_conj_complete.csv")

######################################################################################
#                       clean complete data
######################################################################################

conj <- read.csv("data/Dynata_02_12_conj_complete.csv")
conj <- conj[,-1]
  raw_responses <- read.csv("data/Dynata_02_12_raw_responses.csv")
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


df %>% write.csv("result/dynata_data_round1.csv")
a %>% write.csv("result/dynata_data_round1_time.csv")



######################################################################################
#                       all data
######################################################################################

qualtrics  <- read.csv("data/Dynata_02_12_qualtrics.csv")
qualtrics <- qualtrics[-c(1:2),]

q_before <- qualtrics[which(qualtrics$bid == ''),]
q_before <- q_before[c("pid", "psid", "StartDate", "EndDate", "IPAddress", "Duration..in.seconds.", 
                       "Finished", "UserLanguage", "Q1", "Q30", "Q31", "Q32", "Q33", "Q53", "Q54")]

q_return <- qualtrics[which(qualtrics$bid != ''),]
q_return <- q_return[c("pid", "bid", "psid", "StartDate", "EndDate", "Duration..in.seconds.", "Q77",
                       "Q11_1", "Q12_1", "Q14_1", "Q56_1", "Q15_1", "Q66_1", "Q17_1",
                       "Q67_1", "Q69_1", "Q75_1", "Q68_1", "Q70_1", "Q71_1", "Q72_1",
                       "Q36_1", "Q43", "Q44", "Q45", "Q47", "Q48", "Q49", "Q50", "Q51",
                       "Q35", "Q27", "Q28", "Q29", "Finished", "UserLanguage")]


conjointly <- read.csv("data/Dynata_02_12_Respondents.csv")
conjointly <- conjointly[c("pid..", "Participant.ID", "Included.in.analysis.", "Status", 
                           "Time.of.opening.survey", "Length.of.interview", "Device",
                           "Location..City", "Location..Region", "Location..Postcode", "Location..Country")]

conjointly <- conjointly[which(conjointly$pid.. %in% q_before$pid),]
x <- conjointly[which(duplicated(conjointly$pid..) == TRUE),]
#z <- conjointly[which(conjointly$pid.. == 1707774377),]  ##1382089647, 1595158994, 1644795689, 1502609979, 1704404045, 1569754338, 1627269087, 1131905905, 1450702811, 1568421418, 
                                                          ##1511105397, 1503559022, 1704951863, 1704937364, 1589174296, 1399868891, 1707774377  



conjointly <- conjointly[which(!(conjointly$Participant.ID %in% 
                                   c(29978435, 29978455, 29978452, 29978462, 29980263, 29972943, 30238467, 
                                     30311266, 30311269, 30311209, 30311272, 30311236, 30311258, 30311279, 
                                     30311275, 30311270, 30311278, 30287631, 30287639, 30262754, 30246387, 
                                     30250363, 30311015, 30437867, 30437869, 30437866, 30437863, 30466232, 
                                     30640166, 30607699, 30524011, 30500793, 30450950, 31221789, 31222018))),]
raw_responses <- read.csv("data/Dynata_02_12_raw_responses.csv")
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


df %>% write.csv("result/dynata_data_round1_all.csv")

