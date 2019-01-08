################################################################################
#                    Prep raw data for use with DynaRanks                      #
#                                                                              #
#                                                                              #
#                            By Eli Strauss                                    #
#                                                                              #
#                            November 2018                                     #
################################################################################
rm(list = ls())
library(dplyr)
options(stringsAsFactors = FALSE)

setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/0.rawdata/')

####Read in data####
aggressions <- read.csv('tblAggression.csv')
aggressions$date <- as.Date(aggressions$date)
aggressions$Year <- as.numeric(format(aggressions$date, '%Y'))

#demographic information
#hyenas <- read.csv('/Volumes/Holekamp/code_repository/R/1_output_tidy_tbls/tblHyenas.csv')
hyenas <- read.csv('tblHyenas.csv')


#Fix bd
hyenas[hyenas$id == 'bd',]$birthdate <- hyenas[hyenas$id == 'bd',]$first.seen

#remove hyena 44 who disappeared before our data begin
hyenas <- hyenas[-which(hyenas$id == '44'),]

hyenas$birthdate <- as.Date(hyenas$birthdate)
hyenas$death.date <- as.Date(hyenas$death.date)
hyenas$disappeared <- as.Date(hyenas$disappeared)

#sessions and hyenas per session to fix some missing disappeared dates
hps <- read.csv('tblHyenasPerSession.csv', colClasses = 'character')
sessions <- read.csv('tblSessions.csv', colClasses = 'character')
hps$date <- left_join(hps, sessions, by = 'session')$date
hps$date <- as.Date(hps$date)

last.seen <- hps %>% group_by(hyena) %>% summarize(last.seen = max(date, na.rm = T))

hyenas[is.na(hyenas$disappeared),]$disappeared <- 
  left_join(hyenas[is.na(hyenas$disappeared),],
            last.seen, by = c('id' = 'hyena'))$last.seen

#Clan membership after fission
talekMembership <- read.csv('ClanMembership.csv')

excludeResponse <- c("ignores", "ignore", "ct", "counterattack", "counter", "counters", "counterattacks")
aggsWinner <- filter(aggressions, !response1 %in% excludeResponse, !response2 %in% excludeResponse, !response3 %in% excludeResponse)



#aggressions
aggs <- filter(aggsWinner, clan == 'talek')

#################################################################################
############Females
#################################################################################
#contestants
initial.ranks <- read.csv('iranks_talek.csv')
females <- filter(hyenas, clan == 'talek', sex == 'f', !is.na(birthdate) | id %in% initial.ranks$ID)


females$EndYear <- format(do.call(pmin, c(females[,c('death.date', 'disappeared')], na.rm = T)), '%Y')

#Females are added the first *complete* year that they are at least 1.5 years old
females$StartYear <- format(females$birthdate + 365*2.5, '%Y')
females <-females[,c('id', 'StartYear', 'EndYear')]
names(females) <- c('ID', 'StartYear', 'EndYear')

##Remove females that during fisiion in 2000

##Make list of talek east
easties <- filter(talekMembership, Membership == 'e')
for(eh in unique(females$ID)){
  ehmom <- filter(hyenas, id == eh)$mom
  if(ehmom %in% easties$ID){easties <- rbind(easties, c(eh, 'e', 'kid', 'EDS'))}
}
####remove talek east
females[females$ID %in% easties$ID,]$EndYear <- 1999

first.year <- min(initial.ranks$Year)
last.year <- as.numeric(format(max(aggs$date, na.rm = TRUE), '%Y')) - 1


###Assemble final dataframes
#Initial ranks
initial.ranks <- initial.ranks$ID
female.initial.ranks <- initial.ranks
females[females$ID %in% initial.ranks,'StartYear'] <- first.year

#Contestants
females <- filter(females, StartYear <= last.year,
                  StartYear <= EndYear)
contestants <- data.frame()
for(id in females$ID){
  contestants <- rbind(contestants, data.frame(id, period = seq(from =filter(females, ID == id)$StartYear, to = filter(females, ID == id)$EndYear, by = 1)))
}
contestants <- filter(contestants, period >= first.year, period <= last.year)
contestants$convention1 <- left_join(contestants, hyenas, by = 'id')$mom
contestants$convention2 <- left_join(contestants, hyenas, by = 'id')$litrank
contestants <- filter(contestants, convention1 != '' | id %in% initial.ranks)
contestants <- arrange(contestants, period)

female.contestants <- contestants

########Ammend previously identified interactions to have updated periods######

#Interactions
interactions <- filter(aggs,
                       aggressor %in% c(initial.ranks, contestants$id),
                       recip %in% c(initial.ranks, contestants$id),
                       Year <= last.year,
                       Year >= first.year) %>%
  semi_join(contestants, by = c('aggressor' = 'id', 'Year' = 'period')) %>%
  semi_join(contestants, by = c('recip' = 'id', 'Year' = 'period')) %>%
  rename(winner = aggressor, loser = recip, period = date) %>%
  select(winner, loser, period) %>%
  arrange(period)


###Assemble final dataframes
#Initial ranks
initial.ranks <- initial.ranks$ID
female.initial.ranks <- initial.ranks



########Overwrite previously identified contestants to have updated periods######

females$EndDate <- do.call(pmin, c(left_join(females, hyenas, by = c('ID' = 'id'))[,c('death.date', 'disappeared')], na.rm = T))

first.win <- interactions %>% 
  group_by(winner) %>%
  summarize(StartDate = min(period))

first.loss <- interactions %>% 
  group_by(loser) %>%
  summarize(StartDate = min(period))

first.intx <- full_join(first.loss, first.win, by = c('loser' = 'winner')) %>%
  rowwise() %>%
  summarize(ID = loser, StartDate = min(StartDate.x, StartDate.y, na.rm = TRUE))
females <- left_join(females, first.intx, by = 'ID')
  

females[females$ID %in% initial.ranks,'StartDate'] <- '1988-01-01'

females <- filter(females, StartDate <= max(interactions$period),
                  StartDate <= EndDate)
                                      
##Contestants
contestants <- data.frame()
for(id in females$ID){
  periods.alive<- filter(interactions, period >= females[females$ID == id,'StartDate'],
         period <= females[females$ID == id,'EndDate'])
  if(nrow(periods.alive)){
    contestants <- rbind(contestants, 
                         data.frame(id = id,
                                    period = unique(periods.alive$period)))
  }
}


contestants$convention1 <- left_join(contestants, hyenas, by = 'id')$mom
contestants$convention2 <- left_join(contestants, hyenas, by = 'id')$litrank
contestants <- filter(contestants, convention1 != '' | id %in% initial.ranks)
contestants <- arrange(contestants, period)



female.contestants <- contestants


female.interactions <- select(interactions, winner, loser, period)


save(file = '../12.period_per_interaction.RData',
     list = c('female.contestants', 'female.interactions', 'female.initial.ranks'))

setwd('../')


