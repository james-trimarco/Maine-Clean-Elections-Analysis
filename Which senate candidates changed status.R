library(dplyr)
library(magrittr)

#Set working directory
setwd("~/Documents/Data/Maine_Elections")

#I imported the data from the Maine Ethics Commission ("https://secure.mainecampaignfinance.com/PublicSite/SearchPages/CandidateQuickListSearch.aspx"),
#and used Google Refine to remove the many duplicate candidates' names. 

#Convert .csv to a table data frame
sens <- tbl_df(read.csv("Sens-cands-general.csv", stringsAsFactors = F))

#Restructure the "Financing" column so that publicly financed runs
#are a 1 and privately financed runs are a 0. I'm throwing out the current
#runs with pending status. 
sens[sens=="MCEA-Qualified"] <- 1
sens[sens=="Traditional"] <- 0
sens <- filter(sens, Financing == 0 | Financing == 1)
sens$Financing <- as.numeric(sens$Financing)

#Remove text so that election year data shows just the year, 
#and remove special elections.
sens$Election <- strtrim(sens$Election, 4)
sens <- filter(sens, Election %in% c("2002", "2004", "2006", 
                                     "2008", "2010", "2012", "2014", "2016"))

#Standardize capitalization of candidate names. 
sens$FullName <- tolower(sens$FullName)
sens$FullName <- capwords(sens$FullName)

#This gives us a list of the candidates who changed status. 

#Here's how it works: Filtering to remove candidates for whom the sum of the Finacing column is 
#zero takes out the candidates who ran only traditional campaigns. 
#Filtering to remove candidates for whom the sum of the Financing column is
#equal to n() takes out the candidates who ran only MCEA-funded campaigns. 
changedStatus <- sens %>% group_by(FullName) %>% 
        dplyr::filter(sum(Financing) > 0 & sum(Financing) < n()) %>%
        distinct(FullName)
 
#This gives us a table of all the runs by candidates who changed status.        
changedStatus.allRuns <- sens %>% group_by(FullName) %>% 
        dplyr::filter(sum(Financing) > 0 & sum(Financing) < n()) 

