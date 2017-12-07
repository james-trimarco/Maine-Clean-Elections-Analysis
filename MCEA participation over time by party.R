library(dplyr)
library(ggplot2)

#Set working directory
setwd("~/Documents/Data/Maine Elections")

#I've imported the CSV from the Maine Ethics Commission ("https://secure.mainecampaignfinance.com/PublicSite/SearchPages/CandidateQuickListSearch.aspx"),
#and removed the many duplicate names using OpenRefine. Then we're ready to do an analysis.

#Convert .csv file to a table data frame
reps <- tbl_df(read.csv("Reps-cand-general.csv", stringsAsFactors = F))

#Restructure the "Financing" column so that publicly financed runs
#are a 1 and privately financed runs are a 0. I'm throwing out the current
#runs with pending status. 
reps[reps=="MCEA-Qualified"] <- 1
reps[reps=="Traditional"] <- 0
reps <- filter(reps, Financing == 0 | Financing == 1)
reps$Financing <- as.numeric(reps$Financing)

#Transform Election year data into just the year, and remove special
#elections.
reps$Election <- strtrim(reps$Election, 4)
reps <- filter(reps, Election %in% c("2002", "2004", "2006", 
                                     "2008", "2010", "2012", "2014", "2016"))

#Chart program participation by party
by_year <- group_by(reps, Election, Party) #Group data by year and party
sums <- summarise(by_year, Fin = mean(Financing), n=n()) #Create a summary column with the percent of reps who ran MCEA-funded campaigns

sums <- filter(sums, Party =="DEMOCRATIC" | Party == "REPUBLICAN") #Remove third-party candidates

ggplot(data=sums, aes(x=Election, y=Fin, group=Party, colour = Party)) +
        geom_line() +
        geom_point() +
        ylim(0, 1) +
        xlab("Election Year") +
        ylab("Percentage of candidates Publicly Financed") +
        ggtitle("Partication in Maine Clean Elections Act by Party, for Candidates for Representative")






