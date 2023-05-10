#file for filtering genbank seq metadata and making some exploratory plots
#set up script
#you'll likely need to download these packages if you are starting R for the first time
library(data.table) 
library(usmap) 
library(ggplot2)
`%notin%` = Negate(`%in%`) #this defines a new function to select items not in a list, really useful! 

#read data
setwd("/Users/kpacheco/Desktop") #change this path to be your specific file path
seq_metadata_full = read.csv("COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv") #this will take a few seconds as the data is large

#convert to a data table to make subsetting faster
seq_metadata_full_table = data.table(seq_metadata_full)
seq_metadata_full_table$date = as.Date(seq_metadata_full_table$date, "%Y-%m") #correct type to allow proper filtering

#filter the original data based on our identified issues
complete_state_data = seq_metadata_full_table

#filter the original data based on our identified issues
complete_state_data = seq_metadata_full_table[!is.na(case_month) &
                                                !is.na(res_state) &
                                                res_state != "" &
                                                !is.na(res_county) &
                                                !is.na(age_group) &
                                                !is.na(sex) &
                                                !is.na(race) &
                                                !is.na(ethnicity) &
                                                !is.na(current_status) &
                                                ethnicity %notin% c("Unknown") &
                                                race %notin% c("Unknown") &
                                                current_status %notin% c("Probable Case")]

#create a new copy of the data selecting only columns we think we will need
reduced_state_data = complete_state_data[ ,c("case_month","res_state","res_county","age_group",
                                             "sex","race","ethnicity","current_status", "county_fips_code")]

#view the file to explore potential issues
View(reduced_state_data)

#get a list of states/counties to identify potential issues --> use [!is.na(res_state)]?
state_list = levels(as.factor(seq_metadata_full_table$res_state))
county_list = levels(as.factor(seq_metadata_full_table$res_county))

#create some summary tables to start investigating what data we have available and identify if there are any remaining issues
samples_by_state = reduced_state_data[ ,.N,res_state]

#create some summary tables to start investigating what data we have available and identify if there are any remaining issues
samples_by_county = reduced_state_data[ ,.N,county_fips_code]

#name samples_by_state, state and N + name samples_by_county, county and N
colnames(samples_by_state) = c("state","N")
colnames(samples_by_county) = c("fips","N")

#View samples_by_state and samples_by_county
View(samples_by_state)
View(samples_by_county)

#make some plots to get a better look at prelim data
plot_usmap(regions="states", data = samples_by_state, values = "N")
plot_usmap(regions="county", data = samples_by_county, values = "N")
#,data = samples_by_state_log, values = "sequences") #log scale is easier for now
#ggplot(monthly_samples, aes(date, sequences)) + geom_line() + scale_x_date(date_labels = "%b-%Y")

