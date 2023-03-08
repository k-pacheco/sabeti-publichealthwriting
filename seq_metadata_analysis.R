#file for filtering genbank seq metadata and making some exploratory plots
#set up script
#you'll likely need to download these packages if you are starting R for the first time
library(data.table) 
library(usmap) 
library(ggplot2)
`%notin%` = Negate(`%in%`) #this defines a new function to select items not in a list, really useful! 

#read data
setwd("/Users/tbrockfi/Desktop/projects/policy/pubhealth_seq_analysis") #change this path to be your specific file path
seq_metadata_full = read.csv("genbank_metadata_USA_2022_01_24.csv") #this will take a few seconds as the data is large

#convert to a data table to make subsetting faster
seq_metadata_full_table = data.table(seq_metadata_full)
seq_metadata_full_table$date = as.Date(seq_metadata_full_table$date, "%Y-%m-%d") #correct type to allow proper filtering

#view (part of) the file to explore potential issues
View(seq_metadata_full_table[1:100,])

#get a list of states to identify potential issues
state_list = levels(as.factor(seq_metadata_full_table$division))

#filter the original data based on our identified issues
complete_state_data = seq_metadata_full_table[virus=="ncov" & country=="USA" & host=="Homo sapiens" & 
                                                division %notin% c("American Samoa", "Guam","Missing","North America",
                                                                   "Northern Mariana Islands","Puerto Rico",
                                                                   "USA","Virgin Islands","Washington DC") & !is.na(date) &
                                                date >= "2020-01-01" & date < "2022-01-01",]


#create a new copy of the data selecting only columns we think we will need
reduced_state_data = complete_state_data[ ,c("genbank_accession","date","division","location",
                                            "age","sex","pango_lineage","submitting_lab","date_submitted",
                                            "date_updated")]

#ensure proper data types on special columns
reduced_state_data$date = as.Date(reduced_state_data$date, "%Y-%m-%d")
reduced_state_data$date_submitted = as.Date(reduced_state_data$date_submitted, "%Y-%m-%d")
reduced_state_data$date_updated = as.Date(reduced_state_data$date_updated, "%Y-%m-%d")
reduced_state_data$monthly_sample_dates = format(as.Date(reduced_state_data$date), "%Y-%m")

#create some summary tables to start investigating what data we have available and identify if there are any remaining issues
samples_by_state = reduced_state_data[ ,.N,division]
colnames(samples_by_state) = c("state","sequences")
samples_by_state_log = reduced_state_data[,log(.N),division]
colnames(samples_by_state_log) = c("state","sequences")
monthly_samples = reduced_state_data[,.N,monthly_sample_dates][order(monthly_sample_dates)]
colnames(monthly_samples) = c("date","sequences")
monthly_samples$date = as.Date(paste(monthly_samples$date, "-01", sep=""))

#make some plots to get a better look at prelim data
plot_usmap(regions="states",data = samples_by_state_log, values = "sequences") #log scale is easier for now
ggplot(monthly_samples, aes(date, sequences)) + geom_line() + scale_x_date(date_labels = "%b-%Y")
