#I learned about the tidyverse packages through school, and ever since
#I've used two of the packages, dplyr and tidyr for data manipulation and data.table 
#for reading data
require(dplyr)
require(tidyr)
require(data.table)
require(ggplot2)
require(stats)
require(broom)

setwd("/Users/chris_000/Documents/Bazean/")

#I normally use fread from data.table or read_table from readr, but neither worked
#due to the encoding of the file so I had to use read.table, as it's the only
#method I know that can  read "UCS-2LE" files.

Production= read.table("EagleFord_Production.txt", header = TRUE, fill = TRUE, fileEncoding = "UCS-2LE", colClasses = c("date" = "character", "api" = "character"))
Production$date = as.Date(Production$date, format="%m/%d/%Y")

#After the update email about the meta_data, I left joined by api using dplyr into the previous data which I call A.
#I joined everything I found could be remotely useful. I then filtered out evey value without
#a company name associated with it. That's the dataset that I will be working with.

meta_data = read.table("EagleFord_Well_Meta.txt", header = TRUE, fill = TRUE, fileEncoding = "UCS-2LE", sep = "\t", colClasses = c("api" = "character", "operator_name" = "character", "well_name" = "character")) %>%
  select(completion_date, cum_12_oil, cum_6_oil, cum_oil, ipmo_oil,max_prod_date, operator_name, prod_months, well_name, api)

meta_data$completion_date = as.Date(meta_data$completion_date, format="%m/%d/%Y") 
  
final_table = left_join(Production, meta_data, by = "api") %>%
  filter(!is.na(operator_name) & operator_name != "")

#To remove outliers, I simply used the common method using inter-quartile ranges.
#From the summary I get the first quartile of oil is at 548 and third at 3529. 

first_quartile_oil = 614
third_quartile_oil = 3503

IQR_oil_lower = first_quartile_oil-1.5*(third_quartile_oil - first_quartile_oil)
IQR_oil_upper = third_quartile_oil+1.5*(third_quartile_oil - first_quartile_oil)

#Here I use dplyr's filter function to remove these values.
#It removes about 20,000 values, which is about 7% of the data....not ideal.
final_table = filter(final_table,volume_oil_formation_bbls >=IQR_oil_lower & volume_oil_formation_bbls <=IQR_oil_upper)
  

#After looking at the data, it all of the variables are clearly stated
#except for API and Index. After checking a few examples, it seems all of the oil
#values from the EagleFord_Well_Meta.txt aren't related in an obvious way to
# volume_oil_formation_bbls, which I am going to assume is the oil production 
#variable we're supposed to track since it's in the original dataset.


#Using dplyr's group_by function, I group by the company name and date to 
#sum over the oil and gas production per company by date

historical_aggregate_oil = group_by(final_table, operator_name,date) %>%
  summarize(oil_volume = sum(volume_oil_formation_bbls)) %>%
  mutate(aggregate_oil_volume = cumsum(oil_volume)) %>%
  select(operator_name, date, aggregate_oil_volume) 

write.table(historical_aggregate_oil, "aggregate_over_time.txt", sep="\t")






#We can use a smilar grouping and simply use an average at each date.
#This works if we assume that 1 month of oil production is associated with 
#just one well. SO I am calculating the average for active wells, not total wells.

historical_average_oil = group_by(final_table,operator_name,date) %>%
  summarize(average_oil_volume = mean(volume_oil_formation_bbls)) %>%
  select(operator_name, date, average_oil_volume)

write.table(historical_average_oil, "average_over_time.txt", sep="\t")







#To evaluate future production, first I summarize by production over well from the previous problem.
#I am also eliminating companies with less than 10 dates to do regression
#I also convert date into numeric to do regression easier.
predict_average_oil = group_by(historical_average_oil, operator_name) %>%
  filter(n()>=10) %>%
  do(linear_regression =lm(as.numeric(date) ~ average_oil_volume, data = .)) 

predict_average_oil = tidy(predict_average_oil, linear_regression)

#Since there's no specified date to see which companies will be the most productive,
#I will just calculate the highest expected value in about five years. The top ten companies
#are listed below. The dates are in numeric, not date form. They need to be converted back
#to do any predictions.

ordered_companies = group_by(predict_average_oil, operator_name) %>%
  spread(term,estimate, fill = 0)
  colnames(ordered_companies)[5] = "intercept"
  
ordered_companies =summarize(ordered_companies, prediction = as.numeric(as.Date("2022/01/01"))*sum(average_oil_volume)+ sum(intercept)) %>%
  arrange(-prediction) %>%
  select(operator_name)

write.table(head(ordered_companies, n=10), "most_efficient_in_five_years.txt", sep="\t")

#If I had more time, I'd rather incorporate a time series analysis. Kenilworth pretty clearly isn't a top
#ten company if you isolate their average production per well! Many are in triple digits, but
#Kenilworth is only in single digits.
#Additionally, I would rather use a different method to reduce outliers, because I find the 
#1.5*IQR method to be pretty rudimentary.