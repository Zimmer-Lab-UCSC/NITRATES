library(tidyverse)
library(ggplot2)
library(lubridate)
library(data.table)
library(readxl)

#input 'munged' data set
munge.R = read.csv("/Users/ahaynes/Downloads/QAQC'd_doc.csv")

#group the data by sample location, i.e. N1PD, N2_C15, etc.
by_sample_location = munge.R %>% group_by(sample_location)

#calculate the confidence intervals by using the mean of each sample location
#and the standard error of the sample location
conf_calc = by_sample_location %>% 
  summarise(
    mean_conc_ppm = mean(concentration_ppm),
    se = sd(concentration_ppm, na.rm = TRUE)/sqrt(length(na.omit(concentration_ppm)))
    )

#calculate the upper and lower confidence interval limit
#https://www.youtube.com/watch?v=gdCvrpPZyRQ&ab_channel=MatthewE.Clapham
conf_calc$upper_conf_int = conf_calc$mean_conc_ppm + (1.96 * conf_calc$se)
conf_calc$lower_conf_int = conf_calc$mean_conc_ppm - (1.96 * conf_calc$se)

#merge the calculations to the data grouped by confidence interval by pairing
#your calculations to the to the sample locations on the 'by_sample_location'
#dataframe
final = list(by_sample_location, conf_calc)
final = final %>% reduce(full_join, by='sample_location')

#create a column for placing flagging values in
final$"Flagged_Value_-3" = NA
final$"Flagged_Value_-2" = NA
final$"Flagged_Value_-1" = NA
final$"Flagged_Value_0" = NA
final$"Flagged_Value_1" = NA
#final$"Flagged_Value_2" = NA
#final$"Flagged_Value_3" = NA

#for "row i" in all the rows in our 'final' data frame...
for (i in 1:nrow(final)) {
  #if the upper and lower confidence interval in 'row i' is NA, then flag it
  #with a value 1, which is suspect data.
  if (is.na(final$upper_conf_int[i] && final$lower_conf_int[i])){
    final$`Flagged_Value_1` [i]= '1'
    #if the concentration ppm is missing, then assign a -1 value to the 
    #`Flagged_Value_-1` column
  } else if(is.na(final$concentration_ppm[i])){
    final$`Flagged_Value_-1`[i]= '-1'
    #if the concentration ppm is greater than the upper confidence level value 
    #OR if the concentration ppm is less than the lower confidence level value,
    #then flag it with a value -2.
  } else if(final$concentration_ppm[i] > final$upper_conf_int[i] | 
            final$concentration_ppm[i] < final$lower_conf_int[i]){
    final$`Flagged_Value_-2`[i]= '-2'
    #if the concentration ppm is greater than the upper detection limit 
    #OR if the concentration ppm is less than the lower detection limit,
    #then flag it with a value -3.
  # } else if(final$concentration_ppm[i] > final$UDL[i] | 
  #           final$concentration_ppm[i] < final$LDL[i]){
  #   final$`Flagged_Value_-3`[i]= '-3'
    #otherwise, assign a 0 to the Flagged_Value_0. 
  } else {
     final$`Flagged_Value_0`[i]= '0'
  #end of if statement
  }
#end of for loop
}
#for the `final` data frame, united all the `Flagged_Value_# columns and separate
#them with a ; and remove any NA values and remove the column which was united after
#the value has been moved to the `flagged_values`column. 

final = final %>% 
    unite(flagged_values, c(`Flagged_Value_1`, `Flagged_Value_0`, `Flagged_Value_-1`, `Flagged_Value_-2`,
                          `Flagged_Value_-3`), sep = " ; ", na.rm = TRUE, remove = TRUE)
