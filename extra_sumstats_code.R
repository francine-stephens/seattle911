## Service Time (Or Response Time Code) Summary Statistics
focal_cases %>%
  group_by(case_type) %>%
  summarize(
    mean_ST = round(mean(Total_Service_Time_Seconds_SUM, na.rm=T),
                    digits = 0),
    sd_ST = round(sd(Total_Service_Time_Seconds_SUM, na.rm=T),
                  digits=0),
    med_ST = median(Total_Service_Time_Seconds_SUM),
    min_ST = min(Total_Service_Time_Seconds_SUM),
    max_ST = max(Total_Service_Time_Seconds_SUM)) %>%
  mutate_if(is.numeric, ~ seconds_to_period(.x))