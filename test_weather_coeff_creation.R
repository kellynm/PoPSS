## Testing weather coefficient creation function
start <-  2014
end <- 2017
time_step <- "weekly"
directory <- "G:/DaymetUS"
output_directory <- "G:/DaymetUS/test"
states_of_interest <- c('Maryland')
pest <- "ECFF"
prcp_index = 'YES'
prcp_method = "threshold" 
prcp_a0 = 0
prcp_a1 = 0 
prcp_a2 = 0 
prcp_a3 = 0 
prcp_x1mod = 0
prcp_x2mod = 0
prcp_x3mod = 0
prcp_thresh = 2.5
temp_index = 'YES' 
temp_method = "polynomial" 
temp_a0 = -0.066
temp_a1 = 0.056
temp_a2 = -0.0036
temp_a3 = -0.0003
temp_x1mod = 0
temp_x2mod = -15
temp_x3mod = -15
temp_thresh = 0

weather_coeff(directory, output_directory, start, end, time_step, states_of_interest, pest, 
                          prcp_index, prcp_method,  prcp_a0, prcp_a1, prcp_a2, prcp_a3, 
                          prcp_thresh, prcp_x1mod, prcp_x2mod, prcp_x3mod,
                          temp_index, temp_method, temp_a0, temp_a1, temp_a2, temp_a3, 
                          temp_thresh, temp_x1mod, temp_x2mod, temp_x3mod)
