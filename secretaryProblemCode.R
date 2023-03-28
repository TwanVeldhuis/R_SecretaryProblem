number_of_simulations <- 10000
number_of_applicants <- 100
look_group_percentages <- c(0.37)

#### LIST OF VECTORS ####
average_application_selection <- c()
all_employees <- c()
all_max_values <- c()
all_sums <- c()
all_mean <- c()
all_max <- c()
all_min <- c()
#### END OF LIST ####

for (look_percentage in look_group_percentages) {
  best_one_percent_selected <- 0
  difference_from_best_one_percent <- c()
  for (i in 1:number_of_simulations) {
    #### THIS PART IS UNKNOWN TO THE INTERVIEWER ####
    all_applicant_qualities <- sample(c(1:1000), number_of_applicants)
    max_of_applicants <- max(all_applicant_qualities)
    min_of_applicants <- min(all_applicant_qualities)
    mean_of_applicants <- mean(all_applicant_qualities)
    one_percent_of_applicants <- quantile(all_applicant_qualities, prob=0.9)
    #### END OF THE UNKNOWN ####
    
    #### LOOK PHASE ####
    best_applicant_of_look <- -10
    size_of_look_group <- number_of_applicants*look_percentage
    
    # get the best possible score from the look group
    best_applicant_of_look <- max(all_applicant_qualities[1:size_of_look_group])
    worst_applicant_of_look <- min(all_applicant_qualities[1:size_of_look_group])
    #### END OF LOOK PHASE ####
    
    #### LEAP PHASE ####
    new_employee <- -10
    
    # select the new employee based on the look group
    for(leap_index in size_of_look_group:number_of_applicants) {
      if (all_applicant_qualities[leap_index] > best_applicant_of_look) {
        new_employee <- all_applicant_qualities[leap_index]
        break
      } else {
        new_employee <- all_applicant_qualities[leap_index]
      }
    }
    #### END OF LEAP PHASE ####
    
    if (new_employee >= one_percent_of_applicants) {
      best_one_percent_selected <- best_one_percent_selected + 1
    }
    
    if ((one_percent_of_applicants - new_employee) > 0){
      difference_from_best_one_percent <- c(difference_from_best_one_percent, (one_percent_of_applicants - new_employee))
    } else {
      difference_from_best_one_percent <- c(difference_from_best_one_percent, 0)
    }
    
    all_max_values <- c(all_max_values, max_of_applicants)
    all_employees <- c(all_employees, new_employee)
  }
  
  average_application_selection <- c(average_application_selection, best_one_percent_selected / number_of_simulations)
  all_sums <- c(all_sums, sum(difference_from_best_one_percent))
  all_mean <- c(all_mean, mean(difference_from_best_one_percent))
  all_max <- c(all_max, max(difference_from_best_one_percent))
  all_min <- c(all_min, min(difference_from_best_one_percent))
}

average_application_selection
all_sums
all_mean
all_max
all_min

plot(look_group_percentages*100, average_application_selection*100, type="l", xlab="Percentage of population used in Look phase", ylab="Percentage of successfull selecting top 1 percent", main="Obtaining optimal Look group percentage")
sum(all_max_values-all_employees)/length(all_max_values)
     