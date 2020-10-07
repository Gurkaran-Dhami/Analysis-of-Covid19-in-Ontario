#Simply reading in the data file, and creating a data frame with the Age group and Outcome 
#data points
data <- read.csv("conposcovidloc.csv", header = TRUE)
case_location <- data$Reporting_PHU_City

#Here im making a frequency table with cities, and number of cases
num_occur_table <- table(case_location)
print(num_occur_table)

#finding some summary statistics 
max_cases <- max(num_occur_table)
mean_cases <- mean(num_occur_table)
min_cases <- min(num_occur_table)
sd_cases <- sd(num_occur_table)
summary_statistics <- c(max_cases, min_cases, mean_cases, sd_cases)

print(summary_statistics)
#In Ontario we see Toronto is the city most affected by COVID, which makes sense since Toronto is the most populated 
#New Liskeard is the city least affected with a total of 16 cases

#Now lets find out which age group was the most affected in terms of cases by COVID
length_list <- length(data$Age_Group)
age_group_list_raw <- vector(mode = "list", length = desired_length)
index <- 1
for (values in data$Age_Group) {
  age_group_list_raw[index] <- values
  index = index + 1
}
print(age_group_list_raw)

#Cleaning the data, by turning the strings into integers
age_group_list <- vector(mode = "list", length = length_list)
counter <- 1
for (values in age_group_list_raw) {
  if (values == "20s" | values == "20") {
    age_group_list[counter] <- 20
    counter = counter + 1
  }
  if (values == "30s" | values == "30") {
    age_group_list[counter] <- 30
    counter = counter + 1
  }
  if (values == "40s" | values == "40") {
    age_group_list[counter] <- 40
    counter = counter + 1
  }
  if (values == "50s" | values == "50") {
    age_group_list[counter] <- 50
    counter = counter + 1
  }
  if (values == "60s" | values == "60") {
    age_group_list[counter] <- 60
    counter = counter + 1
  }
  if (values == "70s" | values == "70") {
    age_group_list[counter] <- 70
    counter = counter + 1
  }
  if (values == "80s" | values == "80") {
    age_group_list[counter] <- 80
    counter = counter + 1
  }
  if (values == "90s" | values == "90") {
    age_group_list[counter] <- 90
    counter = counter + 1
  }
  if (values == "<20s") {
    age_group_list[counter] <- 20
    counter = counter + 1
  }
}
vec_age <- unlist(age_group_list)
hist(vec_age)
#we see here that though the number of deaths for older patients is higher, the younger adults have a higher frequency of cases




