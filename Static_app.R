# Elle Whitlock
# Static work for project 2 app
# ST 557


# packages needed
library("tidyverse")

# read in data (mobile device usage)
# rename column names
# mutate character variables to factors 
mobile_data <- read_csv("user_behavior_dataset.csv") |>
  rename(
         user_id = `User ID`,
         device_model = `Device Model`,
         op_system = `Operating System`,
         app_usage = `App Usage Time (min/day)`,
         screen_on = `Screen On Time (hours/day)`,
         battery_drain = `Battery Drain (mAh/day)`,
         num_apps = `Number of Apps Installed`,
         data_usage = `Data Usage (MB/day)`,
         age = Age,
         gender = Gender,
         behavior_class = `User Behavior Class`
  ) |>
  mutate(
        device_model = as.factor(device_model),
        op_system = as.factor(op_system),
        gender = as.factor(gender),
        behavior_class = factor(behavior_class,
        levels = c(1, 2, 3, 4, 5),
        # based on source of data ranges from light to extreme
        labels = c("Light", "Light to Moderate", "Moderate", "Moderate to Extreme", "Extreme")
    )
  )

# validate our data read in as desired
glimpse(mobile_data)
colSums(is.na(mobile_data))

# numeric variable summary grouped by gender
num_summary <- mobile_data |>
  group_by(gender)|>
  summarize(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max)))
num_summary

# box plot of screen time grouped by operating system
ggplot(mobile_data, aes(x=op_system, y = screen_on, fill = op_system)) +
  geom_boxplot() +
  labs(title = "Boxplot of Screen on Time by Operating System", x = "Operatng System", y = "Screen on Time (hours/day)")

# box plot of app usage grouped by operating system
ggplot(mobile_data, aes(x=op_system, y = app_usage, fill = op_system)) +
  geom_boxplot() +
  labs(title = "Boxplot of App Usage Time by Operating System", x = "Operatng System", y = "App Usage Time (min/day)")


# categorical counts via one way tables
n_gender <- table(mobile_data$gender)
n_gender

n_behavior <- table(mobile_data$behavior_class)
n_behavior

n_system <- table(mobile_data$op_system)
n_system

# two way tables
n_system_gender <- table(mobile_data$op_system, mobile_data$gender)
n_system_gender

