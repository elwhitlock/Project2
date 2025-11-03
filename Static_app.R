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
        labels = c("Light", "Light to Moderate", "Moderate", "Moderate to Extreme", "Extreme")
    )
  )


