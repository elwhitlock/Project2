# Elle Whitlock
# Static work for project 2 app
# ST 558


# packages needed
library("tidyverse")
library("hexbin")

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
num_summary_g <- mobile_data |>
  group_by(gender)|>
  summarize(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max)))
view(num_summary_g)

# numeric variable summary grouped by behavior class
num_summary_b <- mobile_data |>
  group_by(behavior_class)|>
  summarize(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max)))
view(num_summary_b)

# box plot of screen time grouped by operating system
ggplot(mobile_data, aes(x=op_system, y = screen_on, fill = op_system)) +
  geom_boxplot() +
  scale_fill_manual(
    name = "Operating System",
    values = c("iOS" = "blue", "Android" = "green")) +
  labs(title = "Boxplot of Screen on Time by Operating System", 
       x = "Operatng System", 
       y = "Screen on Time (hours/day)")

# box plot of app usage grouped by operating system
ggplot(mobile_data, aes(x=op_system, y = app_usage, fill = op_system)) +
  geom_boxplot() +
  scale_fill_manual(
    name = "Operating System",
    values = c("iOS" = "blue", "Android" = "green")) +
  labs(title = "Boxplot of App Usage Time by Operating System", 
       x = "Operatng System", 
       y = "App Usage Time (min/day)")


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

n_behavior_gender <- table(mobile_data$behavior_class, mobile_data$gender)
n_behavior_gender

# scatter plot number of apps vs app usage time, multivariate
ggplot(mobile_data, aes(x = num_apps, y = app_usage, color = gender)) +
  geom_jitter() +
  scale_color_manual(
    name = "Gender",
    values = c("Female" = "pink", "Male" = "lightblue")) +
  labs(title = "Number of Apps Installed vs App Usage Time", 
       x = "Number of Apps Installed", 
       y = "App Usage Time (min/day)")

# same plot as above with faceting used for device model, multivariate
ggplot(mobile_data, aes(x = num_apps, y = app_usage, color = gender)) +
  geom_jitter() +
  facet_wrap(~device_model)+
  scale_color_manual(
    name = "Gender",
    values = c("Female" = "pink", "Male" = "lightblue")) +
  labs(title = "Number of Apps Installed vs App Usage Time Faceted by Device Model", 
       x = "Number of Apps Installed", 
       y = "App Usage Time (min/day)")

# density plot age by operating system, multivariate
ggplot(mobile_data, aes(x = age, fill = op_system)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Operating System",
    values = c("iOS" = "blue", "Android" = "green")) +
  labs(title = "Density of age by Operating System",
       x = "Age", 
       y = "Density")


# bar chart of device model by behavior class
ggplot(mobile_data, aes(x = device_model, fill = behavior_class)) +
  geom_bar() +
  # I referenced outside documentation to create a gradient
  scale_fill_brewer(palette = "Reds", name = "Behavior Class") +
  labs(title = "Bar Chart of Device Model by User Behavior Class",
       x = "Device Model",
       y = "Count")

# violin plots of screen time, multivariate
ggplot(mobile_data, aes(x = behavior_class, y = age)) +
  geom_violin(fill = "lightyellow") +
  facet_wrap(~gender) +
  labs(title = "Distribution of Age by Behavior Class",
       x = "Behavior Class",
       y = "Age")

# same plot as above faceting on operating system as well, multivariate
ggplot(mobile_data, aes(x = behavior_class, y = age)) +
  geom_violin(fill = "lightyellow") +
  facet_wrap(op_system ~ gender) +
  labs(title = "Distribution of Age by Behavior Class",
       x = "Behavior Class",
       y = "Age")

# Hexbin NEW (did not cover in class) battery drain vs data usage
ggplot(mobile_data, aes(x = battery_drain, y = data_usage)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Hexbin Plot of Battery Drain vs Data Usage",
       x = "Battery Drain (mAh/day)",
       y = "Data Usage (MB/day)") 

