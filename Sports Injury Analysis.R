
##SPORTS INJURY ANALYSIS


# Loading the necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Reading the dataset
data <- read.csv("C:/Users/Asus/Desktop/Sports Dataset.csv")

###Data_Cleaning
# Merge columns for Body_Part, Diagnosis, and Products
data <- data %>%
  mutate(
    # Combine Body_Part_1 and Body_Part_2, removing duplicates
    Body_Part = sapply(
      strsplit(paste(Body_Part_1, Body_Part_2, sep = ", "), ", "), 
      function(x) paste(unique(trimws(x)), collapse = ", ")
    ),
    
    # Combine Diagnosis_1 and Diagnosis_2, removing duplicates
    Diagnosis = sapply(
      strsplit(paste(Diagnosis_1, Diagnosis_2, sep = ", "), ", "), 
      function(x) paste(unique(trimws(x)), collapse = ", ")
    ),
    
    # Combine Product_1, Product_2, and Product_3, removing duplicates
    Products = sapply(
      strsplit(paste(Product_1, Product_2, Product_3, sep = ", "), ", "), 
      function(x) paste(unique(trimws(x[x != "0"])), collapse = ", ")  # Exclude "0"
    )
  ) %>%
  # Drop old columns
  select(-Body_Part_1, -Body_Part_2, -Diagnosis_1, -Diagnosis_2, -Product_1, -Product_2, -Product_3)

# View the resulting dataset
print(head(data))





####Find the Top Injuries by Each Product
# Expand the Products column into multiple rows for analysis
products_count <- data %>%
  separate_rows(Products, sep = ", ") %>%  
  filter(!is.na(Products) & Products != "" & Products != "0") %>%  
  group_by(Products) %>%                  
  summarise(Count = n(), .groups = "drop") %>%  
  arrange(desc(Count))                    

# View the results
print(products_count)     

# Create a barplot with dark-to-light color scheme and counts displayed
library(ggplot2)
ggplot(products_count, aes(x = reorder(Products, -Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +  # Add text labels for counts
  labs(
    title = "Injury Counts by Different Sports",
    x = "Products",
    y = "Count"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Dark-to-light gradient
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))




####Most Common Body Part Injuries by Each Product
# Process the data 
body_parts_by_product <- data %>%
  separate_rows(Products, sep = ", ") %>%        
  separate_rows(Body_Part, sep = ", ") %>%      
  filter(!is.na(Products) & Products != "" & Products != "0") %>%  
  group_by(Products, Body_Part) %>%              
  summarise(Count = n(), .groups = "drop") %>%   
  arrange(desc(Count))

# Select only the highest count body part injury for each product
most_common_body_part_by_product <- body_parts_by_product %>%
  group_by(Products) %>% 
  slice_max(order_by = Count, n = 1, with_ties = FALSE)  # Keep the highest Count only

# View the results
print(most_common_body_part_by_product)

##Create a treemap visual
ggplot(most_common_body_part_by_product, aes(
  area = Count,
  fill = Body_Part, 
  label = paste(Products, "\n", Count)
)) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "bold",
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  scale_fill_manual(values = c(
    "Head" = "#1f77b4",
    "Face" = "#ff7f0e",
    "Neck" = "#2ca02c",
    "Ankle" = "#d62728",
    "Shoulder" = "#9467bd",
    "Finger" = "#8c564b",
    "Hand" = "#e377c2",
    "Others" = "#7f7f7f"
    # Add other body parts as needed with unique colors
  )) +
  labs(
    title = "Most Common Body Part Injury by Differernt Sports",
    fill = "Body_Part"
  ) +
  theme_minimal()




####Most Common Diagnosis by Each Product
# Process the data to find the top diagnosis for each product
top_diagnosis_by_product <- data %>%
  separate_rows(Products, sep = ", ") %>%         # Expand Products into multiple rows
  separate_rows(Diagnosis, sep = ", ") %>%       # Expand Diagnosis into multiple rows
  filter(!is.na(Products) & Products != "" & Products != "0") %>%  # Remove invalid products
  group_by(Products, Diagnosis) %>%              # Group by Products and Diagnosis
  summarise(Count = n(), .groups = "drop") %>%   # Count occurrences
  arrange(desc(Count)) %>%                       # Sort by count
  group_by(Products) %>% 
  slice_max(order_by = Count, n = 1, with_ties = FALSE)  # Keep only the top diagnosis for each product

# View the result
print(top_diagnosis_by_product)

##Create a treemap visual
ggplot(top_diagnosis_by_product, aes(
  area = Count,
  fill = Diagnosis,  # Fill based on 'Diagnosis'
  label = paste(Products, "\n", Count)
)) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "bold",
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  scale_fill_manual(values = c(
    "Contusion" = "#1f77b4",
    "Fracture" = "#ff7f0e",
    "Internal injury" = "#2ca02c",
    "Others" = "#d62728"
  )) +
  labs(
    title = "Most Common Diagnosis by Sports",
    fill = "Diagnosis"
  ) +
  theme_minimal()




####Most Common Diagnosis for Each body part injury
# Separate Body_Part into individual rows and count diagnoses
diagnosis_by_body_part <- data %>%
  separate_rows(Body_Part, sep = ", ") %>%  
  separate_rows(Diagnosis, sep = ", ") %>% 
  filter(!is.na(Body_Part) & Body_Part != "") %>% 
  group_by(Body_Part, Diagnosis) %>%             
  summarise(Count = n(), .groups = "drop") %>%   
  arrange(desc(Count))       

# Filter to keep the top diagnosis for each body part
top_diagnosis_by_body_part <- diagnosis_by_body_part %>%
  group_by(Body_Part) %>% 
  slice_max(order_by = Count, n = 1, with_ties = FALSE)  # Keep only the top diagnosis

# View the results
print(top_diagnosis_by_body_part)

# Create the bar chart with count labels
ggplot(top_diagnosis_by_body_part, aes(
  x = reorder(Body_Part, -Count),  # Order by count in descending order
  y = Count,
  fill = Diagnosis                # Fill color based on diagnosis
)) +
  geom_bar(stat = "identity", width = 0.8) +      # Bar plot
  geom_text(aes(label = Count),                   # Add count labels
            vjust = -0.3,                         # Position labels above bars
            size = 2) +                           # Adjust text size
  scale_fill_brewer(palette = "Set2") +           # Use a distinct color palette
  labs(
    title = "Top Diagnosis for Each Body Part",
    x = "Body Part",
    y = "Count",
    fill = "Diagnosis"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "right"                           # Place legend on the right
  )




#### Calculate proportions of injuries by gender for each product
injury_proportion_by_gender <- data %>%
  separate_rows(Products, sep = ", ") %>%        # Expand Products into multiple rows
  filter(!is.na(Products) & Products != "" & Products != "0") %>%  # Remove invalid products
  group_by(Products, Gender) %>%                # Group by Products and Gender
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(Products) %>%                        # Group by Products again
  mutate(Proportion = Count / sum(Count)) %>%   # Calculate proportion within each product
  select(Products, Gender, Proportion) %>%      # Select relevant columns
  pivot_wider(names_from = Gender, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  rename(Male = Male, Female = Female)          # Rename columns for clarity

# View the results
print(injury_proportion_by_gender)

# Convert data to long format for plotting
stacked_data <- injury_proportion_by_gender %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "Gender", values_to = "Proportion")

ggplot(stacked_data, aes(
  x = reorder(Products, Proportion),
  y = Proportion,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Female" = "#ff7f0e", "Male" = "#1f77b4")) +
  labs(
    title = "Male Vs Female Injury Proportions by Product",
    x = "Products",
    y = "Proportion",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





####Calculate proportions of injuries by gender for each body part
body_part_injury_proportion_by_gender <- data %>%
  separate_rows(Body_Part, sep = ", ") %>%        # Expand Body_Part into multiple rows
  filter(!is.na(Body_Part) & Body_Part != "") %>% # Remove invalid body parts
  group_by(Body_Part, Gender) %>%                # Group by Body_Part and Gender
  summarise(Count = n(), .groups = "drop") %>%   # Count occurrences
  group_by(Body_Part) %>%                        # Group by Body_Part again
  mutate(Proportion = Count / sum(Count)) %>%    # Calculate proportion within each body part
  pivot_wider(names_from = Gender, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  group_by(Body_Part) %>%                        # Ensure Body_Part is unique
  summarise(
    Male = sum(Male, na.rm = TRUE),
    Female = sum(Female, na.rm = TRUE)
  )

# View the results
print(body_part_injury_proportion_by_gender)

# Convert data to long format for plotting
stacked_gender_data <- body_part_injury_proportion_by_gender %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "Gender", values_to = "Proportion")

# Create the stacked bar chart
ggplot(stacked_gender_data, aes(
  x = reorder(Body_Part, -Proportion),
  y = Proportion,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar with proportions
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +  # Custom colors
  labs(
    title = "Proportion of Injuries by Gender for Each Body Part",
    x = "Body Part",
    y = "Proportion",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )




####Calculate proportions of injuries by race for each product 
injury_proportion_by_race <- data %>%
  separate_rows(Products, sep = ", ") %>%        # Expand Products into multiple rows
  filter(!is.na(Products) & Products != "" & Products != "0") %>%  # Remove invalid products
  group_by(Products, Race) %>%                  # Group by Products and Race
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(Products) %>%                        # Group by Products again
  mutate(Proportion = Count / sum(Count)) %>%   # Calculate proportion within each product
  select(Products, Race, Proportion) %>%        # Select relevant columns
  pivot_wider(names_from = Race, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  rename_with(~ gsub(" ", "_", .))              # Replace spaces in column names for clarity

# View the results
print(injury_proportion_by_race)

# Convert data to long format for plotting
stacked_race_data <- injury_proportion_by_race %>%
  pivot_longer(cols = -Products, names_to = "Race", values_to = "Proportion")

# Create the stacked bar chart
ggplot(stacked_race_data, aes(
  x = reorder(Products, -Proportion),
  y = Proportion,
  fill = Race
)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar with proportions
  scale_fill_brewer(palette = "Set3") +            # Use a distinct color palette
  labs(
    title = "Proportion of Injuries by Race for Each Product",
    x = "Products",
    y = "Proportion",
    fill = "Race"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )





####Calculate proportions of injuries by race for each body part
body_part_injury_proportion_by_race <- data %>%
  separate_rows(Body_Part, sep = ", ") %>%        # Expand Body_Part into multiple rows
  filter(!is.na(Body_Part) & Body_Part != "") %>% # Remove invalid body parts
  group_by(Body_Part, Race) %>%                  # Group by Body_Part and Race
  summarise(Count = n(), .groups = "drop") %>%   # Count occurrences
  group_by(Body_Part) %>%                        # Group by Body_Part again
  mutate(Proportion = Count / sum(Count)) %>%    # Calculate proportion within each body part
  pivot_wider(names_from = Race, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  group_by(Body_Part) %>%                        # Ensure Body_Part is unique
  summarise(across(everything(), sum, na.rm = TRUE))  # Combine rows for same Body_Part

# View the results
print(body_part_injury_proportion_by_race)


# Drop the 'Count' column explicitly
body_part_injury_proportion_by_race <- body_part_injury_proportion_by_race %>%
  dplyr::select(-Count)

# Convert data to long format for plotting
stacked_race_body_part_data <- body_part_injury_proportion_by_race %>%
  pivot_longer(cols = -Body_Part, names_to = "Race", values_to = "Proportion")

# Create the stacked bar chart
ggplot(stacked_race_body_part_data, aes(
  x = reorder(Body_Part, -Proportion),  # Sort body parts by total proportion
  y = Proportion,
  fill = Race
)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar with proportions
  scale_fill_brewer(palette = "Set3") +            # Use a visually distinct color palette
  labs(
    title = "Proportion of Injuries by Race for Each Body Part",
    x = "Body Part",
    y = "Proportion",
    fill = "Race"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )




####Calculate proportions of injuries by age group for each product 
# Define age groups with the new categories
library(dplyr)
library(tidyr)

data <- data %>%
  mutate(Age_Group = case_when(
    Age < 6 ~ "Under_6",
    Age >= 6 & Age <= 12 ~ "7_to_12",
    Age >= 13 & Age <= 17 ~ "13_to_17",
    Age >= 18 & Age < 30 ~ "18_to_29",
    Age >= 30 & Age < 45 ~ "30_to_44",
    Age >= 45 & Age < 60 ~ "45_to_59",
    Age >= 60 ~ "60_and_above",
    TRUE ~ "Unknown"
  ))

injury_proportion_by_age <- data %>%
  separate_rows(Products, sep = ", ") %>%        # Expand Products into multiple rows
  filter(!is.na(Products) & Products != "" & Products != "0") %>%  # Remove invalid products
  group_by(Products, Age_Group) %>%             # Group by Products and Age_Group
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(Products) %>%                        # Group by Products again
  mutate(Proportion = Count / sum(Count)) %>%   # Calculate proportion within each product
  dplyr::select(Products, Age_Group, Proportion) %>%   # Explicitly use dplyr::select
  pivot_wider(names_from = Age_Group, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  rename_with(~ gsub(" ", "_", .))              # Replace spaces in column names for clarity

# View the resulting table
print(injury_proportion_by_age)

##Visualization
library(ggplot2)

# Convert the data to a long format for ggplot
long_data <- injury_proportion_by_age %>%
  pivot_longer(
    cols = starts_with("Under_6"):ends_with("60_and_above"),
    names_to = "Age_Group",
    values_to = "Proportion"
  )
# Create the stacked bar chart
ggplot(long_data, aes(x = Products, y = Proportion, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  labs(
    title = "Proportion of Injuries by Age Group and Product",
    x = "Products",
    y = "Proportion",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis labels for readability
    plot.title = element_text(hjust = 0.5)             # Center the title
  ) +
  scale_y_continuous(labels = scales::percent)





####Calculate proportions of injuries by age group for each body part
body_part_injury_proportion_by_age <- data %>%
  separate_rows(Body_Part, sep = ", ") %>%        # Expand Body_Part into multiple rows
  filter(!is.na(Body_Part) & Body_Part != "") %>% # Remove invalid body parts
  group_by(Body_Part, Age_Group) %>%             # Group by Body_Part and Age_Group
  summarise(Count = n(), .groups = "drop") %>%   # Count occurrences
  group_by(Body_Part) %>%                        # Group by Body_Part again
  mutate(Proportion = Count / sum(Count)) %>%    # Calculate proportion within each body part
  pivot_wider(names_from = Age_Group, values_from = Proportion, values_fill = 0) %>%  # Reshape to wide format
  group_by(Body_Part) %>%                        # Ensure Body_Part is unique
  summarise(across(everything(), sum, na.rm = TRUE))  # Combine rows for same Body_Part

colnames(body_part_injury_proportion_by_age)

# Reorder columns from youngest to oldest
body_part_injury_proportion_by_age <- body_part_injury_proportion_by_age %>%
  select(Body_Part, Count, Under_6, `7_to_12`, `13_to_17`, `18_to_29`, `30_to_44`, `45_to_59`, `60_and_above`)

# View the results
print(body_part_injury_proportion_by_age)





###Assess injury severity based on diagnosis or body parts affected
#Categorize severity based on diagnosis
data <- data %>%
  separate_rows(Diagnosis, sep = ",\\s*") %>% # Splits by comma and optional space
  
  # Categorize Severity
  mutate(Severity = case_when(
    Diagnosis%in% c("Amputation", "Scald burn", "Concussion", "Fracture", "Internal injury", "Hemorrhage",
                          "Anoxia", "Poisoning", "Radiation",
                          "Avulsion", "Crushing", "Nerve damage") ~ "Severe",
    Diagnosis%in% c("Laceration", "Strain & Sprain", "Dislocation", "Contusion", "Foreign body",
                          "Puncture", "Hematoma", "Dental injury") ~ "Moderate",
    Diagnosis%in% c("Derma conjunct", "Others") ~ "Minor",
    TRUE ~ "Uncategorized" # For any diagnosis not listed
  ))

# Analyze severity proportions by Gender
severity_by_gender <- data %>%
  group_by(Gender, Severity) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = Count / sum(Count)) %>%
  select(Gender, Severity, Proportion) %>%
  pivot_wider(names_from = Gender, values_from = Proportion, values_fill = 0)

# View results
print(severity_by_gender)

# Convert severity proportions into long format for visualization
severity_by_gender_long <- severity_by_gender %>%
  pivot_longer(cols = -Severity, names_to = "Gender", values_to = "Proportion")

# Create the stacked bar chart
ggplot(severity_by_gender_long, aes(
  x = Severity,
  y = Proportion,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bar showing proportions
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +  # Custom gender colors
  labs(
    title = "Proportion of Injury Severity by Gender",
    x = "Severity Level",
    y = "Proportion",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels if needed
  )




####Multi bodypart injury proportion based on gender
library(stringr)

# Count body parts per case
data <- data %>%
  mutate(Body_Part_Count = str_count(Body_Part, ",") + 1)

# Analyze multi-body-part injuries by Gender and calculate proportions
multi_body_part_proportion <- data %>%
  mutate(Multi_Body_Part = Body_Part_Count >= 2) %>% # Identify multi-body-part cases
  group_by(Gender, Multi_Body_Part) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(Multi_Body_Part == TRUE) %>% # Keep only multi-body-part injuries
  select(Gender, Proportion)

# View results
print(multi_body_part_proportion)

# Create a pie chart
ggplot(multi_body_part_proportion, aes(
  x = "",
  y = Proportion,
  fill = Gender
)) +
  geom_bar(stat = "identity", width = 1) +  # Bar chart with pie structure
  coord_polar(theta = "y") +               # Convert bar chart to pie chart
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +  # Custom colors
  labs(
    title = "Proportion of Multi-Body-Part Injuries by Gender",
    fill = "Gender"
  ) +
  theme_void()  # Remove unnecessary chart elements


# Analyze alcohol and drug influence on severity
alcohol_drug_analysis <- data %>%
  filter(Alcohol == 1 | Drug == 1) %>%
  group_by(Severity) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = Count / sum(Count))

# View results
print(alcohol_drug_analysis)

# Create a pie chart to display severity proportions
ggplot(alcohol_drug_analysis, aes(
  x = "",
  y = Proportion,
  fill = Severity
)) +
  geom_bar(stat = "identity", width = 1) +           # Bar chart with pie structure
  coord_polar(theta = "y") +                        # Convert bar chart to pie chart
  scale_fill_brewer(palette = "Blues") +             # Use a visually distinct color palette
  labs(
    title = "Proportion of Injury Severity Influenced by Alcohol and Drugs",
    fill = "Severity"
  ) +
  theme_void()  # Remove unnecessary chart elements




####Find out most risky product
# Drop blank or NA product rows
cleaned_data <- data %>%
  filter(!is.na(Products) & Products != "")

# Pivot Severity into separate columns, calculating proportions dynamically
risky_product <- cleaned_data %>%
  group_by(Products, Severity) %>%
  summarise(Count = n(), .groups = "drop") %>%   # Aggregate counts
  group_by(Products) %>%                         # Group by Products to calculate proportions
  mutate(Proportion = Count / sum(Count)) %>%    # Calculate proportions
  pivot_wider(
    names_from = Severity, 
    values_from = c(Count, Proportion), 
    values_fill = 0
  ) %>%
  arrange(desc(Proportion_Severe))               # Sort by highest proportion of severe injuries

# View the sorted data
print(risky_product)

library(ggplot2)

ggplot(risky_product, aes(x = reorder(Products, -Proportion_Severe), y = Proportion_Severe)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +  # Red bars to indicate severity
  labs(
    title = "Most Risky Sports by Proportion of Severe Injuries",
    x = "Products",
    y = "Proportion of Severe Injuries"
  ) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentage
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)             # Center align the title
  )





####Analyze combined effects of race and gender on injuries.
race_gender_analysis <- data %>%
  group_by(Race, Gender) %>%
  summarise(Count = n(), .groups = "drop") %>%   # Aggregate counts
  pivot_wider(
    names_from = Gender, 
    values_from = Count, 
    values_fill = 0
  ) %>% 
  mutate(
    Male = Male / sum(Male + Female, na.rm = TRUE),
    Female = Female / sum(Male + Female, na.rm = TRUE)
  ) %>%
  select(Race, Male, Female)

# View the analysis
print(race_gender_analysis)

# Prepare data for grouped bar chart
race_gender_long <- race_gender_analysis %>%
  pivot_longer(
    cols = c("Male", "Female"),
    names_to = "Gender",
    values_to = "Proportion"
  )

# Create a grouped bar chart
ggplot(race_gender_long, aes(
  x = Race,
  y = Proportion,
  fill = Gender
)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bars
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +
  labs(
    title = "Proportion of Injuries by Race and Gender",
    x = "Race",
    y = "Proportion",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )
