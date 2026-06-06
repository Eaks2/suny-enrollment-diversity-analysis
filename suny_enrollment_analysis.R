#########
library(tidyverse)
library(car)
install.packages("skimr")
library(skimr)

df_original <- read.csv("State_University_of_New_York__SUNY__Trends_in_Enrollment_of_Students_by_Race_Ethnicity_and_by_SUNY_Sector__Beginning_Fall_2002_20250413.csv")

############# data profile

glimpse(df)

## filter the year 2023 of the raw data
df_original <- df_original %>%
  mutate(Year = as.numeric(gsub("Fall ", "", Term))) %>%
  filter(Year == 2023)

summary(df_original)

## show specific categories in sector and ethnicity
list_categories <- function(variable) {
  df %>%
    distinct({{variable}}) %>%
    arrange({{variable}})
}

print(list_categories(`Institution.Sector`))
print(list_categories(Ethnicity))

########### data cleaning

## Clean column names (remove extra spaces)
names(df) <- str_trim(names(df))

## Convert from wide to long format
df_long <- df %>%
  pivot_longer(
    cols = c("White", "Black or African American", "Hispanic/Latino",
             "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander",
             "Two or More Races", "Asian", "Non-resident Alien", "Unknown"),
    names_to = "Ethnicity",
    values_to = "Enrollment"
  )

write.csv(df_long, "SUNY_Enrollment_Long_Format.csv", row.names = FALSE)


############  analysis
df <- read.csv("SUNY_Enrollment_Long_Format.csv")

# Extract Year and filter for Fall 2023 only
df_2023 <- df %>%
  mutate(Year = as.numeric(gsub("Fall ", "", Term))) %>%
  filter(Year == 2023)

# One-way ANOVA: Do enrollments differ by ethnicity?
anova_2023 <- aov(Enrollment ~ Ethnicity, data = df_2023)
summary(anova_2023)

# Post hoc test if ANOVA is significant
TukeyHSD(anova_2023)


###########
## follow-up question about the proportion change in the minority groups
df2 <- df %>%
  mutate(Year = as.numeric(gsub("Fall ", "", Term)))

# Define URM ethnicities
urm_groups <- c("Black or African American", "Hispanic/Latino",
                "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander",
                "Two or More Races")

summary_df2 <- df2 %>%
  group_by(Year, `Institution.Sector`) %>%
  summarise(
    Total_Enrollment = sum(Enrollment[Ethnicity != "Unknown"]),
    URM_Enrollment = sum(Enrollment[Ethnicity %in% urm_groups])
  ) %>%
  mutate(URM_Proportion = URM_Enrollment / Total_Enrollment)


ggplot(summary_df2, aes(x = Year, y = URM_Proportion, color = `Institution.Sector`)) +
  geom_line(size = 1.2) +
  labs(title = "URM Enrollment Proportion Over Time by SUNY Sector",
       y = "URM Proportion", x = "Year") +
  theme_minimal()

