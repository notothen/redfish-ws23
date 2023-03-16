## Redfish workshop

library("tidyverse")
library("readxl")

ID_data <- read_excel("1.Data/RedWorkshop_dataID.xlsx")

ID_data <- ID_data %>%
  filter(S_mentella == 1 | S_norvegicus == 1 | Other == 1) # Remove fish_id without assignment

ID_data <- ID_data %>% 
  group_by(Person) %>% 
  distinct(Fish_ID, .keep_all = TRUE) %>% 
  ungroup() # remove duplicates by person

long_data <- ID_data%>%
  pivot_longer(
    cols = c(S_norvegicus, S_mentella, Other),
    names_to = "Species",
    values_to = "Values"
  ) %>%
  filter(Values != 0) %>%
  select(-Values) # Generate long data

long_data <-
  within(long_data, Species[Species == "Other" &
                              Comment == "S_viviparus"] <- "S_viviparus")


tmp <- ID_data %>%
  select(-c(Person, Institute, Comment, Years, Name)) %>%
  aggregate(by = list(ID_data$Fish_ID), FUN = sum) %>%
  select(-Fish_ID) %>%
  rename(Fish_ID = Group.1) %>%
  pivot_longer(
    names_to = "Species",
    values_to = "Matches",
    cols = c("S_norvegicus", "S_mentella", "Other")
  )

ggplot(tmp, aes(x = Fish_ID, y = Matches, fill = Species)) + 
  geom_col(position = "dodge") + 
  xlim(0, 250)

