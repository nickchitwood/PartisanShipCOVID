library(tidyverse)
library(ggrepel)
library(ggthemes)
options(scipen = 999) 

# Get current election results
download.file('https://docs.google.com/spreadsheets/d/e/2PACX-1vS3Z8Rq9xqOLISwoKdK0n6CFLBuPSCoXbbLeY8vhi-rzFS3ZFNEtR0BCdEbHcS-2Tlh5aPcnZbwBLao/pub?output=csv',
              destfile = 'results.csv')
election <- read_csv('results.csv')
election_states <- election %>% filter(State_num < 100) %>%
  mutate(State_num = as.character(State_num) %>% str_pad(2, pad = "0"),
         dem_this_margin = as.double(substr(dem_this_margin,1,nchar(dem_this_margin)-1))
  )

# Get current COVID data by state
download.file('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
              destfile = 'covid_state_daily.csv')
covid_daily <- read_csv('covid_state_daily.csv')
dates = seq(Sys.Date() - 8, Sys.Date()-1, by="days")
covid_recent <- covid_daily %>%
  filter(date %in% dates) %>%
  arrange(state, date) %>%
  mutate(new_cases = cases - lag(cases),
         new_deaths = deaths - lag(deaths)) %>%
  filter(date != Sys.Date() - 8)

covid_avg_7_days <- covid_recent %>%
  filter(`date` %in% dates) %>%
  group_by(fips) %>%
  summarize(avg_cases = mean(new_cases), avg_deaths = mean(new_deaths))

# Get Census Count
download.file('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv',
              destfile = 'census2019.csv')
census <- read_csv('census2019.csv') %>%
  filter(!STATE %in% c("00", "72"))

# Combined dataset
final_data <-
  election_states %>% 
  left_join(census %>% select(State_num = STATE, population = POPESTIMATE2019)) %>%
  left_join(covid_avg_7_days %>% rename(State_num = fips), by="State_num") %>%
  mutate(cases_per_100000 = avg_cases / population * 100000)

# Model
rate_model <- lm(cases_per_100000 ~ dem_this_margin, data = final_data)
summary(rate_model)

# Plot scatter plot
data_plot <- ggplot(data=final_data, mapping = aes(x=dem_this_margin, y=cases_per_100000)) +
  geom_point() +
  geom_text_repel(aes(label=stateid)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgray") +
  labs(title="COVID Cases Per 100,000 by Democratic Vote Margin",
       caption = "Source code available: https://github.com/nickchitwood/PartisanShipCOVID") +
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed") +
  theme_few()
  
data_plot
ggsave(filename = "scatter.png",
       width = 10,
       height = 7.5)
  