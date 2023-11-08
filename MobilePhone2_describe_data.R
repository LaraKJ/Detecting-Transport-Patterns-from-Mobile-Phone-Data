# Describe BetterPoints app data


# Total number of unique users
length(unique(df$DFT_ID))

# Number of users in November from April
df$DFT_ID[month(df$date_start) == 11] %in% df$DFT_ID[month(df$date_start) == 4]

df %>% mutate(month = month(date_start)) %>% 
  distinct(DFT_ID, month) %>%
  pivot_wider(names_from = month, values_from = month) %>%
  summarise(num_april_users = n_distinct(DFT_ID[!is.na(`4`)]),
            num_nov_users = n_distinct(DFT_ID[!is.na(`11`)]),
            # Calculate number of drop-outs (used app in April but not in Nov)
            num_attrition = n_distinct(DFT_ID[!is.na(`4`) & is.na(`11`)]),
            # Calculate number of new users in November
            num_new_users = n_distinct(DFT_ID[!is.na(`11`) & is.na(`4`)]))

# Histogram of unique users per postcode
postcodes <- df_raw %>% distinct(DFT_ID, PostcodeArea)
df %>% left_join(postcodes) %>% 
  group_by(PostcodeArea) %>%
  summarise(num_users = n_distinct(DFT_ID)) %>%
  ggplot(aes(x = num_users))+
  geom_histogram()

df %>% left_join(postcodes) %>% 
  group_by(PostcodeArea) %>%
  summarise(num_users = n_distinct(DFT_ID)) %>%
  ggplot(aes(x = reorder(PostcodeArea, num_users), y = num_users))+
  geom_col()+
  coord_flip()+
  theme_void()+
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(angle = 90),
        axis.text.x = element_text())+
  xlab("Postcode Area")+
  ylab("Number of App Users")

df %>% left_join(postcodes) %>% 
  group_by(PostcodeArea) %>%
  summarise(num_users = n_distinct(DFT_ID)) %>% #filter(num_users <20) %>%
  arrange(num_users) %>% view()

# Mean number of daily trips and journeys per user
mean_trips_journeys <- df %>% group_by (DFT_ID, date_start) %>%
  summarise(num_daily_trips = max(trip_counter),
            num_daily_journeys = max(journey_counter)) %>% ungroup() %>%
  group_by(DFT_ID) %>%
  summarise(mean_daily_trips = mean(num_daily_trips),
            sd_daily_trips = sd(num_daily_trips),
            mean_daily_journeys = mean(num_daily_journeys),
            sd_daily_journeys = sd(num_daily_journeys))

median(mean_trips_journeys$mean_daily_journeys)

ggplot(mean_trips_journeys, aes(x = mean_daily_journeys))+
  geom_histogram()+
  theme_minimal()+
  xlab("Mean Number of Daily Journeys per App User")+
  ylab("Number of App Users")

ggplot(mean_trips_journeys, aes(x = mean_daily_trips))+
  geom_histogram()+
  theme_minimal()+
  xlab("Mean Number of Daily Trips per App User")+
  ylab("Number of App Users")+
  ylim(0,1500)

# df %>% head(100) %>% view()

df$peak_flag[is.na(df$peak_flag)] <- "Non-peak"
waitingtimes <- df %>% filter(wait_since_prior >= 0,
                              new_journey == FALSE,
                              JourneyMode %in% c("Train", "Bus"))

waitingtimes %>% head(100) %>% view()
waitingtimes %>% filter(wait_since_prior <= 0) %>% view()

# Show mean waiting times
waitingtimes %>% group_by(wday, peak_flag, JourneyMode) %>%
  summarise(mean_wait = mean(wait_since_prior, na.rm = T),
            std_wait= sd(wait_since_prior, na.rm = T)) %>%
  ggplot(aes(x = wday, y = mean_wait, group = peak_flag, colour = peak_flag))+
  geom_line()+
  #geom_errorbar(aes(ymin=mean_wait-std_wait, ymax = mean_wait+std_wait), width = 0.2)+
  facet_wrap(~JourneyMode)+
  ylim(0,15)+
  xlab("")+
  ylab("Mean Waiting Time (Minutes)")+
  theme_minimal()

# Experimental version
waitingtimes %>% group_by(wday, peak_flag, JourneyMode) %>%
  summarise(mean_wait = mean(wait_since_prior, na.rm = T),
            std_wait= sd(wait_since_prior, na.rm = T)) %>%
  ggplot(aes(x = wday, y = mean_wait, group = peak_flag, colour = peak_flag, fill = peak_flag))+
  geom_line()+
  geom_ribbon(aes(ymin=mean_wait-std_wait, ymax = mean_wait+std_wait), alpha = 1)+
  facet_wrap(~JourneyMode)+
  ylim(0,15)+
  xlab("")+
  ylab("Mean Waiting Time (Minutes)")+
  theme_minimal()+
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Two-samples t test to see if there is a statistical difference between peak and off peak waiting times
t.test(wait_since_prior ~ peak_flag, data = waitingtimes)
sd(waitingtimes$wait_since_prior[waitingtimes$peak_flag == "Peak"])
sd(waitingtimes$wait_since_prior[waitingtimes$peak_flag == "Non-peak"])

# Proportion of public transport journeys that don't have a walk at the beginning

ggplot(waitingtimes, aes(x = peak_flag, y = wait_since_prior, colour = peak_flag))+
  geom_violin()+
  geom_jitter(alpha = .2, size = .1)+
  facet_wrap(~JourneyMode)+
  theme_minimal()+
  xlab("")+
  ylab("Waiting Time (Minutes")+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(waitingtimes, aes(x = JourneyMode, y = wait_since_prior, colour = peak_flag))+
  geom_violin()+
  theme_minimal()+
  xlab("")+
  ylab("Waiting Time (Minutes)")

ggplot(waitingtimes[waitingtimes$duration < 120,], aes(x = hour_start, y = wait_since_prior, colour = peak_flag))+
  geom_violin()+
  facet_wrap(~JourneyMode)

model1 <- lm(wait_since_prior ~ JourneyMode + peak_flag, data = waitingtimes)
summary(model1)

model2 <- lm(wait_since_prior ~ JourneyMode + peak_flag + duration, data = waitingtimes)
summary(model2)

model3 <- lm(wait_since_prior ~ JourneyMode + peak_flag + wday + 
               as.factor(hour_start) + as.factor(hour_end), 
             data = waitingtimes)
summary(model3)

