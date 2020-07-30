

# This code calculates and graphs hours below freezing at AZMET stations where
# pecans are grown, for the APGA 2020 conference

# AZMET data are at: https://cals.arizona.edu/azmet/

# Author:
# Jeremy Weiss, Climate and Geospatial Extension Scientist
# School of Natural Resources and the Environment
# University of Arizona
# 520-626-8063, jlweiss@email.arizona.edu


# SETUP --------------------


# Load needed libraries
library("dplyr")
library("reshape2")
library("ggplot2")
library("lubridate")
library("extrafont")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")

# Load AZMET station list
stn_list <- read.csv("azmet-station-list.csv", sep = ",")

# Set the AZMET station names and years of record
#stn_name <- "Bowie"      
stn_names <- c("Bowie", "Maricopa", "Safford", "Sahuarita", "San Simon",
               "Willcox Bench")
#yr_start <- (stn_list$start_yr[which(stn_list$stn == stn_name)])
#yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.hourly.data.download.R")


# DOWNLOAD AND TRANSFORM DAILY AZMET DATA --------------------


for (s in 1:length(stn_names)) {
  stn <- stn_names[s]
  
  s_data <- azmet.hourly.data.download(stn)
  
  # Retain necessary variables
  s_data <- select(s_data, DateTime, Date, Year, Month, Day, JDay, Hour, Temp)
  
  # Filter hourly data to the months of June - September
  s_data <- filter(s_data, Month >=10 & Month <= 12)
  
  # Convert temperature from Celsius to Fahrenheit
  s_data$Temp <- (1.8 * s_data$Temp) + 32
  
  # Generate new variables
  s_data["Station"] <- stn
  #s_data["Below28F"] <- NA
  #s_data["Below32F"] <- NA
  #for (entry in 1:nrow(s_data)) {
  #  if (is.na(s_data$Tmin[entry]) == FALSE) {
      # Below 28F
  #    if (s_data$Tmin[entry] < 28) {
  #      s_data$Below28F[entry] <- 1
  #  } else {
  #    s_data$Below28F[entry] <- 0
  #  }
      # Below 32F
  #    if (s_data$Tmin[entry] < 32) {
  #      s_data$Below32F[entry] <- 1
  #    } else {
  #      s_data$Below32F[entry] <- 0
  #    }
  #  }
  #}
  #rm(entry)
  
  # Concatenate data from different stations
  if (s == 1) {
    stn_data_hourly <- s_data
  } else {
    stn_data_hourly <- rbind(stn_data_hourly, s_data)
  }
}
rm(stn, s_data)
rm(s)

# Check for station start dates that could interfere with first fall freeze
# dates calculations
for (s in 1:length(stn_names)) {
  stn <- stn_names[s]
  yr_start <- (stn_list$start_yr[which(stn_list$stn == stn)])
  x <- filter(stn_data_hourly, Station == stn, Year == yr_start)
  print(select(x[which(x$JDay == min(x$JDay)),], Date, Station))
  flush.console()
}
rm(stn, yr_start, x)
rm(s)

# Set and define temperature categories
stn_data_hourly["below28"] <- NA
stn_data_hourly["bet2832"] <- NA

measurements <- which(is.na(stn_data_hourly$Temp) == FALSE)

stn_data_hourly$below28[measurements] <- 0
stn_data_hourly$bet2832[measurements] <- 0

rm(measurements)

below28 <- which(stn_data_hourly$Temp < 28)

stn_data_hourly$below28[below28] <- 1

rm(below28)

bet2832 <- which(stn_data_hourly$Temp >= 28 & stn_data_hourly$Temp < 32)

stn_data_hourly$bet2832[bet2832] <- 1

rm(bet2832)

# Aggregate hourly data to daily data, summing the temperature categories
stn_data_daily <- stn_data_hourly %>%
  group_by(Year, JDay, Date, Month, Day, Station) %>%
  summarize(below28 = sum(below28, na.rm = TRUE), 
            bet2832 = sum(bet2832, na.rm = TRUE))

# Aggregate daily data to monthly data, summing the temperature categories
stn_data_monthly <- stn_data_daily %>%
  group_by(Year, Month, Station) %>%
  summarize(below28 = sum(below28, na.rm = TRUE), 
            bet2832 = sum(bet2832, na.rm = TRUE))

# Aggregate monthly data to yearly data, summing the temperature categories
stn_data_yearly <- stn_data_monthly %>%
  group_by(Year, Station) %>%
  summarize(below28 = sum(below28, na.rm = TRUE), 
            bet2832 = sum(bet2832, na.rm = TRUE))

# Transform daily data from wide to long
stn_data_daily <- melt(data = stn_data_daily,
                       id.vars = colnames(stn_data_daily)[1:6],
                       measure.vars = colnames(stn_data_daily[7:8]),
                       variable.name = "TempCode",
                       value.name = "Hours",
                       na.rm = FALSE)

# Transform monthly data from wide to long
stn_data_monthly <- melt(data = stn_data_monthly,
                         id.vars = colnames(stn_data_monthly)[1:3],
                         measure.vars = colnames(stn_data_monthly[4:5]),
                         variable.name = "TempCode",
                         value.name = "Hours",
                         na.rm = FALSE)

# Transform yearly data from wide to long
stn_data_yearly <- melt(data = stn_data_yearly,
                        id.vars = colnames(stn_data_yearly)[1:2],
                        measure.vars = colnames(stn_data_yearly[3:4]),
                        variable.name = "TempCode",
                        value.name = "Hours",
                        na.rm = FALSE)


# MAKE AND SAVE TIMESERIES PLOT --------------------


ggplot() +
  
  geom_line(data = stn_data_monthly,
            mapping = aes(x = Year, y = Hours, color = Station),
            size = 2) +
  
  facet_wrap(vars(Month, TempCode), ncol = 2) +
  
  # Accent, Dark2, Set2
  scale_color_brewer(type = "qual", palette = "Dark2")


ggplot() +
  
  geom_line(data = stn_data_yearly,
            mapping = aes(x = Year, y = Hours, color = Station),
            size = 2) +
  
  facet_wrap(vars(TempCode), ncol = 1) +
  
  # Accent, Dark2, Set2
  scale_color_brewer(type = "qual", palette = "Dark2")







# NEED TO EDIT ----------


p <- ggplot() +
  geom_line(data = df4plot,
            mapping = aes(x = Year, y = JDay, color = Station),
            size = 2) +
  
  facet_wrap(~ Threshold, ncol = 1) +
  
  # Accent, Dark2, Set2
  scale_color_brewer(type = "qual", palette = "Dark2") +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(
    breaks = seq(from = 1900, to = max(df4plot$Year), by = 5),
    limits = c(min(df4plot$Year, na.rm = TRUE), 
               max(df4plot$Year, na.rm = TRUE)),
    expand = c(0.0, 0.0)
  ) +
  
  scale_y_continuous(
    breaks = c(274, 288, 305, 319, 335, 349),
    labels = c("Oct 1", "Oct 15","Nov 1", "Nov 15", "Dec 1", "Dec 15"),
    limits = c((min(df4plot$JDay, na.rm = TRUE) - 1), 
               (max(df4plot$JDay, na.rm = TRUE) + 1)),
    #limits = c(0, max(stn_data_daily$Hours, na.rm = TRUE)),
    #minor_breaks = seq(from = 0, to = 24, 
    #                   #to = max(stn_data_daily$Hours, na.rm = TRUE), 
    #                   by = 1),
    expand = c(0.06, 0.0)
  ) +
  
  # Add the graph title, subtitle, axis, and legend labels
  ggtitle("Date of First Fall Freeze") +
  labs(#subtitle = "AZMET Stations near Pecan Orchards",
       x = "\nYear",
       y = "Date\n",
       caption = "\ntemperature data from Arizona Meteorological Network (cals.arizona.edu/azmet)") +
  
  guides(color = guide_legend("AZMET Station")) +
  
  # Further customize the figure appearance
  theme_light(base_family = "Source Sans Pro") +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(color = "gray40", size = 10),
        axis.text.y = element_text(color = "gray40", size = 10),
        axis.ticks.x.bottom = element_line(color = "gray80", size = 0.25),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(2.0, "mm"),
        axis.ticks.length.y = unit(0.0, "mm"),
        axis.title.x = element_text(color = "gray40", size = 10),
        axis.title.y = element_text(color = "gray40", size = 10),
        legend.direction = "vertical",
        legend.text = element_text(color = "gray40", size = 10),
        legend.title = element_text(color = "gray40", size = 10, face = "bold"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(color = "gray40", hjust = 0.0, size = 7),
        plot.caption.position = "plot",
        plot.margin = unit(c(1, 1 ,1, 1), "mm"),
        plot.subtitle = element_text(family = "Source Serif Pro", size = 12), 
        plot.title = element_text(
          face = "bold", family = "Source Serif Pro", size = 16
        ),
        plot.title.position = "plot",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(color = "gray40", size = 12, face = "bold")
  )
p

#  Save the figure
ggsave(file = paste("azmet-first-fall-freeze-apga2020-",
                    Sys.Date(),
                    ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 5, units = "in", dpi = 300)

ggsave(file = paste("azmet-first-fall-freeze-apga2020-",
                    Sys.Date(),
                    ".png"),
       plot = p, device = "png", path = NULL, scale = 1,
       width = 6, height = 5, units = "in", dpi = 300)

