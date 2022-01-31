
# Check and load Packages and libraries

#Install packages
if(!'ggplot2'%in%installed.packages()){
  install.packages('ggplot2')}
if(!'ggthemes'%in%installed.packages()){
  install.packages('ggthemes')}
if(!'nlme'%in%installed.packages()){
  install.packages('nlme')}
if(!'gapminder'%in%installed.packages()){
  install.packages('gapminder')}
if(!'gganimate'%in%installed.packages()){
  install.packages('gganimate')}
if(!'ggExtra'%in%installed.packages()){
  install.packages('ggExtra')}
if(!'psych'%in%installed.packages()){
  install.packages('psych')}
if(!'reshape2'%in%installed.packages()){
  install.packages('reshape2')}
if(!'dplyr'%in%installed.packages()){
  install.packages('dplyr')}
if(!'nycflights13'%in%installed.packages()){
  install.packages('nycflights13')}
if(!'ggcorrplot'%in%installed.packages()){
  install.packages('ggcorrplot')}
if(!'waffle'%in%installed.packages()){
  install.packages('waffle')}
if(!'tidyr'%in%installed.packages()){
  install.packages('tidyr')}
if(!'scales'%in%installed.packages()){
  install.packages('scales')}
if(!'ggalt'%in%installed.packages()){
  install.packages('ggalt')}
if(!'data.table'%in%installed.packages()){
  install.packages('data.table')}
if(!'extrafont'%in%installed.packages()){
  install.packages('extrafont')}
if(!'lubridate'%in%installed.packages()){
  install.packages('lubridate')}
if(!'DT'%in%installed.packages()){
  install.packages('DT')}
if(!'grid'%in%installed.packages()){
  install.packages('grid')}
if(!'gridExtra'%in%installed.packages()){
  install.packages('gridExtra')}
if(!"prettydoc" %in% installed.packages()) {
  install.packages("prettydoc")}
if(!"devtools" %in% installed.packages()) {
  install.packages("devtools")}
if(!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")}
if(!"ggdark" %in% installed.packages()) {
  install.packages("ggdark")}
if(!"here" %in% installed.packages()) {
  install.packages("here")}
if(!"gifski" %in% installed.packages()) {
  install.packages("gifski")}
if(!"forcats" %in% installed.packages()) {
  install.packages("forcats")}
if(!"tufte" %in% installed.packages()) {
  install.packages("tufte")}
if(!"colorspace" %in% installed.packages()) {
  install.packages("colorspace")}
if(!"viridisLite" %in% installed.packages()) {
  install.packages("viridisLite")}
if(!"formatR" %in% installed.packages()) {
  install.packages("formatR")}
if(!"DiagrammeR" %in% installed.packages()) {
  install.packages("DiagrammeR")}
if(!"xaringan" %in% installed.packages()) {
  install.packages("xaringan")}
if(!"ggridges" %in% installed.packages()) {
  install.packages("ggridges")}
if(!"GGally" %in% installed.packages()) {
  install.packages("GGally")}
if(!"corrplot" %in% installed.packages()) {
  install.packages("corrplot")}
if(!"ggplot2movies" %in% installed.packages()) {
  install.packages("ggplot2movies")}
if(!"ggpointdensity" %in% installed.packages()) {
  install.packages("ggpointdensity")}
if(!"rstat" %in% installed.packages()) {
  install.packages("rstat")}
if(!"ggstatsplot" %in% installed.packages()) {
  install.packages("ggstatsplot")}
if(!"ggbeeswarm" %in% installed.packages()) {
  install.packages("ggbeeswarm")}
if (!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github('Ather-Energy/ggTimeSeries')
devtools::install_github('erocoar/gghalves')

#Connect with the libraries
library(ggplot2)
library(ggthemes)
library(nlme)
library(gganimate)
library(gapminder)
library(ggExtra)
library(psych)
library(reshape2)
library(dplyr)
library(nycflights13)
library(ggcorrplot)
library(waffle)
library(tidyr)
library(scales)
library(ggalt)
library(data.table)
library(extrafont)
library(lubridate)
library(DT)
library(grid)
library(gridExtra)
library(prettydoc)
library(devtools)
library(tidyverse)
library(ggdark)
library(here)
library(gifski)
library(forcats)
library(tufte)
library(colorspace)
library(viridisLite)
library(formatR)
library(DiagrammeR)
library(xaringan)
library(ggridges)
library(GGally)
library(ggplot2movies)
library(corrplot)
library(ggpointdensity)
library(ggstatsplot)
library(ggTimeSeries)
library(ggbeeswarm)
library(gghalves)

# dark theme option with black background
theme_set(dark_theme_gray()+ theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 14, hjust = 0, color = decoration_color),
  axis.ticks = element_blank(),
  axis.title = element_text(size = 10, hjust = 0.5, color = decoration_color),
  legend.title = element_blank(),
  panel.background =element_rect(fill = fill_color),
  strip.background =element_rect(fill = fill_color), 
  plot.background = element_rect(fill = fill_color),
  legend.background = element_rect(fill = fill_color)
))

#Introducing the color package 
library(RColorBrewer)
head(brewer.pal.info, 20)

###############
# 1. Time series with R/ggplot2 (15p)
###############

# I am choosing gapminder dataset for this time series assignment with R
names(gapminder)
head(gapminder, n=10)
str(gapminder)
summary(gapminder)

# copying the dataset to another variable and then converting year column as datetime
gapminder_copy <- gapminder
gapminder_copy$year = as.Date(as.character(gapminder$year), format="%Y")
head(gapminder_copy)

#   * Line chart for all categories over time
#   * Line chart for all categories over time coloured by the group variable (e.g. continent, genre)
#   * Small multiple line chart by group variable with and without trend line

# life expectancy over time
ggplot(gapminder_copy, aes(year, lifeExp, group=country))+ geom_line(alpha=0.3, lwd=0.3)+
  ylab("life expectancy") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13)) +
  labs(title = "Life expectancy by country")+theme(plot.title=element_text(hjust = 0.5))

ggplot(gapminder_copy, aes(year, lifeExp, colour = continent, group=country))+
  geom_line(alpha=0.5, lwd=0.3)+ scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("life expectancy") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))+
  labs(title = "Life expectancy by continent")+theme(plot.title=element_text(hjust = 0.5))

ggplot(gapminder_copy, aes(year, lifeExp, colour = continent, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+ 
  ylab("life expectancy") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Life expectancy by continent")+theme(plot.title=element_text(hjust = 0.5))

ggplot(gapminder_copy, aes(year, lifeExp, colour = continent, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+ 
  geom_smooth(data=gapminder_copy, aes(year, lifeExp, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE, show.legend=FALSE)+
  ylab("life expectancy") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Life expectancy by continent")+theme(plot.title=element_text(hjust = 0.5))

# Different color with different continent sounded easy to follow to me. However, doesn't look good
# when I added trendline. Therefore, switching to mono color/one color to better visualize when displaying trendline.
ggplot(gapminder_copy, aes(year, lifeExp, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  geom_smooth(data=gapminder_copy, aes(year, lifeExp, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE)+
  ylab("life expectancy") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Life expectancy by continent")+theme(plot.title=element_text(hjust = 0.5))

# population over time
ggplot(gapminder_copy, aes(year, pop, group=country))+ geom_line(alpha=0.2)+
  ylab("population") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))

ggplot(gapminder_copy, aes(year, pop, colour = continent, group=country))+
  geom_line(alpha=0.5, lwd=0.3)+ scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("population") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))

# Let's apply log scaling on y axis
ggplot(gapminder_copy, aes(year, pop, colour = continent, group=country))+
  geom_line(alpha=0.5, lwd=0.3)+ scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("population") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))+
  scale_y_log10()

ggplot(gapminder_copy, aes(year, pop, colour = continent, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("population") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()+labs(title = "Population by continent")+theme(plot.title=element_text(hjust = 0.5))

ggplot(gapminder_copy, aes(year, pop, colour = continent, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  geom_smooth(data=gapminder_copy, aes(year, pop, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE)+
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("population") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()+labs(title = "Population by continent")+theme(plot.title=element_text(hjust = 0.5))

# Different color with different continent sounded easy to follow to me. However, doesn't look good
# when I added trendline. Therefore, switching to mono color/one color to better visualize when displaying trendline.
ggplot(gapminder_copy, aes(year, pop, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  geom_smooth(data=gapminder_copy, aes(year, pop, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE)+
  ylab("population") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()+labs(title = "Population by continent")+theme(plot.title=element_text(hjust = 0.5))


# GDP per capita over time
ggplot(gapminder_copy, aes(year, gdpPercap, group=country))+ geom_line(alpha=0.2, lwd=0.3)+
  ylab("GDP per capita") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))

ggplot(gapminder_copy, aes(year, gdpPercap, colour = continent, group=country))+
  geom_line(alpha=0.5, lwd=0.3)+ scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("GDP per capita") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))


# There is an outlier. Limiting y axis should give a better view for the rests
ggplot(gapminder_copy, aes(year, gdpPercap, colour = continent, group=country))+
  geom_line(alpha=0.5, lwd=0.3)+ scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("GDP per capita") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))+ylim(0,50000)

# Let's apply log scaling on y axis
ggplot(gapminder_copy, aes(year, gdpPercap, colour = continent, group=country))+
  geom_line(alpha=0.5, lwd=0.3)+ scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("GDP per capita") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))+
  scale_y_log10()

ggplot(gapminder_copy, aes(year, gdpPercap, colour = continent, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("GDP per capita") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()+labs(title = "GDP per capita by continent")+theme(plot.title=element_text(hjust = 0.5))

ggplot(gapminder_copy, aes(year, gdpPercap, colour = continent, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  geom_smooth(data=gapminder_copy, aes(year, gdpPercap, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE, show.legend=FALSE)+
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd"))+
  ylab("GDP per capita") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()+labs(title = "GDP per capita by continent")+theme(plot.title=element_text(hjust = 0.5))


# Different color with different continent sounded easy to follow to me. However, doesn't look good
# when I added trendline. Therefore, switching to mono color/one color to better visualize when displaying trendline.
ggplot(gapminder_copy, aes(year, gdpPercap, group=country))+
  geom_line(alpha=0.6, show.legend=FALSE, lwd=0.3)+ 
  geom_smooth(data=gapminder_copy, aes(year, gdpPercap, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE)+
  ylab("GDP per capita") + facet_wrap(~ continent, ncol = 5)+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()+labs(title = "GDP per capita by continent")+theme(plot.title=element_text(hjust = 0.5))


#   * Now filter on one group (e.g. the continent Europe, the genre Drama)
#       * Line chart for all categories with trend line
#       * Small multiple by category (e.g. countries, movies)
#       * Line chart with all lines (without the filter) in the background and lines from the filtered group highlighted through colour, line-width and transparency.


# I would like to select Asia here because it looks like lots of interesting trend is going on in Asia.
# Some countries scored so good on everything, others are lagging behind, some are improving significantly etc.
asia <- dplyr::filter(gapminder_copy, continent == "Asia")

ggplot(asia, aes(year, lifeExp, group=country))+
  geom_line(alpha=0.5, show.legend=FALSE, lwd=0.4)+ 
  geom_smooth(data=asia, aes(year, lifeExp, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE)+
  ylab("life expectancy") +
  labs(title = "Life expectancy in Asia")+theme(plot.title=element_text(hjust = 0.5))

ggplot() +
  geom_line(data = transform(gapminder_copy, continent = NULL), aes (year, lifeExp, group = country), alpha = 0.5, lwd = 0.1, colour = "white") +
  geom_line(data=asia, aes (year, lifeExp, group = country), lwd = 0.3, show.legend = FALSE, color= main2_color) +
  geom_smooth(data=asia, aes(year, lifeExp, group = 1), lwd = 1, method = 'loess', span = 0.1, se = TRUE) +
  ylab("life expectancy") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))+
  labs(title = "Life expectancy by country in Asia with rest of the World data in the back")+theme(plot.title=element_text(hjust = 0.5))

ggplot(asia, aes(year, lifeExp)) + geom_line(color=main2_color) + facet_wrap(~country, strip.position = "bottom", ncol=11) +
  ylab("life expectancy") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Changes in Life Expectancy in Asian countries") + theme(plot.title=element_text(hjust = 0.5))+
  theme(axis.title = element_text(size = 13))

ggplot(asia, aes(year, pop, group=country))+ geom_line(alpha=0.5, show.legend=FALSE, lwd=0.4)+ 
  geom_smooth(data=asia, aes(year, pop, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE)+
  ylab("population") + scale_y_log10()+labs(title = "Population in Asia")+theme(plot.title=element_text(hjust = 0.5))

ggplot() +
  geom_line(data = transform(gapminder_copy, continent = NULL), aes (year, pop, group = country), alpha = 0.5, lwd = 0.1, colour = "white") +
  geom_line(data=asia, aes (year, pop, group = country), lwd = 0.3, show.legend = FALSE, color= main2_color) +
  geom_smooth(data=asia, aes(year, pop, group = 1), lwd = 1, method = 'loess', span = 0.1, se = TRUE) +
  scale_y_log10()+ ylab("population") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))+
  labs(title = "Population by country in Asia with rest of the World data in the back")+theme(plot.title=element_text(hjust = 0.5))

ggplot(asia, aes(year, pop)) + geom_line(color=main2_color) + facet_wrap(~country, strip.position = "bottom", ncol=11) +
  ylab("population") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Changes in population in Asian countries") + theme(plot.title=element_text(hjust = 0.5))+
  theme(axis.title = element_text(size = 13))

ggplot(asia, aes(year, pop)) + geom_line(color=main2_color) + facet_wrap(~country, strip.position = "bottom", ncol=11) +
  scale_y_log10()+ylab("population") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Changes in population in Asian countries") + theme(plot.title=element_text(hjust = 0.5))+
  theme(axis.title = element_text(size = 13))

ggplot(asia, aes(year, gdpPercap, group=country))+
  geom_line(alpha=0.5, show.legend=FALSE, lwd=0.4)+ 
  geom_smooth(data=asia, aes(year, gdpPercap, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE)+
  ylab("GDP per capita") +
  scale_y_log10()+labs(title = "GDP per capita for Asia")+theme(plot.title=element_text(hjust = 0.5))

ggplot() +
  geom_line(data = transform(gapminder_copy, continent = NULL), aes (year, gdpPercap, group = country), alpha = 0.5, lwd = 0.1, colour = "white") +
  geom_line(data=asia, aes (year, gdpPercap, group = country), lwd = 0.3, show.legend = FALSE, color= main2_color) +
  geom_smooth(data=asia, aes(year, gdpPercap, group = 1), lwd = 1, method = 'loess', span = 0.1, se = TRUE) +
  scale_y_log10()+ ylab("GDP per capita") + theme(axis.text = element_text(size = 11)) + theme(axis.title = element_text(size = 13))+
  labs(title = "GDP per capita by country in Asia with rest of the World data in the back")+theme(plot.title=element_text(hjust = 0.5))

ggplot(asia, aes(year, gdpPercap)) + geom_line(color=main2_color) + facet_wrap(~country, strip.position = "bottom", ncol=11) +
  scale_y_log10()+ylab("GDP per capita") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Changes in GDP per capita in Asian countries") + theme(plot.title=element_text(hjust = 0.5))+
  theme(axis.title = element_text(size = 13))

###############
# 2. Time series with Flourish (10p)
###############
# Now use the dataset from Part 1 and create one bar race or line race chart using Flourish.
# 
# Colour your bars by the group variable
# Chose your own style (colours, font, layout)
# The submission should be a link to your Flourish Visualization  


# A simple one:
# https://public.flourish.studio/visualisation/8103629/
# This dataset doesn't have image files for displaying flags instead of country names. 
  


###############
# 3. Dynamic Visualisation with Flourish (15p)
###############

# Now choose another dataset and create one of the presented dynamic visualisation in Flourish:
#   
#   Chord diagram
#   Radar chart
# One of the hierarchical visualisations like Treemap, Sunburst etc.

# https://public.flourish.studio/visualisation/8110505/
# https://public.flourish.studio/visualisation/8173935/



