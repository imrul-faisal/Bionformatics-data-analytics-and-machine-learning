
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

# 1. Assignment

#Data loading
Happy_world <- read.csv("https://gist.githubusercontent.com/sandravizz/8b7cf476e4d07331eee00f1fa0249e12/raw/65395ac49ea7154a43f47f455ec26e270c0fb4a3/World%2520Happiness%2520Report")

#Data checking
names(Happy_world)
head(Happy_world, n=10)
str(Happy_world)
summary(Happy_world)
view(Happy_world)

#Simple distirbution
ggplot(Happy_world, aes(Generosity)) + 
  geom_freqpoly() 

#Simple scatter 
ggplot(Happy_world, aes(Generosity, Family)) +
  geom_point() 

###############
# Task 1:
# Please investigate on the distribution of 
# two numeric variables by region (at least 5 visualizations).
###############

# 1 (Histogram)
ggplot(Happy_world, aes(Happiness.Score, colour = Region, fill=Region)) +  
  geom_histogram(size=0) + scale_fill_brewer(palette = "Paired", direction = -1) + xlab("happiness score") + theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 12))

# 2 (Frequency Polygon)
ggplot(Happy_world, aes(Happiness.Score, colour = Region)) + 
  scale_colour_brewer(palette = "Paired", direction = -1) +  geom_freqpoly() + xlab("happiness score") + 
  theme(axis.title = element_text(size = 14)) + theme(axis.text = element_text(size = 12))

table(Happy_world['Region'])

# 3 (histogram + Frequency Polygon)
ggplot(Happy_world, aes(Happiness.Score, colour = Region)) + 
  scale_colour_brewer(palette = "Paired", direction = -1) + 
  geom_histogram(colour=decoration_color, fill=decoration_color, alpha=0.15, size=0) + 
  geom_freqpoly() + xlab("happiness score") + theme(axis.title = element_text(size = 13)) + 
  theme(axis.text = element_text(size = 11))

# zoom in on y axis to view better
ggplot(Happy_world, aes(Happiness.Score, colour = Region)) + 
  scale_colour_brewer(palette = "Paired", direction = -1) + 
  geom_histogram(colour=decoration_color, fill=decoration_color, alpha=0.15, size=0) + 
  geom_freqpoly() + ylim(0,8) + xlab("happiness score")+ theme(axis.title = element_text(size = 13)) + 
  theme(axis.text = element_text(size = 11))

# comment: The colors are very busy because there are 10 groups. It is clear that Western 
# Europe scores very high, whereas Sub-Saharan Africa scores low. It would be nice to split
# this figure into two, or reduce the group numbers to focus on more interesting ones, or 
# merge the groups into whole continents. However, this is the primary assignment, and I am
# mainly focusing on creating the graphs. So, I will not spend too much time on it right now.
# I'll have a clearer idea by plotting small multiples, etc.

# 4 (small multiples)
ggplot(Happy_world, aes(Happiness.Score, fill = Region)) + 
  geom_histogram(show.legend = FALSE, color="black") + scale_fill_brewer(palette = "Paired") +
  facet_wrap(. ~ Region) + xlab("happiness score") + theme(axis.title = element_text(size = 14))

ggplot(Happy_world, aes(Happiness.Score, color = Region)) + 
  geom_freqpoly(show.legend = FALSE) + scale_colour_brewer(palette = "Paired") +
  facet_wrap(. ~ Region) + xlab("happiness score")+ theme(axis.title = element_text(size = 14))


# 5 (boxplot)
ggplot(Happy_world, aes(Region, Happiness.Score)) + geom_boxplot(colour=main2_color) + 
  ylab("happiness score") + theme(axis.text.x = element_text(size=8, angle = 60, hjust = 1)) + 
  theme(axis.text.y = element_text(size = 11)) + theme(axis.title = element_text(size = 13))

# 6 (density plot small multiples)
ggplot(Happy_world, aes(Happiness.Score, stat(density), fill=Region)) + 
  geom_density(colour= NA, show.legend = FALSE) + scale_fill_brewer(palette = "Paired") +  
  ylim(0,1) + xlab("happiness score") + facet_wrap(. ~ Region) + 
  theme(strip.text = element_text(size=6)) + theme(axis.title = element_text(size = 12))

ggplot(Happy_world, aes(x=Happiness.Score, y=Region, fill=Region)) + 
  geom_density_ridges(colour= NA, show.legend = FALSE) + 
  scale_fill_brewer(palette = "Paired", direction = -1) + xlab("happiness score") + 
  theme(axis.title = element_text(size = 12)) + theme(axis.text.x = element_text(size=12)) + 
  scale_y_discrete(limits=rev)

# Here I have assigned happiness as cool color (i.e. unhappiness = pinkish/reddish)
ggplot(Happy_world, aes(x=Happiness.Score, y=Region, fill=stat(x))) + 
  geom_density_ridges_gradient(colour= "black", scale=2) + 
  scale_fill_gradient(low=main1_color, high=main2_color) + 
  xlab("happiness score") + theme(axis.title = element_text(size = 11)) + 
  theme(axis.text.x = element_text(size=11)) + scale_y_discrete(limits=rev)
# For some reason, the Australia & New Zealand, and North America don't appear in these plots.
# Probably die to very low frequencies of countries/data points

ggplot(Happy_world, aes(x=Happiness.Score, y=Region, fill=Region, color=Region)) + 
  geom_density_ridges(scale=0.5, jittered_points=TRUE, point_alpha=1, size=0.8, point_size=0.8, position="raincloud", show.legend = FALSE) + 
  scale_colour_brewer(palette = "Paired", direction = -1) + 
  scale_fill_brewer(palette = "Paired", direction = -1) + xlab("happiness score") + 
  theme(axis.title = element_text(size = 11)) + theme(axis.text.x = element_text(size=11)) + 
  scale_y_discrete(limits=rev)


### Now I'll plot another variable ("Economy..GDP.per.Capita.") to check on the distribution.

# 1 (Histogram)
ggplot(Happy_world, aes(Economy..GDP.per.Capita., colour = Region, fill=Region)) +  
  geom_histogram(size=0) + scale_fill_brewer(palette = "Paired", direction = -1) + 
  xlab("GDP per capita") + theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 12))

# 2 (Frequency Polygon)
ggplot(Happy_world, aes(Economy..GDP.per.Capita., colour = Region)) + 
  scale_colour_brewer(palette = "Paired", direction = -1) +  geom_freqpoly() + 
  xlab("GDP per capita") + theme(axis.title = element_text(size = 14)) + 
  theme(axis.text = element_text(size = 12))

# 3 (histogram + Frequency Polygon)
ggplot(Happy_world, aes(Economy..GDP.per.Capita., colour = Region)) + 
  scale_colour_brewer(palette = "Paired", direction = -1) + 
  geom_histogram(colour=decoration_color, fill=decoration_color, alpha=0.15, size=0) + 
  geom_freqpoly() + xlab("GDP per capita") + theme(axis.title = element_text(size = 13)) + 
  theme(axis.text = element_text(size = 11))

# zoom in on y axis to view better
ggplot(Happy_world, aes(Economy..GDP.per.Capita., colour = Region)) + 
  scale_colour_brewer(palette = "Paired", direction = -1) + 
  geom_histogram(colour=decoration_color, fill=decoration_color, alpha=0.15, size=0) + 
  geom_freqpoly() + ylim(0,7.5) + xlab("GDP per capita")+ theme(axis.title = element_text(size = 13)) + 
  theme(axis.text = element_text(size = 11))

# comment: The colors are very busy because there are 10 groups. It is clear that 
# Europe scores very high, whereas Sub-Saharan Africa scores low. It would be nice to split
# this figure into two, or reduce the group numbers to focus on more interesting ones, or 
# merge the groups into whole continents. However, this is the primary assignment, and I am
# mainly focusing on creating the graphs. So, I will not spend too much time on it right now.
# I'll have a clearer idea by plotting small multiples, etc.

# 4 (small multiples)
ggplot(Happy_world, aes(Economy..GDP.per.Capita., fill = Region)) + 
  geom_histogram(show.legend = FALSE, color="black") + scale_fill_brewer(palette = "Paired") +
  facet_wrap(. ~ Region) + xlab("GDP per capita") + theme(axis.title = element_text(size = 12))

ggplot(Happy_world, aes(Economy..GDP.per.Capita., color = Region)) + 
  geom_freqpoly(show.legend = FALSE) + scale_colour_brewer(palette = "Paired") +
  facet_wrap(. ~ Region) + xlab("GDP per capita")+ theme(axis.title = element_text(size = 12))

# 5 (boxplot)
ggplot(Happy_world, aes(Region, Economy..GDP.per.Capita.)) + geom_boxplot(colour=main2_color) + 
  ylab("GDP per capita") + theme(axis.text.x = element_text(size=8, angle = 60, hjust = 1)) + 
  theme(axis.text.y = element_text(size = 11)) + theme(axis.title = element_text(size = 12))

# 6 (density plot small multiples)
ggplot(Happy_world, aes(Economy..GDP.per.Capita., stat(density), fill=Region)) + 
  geom_density(colour= NA, show.legend = FALSE) + scale_fill_brewer(palette = "Paired") +  
  ylim(0,1) + xlab("GDP per capita") + facet_wrap(. ~ Region) + 
  theme(strip.text = element_text(size=6)) + theme(axis.title = element_text(size = 12))

ggplot(Happy_world, aes(x=Economy..GDP.per.Capita., y=Region, fill=Region)) + 
  geom_density_ridges(colour= NA, show.legend = FALSE) + 
  scale_fill_brewer(palette = "Paired", direction = -1) + xlab("GDP per capita") + 
  theme(axis.title = element_text(size = 12)) + theme(axis.text.x = element_text(size=12)) + 
  scale_y_discrete(limits=rev)

# Here I have plotted heat colors as more income, and cool colors as less income
ggplot(Happy_world, aes(x=Economy..GDP.per.Capita., y=Region, fill=stat(x))) + 
  geom_density_ridges_gradient(colour= "black", scale=2) + 
  scale_fill_gradient(low=main2_color, high=main1_color) + 
  xlab("GDP per capita") + theme(axis.title = element_text(size = 11)) + 
  theme(axis.text.x = element_text(size=11)) + scale_y_discrete(limits=rev)

ggplot(Happy_world, aes(x=Economy..GDP.per.Capita., y=Region, fill=Region, color=Region)) + 
  geom_density_ridges(scale=0.5, jittered_points=TRUE, point_alpha=1, size=0.8, point_size=0.8, position="raincloud", show.legend = FALSE) + 
  scale_colour_brewer(palette = "Paired", direction = -1) + 
  scale_fill_brewer(palette = "Paired", direction = -1) + xlab("GDP per capita") + 
  theme(axis.title = element_text(size = 11)) + theme(axis.text.x = element_text(size=11)) + 
  scale_y_discrete(limits=rev)


###############
# Task 2:
# Afterwards check the relationship of these two variables 
# also by region (at least 5 visualizations).
###############

# Now I'll check the relationships between "Economy..GDP.per.Capita." and "Happiness.Score".

# 1 (Basic scatter plot)
ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  geom_point(color=main2_color) + xlab("happiness score") + ylab("GDP per capita") + 
  theme(axis.text = element_text(size=11)) + theme(axis.title = element_text(size = 12))

# Adding a trend line
ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  geom_point(color=main2_color) + xlab("happiness score") + ylab("GDP per capita") + 
  theme(axis.text = element_text(size=11)) + theme(axis.title = element_text(size = 12)) + 
  stat_smooth(color=decoration_color)

# 2 (Small multiples (same scale))
ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  geom_point(color=main2_color, size=0.6, alpha=0.8) + xlab("happiness score") + ylab("GDP per capita") + 
  theme(axis.title = element_text(size = 12)) + stat_smooth(color=decoration_color, size=0.5) + 
  facet_wrap( ~ Region)

# Small multiples (free scale)
ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  geom_point(color=main2_color, size=0.6, alpha=0.8) + xlab("happiness score") + ylab("GDP per capita") + 
  theme(axis.title = element_text(size = 12)) + stat_smooth(color=decoration_color, size=0.35) + 
  facet_wrap( ~ Region, scales = "free")

# 3 (Marginal plot to compare all simple distributions with the scatterplot)
p<-ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  geom_point(color=main2_color) + xlab("happiness score") + ylab("GDP per capita")
# Density
ggMarginal(p, type = "density", fill = main2_color, color='transparent')

# Boxplot 
ggMarginal(p, type = "boxplot", size=10, fill=main2_color)

# 4 (Hexagonal binning)
ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  geom_hex(bins=20, alpha =0.7) + xlab("happiness score") + ylab("GDP per capita") + 
  scale_fill_gradient(low=main2_color, high=main1_color) + 
  theme(axis.text = element_text(size=11)) + theme(axis.title = element_text(size = 12))

# 5 (Heatmap based on rectangles)
ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  geom_bin2d(bins=10, alpha =0.7) + xlab("happiness score") + ylab("GDP per capita") + 
  scale_fill_gradient(low=main2_color, high=main1_color) + 
  theme(axis.text = element_text(size=11)) + theme(axis.title = element_text(size = 12)) 

# 6 (Density estimation with contours)
ggplot(Happy_world, aes(x=Happiness.Score, y=Economy..GDP.per.Capita.)) + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon",  colour="white") + 
  xlab("happiness score") + ylab("GDP per capita") + 
  scale_fill_gradient(low=main2_color, high=main1_color) + 
  theme(axis.text = element_text(size=11)) + theme(axis.title = element_text(size = 12))



