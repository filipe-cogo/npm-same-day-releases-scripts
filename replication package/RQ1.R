set.seed(21)

options(scipen = 999)

library(data.table)
library(doParallel)
library(ggplot2)
library(gridExtra)
library(scales)
library(lubridate)
library(stats)
library(rms)
library(randomForest)
library(xtable)
library(viridis)

#############################################
### Read CSV dataset
#############################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-data"
setwd(wd)

dependencies = fread("popular-packages-dependencies.csv")
releases = fread("popular-packages-releases.csv")

#####################################################################
### RQ.1. How often do same-day releases occur on npm?
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")

#######################################
### Functions
#######################################

sample.size = function(c.lev, margin=.5, c.interval=.05, population) {
  z.val  = qnorm(.5+c.lev/200)
  ss     = (z.val^2 * margin * (1-margin))/c.interval^2
  p.ss   = round((ss/(1 + ((ss-1)/population))), digits=0)
  METHOD = paste("Recommended sample size for a population of ",  population,
                 " at a ", c.lev, "% confidence level", sep = "")
  moe    = paste((c.interval*100), "%", sep="")
  resp.dist = paste((margin*100),"%", sep="")
  structure(list(Population = population, "Confidence level" = c.lev, 
                 "Margin of error" = moe, "Response distribution" = resp.dist,
                 "Recommended sample size" = p.ss, method = METHOD), class = "power.htest")
  return(p.ss)
}

same_day_releases_pattern_1_count = function(releases){
  # aggregate by client releases
  sdr1.clients = releases[, .(.N), by = .(package_name, package_version_num_1, package_version_num_2, package_version_timestamp_1, package_version_timestamp_2, same_day_release = same_day_release_1)]
  sdr1.clients = sdr1.clients[, N := NULL]
  
  # count the number of releases per client
  sdr1.clients = sdr1.clients[, .(num_releases = .N), by = .(package_name, same_day_release)]
  
  # dcast table to easily calculate proportions
  sdr1.clients = data.table::dcast(sdr1.clients, formula = package_name ~ ..., value.var = "num_releases", fill = 0)
  #adjust names
  names(sdr1.clients) = c("package_name", "num_regular_releases", "num_same_day_releases")
  
  #calculate proportions
  sdr1.clients$total_releases = sdr1.clients$num_same_day_releases + sdr1.clients$num_regular_releases
  sdr1.clients$proportion_same_day_releases = sdr1.clients$num_same_day_releases / (sdr1.clients$num_same_day_releases + sdr1.clients$num_regular_releases)
  
  return(sdr1.clients)
}

### Proportion of provider-driven same-day releases - clients ###
same_day_releases_pattern_2.clients = function(dependencies){
  #get releases that have a provider with less than 
  #24 hours difference from the client
  clients_releases = dependencies[,.GRP,by = .(package_name = client_name, 
                                             release_order, 
                                             package_version_num_1 = client_version_num_1, 
                                             package_version_num_2 = client_version_num_2,
                                             package_version_change_was_in = client_version_change_was_in,
                                             package_version_timestamp_1 = client_version_timestamp_1,
                                             package_version_timestamp_2 = client_version_timestamp_2,
                                             package_version_timestamp_diff_secs = client_version_timestamp_diff_secs,
                                             same_day_release_2
                                             )]
  clients_releases[, GRP := NULL]
  
  #find rows that are exclusively regular releases
  reg_releases_2 = dcast(clients_releases, ... ~ same_day_release_2, value.var = "same_day_release_2")[`FALSE` == FALSE & is.na(`TRUE`), .(package_name, 
                                                                                                                                           release_order,
                                                                                                                                           package_version_num_1, 
                                                                                                                                           package_version_num_2, 
                                                                                                                                           package_version_change_was_in,
                                                                                                                                           package_version_timestamp_1,
                                                                                                                                           package_version_timestamp_2,
                                                                                                                                           same_day_release_2 = `FALSE`)]
  #find rows that are same_day_release_2
  sdr_releases_2 = dcast(clients_releases, ... ~ same_day_release_2, value.var = "same_day_release_2")[`TRUE` == TRUE, .(package_name, 
                                                                                                                        release_order,
                                                                                                                        package_version_num_1, 
                                                                                                                        package_version_num_2, 
                                                                                                                        package_version_change_was_in,
                                                                                                                        package_version_timestamp_1,
                                                                                                                        package_version_timestamp_2,
                                                                                                                        same_day_release_2 = `TRUE`)]
  stopifnot(nrow(dependencies[,.GRP,by=.(client_name, release_order)]) ==  nrow(reg_releases_2) + nrow(sdr_releases_2))
  
  return(rbind(reg_releases_2,sdr_releases_2))
}

same_day_releases_pattern_2_clients_count = function(dependencies){
  # aggregate provider releases
  sdr2.clients = same_day_releases_pattern_2.clients(dependencies)
  sdr2.clients = sdr2.clients[, .(.N), by = .(package_name, package_version_num_1, package_version_num_2, package_version_timestamp_1, package_version_timestamp_2, same_day_release = same_day_release_2)]
  sdr2.clients[, N := NULL]
  
  # count the number of releases per client
  sdr2.clients = sdr2.clients[, .(num_releases = .N), by = .(package_name, same_day_release)]
  
  # dcast table to easily calculate proportions
  sdr2.clients = data.table::dcast(sdr2.clients, formula = package_name ~ ..., value.var = "num_releases", fill = 0)
  #adjust names
  names(sdr2.clients) = c("package_name", "num_regular_releases", "num_same_day_releases")
  
  #calculate proportions
  sdr2.clients$total_releases = sdr2.clients$num_same_day_releases + sdr2.clients$num_regular_releases
  sdr2.clients$proportion_same_day_releases = sdr2.clients$num_same_day_releases / (sdr2.clients$num_same_day_releases + sdr2.clients$num_regular_releases)
  
  return (sdr2.clients)
}

########################################################
### RQ1.1. What is the proportion of same-day releases?
########################################################

sdr1.clients = same_day_releases_pattern_1_count(releases)[num_same_day_releases > 0 & ((num_regular_releases + num_same_day_releases) >= 10)]
sdr2.clients = same_day_releases_pattern_2_clients_count(dependencies)[num_same_day_releases > 0 & ((num_regular_releases + num_same_day_releases) >= 10)]

summary(sdr1.clients)
summary(sdr2.clients)

### Prepare data for plot ###
sdr1.boxplot = sdr1.clients[, .(package_name, proportion_same_day_releases, ratio_regular_same_day_release = (num_regular_releases/num_same_day_releases), type = "Self-driven same-day releases")]
sdr2.boxplot = sdr2.clients[, .(package_name, proportion_same_day_releases, ratio_regular_same_day_release = (num_regular_releases/num_same_day_releases), type = "Provider-driven same-day releases")]
sdr.boxplot = rbind(sdr1.boxplot, sdr2.boxplot)

summary(sdr1.boxplot)
summary(sdr2.boxplot)

#### Plot the scatterplot for the proportions and the total of same-day releases
pdf(file = "RQ1/images/dist_packages_same_day.pdf")
p0 = ggplot(sdr1.clients, aes(x = proportion_same_day_releases, y = total_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  scale_x_continuous(name = "% same-day releases", labels = percent) +
  scale_y_log10(name = "# releases", labels = comma) +
  geom_hline(yintercept = median(sdr1.clients$total_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(sdr1.clients$proportion_same_day_releases), linetype = "dashed", colour = "red", size = 1) +
  guides(fill=FALSE) +
  scale_colour_viridis(guide = 'none') +
  ggtitle("Self-driven") +
  labs(caption="(b)") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5), plot.caption = element_text(size = 14, hjust = 0.5, face = "bold", family = "serif"), axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 13), axis.title = element_text(size = 14))

p1 = ggplot(sdr2.clients, aes(x = proportion_same_day_releases, y = total_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  scale_x_continuous(name = "% same-day releases", labels = percent) +
  scale_y_log10(name = "# releases", labels = comma) +
  geom_hline(yintercept = median(sdr2.clients$total_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(sdr2.clients$proportion_same_day_releases), linetype = "dashed", colour = "red", size = 1) +
  guides(fill=FALSE) +
  scale_colour_viridis(guide = 'none') +
  ggtitle("Provider-driven") +
  labs(caption="(c)") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5), plot.caption = element_text(size = 14, hjust = 0.5, face = "bold", family = "serif"), axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 13), axis.title = element_text(size = 14))

p3 = ggplot(sdr.boxplot, aes(x = type, y = proportion_same_day_releases, fill = type)) +
  geom_boxplot() +
  scale_x_discrete(name="", 
                   breaks = c("Self-driven same-day releases", "Provider-driven same-day releases"), 
                   labels=c("Self-driven\nreleases", "Provider-driven\nreleases")) +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(name = "% same-day releases",
                     labels = percent) +
  labs(caption="(a)") +
  coord_flip() +
  theme_bw() +
  theme(plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text = element_text(size = 13), axis.title = element_text(size = 14), plot.title = element_text(hjust = 1.0))

grid.arrange(p0, p1, p3, layout_matrix = rbind(c(3,3), c(0,1)))
dev.off()

#packages over the medians performing self-driven same-day release
median_num_releases_1 = median(sdr1.clients$num_regular_releases + sdr1.clients$num_same_day_releases)
median_proportion_sd_releases_1 = median(sdr1.clients$proportion_same_day_releases)
packages_same_day_release_1_above_median = sdr1.clients[num_regular_releases >= median_num_releases_1 & proportion_same_day_releases >= median_proportion_sd_releases_1]

#packages over the mean performing self-driven same-day release
mean_num_releases_1 = mean(sdr1.clients$total_releases)
mean_proportion_sd_releases_1 = mean(sdr1.clients$proportion_same_day_releases)
packages_same_day_release_1_above_mean = sdr1.clients[num_regular_releases >= mean_num_releases_1 & proportion_same_day_releases >= mean_proportion_sd_releases_1]

paste("The percentage of packages publishing self-driven same-day releases that are above the median number of releases and the median proportion of same-day release is", percent(nrow(packages_same_day_release_1_above_median)/nrow(sdr1.clients)))

#packages over the medians performing provider-driven same-day release
median_num_releases_2 = median(sdr2.clients$num_regular_releases + sdr2.clients$num_same_day_releases)
median_proportion_sd_releases_2 = median(sdr2.clients$proportion_same_day_releases)
packages_same_day_release_2_above_median = sdr2.clients[num_regular_releases >= median_num_releases_2 & proportion_same_day_releases >= median_proportion_sd_releases_2]

#packages over the mean performing provider-driven same-day release
mean_num_releases_2 = mean(sdr2.clients$total_releases)
mean_proportion_sd_releases_2 = mean(sdr2.clients$proportion_same_day_releases)
packages_same_day_release_2_above_mean = sdr2.clients[num_regular_releases >= mean_num_releases_2 & proportion_same_day_releases >= mean_proportion_sd_releases_2]

paste("The percentage of packages publishing provider-driven same-day releases that are above the median number of releases and the median proportion of same-day release is", percent(nrow(packages_same_day_release_2_above_median)/nrow(sdr2.clients)))

#compare mean and median
median(sdr1.clients$proportion_same_day_releases)
mean(sdr1.clients$proportion_same_day_releases)

#compare mean and median
median(sdr2.clients$proportion_same_day_releases)
mean(sdr2.clients$proportion_same_day_releases)

########################################################
### RQ2.1. Top packages performing same-day releases
########################################################

sdr1.clients = same_day_releases_pattern_1_count(releases)
sdr2.clients = same_day_releases_pattern_2_clients_count(dependencies)

### For self-driven same-day releases (pattern 1) ###

#count the number of clients for each package publishing self-driven same-day release
sdr1.clients = merge(sdr1.clients, dependencies[dependency_name %in% sdr1.clients$package_name, .GRP, by = .(client_name, dependency_name)][, .(num_clients = .N), by = .(package_name = dependency_name)])

#sort by total number of same-day releases
top_sdr1_sort_num_sdr = sdr1.clients[order(-num_same_day_releases)][1:100]

ggplot(top_sdr1_sort_num_sdr, aes(x = proportion_same_day_releases, y = total_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  geom_text_repel(aes(proportion_same_day_releases, total_releases, label = top_sdr1_sort_num_sdt$package_name), force = 3, segment.alpha = 0.3, size = 4.5) +
  scale_x_continuous(name = "Proportion of self-driven\nsame-day releases", label = percent) +
  scale_y_log10(name = "Total number of releases") +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  ggtitle("Self-driven same-day releases") +
  #labs(caption = "(a)") +
  theme_bw() +
  theme(legend.position = "none", title = element_text(hjust = 0.5), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.7), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

#sort by proportion of same-day releases
top_sdr1_sort_prop_sdr = sdr1.clients[order(-proportion_same_day_releases)][1:100]

ggplot(top_sdr1_sort_prop_sdr, aes(x = proportion_same_day_releases, y = total_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  geom_text_repel(aes(proportion_same_day_releases, total_releases, label = top_sdr1_sort_prop_sdr$package_name), force = 3, segment.alpha = 0.3, size = 4.5) +
  scale_x_continuous(name = "Proportion of self-driven\nsame-day releases", label = percent) +
  scale_y_log10(name = "Total number of releases") +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  ggtitle("Self-driven same-day releases") +
  #labs(caption = "(b)") +
  theme_bw() +
  theme(legend.position = "none", title = element_text(hjust = 0.5), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.7), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))



### For provider-driven same-day releases (pattern 2) ###

# get top 100 packages publishing same-day releases, sorted by total of same-day releases
top_sdr2_sort_num_sdr = sdr2.clients[order(-num_same_day_releases)][1:100]

ggplot(top_sdr2_sort_num_sdr, aes(x = proportion_same_day_releases, y = total_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  geom_text_repel(aes(proportion_same_day_releases, total_releases, label = top_sdr2_sort_num_sdr$package_name), force = 3, segment.alpha = 0.3, size = 4.5) +
  scale_x_continuous(name = "Proportion of self-driven\nsame-day releases", label = percent) +
  scale_y_log10(name = "Total number of releases") +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  ggtitle("Self-driven same-day releases") +
  #labs(caption = "(a)") +
  theme_bw() +
  theme(legend.position = "none", title = element_text(hjust = 0.5), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.7), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))


# get top 10 packages publishing same-day releases, sorted by proportion of same-day releases
top_sdr2_sort_prop_sdr = sdr2.clients[order(-proportion_same_day_releases)][1:100]

ggplot(top_sdr2_sort_prop_sdr, aes(x = proportion_same_day_releases, y = total_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  geom_text_repel(aes(proportion_same_day_releases, total_releases, label = top_sdr2_sort_prop_sdr$package_name), force = 3, segment.alpha = 0.3, size = 4.5) +
  scale_x_continuous(name = "Proportion of self-driven\nsame-day releases", label = percent) +
  scale_y_log10(name = "Total number of releases") +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  ggtitle("Self-driven same-day releases") +
  #labs(caption = "(b)") +
  theme_bw() +
  theme(legend.position = "none", title = element_text(hjust = 0.5), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.7), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

