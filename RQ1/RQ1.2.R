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
library(beanplot)
library(viridis)

#############################################
### Read CSV dataset
#############################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-data"
setwd(wd)

dependencies = fread("popular-packages-dependencies.csv")
releases = fread("popular-packages-releases.csv")

#####################################################################
### RQ.1. What are the characteristics of same-day releases on npm ?
### RQ1.2. Adoption of voluntary same-day releases
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")
source("RQ1/iPlot4R.R")


same_day_releases_pattern_1 = releases[same_day_release_1 == TRUE]
same_day_releases_pattern_2 = dependencies[same_day_release_2 == TRUE]


####################################################
### Violinplot with time to update to same-day releases vs.
### time to update to a regular release
#####################################################

# calculate client releases using a same-day release pattern 1
client_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_ptrn1(dependencies, releases[same_day_release_1 == TRUE])
# calculate client releases using a regular release
client_releases_using_regular_releases = client_releases_using_same_day_ptrn1(dependencies, releases[same_day_release_1 == FALSE])

# filter only the upgrades for a same-day and regular release
client_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_release_ptrn1[package_pttnr1_version_change == "upgrade"]
client_releases_using_regular_releases = client_releases_using_regular_releases[package_pttnr1_version_change == "upgrade"]

# calculate the time for upgrade to a same-day release
time_update_same_day_release_1 = client_releases_using_same_day_release_ptrn1[package_pttnr1 != client_name, .(package_pttnr1, package_pttnr1_version_num_1, package_pttnr1_version_num_2, client_name, client_version_num_1, client_version_num_2, adotpion_time = as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(package_pttnr1_timestamp_max_satisf_2), units = "hours")), package_pttnr1_version_change_was_in = dependency_version_change_was_in)]
time_update_regular_release = client_releases_using_regular_releases[package_pttnr1 != client_name, .(package_pttnr1, package_pttnr1_version_num_1, package_pttnr1_version_num_2, client_name, client_version_num_1, client_version_num_2, adotpion_time = as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(package_pttnr1_timestamp_max_satisf_2), units = "hours")), package_pttnr1_version_change_was_in = dependency_version_change_was_in)]

# bind rows to put in a format to plot
time_update_same_day_release_1$type = "sd"
time_update_regular_release$type = "reg"
time_update = rbind(time_update_same_day_release_1, time_update_regular_release)
time_update$jack = "anything"

summary(time_update_same_day_release_1$adotpion_time)
summary(time_update_regular_release$adotpion_time)

#plot a violin of the two distributions
pdf(file = "RQ1/images/clients_adopting_same_day_releases_ptrn_1.pdf")
png(file = "RQ1/images/clients_adopting_same_day_releases_ptrn_1.png")
plot_violin(time_update,
            xcol = "jack",
            xlab = "", 
            xticklab = c("Regular releases vs. Same-day release"), 
            ylab = "Time to adoption (hours)", 
            ycol = "adotpion_time", 
            groupcol = "type",
            split = TRUE, 
            transformation = "log10", 
            show_legend = FALSE,
            coloured = TRUE,
            fontsize = 12)
dev.off()

#plot a violin of the two distributions separated by major, minor, patch ...
pdf(file = "RQ1/images/clients_adopting_same_day_releases_ptrn_1_divided_by_sv_change_level.pdf")
png(file = "RQ1/images/clients_adopting_same_day_releases_ptrn_1_divided_by_sv_change_level.png")
plot_violin(time_update,
            xcol = "package_pttnr1_version_change_was_in",
            xlab = "", 
            xticklab = c("Minor", "Patch", "Major", "Pre-release", "Build"), 
            ylab = "Time to adoption (hours)", 
            ycol = "adotpion_time", 
            groupcol = "type",
            split = TRUE, 
            transformation = "log10", 
            show_legend = TRUE,
            legend_title = "Release\ntype",
            legend_labels = c("Regular", "Same-day"),
            coloured = TRUE,
            fontsize = 12)
dev.off()


#check if difference in mean is statistical significant
wilcox.test(time_update_same_day_release_1$adotpion_time, time_update_regular_release$adotpion_time, paired = FALSE, alternative = "less")
?wilcox.test

pdf(file = "RQ1/images/clients_upgrading_to_same_day_releases_ptrn_1.pdf")
png(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.png")
ggplot(time_update, aes(x = "", y = adotpion_time, fill = type)) +
  geom_boxplot() +
  scale_x_discrete(name="", 
                   breaks = c("sd", "reg"), 
                   labels=c("Same-day\nreleases", "regular\nreleases")) +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_log10(name = "Time to upgrade to a new release (hours)",
                labels = comma) +
  #ggtitle("Distribution of same-day releases usage by client") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()


####################################################
### Boxplot with % of client releases upgrading to vonluntary same-day releases
### % is calculated based on the number of upgrade, no change and rollback
#####################################################

# # calculate how many client releases used a same_day release pattern 1
# clients_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_ptrn1(dependencies, same_day_releases_pattern_1)
# # calculate how many clients used a same_day release pattern 1
# clients_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[, .(.GRP), by = .(package_pttnr1, package_pttnr1_version_num_1, package_pttnr1_version_num_2, package_pttnr1_timestamp_max_satisf_2, package_pttnr1_version_change, client_name, client_version_timestamp_2)]
# clients_using_same_day_release_ptrn1 = clients_using_same_day_release_ptrn1[, .(num_clients_per_sd_release_ptrn1 = .N), by = .(package_pttnr1, package_pttnr1_version_release_version, package_pttnr1_timestamp_max_satisf_2, package_pttnr1_version_change)]
# 
# # format to calculate proportions
# proportion_cliets_upgrading_to_sdr1 = data.table::dcast(clients_using_same_day_release_ptrn1, package_pttnr1 + package_pttnr1_version_release_version + package_pttnr1_timestamp_max_satisf_2 ~ ..., fun.aggregate = sum, fill = 0, value.var = "num_clients_per_sd_release_ptrn1")
# proportion_cliets_upgrading_to_sdr1[, proportion_ugrades := upgrade / (upgrade + no_change + rollback)]
# summary(proportion_cliets_upgrading_to_sdr1$proportion_ugrades)
# 
# pdf(file = "RQ1/images/clients_upgrading_to_same_day_releases_ptrn_1.pdf")
# png(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.png")
# ggplot(proportion_cliets_upgrading_to_sdr1, aes(x = "", y = proportion_ugrades)) +
#   geom_boxplot() +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "Percentage of clients upgrading\nto a self-driven same-day release",
#                 labels = comma) +
#   #ggtitle("Distribution of same-day releases usage by client") +
#   coord_flip() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
# dev.off()





####################################################
### Boxplot with adoption of vonluntary same-day releases
#####################################################

# count how many client relases uses same-day releases (pattern 1)
# clients_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_ptrn1(dependencies, same_day_releases_pattern_1)
# clients_releases_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[package_pttnr1_version_change == "upgrade"][difftime(package_pttnr1_timestamp_max_satisf_2, client_version_timestamp_2) < 86400]
# clients_releases_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[, .(.GRP), by = .(package_pttnr1, package_pttnr1_version_num_1, package_pttnr1_version_num_2, client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2, client_versioning_change)]
# 
# # print results
# print(paste("There are", nrow(clients_releases_using_same_day_release_ptrn1), "adoptions of same-day releases on pattern 1."))
# print(paste("There are", nrow(clients_releases_using_same_day_release_ptrn1[, .(.N), by = client_name]), "different client packages using same-day releases on pattern 1. This is", (nrow(clients_releases_using_same_day_release_ptrn1[, .(.GRP), by = client_name])/nrow(dependencies[,.(.GRP),by = client_name]))*100, "percent of the total packages"))
# 
# # count how many times each client used a same-day release pattern1
# clients_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[, .(count = .N), by = .(client = client_name)] #ready for plot! 
# 
# #TODO: the number of times each client used a same-day release depends on the number of providers this client has
# #TODO: the line below returns the clients using same-day releases
# #clients_using_same_day_release_ptrn1[order(-count)]
# #TODO: the line velow returns the number of provider each client has
# #nrow(dependencies[client_name == c, .(number_providers = .N), by = dependency_name])
# 
# # check the distribution of same-day releases (pattern 1) usage per month
# # count how many times each client used a same-day release pattern1
# clients_using_same_day_release_ptrn1_per_month = clients_releases_using_same_day_release_ptrn1[, .(count = .N), by = .(client_name, client_timestamp_month = floor_date(ymd_hms(client_version_timestamp_1), "month"))]
# 
# total_clients_using_same_day_release_ptrn1_per_month = clients_using_same_day_release_ptrn1_per_month[, .(client_using_sdr1 = sum(count)), by = client_timestamp_month]
# 
# # plot the boxplot
# pdf(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.pdf")
# png(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.png")
# p0 = ggplot(clients_using_same_day_release_ptrn1, aes(x = "", y = count), fill = change) +
#   geom_boxplot() +
#   scale_x_discrete(name = "") +
#   scale_y_log10(name = "# voluntary same-day releases\nadoption per client",
#                 labels = comma) +
#   #ggtitle("Distribution of same-day releases usage by client") +
#   coord_flip() +
#   
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
# p1 = ggplot(total_clients_using_same_day_release_ptrn1_per_month[client_timestamp_month > "2010-12-01" & client_timestamp_month < "2017-05-01"]) +
#   geom_line(aes(ymd(client_timestamp_month), client_using_sdr1)) +
#   scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
#   xlab("") +
#   ylab("# clients adopting\nvoluntary same-day release") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
# grid.arrange(p1, p0, ncol = 1)
# dev.off()





####################################################
### How do vonluntary same-day releases are being used after adoption?
#####################################################

### Examine client releases after the adoption of a same-day release pattern 1 ###
clients_sd1 = client_releases_changing_from_same_day_release_ptrn1(dependencies, same_day_releases_pattern_1)
#the below line should return 0 rows if it's all right
#clients_sd1[difftime(ymd_hms(client_timestamp_2), ymd_hms(package_pttnr1_timestamp_2)) < 0]
clients_sd1 = clients_sd1[, .(count = .N), by = .(client_name, package_pttnr1_version_change)]

clients_sd1.cast = data.table::dcast(formula = client_name ~ package_pttnr1_version_change, data = clients_sd1, fun.aggregate = sum, value.var = "count")

clients_sd1.cast$no_change_prop = clients_sd1.cast$no_change / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)
clients_sd1.cast$upgrade_prop = clients_sd1.cast$upgrade / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)
clients_sd1.cast$rollback_prop = clients_sd1.cast$rollback / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)

clients_sd1.cast[, no_change := NULL]
clients_sd1.cast[, upgrade := NULL]
clients_sd1.cast[, rollback := NULL]

clients_sd1.melt = melt(clients_sd1.cast, id = "client_name")
clients_sd1.melt[, change_or_not := ifelse(variable == "no_change_prop", "Resolved version doesn't change", "Resolved version changes")]

pdf(file = "RQ1/images/after_sd1_adoption.pdf")
png(file = "RQ1/images/after_sd1_adoption.png")
ggplot(clients_sd1.melt, aes(x = "", y = value, fill = variable)) +
  geom_boxplot() +
  facet_grid( ~ change_or_not, scales="free") +
  scale_y_continuous(name = "Proportion of resolution type",
                     labels = comma) +
  xlab("") +
  #scale_x_discrete() +
  #guides(fill=FALSE) +
  scale_fill_viridis(name="",
                     breaks=c("no_change_prop", "upgrade_prop", "rollback_prop"),
                     labels=c("No change", "Upgrade", "Downgrade"),
                     discrete = TRUE) +
  #ggtitle("Distribution of same-day releases usage by client") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
 dev.off()


####################################################
### How many times each client used a same-day 
### release pattern1 per month?
####################################################

clients_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_ptrn1(dependencies, same_day_releases_pattern_1)
snaps.pt1 = clients_releases_using_same_day_release_ptrn1[, .(count = .N), by = timestamp]

#pdf(file = "RQ1/images/monthly_snaps_total.pdf")
png(file = "RQ1/images/client_releases_using_same_day_monthly_snaps_total.png")
ggplot(snaps.pt1[timestamp < "2017-05-01"], aes(ymd(timestamp), count)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("12 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("Client releases using same-day updates") +
  theme_bw()
dev.off()
