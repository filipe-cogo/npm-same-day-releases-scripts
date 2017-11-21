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
### RQ.1. What are the characteristics of same-day releases on npm ?
### RQ1.2. Adoption of voluntary same-day releases
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")

same_day_releases_pattern_1 = releases[same_day_release_1 == TRUE]

####################################################
### Boxplot with adoption of vonluntary same-day releases
#####################################################

# count how many client relases uses same-day releases (pattern 1)
clients_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_ptrn1(dependencies, same_day_releases_pattern_1)
clients_releases_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[package_pttnr1_version_change == "upgrade"][difftime(package_pttnr1_timestamp_max_satisf_2, client_version_timestamp_2) < 86400]
clients_releases_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[, .(.GRP), by = .(package_pttnr1, package_pttnr1_version_num_1, package_pttnr1_version_num_2, client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2, client_versioning_change)]

# print results
print(paste("There are", nrow(clients_releases_using_same_day_release_ptrn1), "adoptions of same-day releases on pattern 1."))
print(paste("There are", nrow(clients_releases_using_same_day_release_ptrn1[, .(.N), by = client_name]), "different client packages using same-day releases on pattern 1. This is", (nrow(clients_releases_using_same_day_release_ptrn1[, .(.GRP), by = client_name])/nrow(dependencies[,.(.GRP),by = client_name]))*100, "percent of the total packages"))

# count how many times each client used a same-day release pattern1
clients_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[, .(count = .N), by = .(client = client_name)] #ready for plot! 

#TODO: the number of times each client used a same-day release depends on the number of providers this client has
#TODO: the line above returns the clients using same-day releases
#clients_using_same_day_release_ptrn1[order(-count)]
#TODO: the line above returns the number of provider each client has
#nrow(dependencies[client_name == c, .(number_providers = .N), by = dependency_name])

# check the distribution of same-day releases (pattern 1) usage per month
# count how many times each client used a same-day release pattern1
clients_using_same_day_release_ptrn1_per_month = clients_releases_using_same_day_release_ptrn1[, .(count = .N), by = .(client_name, client_timestamp_month = floor_date(ymd_hms(client_version_timestamp_1), "month"))]

total_clients_using_same_day_release_ptrn1_per_month = clients_using_same_day_release_ptrn1_per_month[, .(client_using_sdr1 = sum(count)), by = client_timestamp_month]

# plot the boxplot
pdf(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.pdf")
png(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.png")
p0 = ggplot(clients_using_same_day_release_ptrn1, aes(x = "", y = count), fill = change) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  scale_y_log10(name = "# voluntary same-day releases\nadoption per client",
                labels = comma) +
  #ggtitle("Distribution of same-day releases usage by client") +
  coord_flip() +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
p1 = ggplot(total_clients_using_same_day_release_ptrn1_per_month[client_timestamp_month > "2010-12-01" & client_timestamp_month < "2017-05-01"]) +
  geom_line(aes(ymd(client_timestamp_month), client_using_sdr1)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("# clients adopting\nvoluntary same-day release") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
grid.arrange(p1, p0, ncol = 1)
dev.off()





####################################################
### How do vonluntary same-day releases are being used after adoption?
#####################################################

### Examine client releases after the adoption of a same-day release pattern 1 ###
clients_sd1 = client_releases_changing_from_same_day_release_ptrn1(dependencies, same_day_releases_pattern_1)
clients_sd1 = clients_sd1[, .(count = .N), by = .(client_name, package_pttnr1_version_change)]

clients_sd1.cast = data.table::dcast(formula = client_name ~ package_pttnr1_version_change, data = clients_sd1, fun.aggregate = sum, value.var = "count")

clients_sd1.cast$no_change_prop = clients_sd1.cast$no_change / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)
clients_sd1.cast$upgrade_prop = clients_sd1.cast$upgrade / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)
clients_sd1.cast$rollback_prop = clients_sd1.cast$rollback / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)

clients_sd1.cast[, no_change := NULL]
clients_sd1.cast[, upgrade := NULL]
clients_sd1.cast[, rollback := NULL]

clients_sd1.melt = melt(clients_sd1.cast, id = "client_name")

pdf(file = "RQ1/images/after_sd1_adoption.pdf")
png(file = "RQ1/images/after_sd1_adoption.png")
ggplot(clients_sd1.melt, aes(x = "", y = value, fill = variable)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Proportion of version change per client",
                     labels = comma) +
  #ggtitle("Distribution of same-day releases usage by client") +
  scale_fill_grey(name="",
                  breaks=c("no_change_prop", "upgrade_prop", "rollback_prop"),
                  labels=c("No change", "Upgrade", "Downgrade"),
                  start = 0.5, end = 1.0) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
dev.off()



### How many times each client used a same-day 
### release pattern1 per month?
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
