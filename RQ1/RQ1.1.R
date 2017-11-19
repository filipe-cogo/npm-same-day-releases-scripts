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
### RQ1.1. How often do same-day releases occur on npm?
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")

####################################################
# Calculate number of same-day and regular releases per package
#####################################################

### Proportion of voluntary same-day releases (pattern 1) ###

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
  sdr1.clients$proportion_same_day_releses = sdr1.clients$num_same_day_releases / (sdr1.clients$num_same_day_releases + sdr1.clients$num_regular_releases)
  
  return(sdr1.clients)
}



### Proportion of triggered same-day releases (pattern 2) - clients ###

same_day_releases_pattern_2_clients_count = function(dependencies){
  # aggregate by provider releases
  sdr2.clients = dependencies[, .(.N), by = .(package_name = client_name, package_version_num_1 = client_version_num_1, package_version_num_2 = client_version_num_2, package_version_timestamp_1 = client_version_timestamp_1, package_version_timestamp_2 = client_version_timestamp_2, same_day_release = same_day_release_2)]
  sdr2.clients = sdr2.clients[, .(.N), by = .(package_name, package_version_num_1, package_version_num_2, package_version_timestamp_1, package_version_timestamp_2, same_day_release)]
  sdr2.clients[, N := NULL]
  
  # count the number of releases per client
  sdr2.clients = sdr2.clients[, .(num_releases = .N), by = .(package_name, same_day_release)]
  
  # dcast table to easily calculate proportions
  sdr2.clients = data.table::dcast(sdr2.clients, formula = package_name ~ ..., value.var = "num_releases", fill = 0)
  #adjust names
  names(sdr2.clients) = c("package_name", "num_regular_releases", "num_same_day_releases")
  
  #calculate proportions
  sdr2.clients$proportion_same_day_releses = sdr2.clients$num_same_day_releases / (sdr2.clients$num_same_day_releases + sdr2.clients$num_regular_releases)
  
  return (sdr2.clients)
}



####################################################
# Boxplot showing the distribution of different patterns of same-day releases
#####################################################

sdr1.clients = same_day_releases_pattern_1_count(releases)
sdr2.clients = same_day_releases_pattern_2_clients_count(dependencies)

### Prepare data for plot ###

sdr1.boxplot = sdr1.clients[, .(package_name, proportion_same_day_releses, num_same_day_releases, type = "Voluntary same-day releases")]
sdr2.boxplot = sdr2.clients[, .(package_name, proportion_same_day_releses, num_same_day_releases, type = "Triggered same-day releases")]
sdr.boxplot = rbind(sdr1.boxplot, sdr2.boxplot)

#### Plot the boxplot for the proportions and the total of same-day releases
pdf(file = "RQ1/images/dist_packages_same_day.pdf")
png(file = "RQ1/images/dist_packages_same_day.png")
p0 = ggplot(sdr.boxplot, aes(x = type, y = proportion_same_day_releses, fill = type)) +
  geom_boxplot(position=position_dodge(1)) +
  #geom_violin(position=position_dodge(1)) +
  #scale_fill_brewer(name="Same-day release type",
  #                  labels=c("Voluntary", "Triggered"),
  #                  palette="RdBu") +
  scale_x_discrete(name = "", labels = c("Voluntary releases", "Triggered releases")) +
  scale_y_continuous(name = "# same-day releases / # releases",
                     labels = comma) +
  #stat_summary(fun.y="median", geom="point", position = position_dodge(1)) +
  #ggtitle("Proportion of packages publishing\nsame-day releases") +
  #scale_color_viridis() +
  scale_fill_grey(name="Same-day \nrelease type",
                  breaks=c("sd1", "sd2"),
                  labels=c("Voluntary", "Triggered"),
                  guide=FALSE,
                  start = 0.65, end = 1.0) +
  theme(legend.position="right", plot.title = element_text(hjust = 1.0)) +
  theme_bw()

p1 = ggplot(sdr.boxplot, aes(x = type, y = num_same_day_releases, fill = type)) +
  geom_boxplot() +
  scale_x_discrete(name = "", labels = c("Voluntary releases", "Triggered releases")) +
  scale_y_continuous(name = "# same-day releases",
                labels = comma) +
  #ggtitle("Number of packages publishing\nsame-day releases") +
  #scale_fill_brewer(name="Release type",
  #                  #breaks=c("same_day_1", "same_day_2"),
  #                  labels=c("Same-day (Pattern 1)", "Triggered (Pattern 2)"),
  #                  palette="RdBu") +
  scale_fill_grey(name="Same-day \nrelease type",
                  breaks=c("sd1", "sd2"),
                  labels=c("Voluntary", "Triggered"),
                  guide=FALSE,
                  start = 0.65, end = 1.0) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="right") +
  theme_bw()

grid.arrange(p0, p1, ncol = 2)
dev.off()


####################################################
# Top-10 packages performing same-day releases, on both patterrns
#####################################################

sdr1.clients = same_day_releases_pattern_1_count(releases)
sdr2.clients = same_day_releases_pattern_2_clients_count(dependencies)

### For voluntary same-day releases (pattern 1) ###

# get top 10 packages publishing same-day releases
sdr1.clients.top10 = sdr1.clients[order(-num_same_day_releases)][1:10]

### export to latex
# create xtable
sdr1.xt = xtable(sdr1.clients.top10, caption = "Proportion of voluntary same-day releases")
# adjust header names
names(sdr1.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of regular releases}", "\\pbox{20cm}{Number of same-day releases}", "\\pbox{20cm}{Proportion of same-day releases}")
# adjust significant digits
digits(sdr1.xt) <- c(0,0,0,0,3)
# print to file
print(sdr1.xt, sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion/top_10_clients_publishing_same_day_releases_proportion_pt1.tex")

### For triggered same-day releases (pattern 2) ###

# get top 10 packages publishing same-day releases
sdr2.clients.top10 = sdr2.clients[order(-num_same_day_releases)][1:10]

### export to latex
# create xtable
sdr2.xt = xtable(sdr2.clients.top10, caption = "Proportion of triggered same-day releases")
# adjust header names
names(sdr2.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of regular releases}", "\\pbox{20cm}{Number of same-day releases}", "\\pbox{20cm}{Proportion of same-day releases}")
# adjust significant digits
digits(sdr2.xt) <- c(0,0,0,0,3)
# print to file
print(sdr2.xt, sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion/top_10_clients_publishing_same_day_releases_proportion_pt2.tex")



####################################################
# Same-day releases along the time
#####################################################

# aggregate voluntary same-day releases (pattern 1) by client releases
sdr1.times = releases[, .(.N), by = .(package_name, package_version_num_1, package_version_num_2, package_version_timestamp_1, package_version_timestamp_2, same_day_release = same_day_release_1)]
sdr1.times = sdr1.times[, N := NULL]

# aggregate triggered same-day releases (pattern 2) by client releases
sdr2.times = dependencies[, .(.N), by = .(package_name = client_name, package_version_num_1 = client_version_num_1, package_version_num_2 = client_version_num_2, package_version_timestamp_1 = client_version_timestamp_1, package_version_timestamp_2 = client_version_timestamp_2, same_day_release = same_day_release_2)]
sdr2.times = sdr2.times[, .(.N), by = .(package_name, package_version_num_1, package_version_num_2, package_version_timestamp_1, package_version_timestamp_2, same_day_release)]
sdr2.times[, N := NULL]

# aggregate voluntary same-day releases by month
sdr1.month_snaps = sdr1.times[, .(num_releases = .N), by = .(pkg_name = package_name, same_day_release, timestamp = floor_date(ymd_hms(package_version_timestamp_2), "month"))]
sdr2.month_snaps = sdr2.times[, .(num_releases = .N), by = .(pkg_name = package_name, same_day_release, timestamp = floor_date(ymd_hms(package_version_timestamp_2), "month"))]

# calculate number of same-day releases per month
sdr1.month_snaps = sdr1.month_snaps[, .(num_packages = .N, total_releases = sum(num_releases)), by = .(timestamp, same_day_release)]
sdr2.month_snaps = sdr2.month_snaps[, .(num_packages = .N, total_releases = sum(num_releases)), by = .(timestamp, same_day_release)]



### Number of same_day releases per package ###

sdr1.same_day_releases_per_package = sdr1.month_snaps[same_day_release == TRUE, .(timestamp, releases_per_packages = total_releases / num_packages, type = "Voluntary")]
sdr2.same_day_releases_per_package = sdr2.month_snaps[same_day_release == TRUE, .(timestamp, releases_per_packages = total_releases / num_packages, type = "Triggered")]

# prepare data for plot
same_day_releases_per_package = rbind(sdr1.same_day_releases_per_package, sdr2.same_day_releases_per_package)



### Number of same_day releases per number of releases ###

# merge in one table the number of voluntary same-day and regular releases
sdr1.month_snaps.merged = merge(sdr1.month_snaps[same_day_release == TRUE], sdr1.month_snaps[same_day_release == FALSE], by = "timestamp")
sdr1.month_snaps.merged = sdr1.month_snaps.merged[, .(timestamp, num_same_day_releases = total_releases.x, num_regular_releases = total_releases.y)]

sdr1.proportion_same_day_releases = sdr1.month_snaps.merged[, .(timestamp, num_same_day_releases, num_regular_releases, proportion_same_day_releases = (num_same_day_releases / (num_same_day_releases + num_regular_releases)) )]

# merge in one table the number of triggered same-day and regular releases
sdr2.month_snaps.merged = merge(sdr2.month_snaps[same_day_release == TRUE], sdr2.month_snaps[same_day_release == FALSE], by = "timestamp")
sdr2.month_snaps.merged = sdr2.month_snaps.merged[, .(timestamp, num_same_day_releases = total_releases.x, num_regular_releases = total_releases.y)]

sdr2.proportion_same_day_releases = sdr2.month_snaps.merged[, .(timestamp, num_same_day_releases, num_regular_releases, proportion_same_day_releases = (num_same_day_releases / (num_same_day_releases + num_regular_releases)) )]

# prepare data for plot
proportion_same_day_releases = rbind(sdr1.proportion_same_day_releases[, .(timestamp, proportion_same_day_releases, type = "Voluntary")],
                                     sdr2.proportion_same_day_releases[, .(timestamp, proportion_same_day_releases, type = "Triggered")])

png(file = "RQ1/images/monthly_snaps.png")
pdf(file = "RQ1/images/monthly_snaps.pdf")
p0 = ggplot(same_day_releases_per_package[timetamp > "2010-12-01" & timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), releases_per_packages, colour = type)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("# same-day releases\n/ # packages") +
  scale_color_grey(name="Same-day release pattern",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Voluntary", "Triggered"), start = 0.7, end = 0.0) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")

p1 = ggplot(proportion_same_day_releases[timetamp > "2010-12-01" & timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), proportion_same_day_releases, colour = type)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("# same-day releases\n/ # releases") +
  scale_color_grey(name="Same-day release pattern",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Voluntary", "Triggered"), start = 0.7, end = 0.0) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")

grid.arrange(p0, p1, ncol = 1)
dev.off()



### Number of same-day releases vs. number of regular releases per month ###

# separete number of same-day releases
sdr1.num_sd = sdr1.month_snaps[same_day_release == TRUE][, .(timestamp, total_releases = total_releases, type = "sdr1")]
sdr2.num_sd = sdr2.month_snaps[same_day_release == TRUE][, .(timestamp, total_releases = total_releases, type = "sdr2")]

sdr1.num_sd = sdr1.num_sd[order(timestamp)][timestamp > "2010-12-01" & timestamp < "2017-05-01"]
sdr2.num_sd = sdr2.num_sd[order(timestamp)][timestamp > "2010-12-01" & timestamp < "2017-05-01"]

# bind rows
sdr.sd = rbind(sdr1.num_sd, sdr2.num_sd)

# separete number of regular releases
sdr1.num_regs = sdr1.month_snaps[same_day_release == FALSE][, .(timestamp, total_releases = total_releases, type = "reg1")][timestamp > "2010-12-01" & timestamp < "2017-05-01"]
sdr2.num_regs = sdr2.month_snaps[same_day_release == FALSE][, .(timestamp, total_releases = total_releases, type = "reg2")][timestamp > "2010-12-01" & timestamp < "2017-05-01"]

# bind rows
sdr1.rels = rbind(sdr1.num_sd, sdr1.num_regs)
sdr2.rels = rbind(sdr2.num_sd, sdr2.num_regs)

pdf(file = "RQ1/images/monthly_snaps_all.pdf")
ggplot() +
  geom_line(data = sdr1.rels, aes(ymd(timestamp), total_releases, color = type)) +
  geom_line(data = sdr2.rels, aes(ymd(timestamp), total_releases, color = type))+
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_grey(name="Packages publishing\nvoluntary same-day releases",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Same-day\nreleases", "Regular\nreleases")) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  scale_y_log10() +
  xlab("") +
  ylab("# same-day releases\n/ # packages") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
dev.off()
