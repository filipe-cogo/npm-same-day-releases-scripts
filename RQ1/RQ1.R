wd1 = "/Users/filipe/Dropbox/same_day_releases_npm"
wd2 = "/home/local/SAIL/filipe-cogo/Dropbox/same_day_releases_npm"
setwd(wd2)

set.seed(21)

options(scipen = 999)

library(data.table)
library(doParallel)
library(ggplot2)
library(scales)
library(gridExtra)
library(lubridate)
library(stats)
library(rms)
library(randomForest)
library(xtable)
library(viridis)

source("RQ1/scripts/RQ1fnc.R")

#############################################
### Read CSV dataset
#############################################

######################
### Read CSV dataset

dependencies = fread(input="datasets/npmdep.csv", header=TRUE, sep=",")
releases = fread(input="datasets/npmreleases.csv", 
                 colClasses = list(
                   numeric = c("release_order",
                               "package_version_num_1_major", 
                               "package_version_num_1_minor", 
                               "package_version_num_1_patch", 
                               "package_version_num_1_pre_release_number", 
                               "package_version_num_1_build_number", 
                               "package_version_num_2_major",
                               "package_version_num_2_minor",
                               "package_version_num_2_patch",
                               "package_version_num_2_pre_release_number",
                               "package_version_num_2_build_number",
                               "package_version_timestamp_1_day",
                               "package_version_timestamp_1_month",
                               "package_version_timestamp_1_year",
                               "package_version_timestamp_1_hour",
                               "package_version_timestamp_1_minute",
                               "package_version_timestamp_1_second",
                               "package_version_timestamp_2_day",
                               "package_version_timestamp_2_month",
                               "package_version_timestamp_2_year",
                               "package_version_timestamp_2_hour",
                               "package_version_timestamp_2_minute",
                               "package_version_timestamp_2_second",
                               "package_version_timestamp_diff_secs"),
                   character = c("package_name",
                                 "package_version_num_1",
                                 "package_version_num_2",
                                 "package_version_change_was_in",
                                 "package_version_num_1_pre_release",
                                 "package_version_num_1_build",
                                 "package_version_num_2_pre_release",
                                 "package_version_num_2_build",
                                 "package_version_timestamp_1",
                                 "package_version_timestamp_2")), 
                 header=TRUE, sep=",")

nrow(same_day_releases_pattern_1[, .(.GRP), by = package_name])
nrow(same_day_releases_pattern_2[, .(.GRP), by = client_name])
nrow(same_day_releases_pattern_2[, .(.GRP), by = dependency_name])
nrow(dependencies[, .(.GRP), by = .(client_name)])

############
### Clean data

# remove cases in which client_version_timestamp_1 < client_version_timestamp_2
# it occurs because of paralles branches being released
releases = releases[package_version_timestamp_1 < package_version_timestamp_2, ]
dependencies = dependencies[client_version_timestamp_1 < client_version_timestamp_2, ]

# package "all-the-packages-name" has more than 
# 3000 releases and is a toy package 
dependencies = dependencies[client_name != "all-the-package-names", ]
releases = releases[package_name != "all-the-package-names", ]

# remove provider packages being used less than 100 times
providers.keep = dependencies[, .(.GRP), by = .(dependency_name, client_name)][, .(count = .N), by = dependency_name][count >= 100]

# clean dependencies
dependencies.clean = dependencies[dependency_name %in% providers.keep$dependency_name, ]
dependencies = dependencies.clean

# clean releases
releases.clean = releases[package_name %in% providers.keep$dependency_name, ]
releases = releases.clean

# save cleaned dataset
#write.csv(releases, file = "datasets/clean_releases.csv")
#write.csv(dependencies, file = "datasets/clean_dependencies.csv")

# fetch cleaned dataset
releases = fread("datasets/clean_releases.csv")
dependencies = fread("datasets/clean_dependencies.csv")

############
### Calculate same-day releases pattern 1

same_day_releases_pattern_1 = releases[package_version_timestamp_diff_secs <= 86400]
#same_day_releases_pattern_1 = releases[difftime(ymd_hms(package_version_timestamp_2), ymd_hms(package_version_timestamp_1), "secs") <= 86400]

############
### Calculate same-day releases pattern 2
# How many client packages releases an update within 24hrs
# of an update that was released by one of it's supllier packages?

#same_day_releases_pattern_2 = same_day_releases_pattern_2.par(releases, dependencies, threads = 20)
same_day_releases_pattern_2 = dependencies[difftime(ymd_hms(package_version_timestamp_1), ymd_hms(client_version_timestamp_1)) <= 86400 & difftime(ymd_hms(package_version_timestamp_2), ymd_hms(client_version_timestamp_2)) <= 86400]
#same_day_releases_pattern_2 = fread("datasets/pattern-2-same-day-releases.csv")
same_day_releases_pattern_2 = same_day_releases_pattern_2[steady == FALSE]

############
### Save the tables with same-day releases
# write.csv(same_day_releases_pattern_1, "datasets/pattern-1-same-day-releases.csv")
# write.csv(same_day_releases_pattern_2, "datasets/pattern-2-same-day-releases.csv")


#####################################################################
### RQ.1. What are the characteristics of same-day releases on npm ?
#####################################################################

#####################
### 1) How often do same-day releases occur on npm?

### Boxplot showing the distribution of different patterns of same-day releases

### For pattern 1
# set key for join tables
setkey(same_day_releases_pattern_1, package_name, package_version_num_1, package_version_num_2)
setkey(releases, package_name, package_version_num_1, package_version_num_2)

# find elements that are in 'releases' but not on 'same_day_releases_pattern_1'
sdr1.diff = releases[!same_day_releases_pattern_1]
# find elements that are in 'releases' but not on 'sdr1.diff'
sdr1.eq = releases[!sdr1.diff]

# check if it's all right
nrow(releases) == nrow(same_day_releases_pattern_1) + nrow(sdr1.diff)
nrow(releases) == nrow(sdr1.eq) + nrow(sdr1.diff)

# create columns with values for same-day release flag
sdr1.diff$same_day_1 = FALSE
sdr1.eq$same_day_1 = TRUE

# bind rows from both tables
sdr1 = rbind(sdr1.diff, sdr1.eq)

# calculate total of releases
sdr1.count = sdr1[, .(num_releases = .N, same_day = same_day_1), by = .(name = package_name)]

# create new colum with total and proportions of regular releases
sdr1.count.reg = sdr1.count[same_day == FALSE]
sdr1.count.reg[, num_specific_releases := .N, by = name]
sdr1.count.reg[, prop_specific_releases := (num_specific_releases / num_releases)]

# create new colum with total and proportions of same-day releases
sdr1.count.sd = sdr1.count[same_day == TRUE]
sdr1.count.sd[, num_specific_releases := .N, by = name]
sdr1.count.sd[, prop_specific_releases := (num_specific_releases / num_releases)]

sdr1.count = unique(rbind(sdr1.count.reg, sdr1.count.sd))

### For pattern 2
# set key for join tables
setkey(same_day_releases_pattern_2, client_name, client_version_num_1, client_version_num_2, dependency_name, dependency_version_max_satisf_1, dependency_version_max_satisf_2)
setkey(dependencies, client_name, client_version_num_1, client_version_num_2, dependency_name, dependency_version_max_satisf_1, dependency_version_max_satisf_2)

# find elements that are in 'dependencies' but not on 'same_day_releases_pattern_2'
sdr2.diff = dependencies[!same_day_releases_pattern_2]
# find elements that are in 'dependencies' but not on 'sdr2.diff'
sdr2.eq = dependencies[!sdr2.diff]

# check if it's all right
nrow(dependencies) == nrow(same_day_releases_pattern_2) + nrow(sdr2.diff)
nrow(dependencies) == nrow(sdr2.eq) + nrow(sdr2.diff)

# create columns with values for same-day release flag
sdr2.diff$same_day_2 = FALSE
sdr2.eq$same_day_2 = TRUE

# bind rows from both tables
sdr2 = rbind(sdr2.diff, sdr2.eq)

# calculate total of releases
sdr2.count = sdr2[, .(num_releases = .N, same_day = same_day_2), by = .(name = client_name)]
#sdr2.count = sdr2[, .(num_releases = .N, same_day = same_day_2), by = .(name = dependency_name)]

# create new colum with total and proportions of regular releases
sdr2.count.reg = sdr2.count[same_day == FALSE]
sdr2.count.reg[, num_specific_releases := .N, by = name]
sdr2.count.reg[, prop_specific_releases := (num_specific_releases / num_releases)]

# create new colum with total and proportions of same-day releases
sdr2.count.sd = sdr2.count[same_day == TRUE]
sdr2.count.sd[, num_specific_releases := .N, by = name]
sdr2.count.sd[, prop_specific_releases := (num_specific_releases / num_releases)]

sdr2.count = unique(rbind(sdr2.count.reg, sdr2.count.sd))

# check statistical significance difference
wilcox.test(sdr1.count$prop_specific_releases, sdr2.count$prop_specific_releases, conf.int=TRUE)
#t.test(sdr1.prop$proportion_sdr, sdr2.prop$proportion_sdr, conf.int=TRUE)


sdr.prop = rbind(sdr1.count[, type := "sd1"], sdr2.count[, type := "sd2"])

# plot the boxplot for the proportions
pdf(file = "RQ1/images/dist_packages_same_day.pdf")
#png(file = "RQ1/images/dist_packages_same_day_proportion.png")
p0 = ggplot(sdr.prop, aes(x = type, y = prop_specific_releases, fill = type)) +
  geom_boxplot(position=position_dodge(1)) +
  #geom_violin(position=position_dodge(1)) +
  #scale_fill_brewer(name="Same-day release type",
  #                  labels=c("Voluntary", "Triggered"),
  #                  palette="RdBu") +
  scale_x_discrete(name = "", labels = c("Voluntary releases", "Triggered releases")) +
  scale_y_continuous(name = "Proportion of same-day releases",
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

p1 = ggplot(sdr.prop[same_day == TRUE], aes(x = type, y = num_specific_releases, fill = type)) +
    geom_boxplot() +
    scale_x_discrete(name = "", labels = c("Voluntary releases", "Triggered releases")) +
    scale_y_log10(name = "Number of same-day releases",
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

# check statistical significance difference
wilcox.test(sdr1.count$prop_specific_releases, sdr2.count$prop_specific_releases)
t.test(sdr1.count$prop_specific_releases, sdr2.count$prop_specific_releases)

### Top-10 packages performing same-day releases, on both patterrns

# Calculate how many times each package publishes same-day release
# sort by number of same-day releases
sdp1.count = sdr1.count[type == "sd1" & same_day == TRUE][order(-num_specific_releases)][1:10][,.(name, num_releases, prop_specific_releases)]
sdp2.count = sdr2.count[type == "sd2" & same_day == TRUE][order(-num_specific_releases)][1:10][,.(name, num_releases, prop_specific_releases)]

sdp1.count.xt = xtable(sdp1.count, caption = "Number of voluntary same-day releases")
digits(sdp1.count.xt) <- c(0,0,0,4)
# for latex
names(sdp1.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of releases}", "\\pbox{20cm}{Proportion of same-day releases}")
# for html
names(sdp1.count.xt) = c("Package name", "Number of releases", "Proportion of same-day releases")
# for latex
print(sdp1.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion_pt1/top_10_clients_publishing_same_day_releases_proportion_pt1.tex")
# for html
print(sdp1.count.xt, type = "html", sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_pt1/top_10_clients_publishing_same_day_releases_pt1.html")

sdp2.count.xt = xtable(sdp2.count, digits = 0, caption = "Number of triggered same-day releases")
digits(sdp2.count.xt) <- c(0,0,0,4)
# for latex
names(sdp2.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of releases}", "\\pbox{20cm}{Proportion of same-day releases}")
# for html
names(sdp2.count.xt) = c("Package name", "Number of releases", "Proportion of same-day releases")
# for latex
print(sdp2.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion_pt2/top_10_clients_publishing_same_day_releases_proportion_pt2.tex")
# for html
print(sdp2.count.xt, type = "html", sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_pt2/top_10_clients_publishing_same_day_releases_pt2.html")

# sort by proportion of same-day releases
sdp1.prop = sdr1.count[type == "sd1" & same_day == TRUE][order(-prop_specific_releases)][1:10][,.(name, num_releases, prop_specific_releases)]
sdp2.prop = sdr2.count[type == "sd2" & same_day == TRUE][order(-prop_specific_releases)][1:10][,.(name, num_releases, prop_specific_releases)]

sdp1.count.xt = xtable(sdp1.prop, caption = "Proportion of voluntary same-day releases")
digits(sdp1.count.xt) <- c(0,0,0,4)
# for latex
names(sdp1.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of releases}", "\\pbox{20cm}{Proportion of same-day releases}")
# for html
names(sdp1.count.xt) = c("Package name", "Number of releases", "Proportion of same-day releases")
# for latex
print(sdp1.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion_pt1/top_10_clients_publishing_same_day_releases_proportion_pt1.tex")
# for html
print(sdp1.count.xt, type = "html", sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion_pt1/top_10_clients_publishing_same_day_releases_proportion_pt1.html")

sdp2.count.xt = xtable(sdp2.prop, digits = 0, caption = "Proportion of triggered same-day releases")
digits(sdp1.count.xt) <- c(0,0,0,4)
# for latex
names(sdp1.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of releases}", "\\pbox{20cm}{Proportion of same-day releases}")
# for html
names(sdp1.count.xt) = c("Package name", "Number of releases", "Proportion of same-day releases")
# for latex
print(sdp1.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion_pt2/top_10_clients_publishing_same_day_releases_proportion_pt2.tex")
# for html
print(sdp1.count.xt, type = "html", sanitize.colnames.function = identity, file = "RQ1/tables/top_10_clients_publishing_same_day_releases_proportion_pt2/top_10_clients_publishing_same_day_releases_proportion_pt2.html")


### Number of same-day releases per month
# take monthly snapshots
snaps = month_snaps(same_day_releases_pattern_1, same_day_releases_pattern_2)
# snaps$num_releases_pattern1 = ifelse(snaps$num_releases_pattern1 != 0, snaps$num_releases_pattern1 / snaps$num_pgks_pattern1, 0)
# snaps$num_releases_pattern2 = ifelse(snaps$num_releases_pattern2 != 0, snaps$num_releases_pattern2 / snaps$num_pgks_pattern2, 0)
snaps[, num_pgks_pattern1 := NULL]
snaps[, num_pgks_pattern2 := NULL]
snaps[, releases_per_pgks_pattern1 := NULL]
snaps[, releases_per_pgks_pattern2 := NULL]
snaps[, num_pgks_pattern1_proportion := NULL]
snaps[, num_pgks_pattern2_proportion := NULL]
snaps$timestamp = as.character(snaps$timestamp)
snaps.m.1 = melt(snaps, id = "timestamp")
snaps.m.1

# plot the proportion occurences of each type
# of same-day release

pdf(file = "RQ1/images/monthly_snaps_total.pdf")
#png(file = "RQ1/images/monthly_snaps_total.png")
ggplot(snaps.m.1[timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), value, colour = variable)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("Number of same-day releases") +
  scale_color_grey(name="Same-day release type",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Voluntary", "Triggered")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
dev.off()

# plot the number of different provider packages
# being used by a client package per month
snaps_deps =  month_snaps_deps(dependencies)

pdf(file = "RQ1/images/monthly_snaps_dependencies_total.pdf")
#png(file = "RQ1/images/monthly_snaps_dependencies_total.png")
ggplot(snaps_deps[timestamp < "2017-05-01"], aes(ymd(timestamp), deps_per_package)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("Number of dependencies / Number of packages") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

snaps = month_snaps(same_day_releases_pattern_1, same_day_releases_pattern_2)
snaps_deps =  month_snaps_deps(dependencies)

snaps[, num_pgks_pattern1 := NULL]
snaps[, num_pgks_pattern2 := NULL]
snaps[, num_releases_pattern1 := NULL]
snaps[, num_releases_pattern2 := NULL]
snaps[, num_pgks_pattern1_proportion := NULL]
snaps[, num_pgks_pattern2_proportion := NULL]
snaps$timestamp = as.character(snaps$timestamp)
snaps.m.2 = melt(snaps, id = "timestamp")


# plot both graphs in the same figure

pdf(file = "RQ1/images/monthly_snaps.pdf")
p0 = ggplot(snaps.m.1[timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), value, colour = variable)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("# same-day releases") +
  scale_color_grey(name="Same-day release type",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Voluntary", "Triggered")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")

p1 = ggplot(snaps_deps[timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), deps_per_package, colour = "Dependencies density")) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("# dependencies\n/ # packages") +
  scale_color_grey(name="Dependencies density",
                   #breaks=c("sd1", "sd2"),
                   labels = c("# dependencies / # packages")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")

p2 = ggplot(snaps.m.2[timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), value, colour = variable)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("# same-day releases\n/ # packages") +
  scale_color_grey(name="Same-day release type",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Voluntary", "Triggered")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
grid.arrange(p0, p1, p2, ncol = 1)
dev.off()





pdf(file = "RQ1/images/monthly_snaps.pdf")
#png(file = "RQ1/images/monthly_snaps_total.png")
p0 = ggplot(snaps.m.1[timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), value, colour = variable)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("Number of same-day releases") +
  scale_color_grey(name="Same-day release type",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Voluntary", "Triggered")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
dev.off()

p1 = ggplot(snaps.m[timestamp < "2017-05-01"]) +
  geom_line(aes(ymd(timestamp), value, colour = variable)) +
  scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %Y")) +
  xlab("") +
  ylab("Num. of same-day releases / num. of packages") +
  scale_color_grey(name="Same-day release type",
                   #breaks=c("sd1", "sd2"),
                   labels=c("Voluntary", "Triggered")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")


grid.arrange(p0, p1, ncol = 1)
dev.off()



#####################
### 2) How same-day releases on pattern 1 are being adopted?

### How many clients are using same-day releases (pattern 1)?

# count how many client relases uses same-day releases (pattern 1)
clients_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_ptrn1(dependencies, same_day_releases_pattern_1)
clients_releases_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[package_pttnr1_version_change == "upgrade"][difftime(package_pttnr1_timestamp_2, client_version_timestamp_2) < 86400]
# print results
print(paste("There are", nrow(clients_releases_using_same_day_release_ptrn1), "usages of same-day releases on pattern 1."))
print(paste("There are", nrow(clients_releases_using_same_day_release_ptrn1[, .(.N), by = client_name]), "different client packages using same-day releases on pattern 1. This is", (nrow(clients_releases_using_same_day_release_ptrn1[, .(.N), by = client_name])/nrow(dependencies[,.(.GRP),by=client_name]))*100, "percent of the total packages"))

# count how many times each client used a same-day release pattern1
clients_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[, .(count = .N), by = client_name]
clients_using_same_day_release_ptrn1[count > 10]
summary(clients_using_same_day_release_ptrn1$count)

# plot the boxplot
pdf(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.pdf")
#png(file = "RQ1/images/clients_using_same_day_releases_ptrn_1.png")
ggplot(clients_using_same_day_release_ptrn1, aes(x = "", y = count), fill = change) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Number of same-day releases usage per client",
                labels = comma) +
  #ggtitle("Distribution of same-day releases usage by client") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()

# see the distribution of same-day releases (pattern 1) usage per month
# count how many times each client used a same-day release pattern1
clients_using_same_day_release_ptrn1_per_month = clients_releases_using_same_day_release_ptrn1[, .(count = .N), by = .(client_name, client_timestamp_month)]

# plot the boxplot for each date
pdf(file = "RQ1/images/clients_using_same_day_releases_ptrn_1_month.pdf")
#png(file = "RQ1/images/clients_using_same_day_releases_ptrn_1_month.png", width = 1000)
ggplot(clients_using_same_day_release_ptrn1_per_month, aes(x = "", y = count, fill = as.character(client_timestamp_month))) +
  geom_boxplot() +
  guides(fill=FALSE) +
  scale_x_discrete(name = "") +
  scale_y_log10(name = "Number of same-day releases usage",
                labels = comma) +
  #ggtitle("Distribution of same-day releases usage by client per month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()


### How many times each client used a same-day release pattern1
### separated by versioning statement change type?
# clients_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_ptrn1(dependencies, same_day_releases_pattern_1)
# 
# clients_using_same_day_release_ptrn1 = clients_releases_using_same_day_release_ptrn1[, .(count_sd = .N), by = .(client_name, client_versioning_change)]
# 
# # select only range to x
# cusdr_range_to_x = clients_using_same_day_release_ptrn1[client_versioning_change %in% c("range to range", "range to tight")]
# # sum columns with range to x
# cusdr_range_to_x[, total := sum(count_sd), by = client_name]
# cusdr_range_to_x$proportion = (cusdr_range_to_x$count_sd / cusdr_range_to_x$total)
# 
# # select only tight to x
# cusdr_tight_to_x = clients_using_same_day_release_ptrn1[client_versioning_change %in% c("tight to range", "tight to tight")]
# # sum columns with rangtight to x
# cusdr_tight_to_x[, total := sum(count_sd), by = client_name]
# cusdr_tight_to_x$proportion = (cusdr_tight_to_x$count_sd / cusdr_tight_to_x$total)
# 
# cusdr_range_to_x[client_versioning_change == "range to range"]
# cusdr_tight_to_x[client_versioning_change == "tight to tight"]
# 
# cusdr_doesnt_change = rbind(cusdr_range_to_x[client_versioning_change == "range to range"], cusdr_tight_to_x[client_versioning_change == "tight to tight"])
# 
# # plot the boxplot showing range to range inside range to x
# pdf(file = "RQ1/images/distribution_proportion_same_day_release_client_doesnt_change_versioning_statement.pdf")
# #png(file = "RQ1/images/distribution_proportion_same_day_release_client_doesnt_change_versioning_statement.png")
# ggplot(cusdr_doesnt_change, aes(x = client_versioning_change, y = proportion, fill = client_versioning_change)) +
#   #geom_violin(position=position_dodge(1)) +
#   geom_boxplot() +
#   scale_fill_grey(name="Versioning\nstatement change",
#                   breaks=c("Range to\n range", "Tight to\n tight"),
#                   labels=c("Remains on\n range", "Remains on\n tight"),
#                   guide=FALSE,
#                   start = 0.65, end = 1.0) +
#   stat_summary(fun.y="median", geom="point", position = position_dodge(1)) +
#   scale_x_discrete(name = "", labels = c("Remains on range\nversioning statement", "Remains on explicit\nversioning statement")) +
#   scale_y_continuous(name = "Proportion of releases that does not change versioning statement",
#                      labels = comma) +
#   #ggtitle("Proportion of releases of clients using same-day release \n that does not change versioning statement") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
# dev.off()

### Examine client releases after the adoption of a same-day release pattern 1
clients_sd1 = client_releases_changing_from_same_day_release_ptrn1(dependencies, same_day_releases_pattern_1)

clients_sd1.cast = dcast(formula = client_name ~ package_pttnr1_version_change, data = clients_sd1, fun.aggregate = sum, value.var = "count")

clients_sd1.cast$no_change_prop = clients_sd1.cast$no_change / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)
clients_sd1.cast$upgrade_prop = clients_sd1.cast$upgrade / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)
clients_sd1.cast$rollback_prop = clients_sd1.cast$rollback / (clients_sd1.cast$no_change + clients_sd1.cast$rollback + clients_sd1.cast$upgrade)

clients_sd1.cast[, no_change := NULL]
clients_sd1.cast[, upgrade := NULL]
clients_sd1.cast[, rollback := NULL]

clients_sd1.melt = melt(clients_sd1.cast, id = "client_name")

pdf(file = "RQ1/images/after_sd1_adoption.pdf")
p1 = ggplot(clients_sd1.melt, aes(x = "", y = value, fill = variable)) +
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

p0 = ggplot(clients_using_same_day_release_ptrn1, aes(x = "", y = count), fill = change) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Number of same-day releases usage per client",
                     labels = comma) +
  #ggtitle("Distribution of same-day releases usage by client") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
grid.arrange(p0, p1, ncol = 2)
dev.off()

# Calculate the proportions of using same-day releases pattern 1
# #find cases of no change
# clients_sd1.no_change = clients_sd1[package_pttnr1_version_change == "no_change", .(releases_sd1_count = .N), by = .(client_name)]
# # calculate the total of releases for the client packages
# client_releases = dependencies[, .(.GRP), by = .(client_name, client_version_num_1, client_version_num_2)]
# clients_sd1.no_change = merge(clients_sd1.no_change, client_releases[, .(num_releases = .N), by = .(client_name)], fill = TRUE)
# # calculate the proportion of releases with no change
# clients_sd1.no_change.prop = clients_sd1.no_change[, .(mean_num_releases = mean(num_releases), releases_sd1_count)]
# 
# general_meantime = clients_sd1[, .(general_mean_time = mean(difftime(ymd_hms(client_timestamp_1), ymd_hms(client_timestamp_2)))), by = client_name]
# clients_sd1.gt = clients_sd1[, .(version_change_mean_time = mean(difftime(ymd_hms(client_timestamp_1), ymd_hms(client_timestamp_2)))), by = .(client_name, package_pttnr1_version_change)]
# merge(clients_sd1.roll_ups, general_meantime)

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


#####################
### 3) What is the relation between the popularity of a package and the number of same-day releases?

### For pattern 1

# Calculate how many times each package publishes same-day release
# sort by number of same-day releases
sdp1.count = sdr1.count[same_day == TRUE][order(-num_specific_releases)][1:10][,.(name, num_specific_releases, num_releases, prop_specific_releases)]

# check the popularity of the top-10 packages that publish same-day releases (pattern 1)
dependency_usage_total = data.table()
dependency_usage_per_client = data.table()
dependency_versioning_change = data.table()
dependency_semver_change = data.table()
dependency_clients_using = data.table()
#dependency_usage_per_client = data.table()

for (i in 1:length(sdr1.count[same_day == TRUE][order(-num_specific_releases)][1:10]$name)){
  name = sdr1.count[same_day == TRUE][order(-num_specific_releases)][1:10]$name[i]
  
  dependency_usage_total = rbind(dependency_usage_total, dependencies[dependency_name == name, .(releases_using = .N), by = .(dependency_name)])
  dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name, .(provider_usage_per_client = .N), by = .(dependency_name, client_name)])
  dependency_clients_using = rbind(dependency_clients_using, dependencies[dependency_name == name, .(.GRP), .(client_name, dependency_name)][,.(num_clients_using = .N), by = dependency_name])
  dependency_versioning_change = rbind(dependency_versioning_change, dependencies[dependency_name == name, .(num_versioning_statement_change = .N, type = paste(dependency_versioning_type_1, dependency_versioning_type_2, sep = " to ")), by = .(dependency_name, dependency_versioning_type_1, dependency_versioning_type_2)][, .(dependency_name, num_versioning_statement_change, type )])
  dependency_semver_change = rbind(dependency_semver_change, releases[package_name == name, .(num_semver_change = .N), by = .(package_name, package_version_change_was_in)])
  #dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name, .(count = .N), by = .(dependency_name, client_name, dependency_version_max_satisf_1, dependency_version_max_satisf_2)])
}

# mount table to summarize the characteristics of the top-10 same-day releases

# how many clients and how many client releases are using same-day releases from the top-10?
top_10_sdr_usage = merge(dependency_usage_total, dependency_clients_using)

sdp1.count.xt = xtable(top_10_sdr_usage, caption = "Usage of top-10 packages that publish same-day releases (pattern 1)")
digits(sdp1.count.xt) <- c(0,0,0, 0)
names(sdp1.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of releases using}", "\\pbox{20cm}{Number of clients using}")
print(sdp1.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/usage_top_10_clients_publishing_same_day_releases_pt1/usage_top_10_clients_publishing_same_day_releases_pt1.tex")

# how top-10 same-day releases changes the versioning statements?
dcast(formula = dependency_name ~ type, data = dependency_versioning_change, fun.aggregate = sum, value.var = "num_versioning_statement_change")

# how top-10 same-day releases changes the version?
dcast(formula = package_name ~ package_version_change_was_in, data = dependency_semver_change, fun.aggregate = sum, value.var = "num_semver_change")


# 10 boxplots, each one showing the distribution of
# number of times each client have used a same-day
# of some provider. For example, if a provider P
# published 10 same-day releases and the client C1
# uses it 2 times and the client C2 uses is 10 times, 
# then the boxplot will have two points, one for each
# client. The values of the points on y-axis will be
# 2 and 10, respectively

# for pattern 1
dependency_usage_per_client

png(file = "RQ1/images/dist_num_clients_using_same_day_releases_pt1.png")
ggplot(dependency_usage_per_client, aes(x = dependency_name, y = provider_usage_per_client, fill = dependency_name)) +
  geom_boxplot() +
  scale_fill_discrete(name="Dependencies\nnames",
                      breaks=unique(dependency_usage_per_client$dependency_name),
                      labels=unique(dependency_usage_per_client$dependency_name)) +
  scale_x_discrete(name = "Top-10 packages", labels = c()) +
  scale_y_log10(name = "Same-day release usage",
                labels = comma) +
  ggtitle("Usage of same-day release (pattern 1) per client") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()


### For pattern 2

sdp2.count = sdr2.count[same_day_2 == TRUE][order(-count)][1:10][,.(name, count, total_releases, proportion_sdr)]

# check the popularity of the top-10 packages that publish same-day releases (pattern 2)
dependency_usage_total = data.table()
dependency_usage_per_client = data.table()
dependency_versioning_change = data.table()
dependency_semver_change = data.table()
dependency_clients_using = data.table()
#dependency_usage_per_client = data.table()

for (i in 1:length(sdr2.count[same_day_2 == TRUE][order(-count)][1:10]$name)){
  name = sdr2.count[same_day_2 == TRUE][order(-count)][1:10]$name[i]
  
  dependency_usage_total = rbind(dependency_usage_total, dependencies[dependency_name == name, .(releases_using = .N), by = .(dependency_name)])
  dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name, .(provider_usage_per_client = .N), by = .(dependency_name, client_name)])
  dependency_clients_using = rbind(dependency_clients_using, dependencies[dependency_name == name, .(.GRP), .(client_name, dependency_name)][,.(num_clients_using = .N), by = dependency_name])
  dependency_versioning_change = rbind(dependency_versioning_change, dependencies[dependency_name == name, .(num_versioning_statement_change = .N, type = paste(dependency_versioning_type_1, dependency_versioning_type_2, sep = " to ")), by = .(dependency_name, dependency_versioning_type_1, dependency_versioning_type_2)][, .(dependency_name, num_versioning_statement_change, type )])
  dependency_semver_change = rbind(dependency_semver_change, releases[package_name == name, .(num_semver_change = .N), by = .(package_name, package_version_change_was_in)])
  #dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name, .(count = .N), by = .(dependency_name, client_name, dependency_version_max_satisf_1, dependency_version_max_satisf_2)])
}

# mount table to summarize the characteristics of the top-10 same-day releases

# how many clients and how many client releases are using same-day releases from the top-10?
top_10_sdr_usage = merge(dependency_usage_total, dependency_clients_using)

sdp2.count.xt = xtable(top_10_sdr_usage, caption = "Usage of top-10 packages that publish same-day releases (pattern 2)")
digits(sdp2.count.xt) <- c(0,0,0, 0)
names(sdp2.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of \\\\ releases using}", "\\pbox{20cm}{Number of \\\\ clients using}")
print(sdp2.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/usage_top_10_clients_publishing_same_day_releases_pt2/usage_top_10_clients_publishing_same_day_releases_pt2.tex")

# how top-10 same-day releases changes the versioning statements?
dcast(formula = dependency_name ~ type, data = dependency_versioning_change, fun.aggregate = sum, value.var = "num_versioning_statement_change")

# how top-10 same-day releases changes the version?
dcast(formula = package_name ~ package_version_change_was_in, data = dependency_semver_change, fun.aggregate = sum, value.var = "num_semver_change")

# 10 boxplots, each one showing the distribution of
# number of times each client have used a same-day
# of some provider. For example, if a provider P
# published 10 same-day releases and the client C1
# uses it 2 times and the client C2 uses is 10 times, 
# then the boxplot will have two points, one for each
# client. The values of the points on y-axis will be
# 2 and 10, respectively

# for pattern 2
dependency_usage_per_client

png(file = "RQ1/images/dist_num_clients_using_same_day_releases_pt2.png")
ggplot(dependency_usage_per_client, aes(x = dependency_name, y = provider_usage_per_client, fill = dependency_name)) +
  geom_boxplot() +
  scale_fill_discrete(name="Dependencies\nnames",
                      breaks=unique(dependency_usage_per_client$dependency_name),
                      labels=unique(dependency_usage_per_client$dependency_name)) +
  scale_x_discrete(name = "Top-10 packages", labels = c()) +
  scale_y_log10(name = "Same-day release usage",
                labels = comma) +
  ggtitle("Usage of same-day release (pattern 2) per client") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()


#####################
### 4) How the semantic version numbers are changing on same-day releases?

### Boxplot showing the distribution of version number changes on same-day releases on pattern 1

#calculate the number of times each package changes the semantic version in a same-day release (pattern 1)
semver_dist_pt1 = same_day_releases_pattern_1[, .(count = .N), by = .(package_name, package_version_change_was_in)]

#pdf(file = "RQ1/images/semver_change_dist_pt1.pdf")
png(file = "RQ1/images/semver_change_dist_pt1.png")
ggplot(semver_dist_pt1, aes(x = package_version_change_was_in, y = count, fill = package_version_change_was_in)) +
  geom_boxplot() +
  scale_fill_discrete(name="Level of\nChange",
                      breaks=c("major", "minor", "patch", "pre_release", "build"),
                      labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_x_discrete(name = "Semantic version change", labels = c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_log10(name = "Number of releases changing semantic version",
                     labels = comma) +
  ggtitle("Distribution of semantic version number changes\non same-day releases per package (Pattern 1)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()

# unmelt to calculate proportions
semver_dist_pt1.cast = dcast(formula = package_name ~  package_version_change_was_in, data = semver_dist_pt1, fun.aggregate = sum, value.var = "count")
semver_dist_pt1.cast$build_proportion = semver_dist_pt1.cast$build / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$major_proportion = semver_dist_pt1.cast$major / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$minor_proportion = semver_dist_pt1.cast$minor / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$patch_proportion = semver_dist_pt1.cast$patch / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$pre_release_proportion = semver_dist_pt1.cast$pre_release / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast = semver_dist_pt1.cast[, .(package_name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

# melt for plot
semver_dist_pt1.prop = melt(semver_dist_pt1.cast, id = "package_name")

pdf(file = "RQ1/images/semver_change_dist_pt1_proportion.pdf")
#png(file = "RQ1/images/semver_change_dist_pt1_proportion.png")
ggplot(semver_dist_pt1.prop, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_grey(name="Level of change",
                  breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                  labels=c("Major", "Minor", "Patch", "Pre-release", "Build"),
                  start = 0.2, end = 1.0) +
  # scale_fill_discrete(name="Level of\nChange",
  #                     breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
  #                     labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_x_discrete(name = "", labels = c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "Proportion of releases changing semantic version",
                labels = comma) +
  #ggtitle("Proportion of semantic version number changes\non same-day releases per package (Pattern 1)") +
  theme_bw() + theme(legend.position="bottom")
dev.off()


### Boxplot showing the distribution of version number changes on same-day releases on pattern 2

#calculate the number of times each package changes the semantic version in a same-day release (pattern 2)
#for the clients
semver_dist_pt2_client = same_day_releases_pattern_2[, .(count = .N, role = "Client"), by = .(name = client_name, change = client_version_change_was_in)]
#for the providers
#semver_dist_pt2_provider = same_day_releases_pattern_2[package_version_change_was_in != "", .(count = .N, role = "Provider"), by = .(name = package_name, change = package_version_change_was_in)]
semver_dist_pt2_provider = same_day_releases_pattern_2[, .(count = .N, role = "Provider"), by = .(name = package_name, change = package_version_change_was_in)]

semver_dist_pt2 = rbind(semver_dist_pt2_client, semver_dist_pt2_provider)

pdf(file = "RQ1/images/semver_change_dist_pt2.pdf")
#png(file = "RQ1/images/semver_change_dist_pt2.png")
ggplot(semver_dist_pt2, aes(x = change, y = count, fill = change)) +
  geom_boxplot() +
  scale_fill_discrete(name="Level of\nChange",
                      breaks=c("major", "minor", "patch", "pre_release", "build"),
                      labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_x_discrete(name = "Semantic version change", labels = c("Build", "Major", "Minor", "Patch", "Pre-release")) +
  scale_y_log10(name = "Number of releases changing semantic version",
                labels = comma) +
  #coord_flip() +
  facet_wrap( ~ role, scales="free") +
  ggtitle("Distribution of semantic version number changes\non triggered same-day releases (Pattern 2)") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()

# unmelt to calculate proportions
# only clients
semver_dist_pt2.cast = dcast(formula = name ~  change, data = semver_dist_pt2[role == "Client"], fun.aggregate = sum, value.var = "count")
semver_dist_pt2.cast$build_proportion = semver_dist_pt2.cast$build / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$major_proportion = semver_dist_pt2.cast$major / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$minor_proportion = semver_dist_pt2.cast$minor / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$patch_proportion = semver_dist_pt2.cast$patch / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$pre_release_proportion = semver_dist_pt2.cast$pre_release / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast.client = semver_dist_pt2.cast[, .(name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

# only providers
semver_dist_pt2.cast = dcast(formula = name ~  change, data = semver_dist_pt2[role == "Provider"], fun.aggregate = sum, value.var = "count")
semver_dist_pt2.cast$build_proportion = semver_dist_pt2.cast$build / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$major_proportion = semver_dist_pt2.cast$major / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$minor_proportion = semver_dist_pt2.cast$minor / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$patch_proportion = semver_dist_pt2.cast$patch / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$pre_release_proportion = semver_dist_pt2.cast$pre_release / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast.provider = semver_dist_pt2.cast[, .(name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

# melt for plot
semver_dist_pt2.melt.client = melt(semver_dist_pt2.cast.client, id = "name")
semver_dist_pt2.melt.client$role = "Client"

semver_dist_pt2.melt.provider = melt(semver_dist_pt2.cast.provider, id = "name")
semver_dist_pt2.melt.provider$role = "Provider"

# bind rows
semver_dist_pt2.prop = rbind(semver_dist_pt2.melt.client, semver_dist_pt2.melt.provider)

#pdf(file = "RQ1/images/semver_change_dist_pt2.pdf")
png(file = "RQ1/images/semver_change_dist_pt2_proportion.png")
ggplot(semver_dist_pt2.prop, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_grid( ~ role, scales="free") +
  scale_fill_discrete(name="Level of\nChange",
                      breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                      labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_x_discrete(name = "Semantic version change", labels = c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "Proportion of releases changing semantic version",
                     labels = comma) +
  ggtitle("Proportion of semantic version number changes\non same-day releases per package (Pattern 2)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()

### plot 2 graphs in one figure

pdf(file = "RQ1/images/semver_change.pdf")
p0 = ggplot(semver_dist_pt1.prop, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_grey(name="Level of\n change",
                  breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                  labels=c("Major", "Minor", "Patch", "Pre-release", "Build"),
                  guide=FALSE,
                  start = 0.2, end = 1.0) +
  scale_x_discrete(name = "", labels = c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "Proportion of releases changing semantic version",
                     labels = comma) +
  theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1), legend.position="right", legend.direction="vertical")

p1 = ggplot(semver_dist_pt2.prop, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_grid( ~ role, scales="free") +
  scale_fill_grey(name="Level of\n change",
                  breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                  labels=c("Major", "Minor", "Patch", "Pre-release", "Build"),
                  guide=FALSE,
                  start = 0.2, end = 1.0) +
  scale_x_discrete(name = "", labels = c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "",
                     labels = comma) +
  theme_bw() + theme(axis.text.x=element_text(angle = 45, hjust = 1), legend.position="bottom")
grid.arrange(p0, p1, ncol = 2)
dev.off()




### Can we identify same-day releases from the version number being changed?
### try to fit regression models to predict same-day releases, using the version number as predictor

# find the cases of same-day releases and not-same-day releases (only pattern 1)
# the idea is: mark with zero the rows that are in 'releases' but not in 'same_day_releases_pattern_1'
# and mark with 1 the rows that are in both tables

# set keys for joining tables
setkey(releases, package_name, package_version_num_1, package_version_num_2, package_version_timestamp_1, package_version_timestamp_2)
setkey(same_day_releases_pattern_1, package_name, package_version_num_1, package_version_num_2, package_version_timestamp_1, package_version_timestamp_2)

# generate a new column called 'same_day' and set value 1 for all same-day releases rows
# the rows that are not same-day releases are give NA on the 'same_day' column
sdr = releases[same_day_releases_pattern_1, same_day := 1]
sdr = sdr[, .(predictor = package_version_change_was_in, predict = same_day)]
sdr$predict = as.factor(sdr$predict)
sdr$predictor = as.factor(sdr$predictor)

# check if it's all right
nrow(releases) == nrow(sdr[predict == 0]) + nrow(sdr[predict == 1])

#fit logistic regression model
logit.model = lrm(predict ~ predictor, data = sdr)
logit.model

#fit random forest model
train = sample(sdr$predict, (nrow(sdr)-(nrow(sdr)/3)))
rf.model = randomForest(predict ~ ., data = sdr, subset = train, type = "classification")
rf.model