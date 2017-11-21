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
### RQ1.1. What is the relation between the popularity of a package and the number of same-day releases?
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")

same_day_releases_pattern_1 = releases[same_day_release_1 == TRUE]

#####################################################################
### What is the relation between the popularity of a package and the number of same-day releases?
#####################################################################



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