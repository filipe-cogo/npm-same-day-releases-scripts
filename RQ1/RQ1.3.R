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
### RQ1.3. What is the relation between the popularity of a package and the number of same-day releases?
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")

same_day_releases_pattern_1 = releases[same_day_release_1 == TRUE]
same_day_releases_pattern_2 = dependencies[same_day_release_2 == TRUE]

#####################################################################
### Number of packages using a provider vs. percentage (proportion)
### of same-day releases of that provider
#####################################################################


# proportion of same-day releases of a provider package
sd_proportion = same_day_releases_pattern_1_count(releases)

# calculate the number of cliens of each provider
number_of_clients_of_provider = data.table()
for (i in 1:length(sd_proportion$package_name)){
  provider = sd_proportion$package_name[i]
  number_of_clients_of_provider = rbind(number_of_clients_of_provider, dependencies[dependency_name == provider, .(.GRP), by = .(client_name, dependency_name) ][, .(number_of_clients = .N), by = .(provider_name = dependency_name)])
}

# create structure to plot
package_publishing_sdr_usage = merge(sd_proportion, number_of_clients_of_provider, by.x = "package_name", by.y = "provider_name")

# plot in a scatterplot
pdf(file = "RQ1/images/num_pkg_using_provider_vs_percent_sd_releases.pdf")
png(file = "RQ1/images/num_pkg_using_provider_vs_percent_sd_releases.png")
ggplot(package_publishing_sdr_usage, aes(x = number_of_clients, y = proportion_same_day_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  geom_smooth(colour = "black") +
  scale_x_log10(name = "Number of clients") +
  scale_y_continuous(name = "Proportion of self-driven same-day releases",
                     labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  theme(legend.position="none", plot.title = element_text(hjust = 1.0)) +
  theme_bw()
dev.off()

#####################################################################
### What is the number of releases and clients using same-day releases from the top-10 packages 
### that publishes same-day releases?
#####################################################################

# # Calculate how many times each package publishes same-day release
# # sort by number of same-day releases
# sdp1.count = releases[same_day_release_1 == TRUE, .(.GRP), by = .(package_name, package_version_num_1, package_version_num_2)][, .(num_releases = .N), by = package_name][order(-num_releases)]
# 
# # check the popularity of the top-10 packages that publish same-day releases (pattern 1)
# dependency_usage_total = data.table()
# dependency_usage_per_client = data.table()
# dependency_versioning_change = data.table()
# dependency_semver_change = data.table()
# dependency_clients_using = data.table()
# 
# 
# setkey(same_day_releases_pattern_1, package_name, package_version_num_2)
# setkey(dependencies, dependency_name, dependency_version_max_satisf_2)
# 
# for (i in 1:length(sdp1.count[order(-num_releases)][1:10]$package_name)){
#   name = sdp1.count[order(-num_releases)][1:10]$package_name[i]
#   
#   dependency_usage_total = rbind(dependency_usage_total, dependencies[dependency_name == name][same_day_releases_pattern_1, nomatch = 0][, .(releases_using = .N), by = .(dependency_name)])
#   dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name][same_day_releases_pattern_1, nomatch = 0][, .(provider_usage_per_client = .N), by = .(dependency_name, client_name)][order(-provider_usage_per_client)])
#   dependency_clients_using = rbind(dependency_clients_using, dependencies[dependency_name == name][same_day_releases_pattern_1, nomatch = 0][, .(.GRP), by = .(client_name, dependency_name)][, .(num_clients_using = .N), by = dependency_name])
#   #dependency_versioning_change = rbind(dependency_versioning_change, dependencies[dependency_name == name, .(num_versioning_statement_change = .N, type = paste(dependency_versioning_type_1, dependency_versioning_type_2, sep = " to ")), by = .(dependency_name, dependency_versioning_type_1, dependency_versioning_type_2)][, .(dependency_name, num_versioning_statement_change, type )])
#   #dependency_semver_change = rbind(dependency_semver_change, releases[package_name == name, .(num_semver_change = .N), by = .(package_name, package_version_change_was_in)])
#   #dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name, .(count = .N), by = .(dependency_name, client_name, dependency_version_max_satisf_1, dependency_version_max_satisf_2)])
# }
# 
# # mount table to summarize the characteristics of the top-10 same-day releases
# 
# # how many clients and how many client releases are using same-day releases from the top-10?
# top_10_sdr_usage = merge(dependency_usage_total, dependency_clients_using)
# 
# sdp1.count.xt = xtable(top_10_sdr_usage, caption = "Top-10 packages that publish voluntary same-day releases")
# digits(sdp1.count.xt) <- c(0,0,0,0)
# names(sdp1.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of client releases\\using same-day release}", "\\pbox{20cm}{Number of client packages\\using same-day releases}")
# print(sdp1.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/usage_top_10_clients_publishing_same_day_releases/usage_top_10_clients_publishing_same_day_releases_pt1.tex")
# 
# # how top-10 same-day releases changes the versioning statements?
# #dcast(formula = dependency_name ~ type, data = dependency_versioning_change, fun.aggregate = sum, value.var = "num_versioning_statement_change")
# 
# # how top-10 same-day releases changes the version?
# #dcast(formula = package_name ~ package_version_change_was_in, data = dependency_semver_change, fun.aggregate = sum, value.var = "num_semver_change")
# 
# 
# # 10 boxplots, each one showing the distribution of
# # number of times each client have used a same-day
# # of some provider. For example, if a provider P (on the top-10)
# # published 10 same-day releases and the client C1
# # uses it 2 times and the client C2 uses is 10 times, 
# # then the boxplot will have two points, one for each
# # client. The values of the points on y-axis will be
# # 2 and 10, respectively
# 
# # for pattern 1
# dependency_usage_per_client
# 
# #pdf(file = "RQ1/images/dist_num_clients_using_same_day_releases_pt1.pdf")
# png(file = "RQ1/images/dist_num_clients_using_same_day_releases_pt1.png")
# ggplot(dependency_usage_per_client, aes(x = dependency_name, y = provider_usage_per_client, fill = dependency_name)) +
#   geom_boxplot() +
#   scale_fill_grey(name = "Top-10\npackage",
#                   breaks = unique(dependency_usage_per_client$dependency_name),
#                   labels = unique(dependency_usage_per_client$dependency_name),
#                   guide = FALSE,
#                   start = 0.0, end = 1.0) +
#   scale_x_discrete(name = "", labels = unique(dependency_usage_per_client$dependency_name)) +
#   scale_y_log10(name = "Same-day release usage",
#                 labels = comma) +
#   #ggtitle("") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
# dev.off()
# 
# 
# ### For pattern 2
# 
# sdp2.count = dependencies[same_day_release_2 == TRUE, .(.GRP), by = .(client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2)][, .(num_releases = .N), by = client_name][order(-num_releases)][1:10]
# 
# setkey(same_day_releases_pattern_2, client_name, client_version_num_2)
# setkey(dependencies, dependency_name, dependency_version_max_satisf_2)
# 
# # check the popularity of the top-10 packages that publish same-day releases (pattern 2)
# dependency_usage_total = data.table()
# dependency_usage_per_client = data.table()
# dependency_versioning_change = data.table()
# dependency_semver_change = data.table()
# dependency_clients_using = data.table()
# 
# for (i in 1:length(sdp2.count$client_name)){
#   name = sdp2.count$client_name[i]
#   print(name)
#   
#   dependency_usage_total = rbind(dependency_usage_total, dependencies[dependency_name == name][same_day_releases_pattern_2, nomatch = 0][, .(releases_using = .N), by = .(dependency_name)])
#   dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name][same_day_releases_pattern_2, nomatch = 0][, .(provider_usage_per_client = .N), by = .(dependency_name, client_name)][order(-provider_usage_per_client)])
#   dependency_clients_using = rbind(dependency_clients_using, dependencies[dependency_name == name][same_day_releases_pattern_2, nomatch = 0][, .(.GRP), by = .(client_name, dependency_name)][, .(num_clients_using = .N), by = dependency_name])
#   #dependency_versioning_change = rbind(dependency_versioning_change, dependencies[dependency_name == name, .(num_versioning_statement_change = .N, type = paste(dependency_versioning_type_1, dependency_versioning_type_2, sep = " to ")), by = .(dependency_name, dependency_versioning_type_1, dependency_versioning_type_2)][, .(dependency_name, num_versioning_statement_change, type )])
#   #dependency_semver_change = rbind(dependency_semver_change, releases[package_name == name, .(num_semver_change = .N), by = .(package_name, package_version_change_was_in)])
#   #dependency_usage_per_client = rbind(dependency_usage_per_client, dependencies[dependency_name == name, .(count = .N), by = .(dependency_name, client_name, dependency_version_max_satisf_1, dependency_version_max_satisf_2)])
# }
# 
# # mount table to summarize the characteristics of the top-10 same-day releases
# 
# # how many clients and how many client releases are using same-day releases from the top-10?
# top_10_sdr_usage = merge(dependency_usage_total, dependency_clients_using)
# 
# sdp2.count.xt = xtable(top_10_sdr_usage, caption = "Top-10 packages that publish voluntary same-day releases")
# digits(sdp2.count.xt) <- c(0,0,0,0)
# names(sdp1.count.xt) = c("\\pbox{20cm}{Package name}", "\\pbox{20cm}{Number of client releases\\using same-day release}", "\\pbox{20cm}{Number of client packages\\using same-day releases}")
# print(sdp2.count.xt, sanitize.colnames.function = identity, file = "RQ1/tables/usage_top_10_clients_publishing_same_day_releases/usage_top_10_clients_publishing_same_day_releases_pt2.tex")
# 
# # how top-10 same-day releases changes the versioning statements?
# #dcast(formula = dependency_name ~ type, data = dependency_versioning_change, fun.aggregate = sum, value.var = "num_versioning_statement_change")
# 
# # how top-10 same-day releases changes the version?
# #dcast(formula = package_name ~ package_version_change_was_in, data = dependency_semver_change, fun.aggregate = sum, value.var = "num_semver_change")
# 
# # 10 boxplots, each one showing the distribution of
# # number of times each client have used a same-day
# # of some provider. For example, if a provider P
# # published 10 same-day releases and the client C1
# # uses it 2 times and the client C2 uses is 10 times, 
# # then the boxplot will have two points, one for each
# # client. The values of the points on y-axis will be
# # 2 and 10, respectively
# 
# # for pattern 2
# dependency_usage_per_client
# 
# png(file = "RQ1/images/dist_num_clients_using_same_day_releases_pt2.png")
# ggplot(dependency_usage_per_client, aes(x = dependency_name, y = provider_usage_per_client, fill = dependency_name)) +
#   geom_boxplot() +
#   scale_fill_grey(name = "Top-10\npackage",
#                   breaks = unique(dependency_usage_per_client$dependency_name),
#                   labels = unique(dependency_usage_per_client$dependency_name),
#                   guide = FALSE,
#                   start = 0.0, end = 1.0) +
#   scale_x_discrete(name = "", labels = unique(dependency_usage_per_client$dependency_name)) +
#   scale_y_log10(name = "Same-day release usage",
#                 labels = comma) +
#   #ggtitle("Usage of same-day release (pattern 2) per client") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
# dev.off()
