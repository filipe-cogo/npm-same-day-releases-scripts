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
### RQ1.4. How the semantic version numbers are changing on same-day releases?
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")

same_day_releases_pattern_1 = releases[same_day_release_1 == TRUE]
same_day_releases_pattern_2 = dependencies[same_day_release_2 == TRUE]

####################################################
# Boxplot showing the distribution of version number changes on same-day releases on pattern 1
#####################################################

#calculate the number of times each package changes the semantic version in a same-day release (pattern 1)
semver_dist_pt1 = same_day_releases_pattern_1[, .(count = .N), by = .(package_name, package_version_change_was_in)]

pdf(file = "RQ1/images/semver_change_dist_pt1.pdf")
png(file = "RQ1/images/semver_change_dist_pt1.png")
ggplot(semver_dist_pt1, aes(x = package_version_change_was_in, y = count, fill = package_version_change_was_in)) +
  geom_boxplot() +
  scale_x_discrete(name="Level of change",
                   breaks=c("major", "minor", "patch", "pre_release", "build"),
                   labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_log10(name = "Number of releases changing semantic version",
                labels = comma) +
  #ggtitle("Distribution of semantic version number changes\non same-day releases per package (Pattern 1)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()

# unmelt to calculate proportions
semver_dist_pt1.cast = data.table::dcast(formula = package_name ~  package_version_change_was_in, data = semver_dist_pt1, fun.aggregate = sum, value.var = "count")
semver_dist_pt1.cast$build_proportion = semver_dist_pt1.cast$build / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$major_proportion = semver_dist_pt1.cast$major / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$minor_proportion = semver_dist_pt1.cast$minor / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$patch_proportion = semver_dist_pt1.cast$patch / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$pre_release_proportion = semver_dist_pt1.cast$pre_release / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast = semver_dist_pt1.cast[, .(package_name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

# melt for plot
semver_dist_pt1.prop = melt(semver_dist_pt1.cast, id = "package_name")

pdf(file = "RQ1/images/semver_change_dist_pt1_proportion.pdf")
png(file = "RQ1/images/semver_change_dist_pt1_proportion.png")
ggplot(semver_dist_pt1.prop, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(name="",
                   breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                   labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "Proportion of semantic version level change",
                     labels = comma) +
  ggtitle("Self-driven same-day releases") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
dev.off()



### Boxplot showing the distribution of version number changes on same-day releases on pattern 2

#calculate the number of times each package changes the semantic version in a same-day release (pattern 2)
#for the clients
semver_dist_pt2_client = same_day_releases_pattern_2[, .(count = .N, role = "Client"), by = .(name = client_name, change = client_version_change_was_in)]
#for the providers
semver_dist_pt2_provider = same_day_releases_pattern_2[, .(count = .N, role = "Provider"), by = .(name = dependency_name, change = dependency_version_change_was_in)]

semver_dist_pt2 = rbind(semver_dist_pt2_client, semver_dist_pt2_provider)

pdf(file = "RQ1/images/semver_change_dist_pt2.pdf")
#png(file = "RQ1/images/semver_change_dist_pt2.png")
ggplot(semver_dist_pt2, aes(x = change, y = count, fill = change)) +
  geom_boxplot() +
  scale_x_discrete(name="Level of change",
                   breaks=c("major", "minor", "patch", "pre_release", "build"),
                   labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_log10(name = "Number of releases changing semantic version",
                labels = comma) +
  #coord_flip() +
  facet_wrap( ~ role, scales="free") +
  #ggtitle("Distribution of semantic version number changes\non triggered same-day releases (Pattern 2)") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
dev.off()

# unmelt to calculate proportions
# only clients
semver_dist_pt2.cast = data.table::dcast(formula = name ~  change, data = semver_dist_pt2[role == "Client"], fun.aggregate = sum, value.var = "count")
semver_dist_pt2.cast$build_proportion = semver_dist_pt2.cast$build / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$major_proportion = semver_dist_pt2.cast$major / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$minor_proportion = semver_dist_pt2.cast$minor / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$patch_proportion = semver_dist_pt2.cast$patch / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast$pre_release_proportion = semver_dist_pt2.cast$pre_release / (semver_dist_pt2.cast$build + semver_dist_pt2.cast$major + semver_dist_pt2.cast$minor + semver_dist_pt2.cast$patch + semver_dist_pt2.cast$pre_release)
semver_dist_pt2.cast.client = semver_dist_pt2.cast[, .(name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

# only providers
semver_dist_pt2.cast = data.table::dcast(formula = name ~  change, data = semver_dist_pt2[role == "Provider"], fun.aggregate = sum, value.var = "count")
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

pdf(file = "RQ1/images/semver_change_dist_pt2.pdf")
#png(file = "RQ1/images/semver_change_dist_pt2_proportion.png")
ggplot(semver_dist_pt2.prop, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_grid( ~ role, scales="free") +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(name="",
                   breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                   labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "Proportion of semantic version level change",
                     labels = comma) +
  ggtitle("Provider-driven same-day release") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
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
