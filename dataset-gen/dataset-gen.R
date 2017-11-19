wd = "/Users/filipe/Dropbox/npm-same-day-releases-data"
setwd(wd)

set.seed(21)

library(data.table)

#############################################
### Read CSV dataset
#############################################

######################
### Read CSV dataset

dependencies = fread(input="npmdep.csv", header=TRUE, sep=",")
releases = fread(input="npmreleases.csv", 
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

#############################################
### Clean data set
#############################################

############
### Basic cleaning

# remove cases in which client_version_timestamp_1 < client_version_timestamp_2
# it occurs because of paralles branches being released
releases = releases[package_version_timestamp_1 < package_version_timestamp_2, ]
dependencies = dependencies[client_version_timestamp_1 < client_version_timestamp_2, ]

# package "all-the-packages-name" has more than 
# 3000 releases and is a toy package 
dependencies = dependencies[client_name != "all-the-package-names", ]
releases = releases[package_name != "all-the-package-names", ]

############
### Consider only popular packages (used by more than 100 other packages)

# fetch provider packages being used less than 100 times
providers.keep = dependencies[, .(.GRP), by = .(dependency_name, client_name)][, .(count = .N), by = dependency_name][count >= 100]

# keep on table 'dependencies' only provider packages being used more than 100 times
dependencies = dependencies[dependency_name %in% providers.keep$dependency_name]

# keep only usefull fields
names(dependencies)
dependencies.clean = dependencies[, .(
  client_name,
  release_order,
  client_version_num_1,
  client_version_num_2,
  client_version_change_was_in,
  client_version_timestamp_1,
  client_version_timestamp_2,
  client_version_timestamp_diff_secs,
  dependency_name,
  dependency_type,
  dependency_version_range_1,
  dependency_versioning_type_1,
  dependency_version_max_satisf_1,
  dependency_timestamp_max_satisf_1,
  last_dep_version_at_client_timestamp_1,
  dependency_version_range_2,
  dependency_versioning_type_2,
  dependency_version_max_satisf_2,
  dependency_timestamp_max_satisf_2,
  last_dep_version_at_client_timestamp_2,
  dependency_timestamp_max_satisf_diff_secs,
  dependency_version_change_was_in,
  rollback,
  upgrade,
  steady
)]

# insert information about same-day releases pattern 2
dependencies.clean[, same_day_release_2 := difftime(ymd_hms(client_version_timestamp_2), ymd_hms(dependency_timestamp_max_satisf_2), unit = "secs") <= 86400  & upgrade == TRUE]

# keep on table 'releases' only client packages being used more than 100 times
releases = releases[package_name %in% providers.keep$dependency_name]

# keep only usefull fields
names(releases)
releases.clean = releases[,.(
  package_name, 
  release_order,
  package_version_num_1,
  package_version_num_2,
  package_version_change_was_in,
  package_version_timestamp_1,
  package_version_timestamp_2,
  package_version_timestamp_diff_secs
)]

# insert information about same-day releases pattern 1
releases.clean = releases.clean[, same_day_release_1 := package_version_timestamp_diff_secs <= 86400]

# save cleaned dataset
write.csv(releases.clean, file = "popular-packages-releases.csv")
write.csv(dependencies.clean, file = "popular-packages-dependencies.csv")

# fetch cleaned dataset
#releases = fread("datasets/clean_releases.csv")
#dependencies = fread("datasets/clean_dependencies.csv")

############
### Calculate same-day releases pattern 1
same_day_releases_pattern_1 = releases.clean[package_version_timestamp_diff_secs <= 86400]
#same_day_releases_pattern_1 = releases[difftime(ymd_hms(package_version_timestamp_2), ymd_hms(package_version_timestamp_1), "secs") <= 86400]

# keep only usefull fields
names(same_day_releases_pattern_1)
same_day_releases_pattern_1.clean = same_day_releases_pattern_1[,.(
  package_name, 
  release_order,
  package_version_num_1,
  package_version_num_2,
  package_version_change_was_in,
  package_version_timestamp_1,
  package_version_timestamp_2,
  package_version_timestamp_diff_secs
)]


############
### Calculate same-day releases pattern 2
# How many client packages releases an update within 24hrs
# of an update that was released by one of it's supllier packages?

#same_day_releases_pattern_2 = same_day_releases_pattern_2.par(releases, dependencies, threads = 20)
same_day_releases_pattern_2 = dependencies.clean[(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(dependency_timestamp_max_satisf_2), unit = "secs") <= 86400)]
#same_day_releases_pattern_2 = fread("pattern-2-same-day-releases.csv")
same_day_releases_pattern_2 = same_day_releases_pattern_2[steady == FALSE]
same_day_releases_pattern_2 = same_day_releases_pattern_2[rollback == FALSE]

# keep usefull fields
names(same_day_releases_pattern_2)
same_day_releases_pattern_2.clean = same_day_releases_pattern_2[, .(
  client_name,
  release_order,
  client_version_num_1,
  client_version_num_2,
  client_version_change_was_in,
  client_version_timestamp_1,
  client_version_timestamp_2,
  client_version_timestamp_diff_secs,
  dependency_name,
  dependency_type,
  dependency_version_range_1,
  dependency_versioning_type_1,
  dependency_version_max_satisf_1,
  dependency_timestamp_max_satisf_1,
  last_dep_version_at_client_timestamp_1,
  dependency_version_range_2,
  dependency_versioning_type_2,
  dependency_version_max_satisf_2,
  dependency_timestamp_max_satisf_2,
  last_dep_version_at_client_timestamp_2,
  dependency_timestamp_max_satisf_diff_secs,
  dependency_version_change_was_in,
  rollback,
  upgrade,
  steady
)]

names(same_day_releases_pattern_2.clean)
same_day_releases_pattern_2_clients = same_day_releases_pattern_2.clean[, .(num_diff_providers_triggering_client_release = .N), by = .(client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2, release_order, client_version_change_was_in, client_version_timestamp_diff_secs)]
same_day_releases_pattern_2_providers  = same_day_releases_pattern_2.clean[, .(num_diff_clients_releases_triggered = .N), by = .(dependency_name, dependency_version_max_satisf_1, dependency_timestamp_max_satisf_1, dependency_version_max_satisf_2, dependency_timestamp_max_satisf_2, dependency_timestamp_max_satisf_diff_secs, dependency_version_change_was_in, rollback, upgrade, steady)]

############
### Save the tables with same-day releases
write.csv(same_day_releases_pattern_1.clean, "pattern-1-same-day-releases.csv")
write.csv(same_day_releases_pattern_2.clean, "pattern-2-same-day-releases.csv")
write.csv(same_day_releases_pattern_2_clients, "pattern-2-same-day-releases-clients.csv")
write.csv(same_day_releases_pattern_2_providers, "pattern-2-same-day-releases-providers.csv")
