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
library(effsize)
library(survival)
library(survminer)

#############################################
### Read CSV dataset
#############################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-data"
setwd(wd)

dependencies = fread("popular-packages-dependencies.csv")
releases = fread("popular-packages-releases.csv")

#####################################################################
### RQ.1. What are the characteristics of same-day releases on npm ?
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")
source("RQ1/iPlot4R.R")


####################################################
### Time to update to same-day releases vs.
### time to update to a regular release
#####################################################

#remove if want to consider on popular packages that depends on popular packages
dependencies_all = fread("/Users/filipe/Dropbox/npm-same-day-releases-data/npmdep.csv")

### For self-driven same-day releases ###
# calculate client releases using a same-day release pattern 1
client_releases_using_same_day_release_ptrn1 = client_releases_using_same_day(dependencies, releases[same_day_release_1 == TRUE]) #use 'dependencies' as first argument if want to consider only popular packages that depends on popular packages
# calculate client releases using a regular release
client_releases_using_regular_releases_1 = client_releases_using_same_day(dependencies, releases[same_day_release_1 == FALSE]) #use 'dependencies' as first argument if want to consider only popular packages that depends on popular packages

# filter only the upgrades for a same-day and regular release
client_releases_using_same_day_release_ptrn1 = client_releases_using_same_day_release_ptrn1[package_version_change == "upgrade"]
client_releases_using_regular_releases_1 = client_releases_using_regular_releases_1[package_version_change == "upgrade"]

# calculate the time for upgrade to a same-day release
time_update_same_day_release_1 = client_releases_using_same_day_release_ptrn1[package_name != client_name, .(package_name, package_version_num_1, package_version_num_2, client_name, client_version_num_1, client_version_num_2, adotpion_time = as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(package_timestamp_max_satisf_2), units = "days")), package_version_change_was_in = package_version_change, type = "sd")]
time_update_regular_release_1 = client_releases_using_regular_releases_1[package_name != client_name, .(package_name, package_version_num_1, package_version_num_2, client_name, client_version_num_1, client_version_num_2, adotpion_time = as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(package_timestamp_max_satisf_2), units = "days")), package_version_change_was_in = package_version_change, type = "reg")]

### For provider-driven same-day releases ####
# calculate client releases using a same-day release pattern 2
sdr2_clients = same_day_releases_pattern_2.clients(dependencies)
client_releases_using_same_day_release_ptrn2 = client_releases_using_same_day(dependencies, sdr2_clients[same_day_release_2 == TRUE]) #use 'dependencies' as first argument if want to consider on popular packages that depends on popular packages
# calculate client releases using a regular release
client_releases_using_regular_releases_2 = client_releases_using_same_day(dependencies, sdr2_clients[same_day_release_2 == FALSE]) #use 'dependencies' as first argument if want to consider on popular packages that depends on popular packages

# filter only the upgrades for a same-day and regular release
client_releases_using_same_day_release_ptrn2 = client_releases_using_same_day_release_ptrn2[package_version_change == "upgrade"]
client_releases_using_regular_releases_2 = client_releases_using_regular_releases_2[package_version_change == "upgrade"]

# calculate the time for upgrade to a same-day release
time_update_same_day_release_2 = client_releases_using_same_day_release_ptrn2[package_name != client_name, .(package_name, package_version_num_1, package_version_num_2, client_name, client_version_num_1, client_version_num_2, adotpion_time = as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(package_timestamp_max_satisf_2), units = "days")), package_version_change_was_in = package_version_change, type = "sd")]
time_update_regular_release_2 = client_releases_using_regular_releases_2[package_name != client_name, .(package_name, package_version_num_1, package_version_num_2, client_name, client_version_num_1, client_version_num_2, adotpion_time = as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(package_timestamp_max_satisf_2), units = "days")), package_version_change_was_in = package_version_change, type = "reg")]

# bind rows to put in a format to plot
time_update_same_day_release_1$type = "sd_1"
time_update_regular_release_1$type = "reg_1"
time_update_1 = rbind(time_update_same_day_release_1, time_update_regular_release_1)
time_update_1$jack = "g1"
time_update_1$facet = "Self-driven same-day release"

time_update_same_day_release_2$type = "sd_2"
time_update_regular_release_2$type = "reg_2"
time_update_2 = rbind(time_update_same_day_release_2, time_update_regular_release_2)
time_update_2$jack = "g2"
time_update_2$facet = "Provider-driven same-day release"

time_update = rbind(time_update_1, time_update_2)

summary(time_update_same_day_release_1$adotpion_time)
summary(time_update_regular_release_1$adotpion_time)

summary(time_update_same_day_release_2$adotpion_time)
summary(time_update_regular_release_2$adotpion_time)

summary(time_update)

#plot a violin of the two distributions
pdf(file = "RQ1/images/clients_adopting_same_day_releases.pdf", height = 5)
  p0 = plot_violin(time_update_1,
            xcol = "jack",
            xlab = "", 
            xticklab = c("Regular vs. Same-day"), 
            ylab = "Time to adoption (days)", 
            ycol = "adotpion_time", 
            groupcol = "type",
            split = TRUE, 
            transformation = "log10", 
            show_legend = FALSE,
            coloured = TRUE,
            fontsize = 14) +
  #geom_segment(aes(x = 0.6, xend = 0.8, y = 24, yend = 24), arrow = arrow()) +
  ggtitle("Self-driven\nsame-day release") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5))
  
  p1 = plot_violin(time_update_2,
                   xcol = "jack",
                   xlab = "", 
                   xticklab = c("Regular vs. Same-day"), 
                   ylab = "Time to adoption (days)",
                   ycol = "adotpion_time", 
                   groupcol = "type",
                   split = TRUE, 
                   transformation = "log10", 
                   show_legend = FALSE,
                   coloured = TRUE,
                   fontsize = 14) +
    #geom_segment(aes(x = 0.5, xend = 0.7, y = 24, yend = 24), arrow = arrow()) +
    ggtitle("Provider-driven\nsame-day release") +
    theme(plot.title = element_text(hjust = 0.5, size = 14), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5))

grid.arrange(p0, p1, ncol = 2)
dev.off()

#plot a violin of the two distributions separated by major, minor, patch ...
pdf(file = "RQ1/images/clients_adopting_same_day_releases_ptrn_1_divided_by_sv_change_level.pdf")
png(file = "RQ1/images/clients_adopting_same_day_releases_ptrn_1_divided_by_sv_change_level.png")
  plot_violin(time_update,
            xcol = "package_pttnr1_version_change_was_in",
            xlab = "", 
            xticklab = c("Minor", "Patch", "Major", "Pre-release", "Build"), 
            ylab = "Time to adoption (days)", 
            ycol = "adotpion_time", 
            groupcol = "type",
            split = TRUE, 
            transformation = "log10", 
            show_legend = TRUE,
            legend_title = "Release\ntype",
            legend_labels = c("Regular", "Same-day"),
            coloured = TRUE,
            fontsize = 14) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5)) +
  facet_wrap(~jack)
dev.off()


#check if difference in mean is statistical significant
wilcox.test(time_update_same_day_release_1$adotpion_time, time_update_regular_release_1$adotpion_time, alternative = "less")
VD.A(time_update_same_day_release_1$adotpion_time, time_update_regular_release_1$adotpion_time)

wilcox.test(time_update_same_day_release_2$adotpion_time, time_update_regular_release_2$adotpion_time, alternative = "greater")
VD.A(time_update_same_day_release_2$adotpion_time, time_update_regular_release_2$adotpion_time)

####################################################
# Time for a client to stop using the same-day vs. regular releases
#####################################################

### for self-driven same-day releases ###
sdsd_rels = releases[same_day_release_1 == TRUE]

#find adoptions of self-driven same-day release
setkey(dependencies, dependency_name, dependency_version_max_satisf_2)
setkey(sdsd_rels, package_name, package_version_num_2)

adoptions_sdr = dependencies[sdsd_rels, nomatch = 0][upgrade == TRUE][, .(client_name, release_order, timestamp_client_release = client_version_timestamp_2, time_to_adopt = difftime(ymd_hms(client_version_timestamp_2), ymd_hms(dependency_timestamp_max_satisf_2), units = "hours"), pkg_sd_release = dependency_name, timestamp_sd_release = dependency_timestamp_max_satisf_2, adopted_sd_release = dependency_version_max_satisf_2)]

#find upgrades from self-driven same-day releases
setkey(dependencies, dependency_name, dependency_version_max_satisf_1)
setkey(sdsd_rels, package_name, package_version_num_2)

upgrades_sdr = dependencies[sdsd_rels, nomatch = 0][upgrade == TRUE][, .(client_name, release_order, timestamp_client_release = client_version_timestamp_2, time_to_abandon = difftime(ymd_hms(client_version_timestamp_2), ymd_hms(dependency_timestamp_max_satisf_2), units = "hours"), pkg_sd_release = dependency_name, timestamp_sd_release = dependency_timestamp_max_satisf_1, abandoned_sd_release = dependency_version_max_satisf_1)]

#find self-driven same-day releases that were adopted AND updated
setkey(adoptions_sdr, client_name, pkg_sd_release, adopted_sd_release)
setkey(upgrades_sdr, client_name, pkg_sd_release, abandoned_sd_release)

adopted_upgraded_sdr = adoptions_sdr[upgrades_sdr, nomatch = 0][, .(client_name, pkg_sd_release, adopted_sd_release, num_release_until_upgrade = (i.release_order - release_order), time_using_sd_release = difftime(ymd_hms(i.timestamp_client_release), ymd_hms(timestamp_client_release), units = "days"), upgraded = TRUE)][num_release_until_upgrade > 0]

#find self-driven same-day releases that were only adopted and never updated
setkey(adoptions_sdr, client_name, pkg_sd_release, adopted_sd_release)
setkey(adopted_upgraded_sdr, client_name, pkg_sd_release, adopted_sd_release)

max_timestamp_observed = max(ymd_hms(sdsd_rels$package_version_timestamp_2))
only_adoptions_sdr = adoptions_sdr[!adopted_upgraded_sdr][, .(client_name, pkg_sd_release, adopted_sd_release, num_release_until_upgrade = NA, time_using_sd_release = difftime(max_timestamp_observed, ymd_hms(timestamp_client_release), units = "days"), upgraded = FALSE)]

#merge adopted AND upgraded with never upgraded same-day releases
sdr_adoption_analysis = rbind(adopted_upgraded_sdr, only_adoptions_sdr)

#agrregate by client packages getting the median time for abandoning the use of self-driven same-day release
sdr_adoption_analysis = sdr_adoption_analysis[, .(median_time_using_release = median(time_using_sd_release)), by = .(client_name, upgraded)]

### perform the same analysis for regular releases ###
reg_rels = releases[same_day_release_1 == FALSE]

#find adoptions of regular releases
setkey(dependencies, dependency_name, dependency_version_max_satisf_2)
setkey(reg_rels, package_name, package_version_num_2)

adoptions_reg = dependencies[reg_rels, nomatch = 0][upgrade == TRUE][, .(client_name, release_order, timestamp_client_release = client_version_timestamp_2, time_to_adopt = difftime(ymd_hms(client_version_timestamp_2), ymd_hms(dependency_timestamp_max_satisf_2), units = "hours"), pkg_reg_release = dependency_name, timestamp_reg_release = dependency_timestamp_max_satisf_2, adopted_reg_release = dependency_version_max_satisf_2)]

#find upgrades from regular releases
setkey(dependencies, dependency_name, dependency_version_max_satisf_1)
setkey(reg_rels, package_name, package_version_num_2)

upgrades_reg = dependencies[reg_rels, nomatch = 0][upgrade == TRUE][, .(client_name, release_order, timestamp_client_release = client_version_timestamp_2, time_to_abandon = difftime(ymd_hms(client_version_timestamp_2), ymd_hms(dependency_timestamp_max_satisf_2), units = "hours"), pkg_reg_release = dependency_name, timestamp_reg_release = dependency_timestamp_max_satisf_1, abandoned_reg_release = dependency_version_max_satisf_1)]

#find regular that were adopted AND updated
setkey(adoptions_reg, client_name, pkg_reg_release, adopted_reg_release)
setkey(upgrades_reg, client_name, pkg_reg_release, abandoned_reg_release)

adopted_upgraded_reg = adoptions_reg[upgrades_reg, nomatch = 0][, .(client_name, pkg_reg_release, adopted_reg_release, num_release_until_upgrade = (i.release_order - release_order), time_using_reg_release = difftime(ymd_hms(i.timestamp_client_release), ymd_hms(timestamp_client_release), units = "days"), upgraded = TRUE)][num_release_until_upgrade > 0]

#find regular releases that were only adopted and never updated
setkey(adoptions_reg, client_name, pkg_reg_release, adopted_reg_release)
setkey(adopted_upgraded_reg, client_name, pkg_reg_release, adopted_reg_release)

max_timestamp_observed = max(ymd_hms(reg_rels$package_version_timestamp_2))
only_adoptions_reg = adoptions_reg[!adopted_upgraded_reg][, .(client_name, pkg_reg_release, adopted_reg_release, num_release_until_upgrade = NA, time_using_reg_release = difftime(max_timestamp_observed, ymd_hms(timestamp_client_release), units = "days"), upgraded = FALSE)]

#merge adopted AND upgraded with never upgraded regular releases
reg_adoption_analysis = rbind(adopted_upgraded_reg, only_adoptions_reg)

#agrregate by client packages getting the median time for abandoning the use of regular releases
reg_adoption_analysis = reg_adoption_analysis[, .(median_time_using_release = median(time_using_reg_release)), by = .(client_name, upgraded)]

#perform survival analysis
surv_sdr = Surv(sdr_adoption_analysis$median_time_using_release, sdr_adoption_analysis$upgraded)
fit_sdr = survfit(surv_sdr ~ 1, data = sdr_adoption_analysis)

surv_reg = Surv(reg_adoption_analysis$median_time_using_release, reg_adoption_analysis$upgraded)
fit_reg = survfit(surv_reg ~ 1, data = reg_adoption_analysis)

fit = list(sd = fit_sdr, reg = fit_reg)

#names(sdr_adoption_analysis) = c("client_name", "provider_name", "adopted_release", "num_releases_until_upgrade", "time_using_release", "upgrade")
#names(reg_adoption_analysis) = c("client_name", "provider_name", "adopted_release", "num_releases_until_upgrade", "time_using_release", "upgrade")

adoption_analysis = rbind(sdr_adoption_analysis, reg_adoption_analysis)

pdf(file = "RQ1/images/probability_of_upgrading_from_release.pdf", height = 4, compress = TRUE)
ggsurvplot(fit, data = adoption_analysis, combine = TRUE, fun = "event", conf.int = TRUE, ggtheme = theme_bw(), legend = "bottom", legend.title = "Release type", legend.labs = c("Self-driven same-day", "Regular"), palette = c("#FDE725FF", "#440154FF"), surv.median.line = "hv", font.tickslab = 13, font.x = 14, font.legend = 13, font.y = 14) + 
  scale_x_continuous(label = comma) + 
  xlab("Time (days)") + 
  ylab("Cumulative probability of\nupgrading from release")
dev.off()


####################################################
# Distribution of version number changes on same-day releases
#####################################################

same_day_releases_pattern_1 = releases[same_day_release_1 == TRUE]
regular_releases_pattern_1 = releases[same_day_release_1 == FALSE]

same_day_releases_pattern_2 = dependencies[same_day_release_2 == TRUE]
regular_releases_pattern_2 = dependencies[same_day_release_2 == FALSE]

### For self-driven same-day releases ###

#calculate the number of times each package changes the semantic version in a same-day release (pattern 1)
semver_dist_pt1 = same_day_releases_pattern_1[, .(count = .N), by = .(package_name, package_version_change_was_in)]
semver_dist_reg = regular_releases_pattern_1[, .(count = .N), by = .(package_name, package_version_change_was_in)]

# unmelt to calculate proportions
#same-day releases
semver_dist_pt1.cast = data.table::dcast(formula = package_name ~  package_version_change_was_in, data = semver_dist_pt1, fun.aggregate = sum, value.var = "count")
semver_dist_pt1.cast$build_proportion = semver_dist_pt1.cast$build / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$major_proportion = semver_dist_pt1.cast$major / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$minor_proportion = semver_dist_pt1.cast$minor / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$patch_proportion = semver_dist_pt1.cast$patch / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast$pre_release_proportion = semver_dist_pt1.cast$pre_release / (semver_dist_pt1.cast$build + semver_dist_pt1.cast$major + semver_dist_pt1.cast$minor + semver_dist_pt1.cast$patch + semver_dist_pt1.cast$pre_release)
semver_dist_pt1.cast = semver_dist_pt1.cast[, .(package_name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

#regular releases
semver_dist_reg.cast = data.table::dcast(formula = package_name ~  package_version_change_was_in, data = semver_dist_reg, fun.aggregate = sum, value.var = "count")
semver_dist_reg.cast$build_proportion = semver_dist_reg.cast$build / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$major_proportion = semver_dist_reg.cast$major / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$minor_proportion = semver_dist_reg.cast$minor / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$patch_proportion = semver_dist_reg.cast$patch / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$pre_release_proportion = semver_dist_reg.cast$pre_release / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast = semver_dist_reg.cast[, .(package_name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

#summarize values
summary(semver_dist_pt1.cast)
summary(semver_dist_reg.cast)

#store mean proportions on a table
proportions_semver = data.table(#"Type" = c("sdr", "reg") ,
  "Major" = c(percent(round(mean(semver_dist_pt1.cast$major_proportion), 5)), percent(round(mean(semver_dist_reg.cast$major_proportion), 5))),
  "Minor" = c(percent(round(mean(semver_dist_pt1.cast$minor_proportion), 5)), percent(round(mean(semver_dist_reg.cast$minor_proportion), 5))),
  "Patch" = c(percent(round(mean(semver_dist_pt1.cast$patch_proportion), 5)), percent(round(mean(semver_dist_reg.cast$patch_proportion), 5))),
  "Pre-release" = c(percent(round(mean(semver_dist_pt1.cast$pre_release_proportion), 5)), percent(round(mean(semver_dist_reg.cast$pre_release_proportion), 5))),
  "Build" = c(percent(round(mean(semver_dist_pt1.cast$build_proportion), 5)), percent(round(mean(semver_dist_reg.cast$build_proportion), 5))))

#proportions_semver = dcast(melt(proportions_semver, id.vars = "Type"), variable ~ Type)
#names(proportions_semver) = c("", "\\pbox{20cm}{Regular\\\\release}", "\\pbox{20cm}{Same-day\\\\release}")

proportions_semver = cbind(c("Same-day release", "Regular release"), proportions_semver)


sdr1.xt = xtable(proportions_semver, caption = "Mean proportion of semantic version numbering change on self-driven same-day releases", label = "tab:prop_semver_change_self_sdr")
print(sdr1.xt, booktabs = TRUE, include.rownames = FALSE, caption.placement = "top", sanitize.colnames.function = identity)

# melt for plot
semver_dist_pt1.prop = melt(semver_dist_pt1.cast, id = "package_name")

### For provider-driven same-day release ###

#calculate the number of times each package changes the semantic version in a provider-driven same-day release
#for the clients
semver_dist_pt2_client = same_day_releases_pattern_2[, .GRP, by = .(client_name, release_order, client_version_change_was_in)][, .(count = .N, role = "Client"), by = .(name = client_name, change = client_version_change_was_in)]
semver_dist_reg_client = regular_releases_pattern_2[, .GRP, by = .(client_name, release_order, client_version_change_was_in)][, .(count = .N, role = "Client"), by = .(name = client_name, change = client_version_change_was_in)]


#for the providers
semver_dist_pt2_provider = same_day_releases_pattern_2[, .(count = .N, role = "Provider"), by = .(name = dependency_name, change = dependency_version_change_was_in)]
semver_dist_reg_provider = regular_releases_pattern_2[, .(count = .N, role = "Provider"), by = .(name = dependency_name, change = dependency_version_change_was_in)]

#bind rows
semver_dist_pt2 = rbind(semver_dist_pt2_client, semver_dist_pt2_provider)
semver_dist_reg = rbind(semver_dist_reg_client, semver_dist_reg_provider)

# unmelt to calculate proportions of same-day releases
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

# unmelt to calculate proportions of regular releases
# only clients
semver_dist_reg.cast = data.table::dcast(formula = name ~  change, data = semver_dist_reg[role == "Client"], fun.aggregate = sum, value.var = "count")
semver_dist_reg.cast$build_proportion = semver_dist_reg.cast$build / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$major_proportion = semver_dist_reg.cast$major / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$minor_proportion = semver_dist_reg.cast$minor / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$patch_proportion = semver_dist_reg.cast$patch / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$pre_release_proportion = semver_dist_reg.cast$pre_release / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast.client = semver_dist_reg.cast[, .(name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

# only providers
semver_dist_reg.cast = data.table::dcast(formula = name ~  change, data = semver_dist_reg[role == "Provider" & change != ""], fun.aggregate = sum, value.var = "count")
semver_dist_reg.cast$build_proportion = semver_dist_reg.cast$build / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$major_proportion = semver_dist_reg.cast$major / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$minor_proportion = semver_dist_reg.cast$minor / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$patch_proportion = semver_dist_reg.cast$patch / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast$pre_release_proportion = semver_dist_reg.cast$pre_release / (semver_dist_reg.cast$build + semver_dist_reg.cast$major + semver_dist_reg.cast$minor + semver_dist_reg.cast$patch + semver_dist_reg.cast$pre_release)
semver_dist_reg.cast.provider = semver_dist_reg.cast[, .(name, major_proportion, minor_proportion, patch_proportion, pre_release_proportion, build_proportion)]

summary(semver_dist_pt2.cast.client)
summary(semver_dist_pt2.cast.provider)
summary(semver_dist_reg.cast.client)
summary(semver_dist_reg.cast.provider)

#store mean proportions on a table
proportions_semver = data.table(#"Type" = c("\\multirow{2}{*}{Same-day}", "", "\\multirow{2}{*}{Regular}", ""),
  "Type" = c("Same-day", "Same-day", "Regular", "Regular"),
  "Perspective" = c("Client", "Provider", "Client", "Provider"),
  "Major" = c(percent(round(mean(semver_dist_pt2.cast.client$major_proportion), 5)), percent(round(mean(semver_dist_pt2.cast.provider$major_proportion), 5)), percent(round(mean(semver_dist_reg.cast.client$major_proportion), 5)), percent(round(mean(semver_dist_reg.cast.provider$major_proportion), 5))),
  "Minor" = c(percent(round(mean(semver_dist_pt2.cast.client$minor_proportion), 5)), percent(round(mean(semver_dist_pt2.cast.provider$minor_proportion), 5)), percent(round(mean(semver_dist_reg.cast.client$minor_proportion), 5)), percent(round(mean(semver_dist_reg.cast.provider$minor_proportion), 5))),
  "Patch" = c(percent(round(mean(semver_dist_pt2.cast.client$patch_proportion), 5)), percent(round(mean(semver_dist_pt2.cast.provider$patch_proportion), 5)), percent(round(mean(semver_dist_reg.cast.client$patch_proportion), 5)), percent(round(mean(semver_dist_reg.cast.provider$patch_proportion), 5))),
  "\\pbox{20cm}{Pre-\\\\release}" = c(percent(round(mean(semver_dist_pt2.cast.client$pre_release_proportion), 5)), percent(round(mean(semver_dist_pt2.cast.provider$pre_release_proportion), 5)), percent(round(mean(semver_dist_reg.cast.client$pre_release_proportion), 5)), percent(round(mean(semver_dist_reg.cast.provider$pre_release_proportion), 5))),
  "Build" = c(percent(round(mean(semver_dist_pt2.cast.client$build_proportion), 5)), percent(round(mean(semver_dist_pt2.cast.provider$build_proportion), 5)),  percent(round(mean(semver_dist_reg.cast.client$build_proportion), 5)), percent(round(mean(semver_dist_reg.cast.provider$build_proportion), 5))))

proportions_semver = dcast(melt(proportions_semver, id.vars = c("Type", "Perspective")), variable ~ Type + Perspective)

sdr2.xt = xtable(proportions_semver, caption = "Mean proportion of semantic version numbering change on provider-driven same-day releases", label = "tab:prop_semver_change_provider_sdr")
print(sdr2.xt, booktabs = TRUE, include.rownames=FALSE, caption.placement = "top", sanitize.colname.function = identity, sanitize.rowname.function = identity, sanitize.text.function = identity)

#check normality of the distributions
shapiro.test(semver_dist_pt2.cast.client$major_proportion[sample(5000)])
shapiro.test(semver_dist_reg.cast.client$major_proportion[sample(5000)])

#test difference with Wilcoxon test
wilcox.test(semver_dist_pt2.cast.client$major_proportion, semver_dist_reg.cast.client$major_proportion, alternative = "greater")
wilcox.test(semver_dist_pt2.cast.client$minor_proportion, semver_dist_reg.cast.client$minor_proportion, alternative = "greater")

wilcox.test(semver_dist_pt2.cast.provider$major_proportion, semver_dist_reg.cast.provider$major_proportion, alternative = "greater")
wilcox.test(semver_dist_pt2.cast.provider$minor_proportion, semver_dist_reg.cast.provider$minor_proportion, alternative = "greater")

# melt for plot
semver_dist_pt2.melt.client = melt(semver_dist_pt2.cast.client, id = "name")
semver_dist_pt2.melt.client$role = "Client"

semver_dist_pt2.melt.provider = melt(semver_dist_pt2.cast.provider, id = "name")
semver_dist_pt2.melt.provider$role = "Provider"

# bind rows
semver_dist_pt2.prop = rbind(semver_dist_pt2.melt.client, semver_dist_pt2.melt.provider)

### plot
pdf(file = "RQ1/images/semver_change.pdf")
p0 = ggplot(semver_dist_pt1.prop, aes(x = variable, y = value, fill = variable)) +
  #geom_boxplot() +
  geom_violin(adjust = 0.5, scale = "width") +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(name="",
                   breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                   labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "Proportion of semantic\nversion level change",
                     labels = comma) +
  ggtitle("Self-driven same-day releases") +
  labs(caption="(a)") +
  theme_bw() + theme(plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), axis.text.y = element_text(hjust = 0.5), axis.text = element_text(size = 13), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

p1 = ggplot(semver_dist_pt2.prop, aes(x = variable, y = value, fill = variable)) +
  #geom_boxplot() +
  geom_violin(adjust = 0.5, scale = "width") +
  facet_grid( ~ role, scales="free") +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(name="",
                   breaks=c("major_proportion", "minor_proportion", "patch_proportion", "pre_release_proportion", "build_proportion"),
                   labels=c("Major", "Minor", "Patch", "Pre-release", "Build")) +
  scale_y_continuous(name = "Proportion of semantic\nversion level change",
                     labels = comma) +
  ggtitle("Provider-driven same-day releases") +
  labs(caption="(b)") +
  theme_bw() + theme(plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text.y = element_text(size = 13), axis.text.x = element_text(angle = 45, hjust = 1, size = 13), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))
grid.arrange(p0, p1, ncol = 1)
dev.off()