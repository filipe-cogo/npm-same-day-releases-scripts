set.seed(21)

options(scipen = 999)

library(data.table)
library(doParallel)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(lubridate)
library(stats)
library(rms)
library(randomForest)
library(xtable)
library(viridis)
library(ggpubr)
library(effsize)

#############################################
### Read CSV dataset
#############################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-data"
setwd(wd)

dependencies = fread("popular-packages-dependencies.csv")
releases = fread("popular-packages-releases.csv")

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
  return(p.ss[1])
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

#######################################
### Sample packages for calculate code metrics
#######################################

### sample projects for self-driven same-day release ###

#calculate packages publishing self-driven same-day release
packages_on_sd1 = releases[, .GRP, by = package_name]$package_name

#calculate sample size for confidence level of 95% and confidence interval of 10%
sample_size = sample.size(c.lev = 95, margin=.5, c.interval=.1, population = length(packages_on_sd1))

#double sample size because of possibly repository URL miss on dataset
sample_size = sample_size * 2

#actually sample
sampled_packages = sample(packages_on_sd1, sample_size)

#prepare data for witing on csv
sampled_packages.output = data.table(package_name = sampled_packages)

#write on csv
write.csv(sampled_packages.output, "sampled-packages-self-driven-sdr.csv")

#generate file for calculate code churn metrics for self-driven same-day releases
packages_srd1 = read.csv("sampled-packages-self-driven-sdr.csv")

releases_sdr1 = releases[package_name %in% packages_srd1$package_name][,.(client_name = package_name, 
                                                                          client_version_num_1 = package_version_num_1,
                                                                          client_version_num_2 = package_version_num_2,
                                                                          client_version_timestamp_1 = package_version_timestamp_1,
                                                                          client_version_timestamp_2 = package_version_timestamp_2,
                                                                          release_order,
                                                                          scenario = same_day_release_1)]
write.csv(releases_sdr1, "sampled-packages-provider-driven-sdr-input-code-churn.csv")

### sample projects for provider-driven same-day release ###

#get top-10 providers triggering provier-driven same-day release
top_10_providers_sd2 = dependencies[same_day_release_2 == TRUE, .(.GRP), by = .(dependency_name, client_name)][, .(num_of_clients = .N), by = dependency_name][order(-num_of_clients)][1:10]$dependency_name

#get the clients from each of the top-10 providers
sampled_packages.output = data.table()
for (i in 1:length(top_10_providers_sd2)){
  #get provider
  provider = top_10_providers_sd2[i]
  
  #get clients of provider
  clients_of_provider = dependencies[dependency_name == provider & same_day_release_2 == TRUE, .GRP, by = .(client_name, dependency_name)]$client_name
  
  #calculate sample size
  sample_size = sample.size(c.lev = 95, margin=.5, c.interval=.1, population = length(clients_of_provider))
  
  #double sample size because of possibly repository URL miss on dataset
  sample_size = sample_size * 2
  
  #actually sample
  sampled_packages = data.table(package_name = sample(clients_of_provider, sample_size))
  
  #prepare data for witing on csv
  sampled_packages.output = rbind(sampled_packages.output, sampled_packages)
}

#write on csv
write.csv(sampled_packages.output, "sampled-packages-provider-driven-sdr.csv")

#generate file for calculate code churn metrics for self-driven same-day releases
packages_srd2 = read.csv("sampled-packages-provider-driven-sdr.csv")

releases_sdr2 = dependencies[client_name %in% packages_srd2$package_name, .GRP, by = .(client_name,
                                                                                       client_version_num_1,
                                                                                       client_version_num_2,
                                                                                       client_version_timestamp_1,
                                                                                       client_version_timestamp_2,
                                                                                       release_order,
                                                                                       scenario = same_day_release_2)]
write.csv(releases_sdr2, "sampled-packages-provider-driven-sdr-input-code-churn.csv")


#####################################################################
### RQ.2. What is the impact of same-day releases on the ecosystem?
#####################################################################

wd = "/Users/filipe/Dropbox/npm-same-day-releases-scripts"
setwd(wd)

source("RQ1/RQ1fnc.R")


########################################################
### RQ2.2. How source code metrics change on same-day releases?
########################################################

### For self-driven same-day releases ###

code_metrics_1 = fread("sampled-packages-self-driven-sdr-output-code-churn.csv")

code_metrics_sdr_1 = code_metrics_1[self_driven_sdr == TRUE]
code_metrics_reg_1 = code_metrics_1[self_driven_sdr == FALSE]

proportion_num_commits = data.table("Code metric" = c("\\multirow{2}{*}{Number of commits}", ""), "Measure" = c("Median", "Mean"),
                                    "Same-day" = c(median(code_metrics_sdr_1$num_commits), mean(code_metrics_sdr_1$num_commits)),
                                    "Regular" = c(median(code_metrics_reg_1$num_commits), mean(code_metrics_reg_1$num_commits)),
                                    "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_1$num_commits) / median(code_metrics_reg_1$num_commits)), (mean(code_metrics_sdr_1$num_commits) / mean(code_metrics_reg_1$num_commits))))

proportion_insertions = data.table("Code metric" = c("\\multirow{2}{*}{Insertions}", ""), "Measure" = c("Median", "Mean"),
                                    "Same-day" = c(median(code_metrics_sdr_1$insertions), mean(code_metrics_sdr_1$insertions)),
                                    "Regular" = c(median(code_metrics_reg_1$insertions), mean(code_metrics_reg_1$insertions)),
                                    "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_1$insertions) / median(code_metrics_reg_1$insertions)), (mean(code_metrics_sdr_1$insertions) / mean(code_metrics_reg_1$insertions))))

proportion_code_metrics = rbind(proportion_num_commits, proportion_insertions)

proportion_deletions = data.table("Code metric" = c("\\multirow{2}{*}{Deletions}", ""), "Measure" = c("Median", "Mean"),
                                   "Same-day" = c(median(code_metrics_sdr_1$deletions), mean(code_metrics_sdr_1$deletions)),
                                   "Regular" = c(median(code_metrics_reg_1$deletions), mean(code_metrics_reg_1$deletions)),
                                   "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_1$deletions) / median(code_metrics_reg_1$deletions)), (mean(code_metrics_sdr_1$deletions) / mean(code_metrics_reg_1$deletions))))

proportion_code_metrics = rbind(proportion_code_metrics, proportion_deletions)

proportion_loc = data.table("Code metric" = c("\\multirow{2}{*}{LOC's modified}", ""), "Measure" = c("Median", "Mean"),
                                  "Same-day" = c(median(code_metrics_sdr_1$lines), mean(code_metrics_sdr_1$lines)),
                                  "Regular" = c(median(code_metrics_reg_1$lines), mean(code_metrics_reg_1$lines)),
                                  "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_1$lines) / median(code_metrics_reg_1$lines)), (mean(code_metrics_sdr_1$lines) / mean(code_metrics_reg_1$lines))))

proportion_code_metrics = rbind(proportion_code_metrics, proportion_loc)

proportion_files = data.table("Code metric" = c("\\multirow{2}{*}{Files modified}", ""), "Measure" = c("Median", "Mean"),
                            "Same-day" = c(median(code_metrics_sdr_1$files), mean(code_metrics_sdr_1$files)),
                            "Regular" = c(median(code_metrics_reg_1$files), mean(code_metrics_reg_1$files)),
                            "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_1$files) / median(code_metrics_reg_1$files)), (mean(code_metrics_sdr_1$files) / mean(code_metrics_reg_1$files))))

proportion_code_metrics = rbind(proportion_code_metrics, proportion_files)

#proportion_code_metrics[, ifelse(Measure == "Median", round(`Same-day`, 0), `Same-day`)]

proportion_code_metrics$`Same-day` = round(proportion_code_metrics$`Same-day`, 3)
proportion_code_metrics$Regular = round(proportion_code_metrics$Regular, 3)
proportion_code_metrics$Ratio = round(proportion_code_metrics$`\\pbox{20cm}{Ratio\\\\S.D./reg.}`, 3)

proportion_code_metrics.table = xtable(proportion_code_metrics, caption = "Source code metrics on regular and self-driven same-day releases", label = "tab:code_metrics")
print(proportion_code_metrics.table, booktabs = TRUE, include.rownames = FALSE, caption.placement = "top", sanitize.rownames.function = identity, sanitize.text.function = identity)

sdr1_code_metrics = rbind(code_metrics_sdr_1, code_metrics_reg_1)
sdr1_code_metrics_by_client = sdr1_code_metrics[, .(median_num_commits = median(num_commits),
                                                    median_lines = median(lines),
                                                    median_files = median(files)), by = .(client_name, self_driven_sdr)]

sdr1_code_metrics_by_client.m = melt(sdr1_code_metrics_by_client, id.vars = c("client_name", "self_driven_sdr"))
labels_strip = c(median_num_commits = "Commits",
                 median_lines = "LOC",
                 median_files = "Files")

#boxplot
ggplot(sdr1_code_metrics_by_client.m, aes(x = self_driven_sdr, y = value, fill = self_driven_sdr)) +
  geom_boxplot() +
  scale_x_discrete(name="", 
                   breaks = c(TRUE, FALSE), 
                   labels=c("Same-day", "Regular")) +
  facet_wrap( ~ variable, scales = "free", labeller = as_labeller(labels_strip)) +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_log10(name = "Median",
                     labels = comma) +
  #coord_flip() +
  ggtitle("Self-driven same-day release") +
  theme_bw() +
  theme(strip.text = element_text(size = 13), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), axis.text.x = element_text(hjust = 1, angle = 45), axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text = element_text(size = 13), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))


### For provider-driven same-day releases ###

code_metrics_2 = fread("sampled-packages-provider-driven-sdr-output-code-churn.csv")

#get releases that have a provider with less than 
#24 hours difference from the client
code_metrics_sdr_2 = code_metrics_2[scenario == TRUE]

#remove rows that have scenario both TRUE and FALSE
code_metrics_reg_2 = dcast(code_metrics_2, ... ~ scenario, value.var = "scenario")[`FALSE` == FALSE & is.na(`TRUE`), .(client_name, 
                                                                                                                      client_version_from, 
                                                                                                                      client_version_to, 
                                                                                                                      release_order,
                                                                                                                      num_commits,
                                                                                                                      insertions,
                                                                                                                      deletions,
                                                                                                                      lines,
                                                                                                                      files,
                                                                                                                      scenario = `FALSE`)]

#check if the intersection between two tables is empty
setkey(code_metrics_sdr_2, client_name, client_version_from, client_version_to, release_order)
setkey(code_metrics_reg_2, client_name, client_version_from, client_version_to, release_order)
stopifnot(nrow(merge(code_metrics_sdr_2, code_metrics_reg_2)) == 0)

proportion_num_commits = data.table("Code metric" = c("\\multirow{2}{*}{Number of commits}", ""), "Measure" = c("Median", "Mean"),
                                    "Same-day" = c(median(code_metrics_sdr_2$num_commits), mean(code_metrics_sdr_2$num_commits)),
                                    "Regular" = c(median(code_metrics_reg_2$num_commits), mean(code_metrics_reg_2$num_commits)),
                                    "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_2$num_commits) / median(code_metrics_reg_2$num_commits)), (mean(code_metrics_sdr_2$num_commits) / mean(code_metrics_reg_2$num_commits))))

proportion_insertions = data.table("Code metric" = c("\\multirow{2}{*}{Insertions}", ""), "Measure" = c("Median", "Mean"),
                                   "Same-day" = c(median(code_metrics_sdr_2$insertions), mean(code_metrics_sdr_2$insertions)),
                                   "Regular" = c(median(code_metrics_reg_2$insertions), mean(code_metrics_reg_2$insertions)),
                                   "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_2$insertions) / median(code_metrics_reg_2$insertions)), (mean(code_metrics_sdr_2$insertions) / mean(code_metrics_reg_2$insertions))))

proportion_code_metrics = rbind(proportion_num_commits, proportion_insertions)

proportion_deletions = data.table("Code metric" = c("\\multirow{2}{*}{Deletions}", ""), "Measure" = c("Median", "Mean"),
                                  "Same-day" = c(median(code_metrics_sdr_2$deletions), mean(code_metrics_sdr_2$deletions)),
                                  "Regular" = c(median(code_metrics_reg_2$deletions), mean(code_metrics_reg_2$deletions)),
                                  "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_2$deletions) / median(code_metrics_reg_2$deletions)), (mean(code_metrics_sdr_2$deletions) / mean(code_metrics_reg_2$deletions))))

proportion_code_metrics = rbind(proportion_code_metrics, proportion_deletions)

proportion_loc = data.table("Code metric" = c("\\multirow{2}{*}{LOC's modified}", ""), "Measure" = c("Median", "Mean"),
                            "Same-day" = c(median(code_metrics_sdr_2$lines), mean(code_metrics_sdr_2$lines)),
                            "Regular" = c(median(code_metrics_reg_2$lines), mean(code_metrics_reg_2$lines)),
                            "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_2$lines) / median(code_metrics_reg_2$lines)), (mean(code_metrics_sdr_2$lines) / mean(code_metrics_reg_2$lines))))

proportion_code_metrics = rbind(proportion_code_metrics, proportion_loc)

proportion_files = data.table("Code metric" = c("\\multirow{2}{*}{Files modified}", ""), "Measure" = c("Median", "Mean"),
                              "Same-day" = c(median(code_metrics_sdr_2$files), mean(code_metrics_sdr_2$files)),
                              "Regular" = c(median(code_metrics_reg_2$files), mean(code_metrics_reg_2$files)),
                              "\\pbox{20cm}{Ratio\\\\S.D./reg.}" = c((median(code_metrics_sdr_2$files) / median(code_metrics_reg_2$files)), (mean(code_metrics_sdr_2$files) / mean(code_metrics_reg_2$files))))

proportion_code_metrics = rbind(proportion_code_metrics, proportion_files)

proportion_code_metrics$`Same-day` = round(proportion_code_metrics$`Same-day`, 3)
proportion_code_metrics$Regular = round(proportion_code_metrics$Regular, 3)
proportion_code_metrics$Ratio = round(proportion_code_metrics$`\\pbox{20cm}{Ratio\\\\S.D./reg.}`, 3)

proportion_code_metrics.table = xtable(proportion_code_metrics, caption = "Source code metrics on regular and provider-driven same-day releases", label = "tab:code_metrics")
print(proportion_code_metrics.table, booktabs = TRUE, include.rownames = FALSE, caption.placement = "top", sanitize.text.function = identity)

sdr2_code_metrics = rbind(code_metrics_sdr_2, code_metrics_reg_2)
sdr2_code_metrics_by_client = sdr2_code_metrics[, .(median_num_commits = median(num_commits),
                                                    median_lines = median(lines),
                                                    median_files = median(files)), by = .(client_name, scenario)]

sdr2_code_metrics_by_client.m = melt(sdr2_code_metrics_by_client, id.vars = c("client_name", "scenario"))
labels_strip = c(median_num_commits = "Commits",
                 median_lines = "LOC",
                 median_files = "Files")


#plot boxplots per client
ggplot(sdr2_code_metrics_by_client.m, aes(x = scenario, y = value, fill = scenario)) +
  geom_boxplot() +
  scale_x_discrete(name="", 
                   breaks = c(TRUE, FALSE), 
                   labels=c("Same-day", "Regular")) +
  facet_wrap( ~ variable, scales = "free", labeller = as_labeller(labels_strip)) +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_log10(name = "Median",
                labels = comma) +
  #coord_flip() +
  ggtitle("Provider-driven same-day release") +
  theme_bw() +
  theme(strip.text = element_text(size = 13), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), axis.text.x = element_text(hjust = 1, angle = 45), axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text = element_text(size = 13), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

#plot two figures in one file

pdf(file = "RQ1/images/code_metrics_dist.pdf")
p0 = ggplot(sdr1_code_metrics_by_client.m, aes(x = self_driven_sdr, y = value, fill = self_driven_sdr)) +
  geom_boxplot(notch = TRUE) +
  scale_x_discrete(name="", 
                   breaks = c(TRUE, FALSE), 
                   labels=c("", "")) +
  facet_wrap( ~ variable, scales = "free", labeller = as_labeller(labels_strip)) +
  #guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE,
                     name = "Release type",
                     breaks = c(FALSE, TRUE),
                     labels = c("Regular", "Same-day")) +
  scale_y_log10(name = "Median",
                labels = comma) +
  ggtitle("Self-driven same-day release") +
  theme_bw() +
  theme(strip.text = element_text(size = 13), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), axis.text.x = element_text(hjust = 1, angle = 45), axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text = element_text(size = 13), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

p1 = ggplot(sdr2_code_metrics_by_client.m, aes(x = scenario, y = value, fill = scenario)) +
  geom_boxplot(notch = TRUE) +
  scale_x_discrete(name="", 
                   breaks = c(TRUE, FALSE), 
                   labels=c("", "")) +
  facet_wrap( ~ variable, scales = "free", labeller = as_labeller(labels_strip)) +
  guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_log10(name = "Median",
                labels = comma) +
  ggtitle("Provider-driven same-day release") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 13), strip.text = element_text(size = 13), plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), axis.text.x = element_text(hjust = 1, angle = 45), axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text = element_text(size = 13), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))
grid_arrange_shared_legend(p0, p1, ncol = 2)
dev.off()

### self-driven same-day releases ###

#calculate the averge time between releases for self-driven and regular releases
setkey(releases, package_name, release_order, package_version_num_1, package_version_num_2)

setkey(code_metrics_sdr_1, client_name, release_order, client_version_from, client_version_to)
releases[code_metrics_sdr_1]
mean(difftime(ymd_hms(releases[code_metrics_sdr_1]$package_version_timestamp_2), ymd_hms(releases[code_metrics_sdr_1]$package_version_timestamp_1), units = "hours"))
median(difftime(ymd_hms(releases[code_metrics_sdr_1]$package_version_timestamp_2), ymd_hms(releases[code_metrics_sdr_1]$package_version_timestamp_1), units = "hours"))

setkey(code_metrics_reg_1, client_name, release_order, client_version_from, client_version_to)
mean(difftime(ymd_hms(releases[code_metrics_reg_1]$package_version_timestamp_2), ymd_hms(releases[code_metrics_reg_1]$package_version_timestamp_1), units = "hours"))
median(difftime(ymd_hms(releases[code_metrics_reg_1]$package_version_timestamp_2), ymd_hms(releases[code_metrics_reg_1]$package_version_timestamp_1), units = "hours"))

### provider-driven same-day releases ###

#calculate if there is a difference between the inter-releases time on provider-driven same-day releases
setkey(dependencies, client_name, release_order, client_version_num_1, client_version_num_2)

#get the mean time between releases of packages and code metrics
setkey(code_metrics_sdr_2, client_name, release_order, client_version_from, client_version_to)
code_metrics_sdr_2_timestamps = unique(dependencies[code_metrics_sdr_2][, .(client_name, release_order, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2)])[, .(mean_difftime_sdr = mean(as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(client_version_timestamp_1), units = "hours")))), by = client_name][sort(client_name)]

setkey(code_metrics_reg_2, client_name, release_order, client_version_from, client_version_to)
code_metrics_reg_2_timestamps = unique(dependencies[code_metrics_reg_2][, .(client_name, release_order, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2)])[, .(mean_difftime_reg = mean(as.numeric(difftime(ymd_hms(client_version_timestamp_2), ymd_hms(client_version_timestamp_1), units = "hours")))), by = client_name][sort(client_name)]

#test the difference between times
code_metrics_timestamps = merge(code_metrics_sdr_2_timestamps, code_metrics_reg_2_timestamps)
wilcox.test(code_metrics_timestamps$mean_difftime_sdr, code_metrics_timestamps$mean_difftime_reg, paired = TRUE)

#calculate the median value for the churn metrics within projects
code_metrics_sdr_2_medians = code_metrics_sdr_2[, .(median_num_commits_sdr = median(num_commits),
                                                    median_insertions_sdr = median(insertions),
                                                    median_deletions_sdr = median(deletions),
                                                    median_lines_sdr = median(lines),
                                                    median_files_sdr = median(files)), by = client_name]

code_metrics_reg_2_medians = code_metrics_reg_2[, .(median_num_commits_reg = median(num_commits),
                                                    median_insertions_reg = median(insertions),
                                                    median_deletions_reg = median(deletions),
                                                    median_lines_reg = median(lines),
                                                    median_files_reg = median(files)), by = client_name]

#code_metrics_2_medians = merge(code_metrics_sdr_2_medians, code_metrics_reg_2_medians) <- for paired comparison

#test statistically significance difference between code metrics
sig_commits_2 = wilcox.test(code_metrics_sdr_2_medians$median_num_commits_sdr, code_metrics_reg_2_medians$median_num_commits_reg, alternative = "greater")
eff_commits_2 = VD.A(code_metrics_sdr_2_medians$median_num_commits_sdr, code_metrics_reg_2_medians$median_num_commits_reg)

sig_insertions_2 = wilcox.test(code_metrics_sdr_2_medians$median_insertions_sdr, code_metrics_reg_2_medians$median_insertions_reg, alternative = "greater")
eff_insertions_2 = VD.A(code_metrics_sdr_2_medians$median_insertions_sdr, code_metrics_reg_2_medians$median_insertions_reg)

sig_deletions_2 = wilcox.test(code_metrics_sdr_2_medians$median_deletions_sdr, code_metrics_reg_2_medians$median_deletions_reg, alternative = "greater")
eff_deletions_2 = VD.A(code_metrics_sdr_2_medians$median_deletions_sdr, code_metrics_reg_2_medians$median_deletions_reg)

sig_lines_2 = wilcox.test(code_metrics_sdr_2_medians$median_lines_sdr, code_metrics_reg_2_medians$median_lines_reg, alternative = "greater")
eff_lines_2 = VD.A(code_metrics_sdr_2_medians$median_lines_sdr, code_metrics_reg_2_medians$median_lines_reg)

sig_files_2 = wilcox.test(code_metrics_sdr_2_medians$median_files_sdr, code_metrics_reg_2_medians$median_files_reg, alternative = "greater")
eff_files_2 = VD.A(code_metrics_sdr_2_medians$median_files_sdr, code_metrics_reg_2_medians$median_files_reg)

pvalues = c(sig_commits_2$p.value, sig_lines_2$p.value, sig_files_2$p.value)
p_adjust = p.adjust(pvalues, method = "BH")

#####################################################################
### RQ1.3 Number of packages using a provider vs. percentage (proportion)
### of same-day releases of that provider
#####################################################################

# proportion of same-day releases of a provider package
sd_proportion = same_day_releases_pattern_1_count(releases)

# calculate the number of clients of each provider
number_of_clients_of_provider = data.table()
for (i in 1:length(sd_proportion$package_name)){
  provider = sd_proportion$package_name[i]
  number_of_clients_of_provider = rbind(number_of_clients_of_provider, dependencies[dependency_name == provider, .(.GRP), by = .(client_name, dependency_name) ][, .(number_of_clients = .N), by = .(provider_name = dependency_name)])
}

# create structure to plot
package_publishing_sdr_usage = merge(sd_proportion, number_of_clients_of_provider, by.x = "package_name", by.y = "provider_name")

# plot in a scatterplot
pdf(file = "RQ1/images/num_pkg_using_provider_vs_percent_sd_releases.pdf")
ggplot(package_publishing_sdr_usage, aes(x = number_of_clients, y = proportion_same_day_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  #geom_smooth(colour = "black") +
  geom_hline(yintercept = median(package_publishing_sdr_usage$proportion_same_day_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(package_publishing_sdr_usage$number_of_clients), linetype = "dashed", colour = "red", size = 1) +
  scale_x_log10(name = "Number of clients") +
  scale_y_continuous(name = "Proportion of self-driven same-day releases",
                     labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.7), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))
dev.off()

#check the proportion of packages that stands above the mean number of clients and the mean proportion of same-day releases
packages_above_means = package_publishing_sdr_usage[number_of_clients >= mean(number_of_clients) & proportion_same_day_releases >= mean(proportion_same_day_releases)]
paste("Packages above the mean number of clients and the mean proportion of same-day relases are", percent(nrow(packages_above_means)/nrow(package_publishing_sdr_usage)),"of the popular packages publishing self-driven same-day releases")

#check the proportion of packages that stands above the median number of clients and the median proportion of same-day releases
packages_above_medians = package_publishing_sdr_usage[number_of_clients >= median(number_of_clients) & proportion_same_day_releases >= median(proportion_same_day_releases)]
paste("Packages above the median number of clients and the median proportion of same-day relases are", percent(nrow(packages_above_medians)/nrow(package_publishing_sdr_usage)),"of the popular packages publishing self-driven same-day releases")
paste("The median number of releases of packages above the medians is", median((packages_above_medians$num_regular_releases+packages_above_medians$num_same_day_releases)))

#check the correlation between the numnber of clients and the proportion of self-driven same-day releases
cor(package_publishing_sdr_usage$number_of_clients, package_publishing_sdr_usage$proportion_same_day_releases, method = "spearman")

#chech the R^2
lm.model = lm(number_of_clients ~ proportion_same_day_releases, package_publishing_sdr_usage)
summary(lm.model)

#check the mean time and standard deviation for regular releases
mean_sd_time_for_regular_releases = releases[same_day_release_1 == FALSE, .(mean_time_between_reg_releases = mean(as.numeric(difftime(ymd_hms(package_version_timestamp_2), ymd_hms(package_version_timestamp_1), units = "days"))), sd_time_between_reg_releases = sd(as.numeric(difftime(ymd_hms(package_version_timestamp_2), ymd_hms(package_version_timestamp_1), units = "days"))), median_time_between_reg_releases = median(as.numeric(difftime(ymd_hms(package_version_timestamp_2), ymd_hms(package_version_timestamp_1), units = "days"))))]

#check the mean time and standard deviation for regular releases per package
mean_sd_time_for_regular_releases = releases[same_day_release_1 == FALSE, .(mean_time_between_reg_releases = mean(as.numeric(difftime(ymd_hms(package_version_timestamp_2), ymd_hms(package_version_timestamp_1), units = "days"))), sd_time_between_reg_releases = sd(as.numeric(difftime(ymd_hms(package_version_timestamp_2), ymd_hms(package_version_timestamp_1), units = "days")))), by = package_name]

#check the number of clients for regular releases time
clients_for_regular_releases = unique(releases[same_day_release_1 == FALSE]$package_name)
num_clients_for_regular_releases = dependencies[dependency_name %in% clients_for_regular_releases, .GRP, by = .(client_name, dependency_name)][, .(number_of_clients = .N), by = .(package_name = dependency_name)]

num_clients_vs_stat_regular_releases = merge(num_clients_for_regular_releases, mean_sd_time_for_regular_releases)
summary(num_clients_vs_stat_regular_releases)

ggplot(num_clients_vs_stat_regular_releases, aes(x = number_of_clients, y = mean_time_between_reg_releases)) +
  geom_point(aes(colour = mean_time_between_reg_releases)) +
  #geom_hline(yintercept = mean(package_publishing_sdr_usage$proportion_same_day_releases), linetype = "dashed", colour = "red", size = 1) +
  #geom_vline(xintercept = mean(package_publishing_sdr_usage$number_of_clients), linetype = "dashed", colour = "red", size = 1) +
  geom_smooth(colour = "black", method = "lm") +
  scale_x_log10(name = "Number of clients") +
  scale_y_log10(name = "Standard deviation of time\nbetween regular releases ",
                labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.7), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

cor(num_clients_vs_stat_regular_releases$number_of_clients, num_clients_vs_stat_regular_releases$mean_time_between_reg_releases)

summary(lm(number_of_clients ~ mean_time_between_reg_releases, num_clients_vs_stat_regular_releases))
summary(lm(number_of_clients ~ mean_time_between_reg_releases, num_clients_vs_stat_regular_releases))

### Do packages triggering more provider-driven same-day releases are less popular? ####

#calculate the number of same-day and total releases triggered by each provider
num_triggered_releases = dependencies[same_day_release_2 == TRUE, .(num_triggered_releases = .N), .(client_name, dependency_name)][, .(num_triggered_releases = sum(num_triggered_releases)), by = dependency_name]
num_releases = dependencies[, .(num_releases = .N), .(client_name, dependency_name)][, .(num_releases = sum(num_releases)), by = dependency_name]

#calculate the number of clients of a provider
num_clients = dependencies[, .GRP, .(client_name, dependency_name)][, .(num_clients = .N), by = dependency_name] 

#merge with the number of releases
num_releases = merge(num_releases, num_clients)

#calculate the proportion of triggered same-day releases
proportion_triggered_releases = merge(num_triggered_releases, num_releases, by = "dependency_name",  all = FALSE)[, .(dependency_name, num_triggered_releases, num_releases, proportion_tiggered_releases = (num_triggered_releases/num_releases), num_clients)]
summary(proportion_triggered_releases)

cor(proportion_triggered_releases$proportion_tiggered_releases, proportion_triggered_releases$num_clients, method = "spearman")

#calculate the number of clients that the provider do and do not trigger same-day releases
num_clients_triggered_releases = dependencies[same_day_release_2 == TRUE, .GRP, .(client_name, dependency_name)][, .(num_clients_triggered_releases = .N), by = dependency_name]
num_clients = dependencies[, .GRP, .(client_name, dependency_name)][, .(num_clients = .N), by = dependency_name]

#calculate the proportion clients that the provider trigger same-day releases
proportion_clients_triggered_releases = merge(num_clients_triggered_releases, num_clients, all = FALSE)[, .(dependency_name, num_clients_triggered_releases, num_clients, proportion_clients_triggered_releases = (num_clients_triggered_releases/num_clients))]
summary(proportion_clients_triggered_releases)

cor(proportion_clients_triggered_releases$proportion_clients_triggered_releases, proportion_clients_triggered_releases$num_clients, method = "spearman")

ggplot(proportion_triggered_releases, aes(x = num_clients, y = proportion_tiggered_releases)) +
  geom_point(aes(colour = proportion_tiggered_releases)) +
  #geom_smooth(colour = "black") +
  geom_hline(yintercept = median(proportion_triggered_releases$proportion_tiggered_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(proportion_triggered_releases$num_clients), linetype = "dashed", colour = "red", size = 1) +
  scale_x_log10(name = "Number of clients") +
  scale_y_log10(name = "Proportion of triggered\nself-driven same-day releases",
                     labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.8), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

ggplot(proportion_clients_triggered_releases, aes(x = num_clients, y = proportion_clients_triggered_releases)) +
  geom_point(aes(colour = proportion_clients_triggered_releases)) +
  #geom_smooth(colour = "black") +
  geom_hline(yintercept = median(proportion_clients_triggered_releases$proportion_clients_triggered_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(proportion_clients_triggered_releases$num_clients), linetype = "dashed", colour = "red", size = 1) +
  scale_x_log10(name = "Number of clients") +
  scale_y_log10(name = "Proportion of clients with triggered\nself-driven same-day releases",
                labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.8), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

#chech the R^2
lm.model = lm(num_clients ~ proportion_clients_triggered_releases, proportion_clients_triggered_releases)
summary(lm.model)

#plot everything in one graph
pdf(file = "RQ1/images/num_pkg_using_provider_vs_percent_sd_releases.pdf")
p0 = ggplot(package_publishing_sdr_usage, aes(x = number_of_clients, y = proportion_same_day_releases)) +
  geom_point(aes(colour = proportion_same_day_releases)) +
  #geom_smooth(colour = "black") +
  geom_hline(yintercept = median(package_publishing_sdr_usage$proportion_same_day_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(package_publishing_sdr_usage$number_of_clients), linetype = "dashed", colour = "red", size = 1) +
  scale_x_log10(name = "Number of clients") +
  scale_y_continuous(name = "Proportion\nof releases",
                     labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  labs(caption = "(a)") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.7), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

p1 = ggplot(proportion_triggered_releases, aes(x = num_clients, y = proportion_tiggered_releases)) +
  geom_point(aes(colour = proportion_tiggered_releases)) +
  #geom_smooth(colour = "black") +
  geom_hline(yintercept = median(proportion_triggered_releases$proportion_tiggered_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(proportion_triggered_releases$num_clients), linetype = "dashed", colour = "red", size = 1) +
  scale_x_log10(name = "Number of clients") +
  scale_y_log10(name = "Proportion\nof releases",
                labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  labs(caption = "(b)") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.8), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))

p2 = ggplot(proportion_clients_triggered_releases, aes(x = num_clients, y = proportion_clients_triggered_releases)) +
  geom_point(aes(colour = proportion_clients_triggered_releases)) +
  #geom_smooth(colour = "black") +
  geom_hline(yintercept = median(proportion_clients_triggered_releases$proportion_clients_triggered_releases), linetype = "dashed", colour = "red", size = 1) +
  geom_vline(xintercept = median(proportion_clients_triggered_releases$num_clients), linetype = "dashed", colour = "red", size = 1) +
  scale_x_log10(name = "Number of clients") +
  scale_y_log10(name = "Proportion\nof clients",
                labels = comma) +
  guides(fill=FALSE) +
  scale_color_viridis(guide = 'none') +
  labs(caption = "(c)") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(size = 16, hjust = 0.5, face = "bold", family = "serif"), strip.text = element_text(size = 13), axis.text = element_text(size = 13, hjust = 0.8), axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5))
grid.arrange(p0, p1, p2, ncol = 1)
dev.off()