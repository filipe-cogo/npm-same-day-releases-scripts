############################################
### Calculate same-day releases pattern 2
############################################

# How many client packages releases an update within 24hrs
# of an update that was released by one of it's supllier packages?

same_day_releases_pattern_2.par = function(releases, dependencies, threads = 4){
  #The general idea is:
  # - Find a release R on 'release' table
  # - Check on table 'dependencies' who is using the library and version corresponding to release R
  # - Check if the client release have less than 24 diff from release R
  # - Record the line on 'dependencies' in which it is occuring
  
  #parallelized version of the general idea
  registerDoParallel(cores = threads)
  getDoParWorkers()
  setkey(dependencies, dependency_name, dependency_version_max_satisf_2, client_version_timestamp_2_year, client_version_timestamp_2_month, client_version_timestamp_2_day)
  clients_updating_24h_after_lib = data.frame()
  same_day_releases_pattern_2 = foreach(i = 0:(getDoParWorkers()-1)) %dopar% {
    for (k in floor((i*nrow(releases) / getDoParWorkers())+1):((i+1)*nrow(releases) / getDoParWorkers())){
      r = releases[k]
      
      clients_of_lib = unique(dependencies[(dependency_name == r$package_name) & (dependency_version_max_satisf_1 == r$package_version_num_1) & (dependency_version_max_satisf_2 == r$package_version_num_2)])
      
      if (nrow(clients_of_lib) > 0){
        clients_of_lib = unique(clients_of_lib[difftime(ymd_hms(client_version_timestamp_2), ymd_hms(dependency_timestamp_max_satisf_2), unit = "secs") <= 86400])  
      } else {
        next
      }
      
      if (nrow(clients_of_lib) > 0){
        clients_of_lib = cbind(clients_of_lib, r)
      }
      
      clients_updating_24h_after_lib = rbind(clients_updating_24h_after_lib, clients_of_lib, fill = TRUE)
    }
    
    #colnames(clients_updating_24h_after_lib) = c(names(dependencies), names(releases))
    return(clients_updating_24h_after_lib)
  }
  
  same_day_releases_pattern_2 = rbindlist(same_day_releases_pattern_2)
  
  return(same_day_releases_pattern_2)
}

####################################
### Count number of times and the proportion of client
### releases using range and tight statements in a month
####################################

month_snaps = function(same_day_pt1, same_day_pt2){
  #get packages grouped by timestamp
  packages.pt1 = same_day_pt1[, .(num_releases = .N, type = "pattern1"), by = .(pkg_name = package_name, timestamp = floor_date(ymd_hms(package_version_timestamp_2), "month"))]
  packages.pt2 = same_day_pt2[, .(num_releases = .N, type = "pattern2"), by = .(pkg_name = client_name, timestamp = floor_date(ymd_hms(client_version_timestamp_2), "month"))]
  
  #pkg_per_month.pt1 = unique(packages.pt1[, .(pkg_per_month = .N), by = .(timestamp)][order(timestamp)])
  #packages.pt1 = merge(packages.pt1, pkg_per_month.pt1)
  
  #pkg_per_month.pt2 = unique(packages.pt2[, .(pkg_per_month = .N), by = .(timestamp)][order(timestamp)])
  #packages.pt2 = merge(packages.pt2, pkg_per_month.pt2)
  
  sdr = rbind(packages.pt1, packages.pt2)
  
  #count how many same-day releases is occurring by month
  sdr.type.count = sdr[, .(num_pgks = .N, num_rel = sum(num_releases)), by = .(type, timestamp)]
  
  #put the data in a format to easily calculate proportion
  sdr.c = as.data.table(data.table::dcast(data = sdr.type.count, formula = ... ~ type, fun.aggregate = sum, value.var = c("num_pgks", "num_rel")))
  
  sdr.c$num_pgks_pattern1_proportion = (sdr.c$num_pgks_pattern1 / (sdr.c$num_pgks_pattern1 + sdr.c$num_pgks_pattern2))
  sdr.c$num_pgks_pattern2_proportion = (sdr.c$num_pgks_pattern2 / (sdr.c$num_pgks_pattern1 + sdr.c$num_pgks_pattern2))
  
  sdr.c$releases_per_pgks_pattern1 = ifelse(sdr.c$num_pgks_pattern1 == 0, 0, sdr.c$num_rel_pattern1 / sdr.c$num_pgks_pattern1)
  sdr.c$releases_per_pgks_pattern2 = ifelse(sdr.c$num_pgks_pattern2 == 0, 0, sdr.c$num_rel_pattern2 / sdr.c$num_pgks_pattern2)
  
  sdr.c.m = sdr.c[, .(num_packages_releases = (sum(num_rel_pattern1) + sum(num_rel_pattern2))), by = timestamp]
  sdr.c = merge(sdr.c, sdr.c.m)
  
  sdr.c$same_day_1_per_total_releases = (sdr.c$num_rel_pattern1 / sdr.c$num_packages_releases)
  sdr.c$same_day_2_per_total_releases = (sdr.c$num_rel_pattern2 / sdr.c$num_packages_releases)
  
  return(sdr.c)
}


####################################
### Count the number of dependencies per month
### If a dependency occurs more than one time 
### per month, it is counted once
####################################

month_snaps_deps = function(depoendencies){
  snaps_interm = depoendencies[, .(.GRP), by = .(client_name, dependency_name, timestamp = floor_date(ymd_hms(client_version_timestamp_1), "month"))]
  snaps.dependencies = snaps_interm[, .(count_deps = .N), by = .(timestamp)][order(timestamp)]
  
  snaps_interm = depoendencies[, .(.GRP), by = .(client_name, timestamp = floor_date(ymd_hms(client_version_timestamp_1), "month"))]
  snaps.packages = snaps_interm[, .(count_packs = .N), by = .(timestamp)][order(timestamp)]
  
  snaps.total = merge(snaps.dependencies, snaps.packages)
  snaps.total$deps_per_package = (snaps.total$count_deps / snaps.total$count_packs)
  
  return(snaps.total)
}

####################################
### Count the number of releases of a
### client that is using same-day 
### releases on pattern 1
####################################

client_releases_using_same_day_ptrn1 = function(dependencies, same_day_releases_pattern_1){
  #set key for joining tables with dependencies and same-day releases
  setkey(dependencies, dependency_name, dependency_version_max_satisf_2)
  setkey(same_day_releases_pattern_1, package_name, package_version_num_2)
  # join the tables - calculate all dependencies that involves same-day releases pattern 1
  #same_day_release_ptrn1_usage = dependencies[same_day_releases_pattern_1, nomatch = 0]
  same_day_release_ptrn1_usage = dependencies[same_day_releases_pattern_1, nomatch = 0]
  
  # count how many client relases uses same-day releases (pattern 1)
  clients_releases_using_same_day_release_ptrn1 = same_day_release_ptrn1_usage[, .(client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2, client_versioning_change = paste(dependency_versioning_type_1, dependency_versioning_type_2, sep = " to " ), package_pttnr1_version_change = ifelse(upgrade, "upgrade", ifelse(rollback, "rollback", "no_change")), package_pttnr1_timestamp_max_satisf_1 = dependency_timestamp_max_satisf_1, package_pttnr1_timestamp_max_satisf_2 = dependency_timestamp_max_satisf_2), by = .(package_pttnr1 = dependency_name, package_pttnr1_version_num_1 = dependency_version_max_satisf_1, package_pttnr1_version_num_2 = dependency_version_max_satisf_2)]
  
  return(clients_releases_using_same_day_release_ptrn1)
}

####################################
### Calculate if a change from some
### initial version resulted on rollback,
### upgrade or no change 
####################################

client_releases_changing_from_same_day_release_ptrn1 = function(dependencies, same_day_releases_pattern_1){
  #set key for joining tables with dependencies and same-day releases
  setkey(dependencies, dependency_name, dependency_version_max_satisf_1)
  setkey(same_day_releases_pattern_1, package_name, package_version_num_2)
  # join the tables - calculate all dependencies that involves same-day releases pattern 1
  same_day_release_ptrn1_usage = dependencies[same_day_releases_pattern_1, nomatch = 0]
  
  clients_releases_using_same_day_release_ptrn1 = same_day_release_ptrn1_usage[, .(.N, client_name, client_version_num_1, client_version_num_2, client_timestamp_1 = client_version_timestamp_1, client_timestamp_2 = client_version_timestamp_2, client_versioning_change = paste(dependency_versioning_type_1, dependency_versioning_type_2, sep = " to " ), package_pttnr1_version_change = ifelse(upgrade, "upgrade", ifelse(rollback, "rollback", "no_change"))), by = .(package_pttnr1 = dependency_name, package_pttnr1_version_num_1 = dependency_version_max_satisf_1, package_pttnr1_version_num_2 = dependency_version_max_satisf_2)]
  
  return(clients_releases_using_same_day_release_ptrn1)
}

####################################
### Calculate the regular and same-day 
### releases on pattern 1
####################################

regular_same_day_releases_pt1 = function(sdrpt1, rels){
  setkey(sdrpt1, package_name, package_version_num_1, package_version_num_2)
  setkey(rels, package_name, package_version_num_1, package_version_num_2)
  
  # find elements that are in 'rels' but not on 'sdrpt1'
  sdr1.diff = rels[!sdrpt1]
  # find elements that are in 'rels' but not on 'sdr1.diff'
  sdr1.eq = rels[!sdr1.diff]
  
  # check if it's all right
  nrow(rels) == nrow(sdrpt1) + nrow(sdr1.diff)
  nrow(rels) == nrow(sdr1.eq) + nrow(sdr1.diff)
  
  # create columns with values for same-day release flag
  sdr1.diff$same_day_1 = FALSE
  sdr1.eq$same_day_1 = TRUE
  
  # bind rows from both tables
  sdr1 = rbind(sdr1.diff, sdr1.eq)
  
  return(sdr1)
}


####################################
### Calculate the regular and same-day 
### releases on pattern 2
####################################

regular_same_day_releases_pt2 = function(sdrpt2, deps){
  #fetch clients performing same-day release on pattern 2
  clients_performing_sdr2 = sdrpt2[, .(num_releases = .N), by = .(name = client_name)]$name
  
  # fetch only the clients on 'deps' tables and aggregate the provider packages
  deps.cli = deps[, .(.GRP), by = .(client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2)]
  
  # keep on 'deps.cli' only the packages performing same-day release on pattern 2
  deps.cli = deps.cli[client_name %in% clients_performing_sdr2]
  
  # set key for join tables
  setkey(sdrpt2, client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2)
  setkey(deps.cli, client_name, client_version_num_1, client_version_num_2, client_version_timestamp_1, client_version_timestamp_2)
  
  # find elements that are in 'deps' but not on 'same_day_releases_pattern_2'
  sdr2.diff = deps.cli[!sdrpt2]
  # find elements that are in 'deps' but not on 'sdr2.diff'
  sdr2.eq = deps.cli[!sdr2.diff]
  
  # check if it's all right
  nrow(deps.cli) - (nrow(sdrpt2) + nrow(sdr2.diff))
  nrow(deps.cli) == nrow(sdrpt2) + nrow(sdr2.diff)
  nrow(deps.cli) == nrow(sdr2.eq) + nrow(sdr2.diff)
  
  # create columns with values for same-day release flag
  sdr2.diff$same_day_2 = FALSE
  sdr2.eq$same_day_2 = TRUE
  
  # bind rows from both tables
  sdr2 = rbind(sdr2.diff, sdr2.eq)
  
  return(sdr2)
}


month_snaps_regular_same_day_releases = function(rels1, rels2){
  
}

