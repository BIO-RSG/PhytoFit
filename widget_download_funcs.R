################################################################################
# MAP ####

filename_map <- function(d) {
  output_str(satellite=d$satellite,
             region=d$region,
             algorithm=d$algorithm,
             year=d$year,
             interval=d$interval,
             log_chla=d$log_chla,
             day_label=gsub(" ", "", strsplit(d$day_label, "[()]+")[[1]][2]),
             polygon=gsub(pattern=" ", replacement="_", x=d$poly_name),
             fitmethod=d$fitmethod,
             custom_end="map.html",
             concentration_type=d$concentration_type,
             cell_size_model1=d$cell_size_model1,
             cell_size_model2=d$cell_size_model2)
}

content_map <- function(d, map) {
  pc <- d$tr
  lt <- d$leg_title
  dl <- d$day_label
  zl <- c(d$zlim1, d$zlim2)
  tr_coloradj <- calc(pc, function(x) ifelse(x <= zl[1], zl[1]+(1e-10), ifelse(x >= zl[2], zl[2]-(1e-10), x)))
  cm <- colorNumeric(palette=map_cols, domain=zl, na.color="#00000000")
  widg <- map %>%
    clearControls() %>% clearGroup("georaster") %>%
    addRasterImage(x = tr_coloradj, colors = cm) %>%
    addLegend(position = 'topright', pal = cm, values = zl,
              title = lt, bins = 10, opacity = 1) %>%
    # Label map with current year and day of year
    addControl(tags$div(tag.map.title, HTML(dl)),
               position = "topleft",
               className = "map-title")
  return(widg)
}

################################################################################
# DENSITY PLOT ####

filename_dens <- function(d) {
  output_str(satellite=d$satellite,
             region=d$region,
             algorithm=d$algorithm,
             year=d$year,
             interval=d$interval,
             log_chla=d$log_chla,
             day_label=gsub(" ", "", strsplit(d$day_label, "[()]+")[[1]][2]),
             polygon=gsub(pattern=" ", replacement="_", x=d$poly_name),
             fitmethod=d$fitmethod,
             custom_end="density_plot.png",
             concentration_type=d$concentration_type,
             cell_size_model1=d$cell_size_model1,
             cell_size_model2=d$cell_size_model2)
}

################################################################################
# BLOOM FIT PLOT ####

filename_bfp <- function(d) {
  output_str(satellite=d$satellite,
             region=d$region,
             algorithm=d$algorithm,
             year=d$year,
             interval=d$interval,
             log_chla=d$log_chla,
             polygon=gsub(pattern=" ", replacement="_", x=d$poly_name),
             fitmethod=d$fitmethod,
             custom_end="bloom_fit.png",
             concentration_type=d$concentration_type,
             cell_size_model1=d$cell_size_model1,
             cell_size_model2=d$cell_size_model2)
}

################################################################################
# ANNUAL STATS ####

filename_stats <- function(d) {
  output_str(satellite=d$satellite,
             region=d$region,
             algorithm=d$algorithm,
             year=d$year,
             interval=d$interval,
             log_chla=d$log_chla,
             polygon=gsub(pattern=" ", replacement="_", x=d$poly_name),
             fitmethod=d$fitmethod,
             custom_end="annual_stats.csv",
             concentration_type=d$concentration_type,
             cell_size_model1=d$cell_size_model1,
             cell_size_model2=d$cell_size_model2)
}

content_stats <- function(d) {
  data.frame(doy=d$doy_vec,
             mean_chl=d$chl_mean,
             median_chl=d$chl_median,
             stdev_chl=d$chl_sd,
             min_chl=d$chl_min,
             max_chl=d$chl_max,
             nobs=d$nobs,
             percent_coverage=d$percent_coverage,
             loess_smooth=d$loess_smooth,
             stringsAsFactors=FALSE)
}

################################################################################
# BLOOM FIT PARAMETERS ####

filename_bfparam <- function(d) {
  output_str(satellite=d$satellite,
             region=d$region,
             algorithm=d$algorithm,
             year=d$year,
             interval=d$interval,
             log_chla=d$log_chla,
             polygon=gsub(pattern=" ", replacement="_", x=d$poly_name),
             fitmethod=d$fitmethod,
             custom_end="bloom_parameters.csv",
             concentration_type=d$concentration_type,
             cell_size_model1=d$cell_size_model1,
             cell_size_model2=d$cell_size_model2)
}

################################################################################
# SETTINGS ####

filename_settings <- function(d) {
  output_str(satellite=d$satellite,
             region=d$region,
             algorithm=d$algorithm,
             year=d$year,
             interval=d$interval,
             log_chla=d$log_chla,
             day_label=gsub(" ", "", strsplit(d$day_label, "[()]+")[[1]][2]),
             polygon=d$box,
             fitmethod=d$fitmethod,
             custom_end="settings.csv",
             concentration_type=d$concentration_type,
             cell_size_model1=d$cell_size_model1,
             cell_size_model2=d$cell_size_model2)
}

content_settings <- function(d, inp) {
  if (d$box=="custom") {
    plons <- d$polylon
    plats <- d$polylat
  } else {
    plons <- plats <- NA
  }
  info <- format_settings_to_save(all_inputs=inp,
                                  custom_name=d$custom_name,
                                  polylon=plons,
                                  polylat=plats,
                                  regions=regions,
                                  sat_algs=sat_algs)
}


