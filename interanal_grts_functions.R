dsgn_check <- function(sframe, sf_type, legacy_sites, legacy_option, stratum, seltype, n_base, caty_n,
                       n_over, n_near, stratum_var, caty_var, aux_var,
                       legacy_stratum_var, legacy_caty_var, legacy_aux_var,
                       legacy_var, mindis,
                       DesignID, SiteBegin, maxtry, projcrs_check) {
  
  # Create a data frame for stop messages
  stop_ind <- FALSE
  stop_df <- NULL
  
  # check that coordinates are NA or geographic # | st_is_longlat(sframe))
  if (projcrs_check & is.na(st_crs(sframe))) {
    stop_ind <- TRUE
    stop_mess <- "The coordinate reference system (crs) for sframe is NA. The coordinate reference system for sframe should instead use projected coordinates. For more information on geographic and projected coordinates, see spsurvey's \"Start Here\" vignette by running vignette(\"start-here\", \"spsurvey\"). To override the check for projected coordinates, set projcrs_check = FALSE."
    stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
  }
  
  if (projcrs_check & st_is_longlat(sframe)) {
    stop_ind <- TRUE
    stop_mess <- "The coordinate reference system (crs) for sframe is geographic. The coordinate reference system for sframe should instead use projected coordinates. For more information on geographic and projected coordinates, see spsurvey's \"Start Here\" vignette by running vignette(\"start-here\", \"spsurvey\"). To override the check for projected coordinates, set projcrs_check = FALSE."
    stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
  }
  
  # check that legacy and sframe coordinates match
  if (!is.null(legacy_sites)) {
    if (sum(is.na(st_crs(sframe)), is.na(st_crs(legacy_sites))) == 1) {
      stop_ind <- TRUE
      stop_mess <- "sframe and legacy_sites must have the same crs. If crs should be ignored completely, run st_crs(sframe) <- NA and st_crs(legacy_sites) <- NA"
      stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
    } else if (st_crs(sframe) != st_crs(legacy_sites)) {
      stop_ind <- TRUE
      stop_mess <- "sframe and legacy_sites must have the same crs. If crs should be ignored completely, run st_crs(sframe) <- NA and st_crs(legacy_sites) <- NA"
      stop_df <- rbind(stop_df, data.frame(func = I("sframe"), I(stop_mess)))
    }
  }
  
  # check that sframe has required variables for stratum, caty, aux and legacy
  # If stratum_var is provided, does the attribute exist in sframe
  if (!is.null(stratum_var)) {
    if (match(stratum_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for stratum variable does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
    }
  }
  
  # If caty_var is provided, does the attribute exist in sframe
  if (!is.null(caty_var)) {
    if (match(caty_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for unequal probability category variable does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("caty_var"), I(stop_mess)))
    }
  }
  
  # If aux_var is provided, does the attribute exist in sframe
  if (!is.null(aux_var)) {
    if (match(aux_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for the auxillary variable for proportional sampling does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("aux_var"), I(stop_mess)))
    }
    # ensure class for aux variable is numeric
    if (!is.numeric(sframe[[aux_var]])) {
      stop_ind <- TRUE
      stop_mess <- "The auxillary variable in sample frame for proportional sampling must be numeric."
      stop_df <- rbind(stop_df, data.frame(func = I("aux_var"), I(stop_mess)))
    } else {
      # check that values are > 0.
      if (any(sframe[[aux_var]] <= 0)) {
        stop_ind <- TRUE
        stop_mess <- "The auxillary variable for proportional sampling must have all values greater than zero"
        stop_df <- rbind(stop_df, data.frame(func = I("aux_var"), I(stop_mess)))
      }
    }
  }
  
  # If legacy_var is provided, does the attribute exist in sframe
  if (sf_type == "sf_point" & !is.null(legacy_var)) {
    if (match(legacy_var, names(sframe), nomatch = 0) == 0) {
      stop_ind <- TRUE
      stop_mess <- "The value provided for the variable identifying legacy sites does not exist as a variable in sframe."
      stop_df <- rbind(stop_df, data.frame(func = I("legacy_var"), I(stop_mess)))
    }
  }
  
  ### Check legacy_sites sf object if present
  if (sf_type %in% c("sf_point", "sf_linear", "sf_area") & !is.null(legacy_sites)) {
    # check that legacy_sites has required variables for stratum, caty, aux and legacy
    # If stratum_var is provided, does the attribute exist
    if (!is.null(stratum_var) & is.null(legacy_stratum_var)) {
      if (match(stratum_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for stratum variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
    # If caty_var is provided, does the attribute exist
    if (!is.null(caty_var) & is.null(legacy_caty_var)) {
      if (match(caty_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for caty variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
    # If aux_var is provided, does the attribute exist
    if (!is.null(aux_var) & is.null(legacy_aux_var)) {
      if (match(aux_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for aux variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
    # If legacy_var is provided, does the attribute exist
    if (!is.null(legacy_var)) {
      if (match(legacy_var, names(legacy_sites), nomatch = 0) == 0) {
        stop_ind <- TRUE
        stop_mess <- "The value provided for legacy variable does not exist as a variable in legacy_sites."
        stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
      }
    }
  }
  
  
  ##### Check design components to ensure they provide what is required.
  
  # check if stratum is provided and values are in sframe
  if (!is.null(stratum)) {
    if (is.null(stratum_var)) {
      stop_ind <- TRUE
      stop_mess <- "Design is stratified and no 'stratum_var' is provided."
      stop_df <- rbind(stop_df, data.frame(func = I("stratum_var"), I(stop_mess)))
    } else {
      if (any(stratum %in% unique(sframe[[stratum_var]]) == FALSE)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Not all stratum values are in sample frame.")
        stop_df <- rbind(stop_df, data.frame(func = I("stratum"), I(stop_mess)))
      }
    }
  }
  
  # check seltype
  if (any(seltype %in% c("equal", "unequal", "proportional") == FALSE)) {
    stop_ind <- TRUE
    stop_mess <- paste0("seltype must be 'equal', 'unequal' or 'proportional'.")
    stop_df <- rbind(stop_df, data.frame(func = I("seltype"), I(stop_mess)))
  }
  
  # check seltype when caty_var and aux_var provided
  if (all(seltype %in% "unequal") | all(seltype %in% "proportional")) {
    if (!is.null(caty_var) & !is.null(aux_var)) {
      stop_ind <- TRUE
      stop_mess <- paste0("aux_var and caty_var cannot both be specified when all elements of seltype are the same.")
      stop_df <- rbind(stop_df, data.frame(func = I("seltype mismatch"), I(stop_mess)))
    }
  }
  
  # check caty_var and caty_n are provided together
  if ((is.null(caty_var) & !is.null(caty_n)) | (!is.null(caty_var) & is.null(caty_n))) {
    stop_ind <- TRUE
    stop_mess <- paste0("caty_n and caty_var must be provided together.")
    stop_df <- rbind(stop_df, data.frame(func = I("caty_var and caty_n"), I(stop_mess)))
  }
  
  # check n_base length and stratum_var are provided together
  if (length(n_base) > 1 & is.null(stratum_var)) {
    stop_ind <- TRUE
    stop_mess <- paste0("if the length of n_base is larger than 1 then stratification is assumed and stratum_var must be provided.")
    stop_df <- rbind(stop_df, data.frame(func = I("n_base and stratum_var"), I(stop_mess)))
  }
  
  # check names of caty_n when it is a list
  if (is.list(caty_n) & is.null(names(caty_n))) {
    stop_ind <- TRUE
    stop_mess <- paste0("caty_n must be a named list (and these names must match the strata)")
    stop_df <- rbind(stop_df, data.frame(func = I("caty_n names"), I(stop_mess)))
  }
  
  
  # check n_base
  if (any(n_base <= 0)) {
    stop_ind <- TRUE
    stop_mess <- paste0("Sample size must be integers greater than 0.")
    stop_df <- rbind(stop_df, data.frame(func = I("n_base"), I(stop_mess)))
  }
  
  # check total sample size
  if (sf_type == "sf_point") {
    if (length(stratum) > 1) {
      if (any(sapply(stratum, function(x) n_base[x] > NROW(sframe[sframe[[stratum_var]] == x, , drop = FALSE])))) {
        stop_ind <- TRUE
        stop_mess <- paste0("Each stratum must have a sample size no larger than the number of rows in 'sframe' representing that stratum")
        stop_df <- rbind(stop_df, data.frame(func = I("n_base"), I(stop_mess)))
      }
    } else {
      if (n_base > NROW(sframe)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Sample size must be no larger than the number of rows in 'sframe'")
        stop_df <- rbind(stop_df, data.frame(func = I("n_base"), I(stop_mess)))
      }
    }
  }
  
  # check caty_n
  if (!is.null(caty_n)) {
    if (!is.list(caty_n)) {
      if (any(names(caty_n) %in% unique(sframe[[caty_var]]) == FALSE)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Not all caty_n values are in sample frame.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
      tst <- function(x, caty_n) {
        x != sum(caty_n)
      }
      if (any(sapply(n_base, tst, caty_n))) {
        stop_ind <- TRUE
        stop_mess <- paste0("Sum of caty_n values do not equal n_base.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
    }
    if (is.list(caty_n)) {
      if (any(names(caty_n) %in% stratum == FALSE)) {
        stop_ind <- TRUE
        stop_mess <- paste0("Names for caty_n list are not values in 'stratum' variable.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
        stop_mess <- paste0("For each stratum make sure caty_n values in 'caty_var' variable.")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
      if (any(sapply(stratum, function(x) sum(caty_n[[x]]) != n_base[[x]]))) {
        stop_ind <- TRUE
        stop_mess <- paste0("The sum of the 'caty_n' values in each strata must match the value in n_base that corresponds to the respective strata")
        stop_df <- rbind(stop_df, data.frame(func = I("caty_n"), I(stop_mess)))
      }
    }
  }
  
  # check n_over
  if (!is.null(n_over)) {
    if (is.null(stratum) | length(stratum) == 1) {
      if (any(seltype %in% c("equal", "proportional"))) {
        if (n_over < 0) {
          stop_ind <- TRUE
          stop_mess <- paste0("n_over value must be zero or positive.")
          stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
        }
      }
      if (any(seltype == "unequal")) {
        if (!is.null(caty_n)) {
          if (!is.list(n_over)) {
            if (any(n_over < 0)) {
              stop_ind <- TRUE
              stop_mess <- paste0("n_over values must be zero or positive.")
              stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
            }
          }
        }
      }
    }
    if (length(stratum) > 1) {
      if (any(seltype %in% c("equal", "proportional", "unequal"))) {
        if (!is.list(n_over)) {
          if (any(n_over < 0)) {
            stop_ind <- TRUE
            stop_mess <- paste0("n_over values must be zero or positive.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
          }
        }
        if (is.list(n_over)) {
          if (any(names(n_over) %in% stratum == FALSE)) {
            stop_ind <- TRUE
            stop_mess <- paste0("Names for n_over list are not values in 'stratum' variable.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
            stop_mess <- paste0("For each stratum make sure n_over values are non-negative.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_over"), I(stop_mess)))
          }
        }
      }
    }
  }
  
  # check total sample size for n_over
  if (sf_type == "sf_point") {
    if (!is.null(n_over)) {
      if (length(stratum) > 1) {
        if (is.list(n_over)) {
          if (any(sapply(stratum, function(x) (n_base[[x]] + ifelse(is.null(n_over[[x]]), 0, sum(n_over[[x]]))) > NROW(sframe[sframe[[stratum_var]] == x, , drop = FALSE])))) {
            stop_ind <- TRUE
            stop_mess <- paste0("For each stratum, the sum of the base sites and 'Over' replacement sites must be no larger than the number of rows in 'sframe' representing that stratum.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_base + n_over"), I(stop_mess)))
          }
        } else {
          if (any(sapply(stratum, function(x) (n_base[[x]] + sum(n_over[[x]])) > NROW(sframe[sframe[[stratum_var]] == x, , drop = FALSE])))) {
            stop_ind <- TRUE
            stop_mess <- paste0("For each stratum, the sum of the base sites and 'Over' replacement sites must be no larger than the number of rows in 'sframe' representing that stratum.")
            stop_df <- rbind(stop_df, data.frame(func = I("n_base + n_over"), I(stop_mess)))
          }
        }
      } else {
        if ((n_base + sum(n_over)) > NROW(sframe)) {
          stop_ind <- TRUE
          stop_mess <- paste0("The sum of the base sites and 'Over' replacement sites must be no larger than the number of rows in 'sframe'.")
          stop_df <- rbind(stop_df, data.frame(func = I("n_base + n_over"), I(stop_mess)))
        }
      }
    }
  }
  
  # check n_near
  if (!is.null(n_near)) {
    if (!(all(unlist(n_near) %in% 1:10))) {
      stop_ind <- TRUE
      stop_mess <- paste0("values of n_near must be from 1 to 10.\n")
      stop_df <- rbind(stop_df, data.frame(func = I("n_near"), I(stop_mess)))
    }
  }
  
  # find system info
  on_solaris <- Sys.info()[["sysname"]] == "SunOS"
  if (on_solaris) {
    stop_ind <- TRUE
    stop_mess <- paste0("grts() and irs() are not supported on Solaris.")
    stop_df <- rbind(stop_df, data.frame(func = I("Solaris"), I(stop_mess)))
  }
  
  ### If any issues, write out stop_df and then stop
  if (stop_ind) {
    names(stop_df) <- c("Design Input", "Error Message")
    stop_df <<- stop_df
    cat("During the check of the input to grtspts, one or more errors were identified.\n")
    cat("Enter the following command to view all input error messages: stopprnt()\n")
    cat("To view a subset of the errors (e.g., errors 1 and 5) enter stopprnt(m=c(1,5))\n\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
}



grts_stratum <- function(stratum, dsgn, sframe, sf_type, wgt_units = NULL, pt_density = NULL,
                         legacy_option = FALSE, legacy_sites = NULL, maxtry = 10,
                         warn_ind = FALSE, warn_df = NULL) {
  
  # Sample sizes required
  n_base <- dsgn[["n_base"]][[stratum]]
  n_over <- sum(dsgn[["n_over"]][[stratum]], na.rm = TRUE)
  if (is.null(n_over)) {
    n_over <- 0
  }
  n_near <- dsgn[["n_near"]][[stratum]]
  if (is.null(n_near)) {
    n_near <- 0
  }
  n_total <- n_base + n_over
  
  # set number of legacy sites to 0
  n_legacy <- 0
  
  # subset sframe to stratum
  sftmp <- sframe[sframe$stratum == stratum, , drop = FALSE]
  
  # find legacy site number for points if legacy_var provided
  if (legacy_option == TRUE & is.null(legacy_sites)) {
    n_legacy <- sum(!is.na(sftmp$legacy))
  }
  
  # subset legacy_sites to stratum if present for linear and area option
  if (legacy_option == TRUE & (sf_type != "sf_point" | ((sf_type == "sf_point") & !is.null(legacy_sites)))) {
    legtmp <- legacy_sites[legacy_sites$stratum == stratum, , drop = FALSE]
    n_legacy <- nrow(legtmp)
  }
  
  # sf_type equals point
  if (sf_type == "sf_point") {
    ip_step1 <- 1
    sftmp$xcoord <- st_coordinates(sftmp)[, "X"]
    sftmp$ycoord <- st_coordinates(sftmp)[, "Y"]
    sftmp$idpts <- 10000 + 1:nrow(sftmp)
  }
  
  # sf_type equals linear
  if (sf_type == "sf_linear") {
    # determine sample size from pt_density and total length of sample frame in stratum
    stratum_len <- sum(st_length(sftmp))
    if (!is.null(wgt_units)) {
      stratum_len <- set_units(stratum_len, wgt_units, mode = "standard")
    }
    # set default equal to 10 population sites per requested sample site
    if (is.null(pt_density)) {
      pt_density <- 10
    }
    n_size <- as.integer(ceiling(pmin(1e9, pt_density * (n_base + n_over))))
    sfpts <- st_sample(sftmp, size = n_size, type = "regular", exact = TRUE)
    sfpts <- st_as_sf(as.data.frame(sfpts), crs = st_crs(sftmp))
    sfpts <- st_cast(sfpts, to = "POINT")
    # drop features with no points
    sfpts <- sfpts[!st_is_empty(sfpts), ]
    # join sites with linear features
    sfpts <- st_cast(sfpts, to = "POINT")
    sftmp <- st_join(sfpts, sftmp, join = st_nearest_feature)
    names(sftmp)[names(sftmp) == "sfpts"] <- "geometry"
    sftmp$xcoord <- st_coordinates(sftmp)[, "X"]
    sftmp$ycoord <- st_coordinates(sftmp)[, "Y"]
    sftmp$idpts <- 10000 + 1:nrow(sftmp)
    # calculate step 1 inclusion probability based on realized sample size
    ip_step1 <- nrow(sftmp) / stratum_len
  }
  
  
  # sf_type equals area
  if (sf_type == "sf_area") {
    # determine sample size from pt_density and total area of sample frame in stratum
    stratum_area <- sum(st_area(sftmp))
    if (!is.null(wgt_units)) {
      stratum_area <- set_units(stratum_area, wgt_units, mode = "standard")
    }
    # set default equal to 10 population sites per requested sample site
    if (is.null(pt_density)) {
      pt_density <- 10
    }
    n_size <- as.integer(ceiling(pmin(1e9, pt_density * (n_base + n_over))))
    sfpts <- st_sample(sftmp, size = n_size, type = "hexagonal", exact = TRUE)
    sfpts <- st_as_sf(as.data.frame(sfpts), crs = st_crs(sftmp))
    sfpts <- st_cast(sfpts, to = "POINT")
    # drop features with no points
    sfpts <- sfpts[!st_is_empty(sfpts), ]
    sftmp <- st_join(sfpts, sftmp)
    sftmp$xcoord <- st_coordinates(sftmp)[, "X"]
    sftmp$ycoord <- st_coordinates(sftmp)[, "Y"]
    sftmp$idpts <- 10000 + 1:nrow(sftmp)
    # calculate step 1 inclusion probability based on realized sample size
    ip_step1 <- nrow(sftmp) / stratum_area
  }
  
  # Determine number of elements in stratum
  Nstratum <- nrow(sftmp)
  
  # Determine if legacy sites are to be included for stratum design
  if (legacy_option == TRUE & n_legacy > 0 & (sf_type != "sf_point" | ((sf_type == "sf_point") & !is.null(legacy_sites)))) {
    tmp <- legtmp
    addtmp <- setdiff(names(tmp), names(sftmp))
    addleg <- setdiff(names(sftmp), names(tmp))
    sftmp[, addtmp] <- NA
    tmp[, addleg] <- NA
    sftmp <- rbind(tmp, sftmp)
    # Determine number of elements in stratum
    Nstratum <- nrow(sftmp)
  }
  
  # set legacy that is NA to FALSE
  if (legacy_option == TRUE & n_legacy > 0) {
    sftmp$legacy <- ifelse(is.na(sftmp$legacy), FALSE, TRUE)
    tmp <- sftmp[sftmp$legacy == TRUE, , drop = FALSE]
    n_legacy <- nrow(tmp)
  }
  
  if (legacy_option == TRUE & n_legacy == 0) {
    sftmp$legacy <- FALSE
  }
  
  # check that number of legacy sites is less than or equal number of base sites
  # stop if not
  if (n_legacy > n_base) {
    cat("Number of legacy sites is greater than number of base sites in at least one\n")
    cat("stratum. Please check that all strata have fewer legacy sites than base sites.\n")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  
  # Step 2 site selection if linear or area; otherwise Step 1 for points.
  # determine overall sample size required from dsgn for stratum
  # account for n_over sample option if present
  if (dsgn[["seltype"]][[stratum]] == "equal" | dsgn[["seltype"]][[stratum]] == "proportional") {
    n_caty <- n_total
  } else {
    if (n_over == 0) {
      n_caty <- dsgn[["caty_n"]][[stratum]]
    } else {
      base_prop <- dsgn[["caty_n"]][[stratum]] / sum(dsgn[["caty_n"]][[stratum]])
      n_caty <- dsgn[["caty_n"]][[stratum]] + dsgn[["n_over"]][[stratum]] * base_prop
    }
  }
  
  # If seltype is "equal" or "proportional", set caty to same as stratum
  if (dsgn[["seltype"]][[stratum]] == "equal" | dsgn[["seltype"]][[stratum]] == "proportional") {
    sftmp$caty <- "None"
  }
  
  # compute inclusion probabilities
  ip <- grtspts_ip(
    type = dsgn[["seltype"]][stratum], n_base = n_caty,
    Nstratum = Nstratum, caty = sftmp$caty, aux = sftmp$aux,
    warn_ind = warn_ind, warn_df = warn_df
  )
  
  # save initial inclusion probabilities
  sftmp$ip_init <- ip$ip
  sftmp$ip <- ip$ip
  # accumulate warning messages if any
  if (ip$warn_ind) {
    warn_ind <- ip$warn_ind
    warn_df <- ip$warn_df
    warn_df$stratum <- ifelse(is.na(warn_df$stratum), stratum, warn_df$stratum)
  }
  
  # If legacy sites, adjust inclusion probabilities to use
  # legacy inclusion probabilities
  if (legacy_option == TRUE & n_legacy > 0) {
    sftmp$ip <- grtspts_ipleg(sftmp$ip_init, sftmp$legacy == TRUE)
    # accumulate warning messages if any
    if (ip$warn_ind) {
      warn_ind <- ip$warn_ind
      warn_df <- ip$warn_df
      warn_df$stratum <- ifelse(is.na(warn_df$stratum), stratum, warn_df$stratum)
    }
  }
  
  # select sites if no minimum distance between sites
  if (is.null(dsgn[["mindis"]][[stratum]])) {
    sites <- sftmp[get_address(sftmp$xcoord, sftmp$ycoord, rand = TRUE), ]
    s <- sampling::UPpivotal(sites$ip)
    sites <- sites[round(s) == 1, ]
    sites <- rhoR::rho(sites)
    sites$siteuse <- NA
    sites$replsite <- NA
    sites <- list(sites = sites, warn_ind = warn_ind, warn_df = warn_df)
  }
  # If minimum distance between sites, select sites
  if (!is.null(dsgn[["mindis"]][[stratum]])) {
    sites <- grtspts_mindis(dsgn[["mindis"]][[stratum]], sftmp,
                            samplesize = n_total,
                            stratum = stratum, maxtry = maxtry, legacy_option = legacy_option,
                            legacy_var = dsgn[["legacy_var"]],
                            warn_ind = warn_ind, warn_df = warn_df
    )
  }
  # check for warning messages
  warn_ind <- sites$warn_ind
  warn_df <- sites$warn_df
  if (warn_ind) {
    warn_df$stratum <- ifelse(is.na(warn_df$stratum), stratum, warn_df$stratum)
  }
  
  # adjust inclusion probabilities when over sample sites present
  sites[["sites"]]$ip_init <- sites[["sites"]]$ip_init * n_base / n_total
  
  # Select replacement sites if n_near not NULL when do not have legacy sites
  if (legacy_option == FALSE | n_legacy == 0) {
    if (!is.null(dsgn[["n_near"]][[stratum]])) {
      sites_near <- replace_near(dsgn[["n_near"]][[stratum]],
                                 sites = sites[["sites"]],
                                 sframe = sftmp
      )
      
      # Adjust inclusion probabilities for replacement sites if over sample sites present
      if (n_over != 0) {
        sites_near$ip_init <- sites_near$ip_init * n_base / n_total
      }
    }
  }
  
  # Select replacement sites if n_near not NULL when have legacy sites
  if (legacy_option == TRUE & n_legacy > 0) {
    if (!is.null(dsgn[["n_near"]][[stratum]])) {
      keep <- sites[["sites"]][sites[["sites"]]$legacy %in% c(TRUE, FALSE), "idpts", drop = TRUE]
      sites_near <- replace_near(dsgn[["n_near"]][[stratum]],
                                 sites = sites[["sites"]][sites[["sites"]]$legacy %in% c(TRUE, FALSE), ],
                                 sframe = subset(sftmp, !(sftmp$idpts %in% keep))
      )
      
      # Adjust inclusion probabilities for replacement sites if over sample sites present
      if (n_over != 0) {
        sites_near$ip_init <- sites_near$ip_init * n_base / n_total
      }
    }
  }
  
  # Assign original inclusion probabilities to sites, create weights and drop legacy ip variable
  sites[["sites"]]$ip <- sites[["sites"]]$ip_init * ip_step1
  sites[["sites"]]$wgt <- 1 / sites[["sites"]]$ip
  tmp <- names(sites[["sites"]])
  sites[["sites"]] <- subset(sites[["sites"]],
                             select = tmp[!(tmp %in% c("ip_init", "geometry"))]
  )
  
  # Do same for sites_near if any
  if (is.null(dsgn[["n_near"]][[stratum]])) {
    sites_near <- NULL
  }
  if (!is.null(dsgn[["n_near"]][[stratum]])) {
    sites_near$ip <- sites_near$ip_init * ip_step1
    sites_near$wgt <- 1 / sites_near$ip
    tmp <- names(sites_near)
    sites_near <- subset(sites_near, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
    sites_near[, setdiff(names(legacy_sites), names(sites_near))] <- NA
  }
  
  # Split sites to have separate sites_base, sites_legacy and sites_over
  # save legacy sites if any and reduce sites_base to non legacy sites
  sites_legacy <- NULL
  if (legacy_option == TRUE & n_legacy > 0) {
    sites_legacy <- sites[["sites"]][sites[["sites"]]$legacy == TRUE, ]
    sites[["sites"]] <- sites[["sites"]][sites[["sites"]]$legacy == FALSE, ]
    n_legacy <- nrow(sites_legacy)
  }
  
  
  
  # save base sites
  sites_base <- NULL
  if (n_base > n_legacy) {
    sites_base <- sites[["sites"]][1:(n_base - n_legacy), ]
    sites_base[, setdiff(names(legacy_sites), names(sites_base))] <- NA
  }
  
  # save n_over sample sites if any
  sites_over <- NULL
  if (n_over != 0) {
    sites_over <- sites[["sites"]][(n_base - n_legacy + 1):(n_total - n_legacy), ]
    sites_over$siteuse <- "Over"
    sites_over[, setdiff(names(legacy_sites), names(sites_over))] <- NA
  }
  
  # if no legacy sites match in strata then put in appropriate column
  if (legacy_option == TRUE & n_legacy == 0) {
    sites_base$legacy <- FALSE
  }
  
  
  # create list for output and return result
  rslts <- list(
    sites_base = sites_base, sites_legacy = sites_legacy,
    sites_over = sites_over, sites_near = sites_near,
    warn_ind = warn_ind, warn_df = warn_df
  )
  
  invisible(rslts)
}


grtspts_ip <- function(type = "equal", n_base, Nstratum = NULL, caty = NULL,
                       aux = NULL, warn_ind = NULL, warn_df = NULL) {
  
  # equal inclusion probabilities
  if (type == "equal") {
    ip <- rep(sum(n_base, na.rm = TRUE) / Nstratum, Nstratum)
  }
  # unequal inclusion probabilities
  if (type == "unequal") {
    gsum <- table(caty)
    catmatch <- match(names(n_base), names(gsum), nomatch = 0)
    piden <- n_base / gsum[catmatch]
    if (any(piden > 1)) {
      stop(paste0("The number of expected samples in levels ", paste0(names(piden[piden > 1]), collapse = " and "), " from the caty_n variable exceeds the number of allowed samples in sframe (if stratification was not used) or in at least one stratum (if stratification was used). If sframe has POINT or MULTIPOINT geometry, this means that the number of expected samples specified by these levels exceeds the number of observations in sframe. Consider 1) incorporating the caty_n variable into stratification or 2) reduce the number of expected samples for these levels in caty_n. Then rerun grts(). If sframe has LINESTRING, MULTILINESTRING, POLYGON, or MULTIPOLYGON geometry, this means that the number of expected samples specified by these levels exceeds the number of observations used in the approximation specified by the pt_density argument. Consider 1) incorporating the caty_n variable into stratification, 2) reduce the number of expected samples for these levels in caty_n, or 3) increase the pt_density argument. Then rerun grts()."), call. = FALSE)
    }
    ip <- rep(NA, length(caty))
    for (i in names(n_base)) {
      ip[caty == i] <- piden[i]
    }
  }
  # proportional inclusion probabilities
  if (type == "proportional") {
    ip <- aux
    # check for "0" and negative values. if negative set to "0".
    nnull <- sum(aux == 0)
    nneg <- sum(aux < 0)
    if (nnull > 0) {
      warn <- "Proportional vector has zero values and their inclusion probabilities are set to 0."
      if (is.null(warn_ind)) {
        warn_df <- data.frame(stratum = NA, func = I("grtspts_ip"), warning = warn)
        warn_ind <- TRUE
      } else {
        if (warn_ind) {
          warn_df <- rbind(warn_df, data.frame(
            stratum = NA, func = I("grtspts_ip"),
            warning = warn
          ))
        } else {
          warn_df <- data.frame(stratum = NA, func = I("grtspts_ip"), warning = warn)
          warn_ind <- TRUE
        }
      }
    }
    
    if (nneg > 0) {
      ip[ip < 0] <- 0
      warn <- paste0("Proportional vector has ", nneg, " negative value(s) and
              their inclusion probabilities are set to 0.")
      if (warn_ind) {
        warn_df <- data.frame(stratum = NA, func = I("grtspts_ip"), warning = warn)
      } else {
        warn_df <- rbind(warn_df, data.frame(
          stratum = NA, func = I("grtspts_ip"),
          warning = warn
        ))
        warn_ind <- TRUE
      }
    }
    # initialize inclusion probabilities
    ip <- n_base * ip / sum(ip)
    
    # identify and subset elements that have ip > 0
    element_gt0 <- ip > 0
    ip_gt0 <- ip[element_gt0]
    # identify elements greater than or equal to 1 and count them.
    element_gt1 <- ip_gt0 >= 1
    ngt1 <- sum(element_gt1)
    # if ngt1 is greater than 0, then set them to 1 and adjust remaining elements
    # to have sample size n - ngt1. Adjustment may cause other elements to become
    # greater than 1 so repeat until all are less than or equal to 1.
    if (ngt1 > 0) {
      tst <- 0
      while (ngt1 != tst) {
        tmp <- ip_gt0[!element_gt1]
        ip_gt0[!element_gt1] <- (n_base - ngt1) * tmp / sum(tmp)
        ip_gt0[element_gt1] <- 1
        tst <- ngt1
        element_gt1 <- ip_gt0 >= 1
        ngt1 <- sum(element_gt1)
      }
      # replace non zero inclusion probabilities with new ones
      ip[element_gt0] <- ip_gt0
    }
    ip
  }
  
  # return list with vector of inclusion probabilities and warning indicator and messages
  ip <- list(ip = ip, warn_ind = warn_ind, warn_df = warn_df)
  
  invisible(ip)
}




get_address <- function(x, y, rand = TRUE) {
  x <- trunc(between(x, 0, 2^31 - 1))
  y <- trunc(between(y, 0, 2^31 - 1))
  if (!rand) {
    a <- 0L
    b <- 1L
    c <- 2L
    d <- 3L
  } else {
    t <- sapply(1:32, function(i) sample(0:3))
    a <- rep(t[1, ], length(x))
    b <- rep(t[2, ], length(x))
    c <- rep(t[3, ], length(x))
    d <- rep(t[4, ], length(x))
  }
  x <- as.logical(rev(intToBits(x)))
  y <- as.logical(rev(intToBits(y)))
  t <- a * (!x & !y) + b * (!x & y) + c * (x & !y) + d * (x & y)
  l <- lapply(2:32L, function(i) {
    rev(t[seq(i, length(t), by = 32L)])
  })
  do.call(order, l)
}

between <- function(x, low = 0, up = 1) {
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  low + (up - low) * (x - minx) / (maxx - minx)
}
