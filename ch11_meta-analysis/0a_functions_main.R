# Custom functions used in Metaketa I ----

# Gather data from all studies --------------------------------------------

# This function also allows user to include additional variables that are not returned by default

pool_studies <- function(add_vars = NULL, dir = "data/temp"){
  #load data
  files <- list.files(dir, pattern = "covinter", full.names = TRUE)
  for (f in files) {
    load(f)
  }
  
  madat <- rbind.fill(ben, bf, brz, mexsur, ug1, ug2)
  
  # outcome and cov vars
  vars_meta <- grep("^m[[:digit:]][[:digit:]]?[[:alnum:]]*?$", names(madat), value = TRUE)
  keep <- c("ctry", "cl", "fe", "ma_id", "treat", "inv_wts", "inv_wts2", vars_meta,
            "location.id", "ug2_contested", "ug2_councilor_dummy",
            "lc5.chair.party.switch", "lc5.councillor.party.switch", "lc5.councillor.redistricted2016",
            "N","N_good","Q_good","correct","paired_block", "P", "Q", "post.prior.diff",
            add_vars)
  
  madat <- madat[,keep]
  
  return(madat)
}

# Main estimation function ------------------------------------------------

estimates <- function(dat = madat,
                      exclude_councilors = TRUE, # ug2 chair sample if TRUE, pooled sample of chairs and councilors if FALSE
                      cov = NULL,                # list covariates in string united by '+' sign
                      weights = TRUE,            # weight estimates
                      with_N = FALSE,            # regress on Nij
                      good = NULL,               # restrict sample to good/bad news
                      Q_good = FALSE,            # use median Q rule for good news
                      depvar = NULL,             # dependent variable
                      country = NULL,            # restrict data set to country
                      interactvar = NULL,
                      treat1and2 = FALSE,        # whether to regress on treat1 and treat2 (private and public treatments)
                      contested_seats = TRUE,    #if TRUE, exclude non-competitive races in uganda 2
                      return_df = FALSE,         # return prepared data.frame instead of model estimates (to run with function 'intercept')
                      skip_prep = FALSE
){
  df <- dat
  
  if(!skip_prep){
    
    if(depvar == "m1" & !treat1and2){
      df <- df %>% subset(!is.na(m1))
    }
    
    if(depvar == "m3" & !treat1and2){
      df <- df %>% subset(!is.na(m3))
    }
    
    #subset to contested seats in Uganda 2
    if(contested_seats & "ug2" %in% unique(df$ctry)){
      df <- filter(df, ctry != "ug2" | (ctry == "ug2" & ug2_contested == 1))
    }
    
    # add council and chair fixed effects from ug2 where individual observations are counted twice
    df$councilor_dummy <- 0 # if individual obs were not duplicated for analysis to include both councilor and chair, create constant fixed effect variable to avoid error in felm functions below.
    df$councilor_dummy[df$ug2_councilor_dummy == 1] <- 1
    
    if(exclude_councilors){
      df <- filter(df, councilor_dummy != 1)
    }
    
    #subset to good or bad news sample
    df$goodnews <- NA
    if(Q_good){
      df$goodnews <- df$Q_good
    }else{
      df$goodnews <- df$N_good
    }
    
    if(!is.null(good)){
      if(good == TRUE){
        df <- filter(df, goodnews==1)
      }else{
        df <- filter(df, goodnews==0)
      }}
    
    #prepare for modeling
    c <- length(unique(df$ctry))
    # imputing
    if(!is.null(cov)){
      df <- df %>%
        group_by(ctry, fe) %>%
        mutate(m14i = ifelse(is.na(m14), mean(m14, na.rm=T), m14)) %>%
        mutate(m17i = ifelse(is.na(m17), mean(m17, na.rm=T), m17)) %>%
        mutate(m18i = ifelse(is.na(m18), mean(m18, na.rm=T), m18)) %>%
        mutate(m20i = ifelse(is.na(m20), mean(m20, na.rm=T), m20)) %>%
        mutate(m21i = ifelse(is.na(m21), mean(m21, na.rm=T), m21)) %>%
        mutate(m22i = ifelse(is.na(m22), mean(m22, na.rm=T), m22)) %>%
        mutate(m24i = ifelse(is.na(m24), mean(m24, na.rm=T), m24)) %>%
        mutate(m26i = ifelse(is.na(m26), mean(m26, na.rm=T), m26)) %>%
        mutate(m27i = ifelse(is.na(m27), mean(m27, na.rm=T), m27)) %>%
        ungroup(df)
    }
    # Mean centering/stdising
    if("data.frame" %in% class(df[,depvar])){
      df$y <- df[,depvar][[1]]
    }
    if("numeric" %in% class(df[,depvar]) | "integer" %in% class(df[,depvar])){
      df$y <- df[,depvar]
    }
    
    df <- df %>% drop_na(y, fe,cl)
    
    if(with_N){
      df <- df %>% drop_na(N)
    }
    if(!is.null(cov)){
      df <- df %>% drop_na(m14i, m17i, m18i, m20i, m21i, m22i, m24i, m26i, m27i)
    }
    
    if(!is.null(interactvar)){
      df <- df %>% drop_na(interactvar)
      if(interactvar == "m11"){
        df <- df %>% subset(ctry != "mex")
      }
      if(interactvar == "m23"){
        df <- df %>% subset(ctry %in% c("ben", "brz", "ug2", "ug1"))
      }
    }
    
    if(depvar == "m8"){ # restrict to mexico and benin sample
      df <- df %>% subset(ctry %in% c("mex", "ben"))
    }
    
    df$inv_wt <- NA
    df$inv_wt[df$ctry == "brz"] <- df$inv_wts[df$ctry == "brz"]
    df$inv_wt[df$ctry == "ug2"] <- df$inv_wts2[df$ctry == "ug2"]
    
    df$inv_wt[!df$ctry %in% c("brz", "ug2") ] <- 1/.5 # assignment probability = .5 for all countries other than brazil and uganda 2
    
    #include IPW only
    if(!weights){
      df <- df %>%
        # mutate(obs = n()) %>%
        # group_by(ctry) %>%
        dplyr::mutate(sum_wt = sum(inv_wt)) %>%
        dplyr::mutate(wt = inv_wt/sum_wt) # create weight
      # ungroup()
    }else{
      df <- df %>%
        # mutate(obs = n()) %>%
        dplyr::mutate(ctry_n = length(unique(ctry))) %>%
        group_by(ctry) %>%
        dplyr::mutate(ctry_sum_wt = sum(inv_wt)) %>%
        dplyr::mutate(wt = inv_wt/(ctry_n * ctry_sum_wt)) %>% 
        dplyr::select(-ctry_n) %>%# create weight
        ungroup()
    }
    
    if(with_N){
      df <- df %>% group_by(ctry) %>%
        mutate(N_temp = (N - mean(N, na.rm = T))/(sd(N, na.rm =T))) %>%
        ungroup(df)
      df$N_temp[df$ctry=="mex"] <- 0 # because when subsetting to good/bad news, N has constant value
      df$N <- df$N_temp
    }
    
    if(depvar == "m6"){ #standardize if regressing m6 because of scaling differences
      df <- df %>% group_by(ctry) %>%
        mutate(y = (y - mean(y, na.rm = T))/(sd(y, na.rm =T))) %>%
        ungroup(df)
    }
    
    if(!is.null(cov)){
      df <- df %>% group_by(ctry) %>%
        mutate(m14i = m14i - mean(m14i, na.rm = T)) %>%
        mutate(m17i = m17i - mean(m17i, na.rm = T)) %>%
        mutate(m18i = m18i - mean(m18i, na.rm = T)) %>%
        mutate(m20i = m20i - mean(m20i, na.rm = T)) %>%
        mutate(m21i = m21i - mean(m21i, na.rm = T)) %>%
        mutate(m22i = m22i - mean(m22i, na.rm = T)) %>%
        mutate(m24i = m24i - mean(m24i, na.rm = T)) %>%
        mutate(m26i = m26i - mean(m26i, na.rm = T)) %>%
        mutate(m27i = m27i - mean(m27i, na.rm = T)) %>%
        ungroup(df)
    }
    
    if(!is.null(interactvar)){
      if("data.frame" %in% class(df[,interactvar])){
        df$interactvar <- df[,interactvar][[1]]
      }
      if("numeric" %in% class(df[,interactvar])){
        df$interactvar <- df[,interactvar]
      }
      df <- df %>% group_by(ctry) %>%
        mutate(interactvar = interactvar - mean(interactvar, na.rm = T)) %>%
        ungroup(df)
    }
    # 
    #   if("brz" %in% unique(df$ctry)){
    # 
    #   sum_ipw <- sum(df$inv_wts, na.rm = T)
    #   sum_metawt_brz <- sum(df$wt[df$ctry=="brz"], na.rm = T)
    #   scale_wt <- sum_metawt_brz/sum_ipw
    #   df$wt[df$ctry=="brz"] <- df$inv_wts[df$ctry=="brz"] * scale_wt
    #   }
    
    # if("ug2" %in% unique(df$ctry)){
    # 
    # sum_ipw2 <- sum(df$inv_wts2[df$ctry=="ug2"], na.rm = T)
    # sum_metawt_ug2 <- sum(df$wt[df$ctry=="ug2"], na.rm = T)
    # scale_wt2 <- sum_metawt_ug2/sum_ipw2
    # df$wt[df$ctry=="ug2"] <- df$inv_wts2[df$ctry=="ug2"] * scale_wt2
    # }
    
    if (!is.null(good)) {
      if (good == TRUE) {
        df$N[df$ctry == "brz"] <- 0
      }
    }
    
    #restrict to country sample if applicable
    if(!is.null(country)){
      df <- df %>% subset(ctry == country)
    }
    
    # # brazil weights if country-specific estimation
    # if(!is.null(country)){
    #   if(country == "brz"){
    #     df$wt <- df$inv_wts
    #   }}
    # 
    # # uganda 2 weights if country-specific estimation
    # if(!is.null(country)){
    #   if(country == "ug2"){
    #     df$wt <- df$inv_wts2
    #   }}
    
    
    if(return_df){
      return(df)
    }
  }
  
  if(!return_df){
    
    # specify model and whether is weighted
    
    if(!with_N & is.null(cov)){
      if(weights & !is.null(interactvar)){
        model <- felm(as.formula(paste0("y ~ treat*", interactvar, "|fe + councilor_dummy|0|cl")), weights = df$wt, data = df)
      }
      if(weights & is.null(interactvar)){
        if(!treat1and2){
          model <- felm(y ~ treat|fe + councilor_dummy|0|cl, weights = df$wt, data = df)
        }else{
          model <- felm(y ~ treat1 + treat2|fe + councilor_dummy|0|cl, weights = df$wt, data = df)
        }}
      if(!weights & is.null(interactvar)){
        if(!treat1and2){
          model <- felm(y ~ treat|fe + councilor_dummy|0|cl, weights = df$wt, data = df)
        }else{
          model <- felm(y ~ treat1 + treat2|fe + councilor_dummy|0|cl, weights = df$wt, data = df)
        }}
    }
    
    if(with_N & is.null(cov)){
      if(weights){
        model <- felm(y ~ treat*N|fe + councilor_dummy|0|cl, weights = df$wt, data = df)
      }else{
        model <- felm(y ~ treat*N|fe + councilor_dummy|0|cl, weights = df$wt, data = df)
      }
    }
    
    if(!with_N & !is.null(cov)){
      if(weights){
        model <- felm(as.formula(paste0("y ~ treat*(", cov, ")|fe + councilor_dummy|0|cl")), weights = df$wt, data = df)
      }else{
        model <- felm(as.formula(paste0("y ~ treat*(", cov, ")|fe + councilor_dummy|0|cl")), weights = df$wt, data = df)
      }
    }
    if(with_N & !is.null(cov)){
      if(weights){
        model <- felm(as.formula(paste0("y ~ treat*(N+", cov, ")|fe + councilor_dummy|0|cl")), weights = df$wt, data = df)
      }else{
        model <- felm(as.formula(paste0("y ~ treat*(N+", cov, ")|fe + councilor_dummy|0|cl")), weights = df$wt, data = df)
      }
    }
    if(!return_df){
      return(model)
    }
  }
}



# Main results (returns estimates, RI p-value and control means) ----------

results <- function(ri_p = "generate",         # 'generate' runs RI code, 'ignore' generates NAs, 'load' loads existing RITE from file in 'fetch_rite_obj'
                    file_rite_obj = NULL,      # path to RITE object if 'ri_p = "load"'
                    # save_rite_obj = NULL,      # path to save RITE object
                    dat = madat,               # dataset (with permutations for randomization inference)
                    exclude_councilors = TRUE, # ug2 chair sample if TRUE, pooled sample of chairs and councilors if FALSE
                    cov = NULL,                # list covariates in string united by '+' sign
                    weights = TRUE,            # weight estimates
                    with_N = FALSE,            # regress on Nij
                    good = NULL,               # restrict sample to good/bad news
                    Q_good = FALSE,            # use median Q rule for good news
                    depvar = NULL,             # dependent variable
                    country = NULL,            # restrict data set to country
                    interactvar = NULL,        # string name of variable interacted with treatment
                    treat1and2 = FALSE,        # TRUE if public vs private analysis
                    contested_seats = TRUE,    # if TRUE, exclude non-competitive races in uganda 2
                    return_df = FALSE,         # return prepared data.frame instead of model estimates (to run with function 'intercept')
                    skip_prep = FALSE,         # perform estimations only (skips subsetting and weight calculation)
                    sims = NULL){             # number of simulations of treatment assignment
  
  
  if(ri_p == "ignore") ignore_perms <- TRUE
  if(ri_p == "generate"){
    
    if(sum(names(dat)== "iter1")>0){
      ignore_perms <- FALSE
      
      ri <- get_rite_p(dat = dat, exclude_councilors = exclude_councilors,
                       cov = cov, weights = weights, with_N = with_N,
                       good = good, Q_good = Q_good, depvar = depvar,
                       country = country, interactvar = interactvar,
                       treat1and2 = treat1and2, contested_seats = contested_seats,
                       sims = sims)
      
      est <- ri$est
      b_ri <- ri$rite
      p <- ri$p
      
      if(!is.null(file_rite_obj)){
        save(ri, file = file_rite_obj)
      }
      
    }else{
      ignore_perms <- TRUE
      warning("data object does not have treatment permutation columns,
              will return NA for randomization inference treatment effects and p-value.")
    }
  }
  
  if(ri_p == "load"){
    ignore_perms <- FALSE
    
    load_obj <- function(obj){
      house <- new.env()
      ri <- load(obj, house)[1]
      house[[ri]]
    }
    
    rite <- load_obj(file_rite_obj)
    
    est <- rite$est
    b_ri <- rite$rite
    p <- rite$p
    
  }
  
  if(!skip_prep){
    dat <- dat[,!names(dat) %in% grep("^iter", names(dat), value = TRUE)]
    df <- estimates(dat = dat, exclude_councilors = exclude_councilors,
                    cov = cov, weights = weights, with_N = with_N,
                    good = good, Q_good = Q_good, depvar = depvar,
                    country = country, interactvar = interactvar,
                    treat1and2 = treat1and2, contested_seats = contested_seats,
                    return_df = TRUE, skip_prep = skip_prep)
  }else{
    df <- dat
  }
  
  
  df$y <- df[[depvar]]
  if(!treat1and2){
    mean.control <- round(lm(y ~ treat, weights = df$wt, data = df)$coefficients[1], 3)
  }else{
    mean.control <- round(lm(y ~ treat1 + treat2, weights = df$wt, data = df)$coefficients[1], 3)
  }
  
  if(ri_p == "ignore" | ignore_perms){
    
    est <- estimates(dat = df, exclude_councilors = exclude_councilors,
                     cov = cov, weights = weights, with_N = with_N,
                     good = good, Q_good = Q_good, depvar = depvar,
                     country = country, interactvar = interactvar,
                     treat1and2 = treat1and2, contested_seats = contested_seats,
                     return_df = FALSE, skip_prep = TRUE)
    b_ri <- NA
    p <- NA
  }
  
  return(list(estimates = est,
              mean.control = mean.control,
              rite = b_ri,
              p = p
  ))
  
  gc()
}

# Prepare data for co-partisanship moderation analysis --------------------

prepdat_copartisan <- function(dat, depvar, binary) {
  
  # dat <- with(olddf, cbind.data.frame(depvar, m19, treat, fe, cl, inv_wts, ctry, block))
  dat$depvar <- depvar
  
  dat <- dat %>%
    drop_na(depvar,m19,fe,cl) %>%
    mutate(c = length(unique(ctry))) %>%
    mutate(obs = n()) %>%
    group_by(ctry) %>%
    mutate(ctryobs = n()) %>%
    mutate(wt = obs/(c*ctryobs)) %>% # create weight
    ungroup(dat)
  
  if (binary == 0) {
    # Mean centering/stdising
    dat <- dat %>%
      group_by(ctry) %>%
      mutate(m19 = m19 - mean(m19, na.rm = T)) %>%
      ungroup(dat)
  }
  else {
    if (binary == 1) {
      # Dichotomising the moderator
      # Benin, Brazil and Uganda 1 (1-7/8)
      df1a <- dat %>%
        filter(ctry == "ben"|ctry == "ug1"|ctry == "brz", m19<=4) %>%
        mutate(m19 = 0)
      df1b <- dat %>%
        filter(ctry == "ben"|ctry == "ug1"|ctry == "brz", m19>4) %>%
        mutate(m19 = 1)
      # Mexico (1-9)
      df2a <- dat %>%
        filter(ctry == "mex", m19<=5) %>%
        mutate(m19 = 0)
      df2b <- dat %>%
        filter(ctry == "mex", m19>5) %>%
        mutate(m19 = 1)
      dat <- rbind.data.frame(df1a,df1b,df2a,df2b,subset(dat,ctry=="ug2"))
    }
    
    sum_ipw <- sum(dat$inv_wts, na.rm = T)
    sum_metawt_brz <- sum(dat$wt[dat$ctry=="brz"], na.rm = T)
    scale_wt <- sum_metawt_brz/sum_ipw
    dat$wt[dat$ctry=="brz"] <- dat$inv_wts[dat$ctry=="brz"] * scale_wt
    
    #create inverse propensity weights for ug2 based on location sample size
    #i.e., villages with fewer than 15 subjects had assignment probability of .5, and otherwise paired with assignment prob .2 and .8
    # dat <- dat %>% group_by(location.id) %>% mutate(n = n(), prop.treat = sum(treat==1)/n)
    # dat$inv_wts2 <- ifelse(dat$n<15, 1/.5,ifelse(dat$prop.treat > 2/3, 1/.8, 1/.2))
    sum_ipw2 <- sum(dat$inv_wts2, na.rm = T)
    sum_metawt_ug2 <- sum(dat$wt[dat$ctry=="ug2"], na.rm = T)
    scale_wt2 <- sum_metawt_ug2/sum_ipw2
    dat$wt[dat$ctry=="ug2"] <- dat$inv_wts2[dat$ctry=="ug2"] * scale_wt2
    
    return(dat)
  }
}

# Prepare data for clientelism moderation analysis ------------------------

prepdat_client <- function(dat, depvar, binary) {
  
  dat$depvar <- depvar
  
  dat <- dat %>%
    drop_na(depvar,m22,fe,cl) %>%
    mutate(c = length(unique(ctry))) %>%
    mutate(obs = n()) %>%
    group_by(ctry) %>%
    mutate(ctryobs = n()) %>%
    mutate(wt = obs/(c*ctryobs)) %>% # create weight
    ungroup(dat)
  
  if (binary == 0) {
    # Mean centering/stdising
    dat <- dat %>%
      group_by(ctry) %>%
      mutate(m22 = m22 - mean(m22, na.rm = T)) %>%
      ungroup(dat)
  }
  else {
    if (binary == 1) {
      # Dichotomising the moderator
      # Benin, Brazil, Mexico, Ug1, Ug2 (1-4/5)
      df1a <- dat %>%
        filter(ctry=="ben"|ctry=="brz"|ctry=="mex", m22<=2) %>%
        mutate(m22 = 0)
      df1b <- dat %>%
        filter(ctry=="ben"|ctry=="brz"|ctry=="mex", m22>2) %>%
        mutate(m22 = 1)
      # Ug1, Ug2 (1-5)
      df2a <- dat %>%
        filter(ctry=="ug1"|ctry=="ug2", m22<=3) %>%
        mutate(m22 = 0)
      df2b <- dat %>%
        filter(ctry=="ug1"|ctry=="ug2", m22>3) %>%
        mutate(m22 = 1)
      dat <- rbind.data.frame(df1a,df1b,df2a,df2b,subset(dat,ctry=="bf"))
    }
    
    sum_ipw <- sum(dat$inv_wts, na.rm = T)
    sum_metawt_brz <- sum(dat$wt[dat$ctry=="brz"], na.rm = T)
    scale_wt <- sum_metawt_brz/sum_ipw
    dat$wt[dat$ctry=="brz"] <- dat$inv_wts[dat$ctry=="brz"] * scale_wt
    
    #create inverse propensity weights for ug2 based on location sample size
    #i.e., villages with fewer than 15 subjects had assignment probability of .5, and otherwise paired with assignment prob .2 and .8
    # dat <- dat %>% group_by(location.id) %>% mutate(n = n(), prop.treat = sum(treat==1)/n)
    # dat$inv_wts2 <- ifelse(dat$n<15, 1/.5,ifelse(dat$prop.treat > 2/3, 1/.8, 1/.2))
    sum_ipw2 <- sum(dat$inv_wts2, na.rm = T)
    sum_metawt_ug2 <- sum(dat$wt[dat$ctry=="ug2"], na.rm = T)
    scale_wt2 <- sum_metawt_ug2/sum_ipw2
    dat$wt[dat$ctry=="ug2"] <- dat$inv_wts2[dat$ctry=="ug2"] * scale_wt2
    
    return(dat)
  }
}

# Prepare data for co-ethnicity moderation analysis -----------------------

prepdat_coethnic <- function(dat, depvar, binary) {
  
  dat$depvar <- depvar
  
  dat <- dat %>%
    drop_na(depvar,m15,fe,cl) %>%
    mutate(c = length(unique(ctry))) %>%
    mutate(obs = n()) %>%
    group_by(ctry) %>%
    mutate(ctryobs = n()) %>%
    mutate(wt = obs/(c*ctryobs)) %>% # create weight
    ungroup(dat)
  
  if (binary == 0) {
    # Mean centering/stdising
    dat <- dat %>%
      group_by(ctry) %>%
      mutate(m15 = m15 - mean(m15, na.rm = T)) %>%
      ungroup(dat)
  }
  else {
    if (binary == 1) {
      # Dichotomising the moderator
      df1a <- dat %>%
        filter(m15 <= 0.5) %>%
        mutate(m15 = 0)
      df1b <- dat %>%
        filter(m15 > 0.5) %>%
        mutate(m15 = 1)
      dat <- rbind.data.frame(df1a,df1b)
    }
    
    sum_ipw <- sum(dat$inv_wts, na.rm = T)
    sum_metawt_brz <- sum(dat$wt[dat$ctry=="brz"], na.rm = T)
    scale_wt <- sum_metawt_brz/sum_ipw
    dat$wt[dat$ctry=="brz"] <- dat$inv_wts[dat$ctry=="brz"] * scale_wt
    
    #create inverse propensity weights for ug2 based on location sample size
    #i.e., villages with fewer than 15 subjects had assignment probability of .5, and otherwise paired with assignment prob .2 and .8
    # dat <- dat %>% group_by(location.id) %>% mutate(n = n(), prop.treat = sum(treat==1)/n)
    # dat$inv_wts2 <- ifelse(dat$n<15, 1/.5,ifelse(dat$prop.treat > 2/3, 1/.8, 1/.2))
    sum_ipw2 <- sum(dat$inv_wts2, na.rm = T)
    sum_metawt_ug2 <- sum(dat$wt[dat$ctry=="ug2"], na.rm = T)
    scale_wt2 <- sum_metawt_ug2/sum_ipw2
    dat$wt[dat$ctry=="ug2"] <- dat$inv_wts2[dat$ctry=="ug2"] * scale_wt2
    
    return(dat)
  }
}

# Functions for tabulation ------------------------------------------------

round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

make_col <- function(est){
  tab <- round_df(est, 3)
  tab$std.error <- paste0("(",as.numeric(tab$std.error),")")
  tab$r.squared <- paste0("[",tab$r.squared,"]")
  tab$estimate <- as.character(tab$estimate)
  tab$estimate[tab$p.value < 0.001] <- paste0(tab$estimate[tab$p.value < 0.001],"***")
  tab$estimate[tab$p.value >= 0.001 & tab$p.value < 0.01] <- paste0(tab$estimate[tab$p.value >= 0.001 & tab$p.value < 0.01],"**")
  tab$estimate[tab$p.value >= 0.01 & tab$p.value < 0.05] <- paste0(tab$estimate[tab$p.value >= 0.01 & tab$p.value < 0.05],"*")
  tab <- tab %>% dplyr::select(-statistic, -p.value) %>% tidyr::gather(key, value, -term)
  names(tab)[2] <- "est"
  tab <- tab %>% dplyr::arrange(term)
  return(tab)
}

# F-test function for equality of coefficients ----------------------------

f_equalcoeff <- function(df, depvar) {
  require(lfe)
  
  dat <- df
  clist <- unique(df$ctry)
  f <- NA
  mod <-  felm(as.formula(paste(depvar, "~ treat1 + treat2|fe|0|cl")), weights = dat$wt,
               data = dat)
  test <- linearHypothesis(mod, c("treat1 = treat2"), test=c("F"))
  f[1] <- round(test$`Pr(>F)`[2], 3)
  
  for (i in 1:length(clist)) {
    mod <-  felm(as.formula(paste(depvar, " ~ treat1 + treat2|fe|0|cl")),
                 data = subset(dat, ctry == clist[i]))
    test <- linearHypothesis(mod, c("treat1 = treat2"), test=c("F"))
    f[i+1] <- round(test$`Pr(>F)`[2], 3)
  }
  
  return(f)
}

# assignment functions ----------------------------------------------------

assign <- function(dat){
  
  p <- block_and_cluster_ra(dat$fe, dat$cl, prob = .5)
  rug2 <- dat$ctry == "ug2"
  p[rug2] <- assign_ug2(dat[rug2,])
  
  # mexico blocks with no variation in treatment assignment
  alwys_control <- dat$fe %in% paste0("mex.", c(22, 49, 81, 85, 92, 107))
  p[alwys_control] <- 0
  return(p)
}

assign_ug2 <- function(dat){
  dat <- dat %>% group_by(fe) %>% mutate(block_n = length(unique(cl)))
  small <- (dat$block_n < 15)
  highlow <- block_and_cluster_ra(dat$paired_block, dat$fe, prob = .5)
  equal <- block_and_cluster_ra(dat$fe, dat$cl, prob = .5)
  unequal <- block_and_cluster_ra(dat$fe, dat$cl, prob = .8)
  assign <- small*equal + (1-small)*(highlow*unequal + (1-highlow)*(1-unequal))
  return(assign)
}

assign_i <- function(dat, sims){
  require(doParallel)
  n_cores <- detectCores()
  ifelse(n_cores > 4, corei <- makeCluster(6), corei <- makeCluster(n_cores - 1)) #if not running on VM
  registerDoParallel(corei)
  
  #get libraries
  clusterEvalQ(corei, {
    library(dplyr)
    library(randomizr)
  })
  #put objects in place that might be needed for the code
  clusterExport(corei,c("assign_ug2","assign"))
  clusterExport(corei,deparse(substitute(dat)))
  
  iters <- parSapply(corei, 1:sims, function(i, ...){set.seed(i); assign(dat)})
  
  stopCluster(corei)
  
  colnames(iters) <- paste0("iter", 1:ncol(iters))
  return(iters)
}


# calculate ATE using assignments under RI --------------------------------

get_rite_p <- function(dat = madat,
                       country = NULL,
                       depvar = NULL,
                       cov = NULL,
                       with_N = FALSE,
                       good = NULL,
                       Q_good = FALSE,
                       exclude_councilors = FALSE,
                       interactvar = NULL,
                       treat1and2 = FALSE,
                       weights = TRUE,
                       contested_seats = TRUE,
                       sims = NULL){
  
  require(doParallel)
  n_cores <- detectCores()
  ifelse(n_cores > 4, corei <- makeCluster(6), corei <- makeCluster(as.integer(n_cores/2)))
  registerDoParallel(corei)
  
  # from <- which(names(dat)== "1")
  # to <- which(names(dat)== as.character(sims))
  # cov <- cov
  
  dat$row_n <- 1:nrow(dat)
  perms <- dat[,c(paste0("iter", 1:sims), "row_n")]
  dat <- dat[,!names(dat) %in% grep("^iter", names(madat_iter), value = TRUE)] #this makes it faster for estimates function
  
  df <- estimates(dat = dat, exclude_councilors = exclude_councilors,
                  cov = cov, weights = weights, with_N = with_N,
                  good = good, Q_good = Q_good, depvar = depvar,
                  country = country, interactvar = interactvar,
                  treat1and2 = treat1and2, contested_seats = contested_seats,
                  return_df = TRUE, skip_prep = FALSE)
  
  b <- estimates(dat = df, exclude_councilors = exclude_councilors,
                 cov = cov, weights = weights, with_N = with_N,
                 good = good, Q_good = Q_good, depvar = depvar,
                 country = country, interactvar = interactvar,
                 treat1and2 = treat1and2, contested_seats = contested_seats,
                 return_df = FALSE, skip_prep = TRUE)
  
  perms <- perms[perms$row_n %in% df$row_n,]
  
  b_ri <- NA
  
  #b_dist <- sapply(1:sims, function(j) {
  #print(j)
  
  b_ri <- foreach(i = 1:sims, .combine = 'cbind', .packages = c('lfe','dplyr'), .export = 'estimates') %dopar% {
    
    df$treat <- perms[[i]]
    
    #reassign inverse propensity weights for brazil based on sector treatment proportion
    #maybe unnecessary
    df <- df %>%
      group_by(fe) %>%
      mutate(n = n(), treat_prop = sum(treat==1)/n) %>%
      ungroup()
    
    if("brz" %in% unique(df$ctry)){
      
      df$inv_wts[df$ctry == "brz"] <- with(df[df$ctry == "brz",], ifelse(treat == 1, 1/treat_prop, 1/(1-treat_prop)))
      
      sum_ipw <- sum(df$inv_wts[df$ctry == "brz"], na.rm = T)
      sum_metawt_brz <- sum(df$wt[df$ctry=="brz"], na.rm = T)
      scale_wt <- sum_metawt_brz/sum_ipw
      df$wt[df$ctry=="brz"] <- df$inv_wts[df$ctry=="brz"] * scale_wt
    }
    
    # reassign inverse propensity weights for ug2 based on location sample size
    # i.e., villages with fewer than 15 subjects had assignment probability of .5, and otherwise paired with assignment prob .2 and .8
    # we infer the propensity of treatment based on share of treated within a village,
    # which due to sample size are between .75 and .84 for .8 treatment density and between .157 and .238 for .2 density.
    
    if("ug2" %in% unique(df$ctry)){
      
      df$inv_wts[df$ctry == "ug2"] <- with(df[df$ctry == "ug2",], ifelse(n<15, 1/.5,ifelse(treat_prop > 2/3,
                                                                                           treat*1/.8 + (1-treat)*1/.2,
                                                                                           treat*1/.2 + (1-treat)*1/.8)))
      
      sum_ipw2 <- sum(df$inv_wts[df$ctry=="ug2"], na.rm = T)
      sum_metawt_ug2 <- sum(df$wt[df$ctry=="ug2"], na.rm = T)
      scale_wt2 <- sum_metawt_ug2/sum_ipw2
      df$wt[df$ctry=="ug2"] <- df$inv_wts[df$ctry=="ug2"] * scale_wt2
    }
    
    # brazil weights if country-specific estimation
    if(!is.null(country)){
      if(country == "brz"){
        df$wt <- df$inv_wts
      }}
    
    # uganda 2 weights if country-specific estimation
    if(!is.null(country)){
      if(country == "ug2"){
        df$wt <- df$inv_wts2
      }}
    
    estimates(dat = df, country = country, depvar = depvar, cov = cov, with_N = with_N, good = good, exclude_councilors = exclude_councilors, interactvar = interactvar, weights = weights, skip_prep = TRUE)$coef[1]
  }
  
  return(list(est = b,
              p = mean(abs(b_ri) >= abs(b$coefficients[1])),
              rite = b_ri))
  
  on.exit(stopCluster(corei))
  invisible(gc())
}

# calculate p-value of joint distribution (good/bad news) -----------------

joint_ps <- function(rite_obj1, rite_obj2, iter = n_iter){
  
  ate1 <- rite_obj1$estimates$beta[1]
  ate2 <- rite_obj2$estimates$beta[1]
  
  if(length(rite_obj1$rite)>1){
    b1s <- rite_obj1$rite[1:n_iter]
    b2s <- rite_obj2$rite[1:n_iter]
    
    b_maxs <- abs(b1s) *(abs(b1s) > abs(b2s)) + abs(b2s) *(abs(b1s) <= abs(b2s))
    b_mins <- abs(b1s) *(abs(b1s) < abs(b2s)) + abs(b2s) *(abs(b1s) >= abs(b2s))
    
    p_function_1 <- function(ate1, ate2){
      b_max <- max(abs(c(ate1, ate2)))
      b_min <- min(abs(c(ate1, ate2)))
      mean((b_mins > b_min) & (b_maxs > b_max))
    }
    
    return(p_function_1(ate1,ate2))
  }else{
    return(NA)
  }
}


# plot treatment assignment probability -----------------------------------

tprob <- function(df, good) {
  
  clist <- unique(df$ctry)
  # Density plots
  for (i in 1:length(clist)) {
    dat <- subset(df, ctry == clist[i])
    
    from <- which(names(dat) == "1")
    to <- which(names(dat) == "10000")
    
    tpr <- rowMeans(dat[, from:to])
    
    if (good == 1) {
      title <- c("Treatment assignment probability, good news",
                 as.character(clist[i]))
    }
    else {
      title <- c("Treatment assignment probability, bad news",
                 as.character(clist[i]))
    }
    plot(density(tpr), main = title)
    abline(v = .5, lty = 2)
    abline(v = mean(tpr), col = "blue")
  }
}


# output .tex table lines -------------------------------------------------

format_output <- function(..., notes = ""){
  t <- capture.output(stargazer(..., df = FALSE, column.sep.width = "1pt",
                                omit = c("Constant", "m"),
                                omit.stat = c("adj.rsq", "ser"),
                                header = F, table.placement = "htb",
                                omit.table.layout = "n"))
  notes <- paste("\\begin{flushleft}\\textit{Note:}", notes, "$^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$. \\end{flushleft}")
  c(t[1:(length(t)-1)], notes, t[length(t)])
}

# BALANCE TABLE FUNCTIONS
reweight <- function(dat, meta){
  if(meta){
    dat <- dat %>%
      dplyr::mutate(ctry_n = length(unique(ctry))) %>%
      group_by(ctry) %>%
      dplyr::mutate(ctry_sum_wt = sum(inv_wt)) %>%
      dplyr::mutate(wt = inv_wt/(ctry_n * ctry_sum_wt)) %>%
      dplyr::select(-ctry_n) %>% # create weight
      ungroup()
  }else{
    dat %<>%
      dplyr::mutate(sum_wt = sum(inv_wt)) %>%
      dplyr::mutate(wt = inv_wt/sum_wt) # create weight
  }
  return(dat)
}
get_mean_sd <- function(df, x, meta){
  
  # df <- dat %>% subset(ug2_councilor_dummy == 0 | is.na(ug2_councilor_dummy))
  
  df$y <- x
  df <- df[!is.na(df$y),]
  
  # df <- df[!is.na(df$y),]
  #include IPW and country-wise weight
  df0 <- subset(df, treat == 0)
  df1 <- subset(df, treat == 1)
  
  df0 <- reweight(df0, meta = meta)
  df1 <- reweight(df1, meta = meta)
  
  #need to average across blocks first
  mean_T0 <- SDMTools::wt.mean(df0$y, df0$wt)
  sd_T0 <- SDMTools::wt.sd(df0$y, df0$wt)
  
  # mean_T1 <- mod$coefficients[1,]
  # se_T1 <- mod$se
  mean_T1 <- SDMTools::wt.mean(df1$y, df1$wt)
  sd_T1 <- SDMTools::wt.sd(df1$y, df1$wt)
  
  d_stat <- (mean_T1-mean_T0)/sd_T0
  
  df_m1 <- reweight(df[!is.na(df$m1) & df$treat == 0,], meta = meta)
  df_m3 <- reweight(df[!is.na(df$m3) & df$treat == 0,], meta = meta)
  
  m1 <- tryCatch(felm(m1 ~ y|fe|0|cl, data = df_m1, weights = df_m1$wt), error = function(e) NA)
  m3 <- tryCatch(felm(m3 ~ y|fe|0|cl, data = df_m3, weights = df_m3$wt), error = function(e) NA)
  
  if(length(m1) == 1 && is.na(m1)){
    beta1 <- beta1_se <- beta1_p <- NA
    
  }else{
    beta1 <- m1$coefficients[1,1]; beta1_se <- m1$se; beta1_p <- m1$pval
  }
  
  if(length(m3) == 1 && is.na(m3)){
    beta2 <- beta2_se <- beta2_p <- NA
  }else{
    beta2 <- m3$coefficients[1,1]; beta2_se <- m3$se; beta2_p <- m3$pval
  }
  
  return(data.frame(mean_T0 = mean_T0, 
                    sd_T0 = sd_T0, 
                    mean_T1 = mean_T1, 
                    sd_T1 = sd_T1,
                    d_stat = d_stat,
                    beta1 = beta1,
                    beta1_se = beta1_se,
                    beta1_p = beta1_p,
                    beta2 = beta2,
                    beta2_se = beta2_se,
                    beta2_p = beta2_p,
                    N = nrow(df0) + nrow(df1)))
}
make_balance_tab <- function(df, country = NULL){
  meta <- TRUE
  
  if(!is.null(country)){
    df <- df[df$ctry == country,]
    meta <- FALSE
  }
  
  balance <- df %>%
    subset(ug2_councilor_dummy == 0 | is.na(ug2_councilor_dummy)) %>%
    select(ctry, fe, cl, treat, inv_wt, m1, m3,
           P, N_good, m13, m14, m15, m17, m18,
           m19, m20, m21, m22, m23, m24)
  # not including m16 (drops sample from 13,312 to  1076)
  
  balance_tab <- do.call(rbind, lapply(balance[, 8:20], function(x) if(!all(is.na(x))) get_mean_sd(balance, x, meta = meta)))# %>% round(., 2)
  
  
  complete <- balance[,names(balance) %in% rownames(balance_tab)] %>% complete.cases()
  balance_complete <- balance[complete,] %>% reweight(., meta = meta)
  
  form <- paste0("treat ~ ",
                 paste(rownames(balance_tab), collapse = " + "),
                 "|fe|0|cl") %>% as.formula()
  
  mod <- felm(form, data = balance_complete, weights = balance_complete$wt)
  # if there is perfect multicollinearity, redefine the model and print perfectly collinear vars
  
  cor_mat <- balance_complete[,rownames(balance_tab)] %>% as.matrix %>% cor(.) %>% round(., 2)
  
  collinear <- NA
  alias <- NA
  if(any(is.na(mod$coefficients))){
    collinear <- rownames(balance_tab)[is.na(mod$coefficients)]
    collinear[collinear == "N_good"] <- "Good news"
    
    form <- paste0("treat ~ ",
                   paste(rownames(balance_tab)[!is.na(mod$coefficients)], collapse = " + "),
                   "|fe|0|cl") %>% as.formula()
    mod <- felm(form, data = balance_complete, weights = balance_complete$wt)
  }
  
  f <- linearHypothesis(mod, paste0(rownames(mod$coefficients), " = 0"))
  Pr_F <- f$`Pr(>Chisq)`[2]
  
  if(!is.na(collinear)){
    form <- paste0("treat ~ ",
                   paste(rownames(balance_tab), collapse = " + "),
                   "+ as.factor(fe)") %>% as.formula()
    mod <- lm(form, data = balance_complete, weights = balance_complete$wt)
    alias <- alias(mod)
  }
  
  return(list(balance_tab = balance_tab, p = Pr_F, collinear = collinear, alias = alias))
}
format_balance <- function(balance_output){
  
  for(col in c("beta1", "beta2")){
    if(!is.na(balance_output$balance_tab[,col])){
      x <- round(balance_output$balance_tab[,col], 2)
      star <- case_when(x >= 0.05 ~ "",
                        x < 0.05 & x >= 0.01 ~ "*",
                        x < 0.01 & x >= 0.001 ~ "**",
                        x < 0.001 ~ "***")
      balance_output$balance_tab[,col] <- paste0(x, star)
    }else{
      balance_output$balance_tab[,col] <- ""
    }
    
  }
  balance_output$balance_tab <- balance_output$balance_tab %>% select(-ends_with("_p"))
  balance_output$balance_tab[,-c(6,8)] <- balance_output$balance_tab[,-c(6,8)] %>% round(., 2)
  relabel <- rownames(balance_output$balance_tab)
  relabel <- case_when(relabel == "P" ~ "Prior",
                       relabel == "N_good" ~ "Good news",
                       relabel == "m13" ~ "Gender",
                       relabel == "m14" ~ "Age", 
                       relabel == "m15" ~ "Co-ethnicity", 
                       relabel == "m17" ~ "Education", 
                       relabel == "m18" ~ "Wealth", 
                       relabel == "m19" ~ "Co-Partisanship", 
                       relabel == "m20" ~ "Voted in past election", 
                       relabel == "m21" ~ "Voted incumbent past election", 
                       relabel == "m22" ~ "Clientelism", 
                       relabel == "m23" ~ "Salience of information", 
                       relabel == "m24" ~ "Credibility of information")
  rownames(balance_output$balance_tab) <- relabel
  
  fill <- rep("", nrow(balance_output$balance_tab))
  balance_output$balance_tab <- cbind(rownames(balance_output$balance_tab), fill,
                                      balance_output$balance_tab[,1:5], fill,
                                      balance_output$balance_tab[,6:10], fill)
  cols <- c(1,3,5,7,9,11,13)
  r <- nrow(balance_output$balance_tab)
  out <- matrix(NA, 2*r, length(cols))
  rownames(balance_output$balance_tab) <- NULL
  
  for(i in seq_along(cols)){
    if(cols[i] %in% c(3,5,9,11)){
      balance_output$balance_tab[,c(cols[i]+1)] <- paste0("(", balance_output$balance_tab[,c(cols[i]+1)], ")")
      balance_output$balance_tab[,c(cols[i]+1)] <- gsub("(NA)", "", balance_output$balance_tab[,c(cols[i]+1)], fixed = TRUE)
    }
    out[2*(1:r) - 1, i] <- paste0(balance_output$balance_tab[,cols[i]])
    out[2*(1:r), i] <- paste0(balance_output$balance_tab[,c(cols[i]+1)])
  }
  
  tex <- capture.output(print(xtable(out), include.rownames = FALSE, table.placement = "h!"))
  tex[5] <- "Baseline covariate & Control mean  & Treat mean & d-stat & $\\hat{\\beta_1}$ & $\\hat{\\beta_2}$ & N \\\\ "
  n <- length(tex)
  tex[n-2] <- paste0("\\hline Pr($\\chi^2$) &", round(balance_output$p, 2), "&&&&& \\\\ \\hline\\hline") 
  tex[n+1] <- tex[n]
  ifelse(is.na(balance_output$collinear),
         tex[n] <- "\\begin{flushleft}\\textit{Note:} Results show the control and treatment means for each of the pre-treatment covariates. Means and standard deviations are weighted by block share of non-missing observations. $d$-stat is calculated as the difference between treatment and control means normalized by one standard deviation of the control mean. $\\hat{\\beta_1}$ ($\\hat{\\beta_2}$) is the coefficient in a regression of vote choice (turnout) on each covariate separately, in the control sample. As with main specification, we include randomization block fixed effects and standard errors clustered at the level of treatment assignment. We also show the probability of rejecting the null that none of the covariates is predictive of treatment. All regressions include block fixed effects, standard errors clustered at the level of assingment and inverse propensity weights, and all countries are weighted equally. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$. \\end{flushleft}",
         tex[n] <- paste0("\\begin{flushleft}\\textit{Note:} Results show the control and treatment means for each of the pre-treatment covariates. Means and standard deviations are weighted by block share of non-missing observations. $d$-stat is calculated as the difference between treatment and control means normalized by one standard deviation of the control mean. $\\hat{\\beta_1}$ ($\\hat{\\beta_2}$) is the coefficient in a regression of vote choice (turnout) on each covariate separately, in the control sample. As with main specification, we include randomization block fixed effects and standard errors clustered at the level of treatment assignment. We also show the probability of rejecting the null that none of the covariates is predictive of treatment. All regressions include block fixed effects, standard errors clustered at the level of assingment and inverse propensity weights, and all countries are weighted equally. Variable(s) ", paste(balance_output$collinear, collapse = ","), " excluded from the F-stat regression because of perfect collinearity with one of more covariates. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$. \\end{flushleft}"))
  return(tex)
}
