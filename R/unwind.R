unwind <- function(model1, model2, use_model2_panel = "ALL", print_summary = TRUE, expandPeriodToLongest = FALSE, demo = FALSE) {
  Base <- "ModelKPI"
  # REMOVED: Base This parameter controls how the variable is unnested,  only change this if you wish the base from the model2 to be unnested as BASE and not as the variable
  options("scipen" = 100, "digits" = 4)
  # This code assumes that the sub_model has equal number of groups/pools
  # as the main model
  # OR the sub-model is not a panel model and thus has the column
  # Groups with only "TOTALS"
  # OR use_model2_panel= has specified the exact panel from sub-model
  # to use for the unwinding
  agg_period_ <- base::names(model1$contributionsTbl)[2]
  base::names(model1$contributionsTbl)[2] <- "agg_period"


  main_m_ini <- model1$contributionsTbl
  main_m_groups <- base::unique(main_m_ini[[1]])


  if (use_model2_panel != "ALL") {
    model2$contributionsTblPerc %>%
      dplyr::filter(Group == use_model2_panel) -> sub_m_ini
    sub_m_groups <- use_model2_panel
  } else {
    sub_m_ini <- model2$contributionsTblPerc
    sub_m_groups <- base::unique(sub_m_ini[[1]])
  }

  # Figure out if the model was pooled across groups and if so,
  # seperate dataset based on the groups
  # add them seperated to a list
  main_m_list <- base::list()
  for (p in 1:base::length(main_m_groups)) {
    main_m_ini[main_m_ini$Group == main_m_groups[p], -1] %>%
      base::list() -> main_m_list[p]
  }

  sub_m_list <- base::list()
  for (p in 1:base::length(sub_m_groups)) {
    sub_m_ini[sub_m_ini$Group == sub_m_groups[p], -1] %>%
      list() -> sub_m_list[p]
  }

  # loop for every panel in the main_model`
  for (q in 1:base::length(main_m_groups)) {
    if (base::length(sub_m_groups) == base::length(main_m_groups)) v <- q else v <- 1

    main_m <- main_m_list[[q]]

    if (q == v) {
      sub_m <- sub_m_list[[v]]

      # Check for missing time periods in 2nd model, then fill in missing
      new_sub_m <- base::data.frame(base::matrix(NA, nrow = base::nrow(main_m), ncol = base::ncol(sub_m))) # create DF with row dimensions from main model and column dimensions from sub model
      names(new_sub_m) <- names(sub_m) # apply column headings from sub model
      new_sub_m[, 1:2] <- main_m[, 1:2] # add Date and KPI column without changes from main model
      mrow <- main_m$Date %in% sub_m$Date # find periods in main model that are present in sub model
      srow <- sub_m$Date %in% main_m$Date # find periods in sub model that are present in main model
      new_sub_m[mrow, -(1:2)] <- sub_m[srow, -(1:2)] # apply periods that both models have in common to new sub model df
      incomp_nsubm <- !stats::complete.cases(new_sub_m) # find rows that are not complete in new sub model df
      comp_nsubm <- stats::complete.cases(new_sub_m) # find rows that are complete in new sub model df
      if (expandPeriodToLongest) {
        # Replace time periods that were not present in the 2nd model, but present in the first.
        # These time periods will be replaced with the closest present time series (naive forecasting)
        for (i in (1:base::length(incomp_nsubm))[incomp_nsubm]) { # loop for the rows in new sub model df that are incomplete
          k <- base::which(base::abs(new_sub_m$Date[comp_nsubm] - new_sub_m$Date[i]) == base::min(base::abs(new_sub_m$Date[comp_nsubm] - new_sub_m$Date[i]))) # find which Date is the closest to each missing Date in loop
          new_sub_m[i, -(1:2)] <- new_sub_m[comp_nsubm, ][k, -(1:2)] # substitute the missing values in each incomplete row with the values from the closest present Date
        }
      } else {
        new_sub_m <- new_sub_m[complete.cases(new_sub_m), ]
        main_m <- main_m[main_m$Date %in% new_sub_m$Date, ]
      }
      sub_m <- new_sub_m # rename new sub model to sub model
      sub_var_name <- base::names(sub_m[3]) # find the variable names in order from sub model
    } ## end if(q==v)


    # Make sure we only use dates that are in both models
    new_sub_m <- new_sub_m[new_sub_m$Date %in% main_m$Date, ]
    main_m <- main_m[main_m$Date %in% new_sub_m$Date, ]

    new_m1 <- main_m[, 1:3] # create df for unwinding
    new_m2 <- main_m[, !(base::names(main_m) %in% sub_var_name)] # set apart the main model variables/categories that do not have counterparts in the sub model, these variables will not be affected
    main_var_for_sub <- main_m[, sub_var_name] # extract categories/variables from main model that are present in sub model
    for (m in base::names(sub_m)[-(1:3)]) {
      if (m != "BASE") new_m1[, m] <- main_var_for_sub * sub_m[, m]
    } # loop for distributing contributions for kpi in sub model in main model

    new_m1[, sub_var_name] <- main_var_for_sub * sub_m[, "BASE"] ##### distribute KPI in submodel based on base values


    aggDateMelt <- function(x) reshape2::melt(x, id.vars = c("agg_period", "Date"))
    base::suppressWarnings(contribution <- dplyr::bind_rows(aggDateMelt(new_m1), aggDateMelt(new_m2[, -3])))
    if (Base != "ModelKPI") contribution$variable[contribution$variable == sub_var_name] <- "BASE"
    contribution <- reshape2::dcast(contribution, agg_period + Date ~ variable, fun.aggregate = sum, value.var = "value")

    # contribution2<-contribution
    # contribution2[,4:ncol(contribution)]<-contribution[,(4):base::ncol(contribution)]/contribution[,3]

    contribution <- aggDateMelt(contribution)
    # contribution2<-aggDateMelt(contribution2)

    base::names(contribution) <- base::c("agg_period", "Date", "Categories", "value")
    # base::names(contribution2)<-base::c("agg_period","Date","Categories","value")


    # Create summary by agg_period
    # sum_by_FY<-function(x){x[,base::c("agg_period","Categories","value")]%>%
    #     reshape2::dcast(agg_period~Categories,fun.aggregate=sum,value.var="value")}
    if (q == v & v == 1) {
      # Add specification on which panel was used from submodel to variableTbl
      if (use_model2_panel != "ALL") {
        model2$variableTbl$Model %>%
          base::paste0("(", use_model2_panel, ")", sep = "") -> model2$variableTbl$Model
      }
      var_cat_tbl <- base::unique(dplyr::bind_rows(model1$variableTbl, model2$variableTbl))

      var_cat_tbl <- var_cat_tbl %>% filter(Categories != "FY")

      # Create order for presenting variables
      var_order <- base::unique(var_cat_tbl$Categories)
      var_order <- var_order[!stringr::str_detect(var_order, "BASE")]
      var_order <- base::c("agg_period", var_order[1:2], "BASE", var_order[-(1:2)])
      if (Base != "ModelKPI") var_order <- var_order[var_order != sub_var_name]
    }
    # print(var_order)
    # summary<-sum_by_FY(contribution)[,var_order[-2]]
    summary <- contribution %>%
      reshape2::dcast(agg_period ~ Categories, fun.aggregate = sum, value.var = "value") %>%
      .[, var_order[-c(2, 3)]]
    # View(summary)
    # summary2<-summary
    # summary2[,-(1:2)]<-summary[,-(1:2)]/summary[[2]]



    # Cast and reorder columns
    rename_and_agg2 <- function(x) {
      reshape2::dcast(x, Date + agg_period ~ Categories, fun.aggregate = sum, value.var = "value") %>%
        .[, var_order]
    }


    contribution <- rename_and_agg2(contribution)
    # contribution2<-rename_and_agg2(contribution2)

    if (q == 1) {
      contribution_return <- base::cbind("Group" = main_m_groups[q], contribution)
      # contribution2_return<-base::cbind("Group"=main_m_groups[q],contribution2)
      summary_return <- base::cbind("Group" = main_m_groups[q], summary)
      # summary2_return<-base::cbind("Group"=main_m_groups[q],summary2)
    } else {
      contribution <- base::cbind("Group" = main_m_groups[q], contribution)
      # contribution2<-base::cbind("Group"=main_m_groups[q],contribution2)
      summary <- base::cbind("Group" = main_m_groups[q], summary)
      # summary2<-base::cbind("Group"=main_m_groups[q],summary2)

      contribution_return <- base::rbind(contribution_return, contribution)
      # contribution2_return<-base::rbind(contribution2_return,contribution2)
      summary_return <- base::rbind(summary_return, summary)
      # summary2_return<-base::rbind(summary2_return,summary2)
    }
  } # this ends the loop for every panel in the group


  if (print_summary == TRUE) {
    base::print(var_cat_tbl)
    # base::names(summary2_return)[2] <- agg_period_
    # base::print(summary2_return)
  }


  if (base::length(main_m_groups) != 1) {
    tmp <- summary_return[, -1] %>%
      dplyr::group_by(agg_period) %>%
      dplyr::summarise_each(funs(sum)) %>%
      base::cbind("Group" = "TOTAL", .)
    if (print_summary == TRUE) base::print(tmp)
    tmp[, -(1:3)] <- tmp[, -(1:3)] / tmp[[3]]
    if (print_summary == TRUE) {
      base::names(tmp)[2] <- agg_period_
      base::print(tmp)
    }
  }
  # CREATE INCREMENTAL CONTRIBUTIONS DF
  modname2 <- base::names(model2$contributionsTbl)[4]
  modname1 <- base::names(model1$contributionsTbl)[4]

  if (length(model1) != 6 & demo == FALSE) {
    i_c <- incremental_contributions(model1, contribution_return, modname2)
    i_c$Date <- base::as.character(i_c$Date)
    tmp.ic <- model1$incrementalContributionsTbl
    tmp.ic$Date <- base::as.character(tmp.ic$Date)
    i_c <- dplyr::bind_rows(tmp.ic, i_c)
    i_c[base::is.na(i_c)] <- 0

    i_c$Date <- base::as.Date(i_c$Date)
  } else {
    i_c1 <- incremental_contributions(NULL, model1, modname1)

    i_c2 <- incremental_contributions(model1, contribution_return, modname2)
    i_c1$Date <- base::as.character(i_c1$Date)
    i_c2$Date <- base::as.character(i_c2$Date)
    # suppressWarnings(i_c<-smartbind(i_c1,i_c2,fill=0))
    i_c <- dplyr::bind_rows(i_c1, i_c2)
    i_c[base::is.na(i_c)] <- 0
    i_c$Date <- base::as.Date(i_c$Date)
  }

  # CREATE SANKEY PLOT DATA
  i_c %>%
    dplyr::filter(agg_period == base::max(agg_period)) %>%
    reshape2::melt(id.vars = base::c("Group", "agg_period", "Date", "Model"), variable.name = "Variable", value.name = "Values") %>%
    dplyr::select(agg_period, Model, Variable, Values) %>%
    dplyr::group_by(agg_period, Model, Variable) %>% # print(sapply(class))
    # View()
    dplyr::mutate(Values = base::as.numeric(Values)) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::filter(Values != 0, Variable != Model) %>%
    .[, -1] -> sank

  # tailsPrep <- unwindWithTails(model1,model2,use_model2_panel,expandPeriodToLongest)

  if (!(base::min(model1$contributionsTbl$Date) == base::min(model2$contributionsTbl$Date)) && !(base::min(model2$contributionsTbl$Date) == base::min(contribution_return$Date))) {
    base::message("MODEL PERIODS DIFFER")
    base::message("______________________")
    base::message("Start periods:")
    base::message("Model1:       ", base::min(model1$contributionsTbl$Date))
    base::message("Model2:       ", base::min(model2$contributionsTblPerc$Date))
    base::message("This decomp:  ", base::min(contribution_return$Date))
    base::message("______________________")
    base::message("End periods:")
    base::message("Model1:       ", base::max(model1$contributionsTbl$Date))
    base::message("Model2:       ", base::max(model2$contributionsTblPerc$Date))
    base::message("This decomp:  ", base::max(contribution_return$Date))
    base::message("______________________")

    if (!expandPeriodToLongest & (base::max(model2$contributionsTblPerc$Date) < base::max(model1$contributionsTbl$Date) | base::min(model2$contributionsTblPerc$Date) > base::min(model1$contributionsTbl$Date))) base::message("Set expandPeriodToLongest=TRUE to impute nearest values in submodel and extend the period to match the main model's.")
  }
  # return(list(contribution_return, contribution2_return, summary_return, summary2_return, var_cat_tbl,i_c,sank))
  base::names(contribution_return)[2] <- agg_period_
  # base::names(contribution2_return)[2] <- agg_period_
  base::names(summary_return)[2] <- agg_period_
  # base::names(summary2_return)[2] <- agg_period_
  base::names(i_c)[2] <- agg_period_

  if (use_model2_panel != "ALL") {
    model2pct <- as.data.frame(model2$varContPct)
    KPI2 <- model2pct[2, 2]
    model2pct <- model2pct[model2pct$Group == use_model2_panel, ]
  }
  model1ttl <- as.data.frame(model1$varCont)
  subtotalmodel1 <- data.frame(matrix(NA, nrow = 1, ncol = (ncol(model1ttl) - 4)))
  model1ttl <- model1ttl[model1ttl$Categories == KPI2, ]

  # for(col in 5:ncol(model1ttl)){
  #  colnames(subtotalmodel1)[col-4] <- colnames(model1ttl)[col]
  # subtotalmodel1[1,(col-4)] <- sum(model1ttl[col], na.rm = T)
  # }
  for (col in 5:ncol(model1ttl)) {
    colyer <- colnames(model1ttl)[col]
    picolyear <- sum(model1ttl[col], na.rm = T)
    model2pct[[colyer]] <- model2pct[[colyer]] * picolyear
  }



  var_tblPG <- rbind(as.data.frame(model1$varCont), model2pct[complete.cases(model2pct), ])
  # var_tblPG %>%
  #   gather("Date", "Value", 5:ncol(.)) %>%
  #   mutate(Date = ymd(Date)) %>%
  #   dplyr::mutate(FiscalYear = FY(Date, FYBegin = 4)) %>%
  #   group_by(variable, Categories, Model, Group, FiscalYear) %>%
  #   summarise(Value = sum(Value, na.rm = T)) %>%
  #   spread(FiscalYear, Value) -> var_tblPG


  return(list(
    contributionsTbl = contribution_return,
    # contributionsTblPerc=contribution2_return,
    summary = summary_return,
    # summaryPerc=summary2_return,
    variableTbl = var_cat_tbl,
    incrementalContributionsTbl = i_c,
    varCont = var_tblPG,
    seankeyReadyTbl = sank
  )) # Return contribution DF from function
}
