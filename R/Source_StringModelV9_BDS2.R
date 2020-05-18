

# Source File
StringGen <- function(model_string, media_data, nonmedia_data, Panel, start_date, end_date) {
  # # # For Meiying Only
  # fmi_data -> media_data
  # nonfmi_data -> nonmedia_data
  # stringformat -> model_string
  # Panel <- 1
  # fmi_dataP -> media_data
  # nonfmi_dataP -> nonmedia_data
  # stringformatPanel_Div -> model_string
  # Panel <- 26
  # start_date <- "2013-04-01"
  # end_date <- "2017-03-01"

  count_date <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "month")
  names(media_data)[1] <- "brand"
  names(nonmedia_data)[1] <- "brand"

  # Determine if Model is Single or Panel
  ifelse(Panel > 1, TYPE <- "Panel", TYPE <- "no")

  # Split string
  kpi.name <- str_split_fixed(string = model_string, pattern = " ~ ", n = 2)[,1]
  var.names <- str_split_fixed(string = model_string, pattern = " ~ ", n = 2)[,2] # grab RHS vars
  var.names <- str_split(var.names, pattern = " \\+ ", n = Inf) # split out vars
  var.names <- unlist(c(kpi.name, var.names))
  var.namesM <- unlist(var.names)
  var.names <- unlist(str_split(unlist(var.names), pattern = "\\<\\*>", n = Inf))
  var.names <- unlist(str_split(unlist(var.names), pattern = "\\<A>", n = Inf))
  var.names <- str_split(var.names, c("\\<D>"), n = Inf)
  # var.names <- str_split(var.names, c("\\<S>"), n = Inf)
  var.names <- unlist(var.names)
  var.names <- str_replace_all(var.names, c("\\{" = "", "\\}_Log" = "", "\\}" = ""))
  var.names <- unlist(var.names)
  var.names <- sort(var.names)

  # REGEX
  param1 <- "_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])" # formatting for adresponse
  param2 <- "_([D][0-9][0-9][P][0-9])" # formatting for adstock
  param3 <- "Log" # formatting for log
  param4 <- "Lag" # formatting for lag
  param5 <- "Movavg" # formatting for moving average
  param6 <- "DUM" # formatting for dummies
  param7 <- "\\<\\*>" # formatting for multiplying two vars
  param8 <- "<D>" # formatting for dividing two vars
  param9 <- "\\{"
  param10 <- "MEANSBY"
  param11 <- "RANGE"
  param12 <- "<A>"
  param13 <- "Panel"
  param14 <- "<S>"

  # All Variable Types
  transvars <- str_subset(var.names, param1) %>% unique(.) # grab only vars to adresponse
  stockvars <- str_subset(var.names, param2) %>% unique(.) # grab only vars to adstock
  othervars <- var.names[!grepl(param1, var.names) & !grepl(param2, var.names)]

  str_replace_all(transvars, param1, "") -> ADparams # names of variables without adresponse parameters
  str_replace_all(stockvars, param2, "") %>%
    str_replace_all(., c("_Log" = "")) -> ASparams # names of variables without adstock parameters
  str_replace_all(othervars, c("_Log" = "", "_Lag[0-9]" = "", "_Movavg[0-9]" = "", "_MEANSBY" = "")) %>%
    unique(.) -> Oparams # names of variables without log
  str_replace_all(kpi.name, c("_Log" = "", "<D>" = " ", "<S>" = " ", "\\{" = "", "\\}" = "", "_MEANSBY" = "")) %>%
    str_split(" ", n = 2) %>% unlist(.) %>% unique(.) -> Kparams

  ADData <- subset(media_data, select = c(ADparams))
  #ADData <- as.data.frame(media_data[, colnames(media_data) %in% ADparams]) # grab columns to adresponse
  names(ADData) <- ADparams
  ASData <- subset(media_data, select = c(ASparams))
  #ASData <- as.data.frame(media_data[, colnames(media_data) %in% ASparams]) # grab columns to adresponse
  names(ASData) <- ASparams
  OData <- nonmedia_data[, colnames(nonmedia_data) %in% Oparams]
  OData <- as.data.frame(OData)
  ifelse(length(OData) == 1, names(OData) <- Kparams, OData)
  KData <- nonmedia_data[, colnames(nonmedia_data) %in% Kparams]
  KData <- as.data.frame(KData)
  names(KData) <- Kparams
  OData <- cbind(OData, KData)
  OData <- as.data.frame(OData[, !duplicated(colnames(OData), fromLast = TRUE)])
  ifelse(test = length(OData) == 1, names(OData) <- Kparams, OData <- OData)

  # Ad Response Section -----
  if (ncol(ADData) != 0) {
    ADDataT <- list()
    for (i in 1:ncol(ADData)) {
      createAdR(data = ADData[i], data_piv = media_data, var_name = transvars[i], type = TYPE) -> ADDataT[i]
    }
    as.data.frame(ADDataT) -> ADDataT
    names(ADDataT) <- transvars
    if (Panel == 1) {
      ADDataT %>%
        cbind(media_data$week) %>%
        mutate(DMA = 100) %>%
        select(`media_data$week`, DMA, c(1:ncol(ADData))) %>%
        rename(Date = `media_data$week`) -> ADDataT
    } else {
      ADDataT %>%
        cbind(media_data$week) %>%
        mutate(DMA = media_data$brand) %>%
        select(`media_data$week`, DMA, c(1:ncol(ADData))) %>%
        rename(Date = `media_data$week`) -> ADDataT
    }
    DateConvertMean(ADDataT) -> ADDataT
    filter(ADDataT, Month >= start_date & Month <= end_date) -> ADDataT
  } else {
    matrix(data = NA, nrow = (length(count_date) * Panel), ncol = 1) -> ADDataT
  }

  # Ad Stock Section ----
  if (ncol(ASData) != 0) {
    ASDataT <- list()
    for (i in 1:ncol(ASData)) {
      createAdStock(data = ASData[i], data_piv = media_data, var_name = stockvars[i], type = TYPE) -> ASDataT[i]
    }
    as.data.frame(ASDataT) -> ASDataT
    names(ASDataT) <- stockvars
    if (Panel == 1){
      ASDataT %>%
        cbind(media_data$week) %>%
        mutate(DMA = 100) %>%
        select(`media_data$week`, DMA, c(1:ncol(ASData))) %>%
        rename(Date = `media_data$week`) -> ASDataT
    } else {
      ASDataT %>%
        cbind(media_data$week) %>%
        mutate(DMA = media_data$brand) %>%
        select(`media_data$week`, DMA, c(1:ncol(ASData))) %>%
        rename(Date = `media_data$week`) -> ASDataT
    }
    ASDataT[is.na(ASDataT)] <- 0
    DateConvertSum(ASDataT) -> ASDataT
    filter(ASDataT, Month >= start_date & Month <= end_date) -> ASDataT
  } else {
    matrix(data = NA, nrow = (length(count_date) * Panel), ncol = 1) -> ASDataT
  }

  if (sum(str_detect(stockvars, "Log")) > 0) {
    select(ungroup(ASDataT), grep("Log", names(ASDataT))) -> ASDataTLog
    log(ASDataTLog + 1) -> ASDataTLog
    cbind(as.data.frame(ASDataT), ASDataTLog) -> ASDataT
    ASDataT <- ASDataT[, !duplicated(colnames(ASDataT), fromLast = TRUE)]
  } else {
    ASDataT -> ASDataT
  }

  # None Adstock and AdResponse Data ---
  if (length(str_subset(c(othervars, kpi.name), param3)) == 0 ) {
    OData -> OData
  } else {
    as.data.frame(OData) -> OData
    c(othervars, kpi.name)  %>%
      str_subset(param3) %>%
      str_replace_all(., c("_Log" = "", "_Lag[0-9]" = "", "_Movavg[0-9]" = "")) -> logvars
    logvars[!grepl("\\{", logvars)] -> logvars
    if (length(logvars) == 0) {OData -> OData} else {
      select(OData, one_of(c(logvars))) -> logvariables
      log(logvariables+1) -> logvariables1
      unique(logvars) -> logvars

      names(logvariables1) <- paste(logvars, "_Log", sep = "")
      cbind(OData, logvariables, logvariables1) -> OData
      OData <- OData[, !duplicated(colnames(OData), fromLast = TRUE)]
      names(OData)
    }
  }

  if (ncol(OData) > 0) {
    if (Panel == 1) { OData %>% mutate(DMA = 100) -> OData} else {cbind(OData, select(nonmedia_data, brand) ) -> OData }
  } else {
    matrix(data = NA, nrow = length(KData), ncol = 1) -> OData }
  str_replace(names(OData), pattern = "brand", replacement = "DMA") -> names(OData)
  cbind(nonmedia_data[,c("month")], OData) %>%
    rename(Month = `nonmedia_data[, c("month")]`) %>%
    filter(Month >= start_date & Month <= end_date) -> OKData

  # Bind all three dataframe into one
  cbind(as.data.frame(OKData), as.data.frame(ADDataT), as.data.frame(ASDataT)) -> DF
  DF <- DF[, !duplicated(colnames(DF), fromLast = TRUE)] # Delete duplicates (DMA and Month columns)
  DF <- DF[, colSums(is.na(DF)) != nrow(DF)] # Delete NAs if not all three dataframes have data
  DF %>%
    select(DMA, Month, c(1:ncol(DF))) %>%
    rename(Brand = DMA) -> DF

  # Lag Data
  if (length(str_subset(var.names, param4)) == 0 ) {
    DF -> DF
  } else {
    var.names %>%
      str_subset(param4) -> lagvarsparams
    as.integer(str_split_fixed(lagvarsparams, pattern = "_Lag", n = 2)[,2]) -> lagparams
    lagvarsparams %>% str_replace_all(., c("_Lag[0-9]" = "")) -> lagvars
    select(DF, one_of(lagvars)) -> lagvariables
    bind_cols(select(DF, Month, Brand), lagvariables) -> lagvariables

    testlag <- list()
    for (i in 3:ncol(lagvariables)) {
      slide(lagvariables, Var = names(lagvariables[i]), GroupVar = names(lagvariables[2]),
            TimeVar = names(lagvariables[1]), slideBy = -(lagparams[i - 2])) -> testlag[[i]]
    }
    bind_cols(testlag) -> testlag
    testlag <- testlag[, !duplicated(colnames(testlag), fromLast = TRUE)]
    names(testlag) <- str_replace_all(names(testlag), pattern = "-", replacement = "_Lag")
    testlag <- cbind(select(testlag, Month, Brand), select(testlag, matches(c(".Lag."))))
    if (Panel > 1){
      testlag %>%
        gather("vars", "value", 3:ncol(testlag)) %>%
        dcast(formula = Month ~ Brand + vars, value.var = "value", fun.aggregate = sum) -> testlag
    } else {
      testlag
    }
    lagfill <- function(data) {
      first(data[!is.na(data)]) -> min1
      min1 -> data[is.na(data)]
      return(data)
    }
    testlagfill <- list()
    for (i in 1:ncol(testlag)) {
      lagfill(testlag[i]) -> testlagfill[i]
    }
    as.data.frame(testlagfill) -> testlagfill
    names(testlag) -> names(testlagfill)

    if (Panel > 1) {
      testlagfill %>%
        gather("vars", "value", 2:ncol(.)) %>%
        mutate(brand = str_split_fixed(vars, pattern = "_", n = 2)[,1]) %>%
        mutate(vars = str_split_fixed(vars, pattern = "_", n = 2)[,2]) %>%
        dcast(brand + Month ~ vars, value.var = "value", fun.aggregate = sum) -> testlagfill
    } else {testlagfill}

    cbind(DF, testlagfill) -> DF
    DF <- DF[, !duplicated(colnames(DF), fromLast = TRUE)]
  }


  # Moving Average
  if (length(str_subset(var.names, param5)) == 0 ) {
    DF -> DF
  } else {
    var.names %>%
      str_subset(param5) -> moveaveragename
    moveaveragename %>% str_replace_all(., c("_Movavg[0-9]" = "")) -> movvars
    select(DF, one_of(movvars)) -> movavgdf
    as.integer(str_split_fixed(moveaveragename, pattern = "_Movavg", n = 2)[,2]) -> movparams

    if ( Panel == 1 ) {
      # Moving Average
      movavgvariables <- list()
      for (i in 1:ncol(movavgdf)) {
        rollmean(movavgdf[i], k = movparams[i], fill = NA) -> movavgvariables[[i]]
      }
      names(movavgvariables) <- moveaveragename
      # Fill NAs with first and last values
      movavgfill <- list()
      for (i in 1:length(movavgvariables)) {
        firstfill(movavgvariables[[i]]) -> movavgfill[[i]]
        lastfill(movavgfill[[i]]) -> movavgfill[[i]]
      }
      as.data.frame(movavgfill) -> movavgfill
      names(movavgfill) <- moveaveragename
      cbind(DF, movavgfill) -> DF
    } else {
      cbind(select(DF, Month, Brand), movavgdf) -> movavgdf
      movavgdf %>%
        gather("vars", "value", 3:ncol(movavgdf)) %>%
        dcast(Month ~ Brand + vars, value.var = "value", fun.aggregate = sum) -> movavgdf
      rep(movparams, Panel) -> movparams
      select(movavgdf, -Month) -> movavgdf
      # Moving Average
      movavgvariables <- list()
      for (i in 1:ncol(movavgdf)) {
        rollmean(movavgdf[i], k = movparams[i], fill = NA) -> movavgvariables[[i]]
      }
      names(movavgvariables) <- names(movavgdf)
      # Fill NAs with first and last values
      movavgfill <- list()
      for (i in 1:length(movavgvariables)) {
        firstfill(movavgvariables[[i]]) -> movavgfill[[i]]
        lastfill(movavgfill[[i]]) -> movavgfill[[i]]
      }
      as.data.frame(movavgfill) -> movavgfill
      names(movavgfill) <- names(movavgdf)
      movavgfill %>%
        gather("vars", "value", 1:ncol(.)) %>%
        mutate(brand = str_split_fixed(vars, pattern = "_", n = 2)[,1]) %>%
        mutate(vars = str_split_fixed(vars, pattern = "_", n = 2)[,2]) %>%
        cbind(select(DF, Month)) %>%
        dcast(brand + Month ~ vars, value.var = "value", fun.aggregate = sum) -> movavgfill
      names(movavgfill)[3] <- moveaveragename
      cbind(DF, movavgfill) -> DF
    }
  }
  DF <- DF[, !duplicated(colnames(DF), fromLast = TRUE)]

  # Dummy Dates
  dummy_date <- function(dummies) {
    data <- DF[,c("Month")]
    ifelse(data == dummies, 1, 0) %>% as.data.frame(.) -> dum
    names(dum) <-
      paste("DUM_", dummies, sep = "")
    return(dum) }

  if (length(str_subset(var.names, param6)) == 0 ) {
    DF -> DF
  } else {
    var.names %>%
      str_subset(param6) %>%
      str_replace_all(., c("DUM_" = "", "_" = "-")) %>%
      as.Date(.) -> dummies

    dum_all <- list()
    for (i in 1:length(dummies)) {
      dummy_date(dummies[i]) -> dum_all[i]
    }
    as.data.frame(dum_all) -> dum_all
    paste("DUM_", str_replace_all(dummies, "-", "_"), sep = "") -> names(dum_all)

    if (Panel == 1) {
      cbind(DF, dum_all) -> DF
    } else {

      dummiesPanel <- list()
      dummiesPanel1 <- list()
      for (i in 1:ncol(dum_all)) {
        rep(dum_all[i], Panel) -> dummiesPanel
        bind_rows(dummiesPanel) -> dummiesPanel1[i]
      }
      as.data.frame((dummiesPanel1)) -> dummiesPanel
      names(dummiesPanel) <- names(dum_all)
      cbind(DF, dummiesPanel) -> DF
    }
    DF <- DF[, !duplicated(colnames(DF), fromLast = TRUE)]
  }

  # Dummy Range
  rep.row <- function(x,n){
    matrix(rep(x, each = n), nrow = n)
  }
  dummy_range <- function(dummyRanges){
    str_split(dummyRanges, "-to-", n = Inf) %>% unlist(.) -> Range1
    as.Date(Range1[[1]], "%Y-%m-%d") -> fromD
    as.Date(Range1[[2]], "%Y-%m-%d") -> toD
    seq.Date(from = fromD, to = toD, by = "month") %>%
      as.data.frame(.) %>% mutate(range = 1) -> Range2
    left_join(as.data.frame(DF$Month), as.data.frame(Range2), by = c("DF$Month" = ".")) %>%
      select(range) -> Range2
    Range2[is.na(Range2)] <- 0
    names(Range2) <- paste("RANGE_", dummyRanges, sep = "")
    as.list(Range2) -> Range2
    return(Range2)
  }

  if (length(str_subset(var.names, param11)) == 0 ) {
    DF -> DF } else {
      var.names %>%
        str_subset(param11) %>%
        str_replace_all(., c("RANGE_" = "", "_" = "-")) -> dummyRanges

      rangedates <- list()
      for (i in 1:length(dummyRanges)) {
        dummy_range(dummyRanges[i]) -> rangedates[i]
      }
      as.data.frame(rangedates) -> rangedates
      names(rangedates) <- paste("RANGE_", dummyRanges, sep = "")
      names(rangedates) <- str_replace_all(names(rangedates), pattern = "-", replacement = "_")
      if (Panel == 1) {
        cbind(DF, rangedates) -> DF
      } else{
        # rep.row(rangedates, Panel) %>%
        #   unlist(.) %>%
        #   matrix(nrow = length(DF$Month)*Panel) %>%
        #   as.data.frame(.) -> panelRange
        names(rangedates) <- paste("RANGE_", dummyRanges, sep = "")
        names(rangedates) <- str_replace_all(names(rangedates), pattern =  "-", replacement = "_")
        cbind(DF, rangedates) -> DF
      }
    }

  # Meansby ----
  if ( Panel == 1) {
    print("Single Panel")
  } else {
    if (sum(str_detect(var.names, c("_MEANSBY"))) == 0 ) {
      DF -> DF
    } else {
      var.names %>%
        str_subset(param10) %>%
        str_replace_all(c("\\{" = "", "\\}_Log" = "", "_MEANSBY" = "")) -> varsMB

      cbind(select(DF, Brand), select(DF, one_of(c(varsMB)))) -> MB
      names(MB)[2] <- "KPI_Panel"
      MB %>%
        group_by(Brand) %>%
        summarise(meanKPI = mean(KPI_Panel)) -> meanKPI
      names(meanKPI)[2] <- paste(varsMB, "_MEANSBY", sep = "")
      left_join(DF, meanKPI) -> DF

    }
  }

  # Panel
  if (sum(str_detect(var.names, "Panel")) == 0) {
    DF -> DF
  } else {
    var.names %>%
      str_subset(param13) -> panels
    str_replace_all(panels, "Panel_", "") -> panelnames

    Panels <- list()
    for (p in 1:length(panelnames)) {
      DF$Brand == panelnames[p] -> Panels[[p]]
    }
    as.data.frame(Panels) -> Panels
    names(Panels) <- panels
    cbind(DF, Panels) -> DF
  }
  DF <- DF[, !duplicated(colnames(DF), fromLast = TRUE)]

  # Adding Vars
  if (sum(str_detect(var.namesM, "\\<A>")) == 0) {
    DF -> DF
  } else {
    var.namesM %>%
      str_subset(param12) -> setsA

    setsA %>%
      str_split(param12, n = Inf) -> varsA

    varsAdd <- list()
    for (i in 1:length(varsA)) {
      DF %>% select(one_of(varsA[[i]])) %>%
        mutate(.[,c(1)] + .[,c(2)]) -> varsAdd[[i]]
    }

    varsAdd %>%
      as.data.frame(.) %>%
      select(seq(from = 3, to = ncol(.), by = 3)) -> varsAdd
    names(varsAdd) <- setsA
    cbind(DF, varsAdd) -> DF
  }

  # Multiplying Vars
  if (sum(str_detect(var.namesM, "\\<\\*>")) == 0) {
    DF -> DF
  } else {
    var.namesM %>%
      str_subset(param7) -> setsM

    setsM %>%
      str_split(param7, n = Inf) -> varsM

    varsMul <- list()
    for (i in 1:length(varsM)) {
      DF %>% select(one_of(varsM[[i]])) %>%
        mutate(.[,c(1)]*.[,c(2)]) -> varsMul[[i]]
    }

    varsMul %>%
      as.data.frame(.) %>%
      select(seq(from = 3, to = ncol(.), by = 3)) -> varsMul
    names(varsMul) <- setsM
    cbind(DF, varsMul) -> DF
  }

  # Dividing
  if (sum(str_detect(var.namesM, "<D>")) == 0) {
    DF -> DF
  } else {
    var.namesM %>%
      str_subset(param8) %>%
      str_replace_all(c("\\{" = "", "\\}_Log" = "")) -> setsD

    setsD %>%
      str_split(param8, n = Inf) -> varsD

    varsDiv <- list()
    for (i in 1:length(varsD)) {
      DF %>% select(one_of(varsD[[i]])) %>%
        mutate(.[,c(1)]/.[,c(2)]) -> varsDiv[[i]]
    }
    varsDiv %>%
      as.data.frame(.) %>%
      select(seq(from = 3, to = ncol(.), by = 3)) -> varsDiv
    names(varsDiv) <- setsD
    cbind(DF, varsDiv) -> DF
  }



  # Order of Operations Log
  if (sum(str_detect(var.namesM, c("\\{"))) == 0) {
    DF -> DF
  } else {
    var.namesM %>%
      str_subset(param9) %>%
      str_split(., pattern = "<S>") %>%
      unlist(.) %>%
      str_subset(param9) -> setsOD
    setsOD %>%
      str_replace_all(c("\\{" = "", "\\}_Log" = "")) -> varsOD
    DF %>% select(one_of(varsOD)) -> varsNestedLog
    varsNestedLog -> salesunits
    #salesunits*100 -> salesunits
    cbind(varsNestedLog, salesunits) -> varsNestedLog
    names(varsNestedLog) -> varsNestedLogNames
    varsNestedLog <- as.data.frame(varsNestedLog[, !duplicated(colnames(varsNestedLog), fromLast = TRUE)])
    names(varsNestedLog) <- unique(varsNestedLogNames)
    as.data.frame(varsNestedLog) -> varsNestedLog
    log(varsNestedLog+1) -> varsNestedLog
    names(varsNestedLog) <- paste("{", names(varsNestedLog), "}_Log", sep = "")
    cbind(DF, varsNestedLog) -> DF }


  # Subtracting
  if (sum(str_detect(var.namesM, "\\<S>")) == 0) {
    DF -> DF
  } else {
    var.namesM %>%
      str_subset(param14) -> setsS
    #%>% str_replace_all(c("\\{" = "", "\\}_Log" = "")) -> setsS

    setsS %>%
      str_split(param14, n = Inf) -> varsS

    varsSub <- list()
    for (i in 1:length(varsS)) {
      DF %>% select(one_of(varsS[[i]])) %>%
        mutate(.[,c(1)] - .[,c(2)]) -> varsSub[[i]]
    }

    varsSub %>%
      as.data.frame(.) %>%
      select(seq(from = 3, to = ncol(.), by = 3)) -> varsSub
    names(varsSub) <- setsS
    cbind(DF, varsSub) -> DF
  }

  DF %>%
    select(Brand, Month, c(1:ncol(DF))) %>%
    filter(Month >= start_date & Month <= end_date) -> DF

  if (sum(str_detect(colnames(DF), pattern = "brand")) > 0) {DF$brand <- NULL} else {DF}
  if (Panel == 1) {DF$Brand <- NULL} else {DF -> DF}
  return(DF)
}

CorTestAdR <- function(variable, data, model, modeltable, eMax, rMax, pMax, dMax){

  #determine if data is panel
  if (sapply(data[2], class) == "Date") {

    #Select data table and Media Variable put into tdf (Tstat Dataframe)
    colnames(data)[1] -> panel
    colnames(data)[2] -> date
    tdf <- select_(data, panel, date, variable)

  } else {
    #Select data table and Media Variable put into tdf (Tstat Dataframe)
    colnames(data)[1] -> date
    tdf <- select_(data, date, variable)

    #Add constant for panel
    tdf$panel <- 100
    tdf <- select_(tdf, "panel", date, variable)

  }

  #rename columns for use in AdResponse function
  colnames(tdf)[1] <- "DMA"
  colnames(tdf)[2] <- "WeekDate"

  #set parameters for AdResponse Run
  perams <-  list("EffectiveFrequency" = c(1, eMax, 1), "Recency" = c(1, rMax, 1), "Period" = c(1, pMax/7, 1), "Decay" = c(10, dMax, 10))


  #run Adresponse
  tdf = calculateAdResponseByDMA(tdf, params = perams)
  colnames(tdf)[2] <- "Date"
  tdf <- DateConvertMean(tdf)


  #filter tdf so it is same length as model table
  tdf %>%
    filter(Month >= min(modeltable$Month)) %>%
    filter(Month <= max(modeltable$Month)) -> tdf

  #put resids in tdf
  tdf$resid <- model$residuals

  #Create lists for the results of test
  nameslist <- vector("list", 50000)
  tlist <- vector("list", 50000)

  #run correlation on each var
  for (i in 3:(length(tdf)-1) ) {

    tlist[[i-2]] <- cor(tdf$resid, tdf[[i]])
    nameslist[[i-2]] <- colnames(tdf)[i]

  }
  #filter out NAs and turn lists to DFs
  tlist <- Filter(Negate(is.null), tlist)
  nameslist <- Filter(Negate(is.null), nameslist)
  Tdata <- do.call(rbind, Map(data.frame, Name=nameslist, Corr=tlist))
  Tdata %>% arrange(desc(Corr)) -> CorrTest
  print(head(CorrTest))
  return(CorrTest)
}

CorTestLag <- function(variable, data, model, modeltable, Log = FALSE, lmax){

  #determine if data is panel
  if (sapply(data[2], class) == "Date") {

    #Select data table and Media Variable put into tdf (Tstat Dataframe)
    colnames(data)[1] -> panel
    colnames(data)[2] -> date
    tdf <- select_(data, panel, date, variable)

  } else {
    #Select data table and Media Variable put into tdf (Tstat Dataframe)
    colnames(data)[1] -> date
    tdf <- select_(data, date, variable)

    #Add constant for panel
    tdf$panel <- 100
    tdf <- select_(tdf, "panel", date, variable)

  }

  if (Log == TRUE){
    tdf[[3]] <- log(tdf[[3]] + 1)
    newvar <- paste(variable, "Log", sep = "_")
    colnames(tdf)[3] <- newvar
    variable <- newvar
  }


  #rename columns for use in AdResponse function
  colnames(tdf)[1] <- "DMA"
  colnames(tdf)[2] <- "Month"


  for (i in 1:lmax) {
    newvar <- paste(variable,"_Lag", i, sep = "")
    tdf <- slide(tdf, Var = variable, GroupVar = 'DMA',
                 TimeVar = 'Month', slideBy = -i)
    colnames(tdf)[i+3] <- newvar
  }

  #filter tdf so it is same length as model table
  tdf %>%
    filter(Month >= min(modeltable$Month)) %>%
    filter(Month <= max(modeltable$Month)) -> tdf


  #put resids in tdf
  tdf$resid <- model$residuals

  #Create lists for the results of test
  nameslist <- vector("list", 50000)
  tlist <- vector("list", 50000)

  #run correlation on each var
  for (i in 3:(length(tdf)-1) ) {

    tlist[[i-2]] <- cor(tdf$resid, tdf[[i]], use = "complete.obs")

    nameslist[[i-2]] <- colnames(tdf)[i]

  }
  #filter out NAs and turn lists to DFs
  tlist <- Filter(Negate(is.null), tlist)
  nameslist <- Filter(Negate(is.null), nameslist)
  Tdata <- do.call(rbind, Map(data.frame, Name=nameslist, Corr=tlist))
  Tdata %>% arrange(desc(Corr)) -> CorrTest
  print(head(CorrTest))
  return(CorrTest)
}


RunModel <- function(Formula, DF, Panel) {
  Formula <- str_replace_all(Formula,
                             c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "", "\\<A\\>" = "_A_", "\\<S\\>" = "_S_"))
  names(DF) <- str_replace_all(names(DF), c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "", "\\<A\\>" = "_A_", "\\<S\\>" = "_S_"))
  if (Panel == 1) {
    lm(Formula, DF) -> model
    print(summary(model))
    return(model)
  }  else {
    reformulate(response = str_split_fixed(Formula, pattern = " ~ ", n = 2)[1],termlabels = str_split_fixed(Formula, pattern = " ~ ", n = 2)[2]) -> Formula
    plm(Formula, DF, model = "within") -> model
    #print(summary(model))
    return(model)
  }
}

DW_Vif_Test <- function(Formula, DF, Panel) {
  Formula <- str_replace_all(Formula,
                             c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "", "\\<A\\>" = "_A_", "\\<S\\>" = "_S_"))
  names(DF) <- str_replace_all(names(DF), c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "", "\\<A\\>" = "_A_", "\\<S\\>" = "_S_"))
  if (Panel == 1) {
    durbinWatsonTest(model = lm(Formula, DF)) %>% print(.)
    return(model)
  }  else {
    reformulate(response = str_split_fixed(Formula, pattern = " ~ ", n = 2)[1],termlabels = str_split_fixed(Formula, pattern = " ~ ", n = 2)[2]) -> Formula
    pdwtest(x = Formula, data = DF, model = "within") %>% print(.)
    vif(plm(Formula, data = DF, model = "pooling")) %>% print(.)
  }
}

exportREG <- function(Formula, DF, Panel, model, nameDF) {
  str_split_fixed(Formula, pattern = " ~ ", n = 2)[1] -> KPI
  str_split_fixed(KPI, pattern = "<S>", n = 2)[1] -> KPI1
    str_split_fixed(KPI1, pattern = "<D>", n = 2)[1] %>%
    str_replace(pattern = "\\{", replacement = "") %>% str_replace(pattern = "_Log", replacement = "") -> OKPI
  as.data.frame(DF[, colnames(DF) %in% KPI]) -> KPIVARS
  KPI -> names(KPIVARS)
  as.data.frame(DF[, colnames(DF) %in% OKPI]) -> OKPIVARS
  OKPI -> names(OKPIVARS)
  str_split_fixed(Formula, pattern = " ~ ", n = 2)[2] -> VARS
  unlist(strsplit(VARS, " \\+ ")) -> VARS
  as.data.frame(DF[, colnames(DF) %in% VARS]) -> VARSCOLS

  if (Panel == 1) {
    cbind(select(DF, Month), OKPIVARS, KPIVARS, VARSCOLS) -> reg_group
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[,1] -> reg_group$YEAR
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[,2] -> reg_group$MONTH
    paste0(reg_group$YEAR, "M", reg_group$MONTH) -> reg_group$Month
    reg_group$YEAR <- NULL
    reg_group$MONTH <- NULL
    names(reg_group) <- str_replace_all(names(reg_group), c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "", "\\<A\\>" = "_A_", "\\<S\\>" = "_S_"))
    names(reg_group)[2:ncol(reg_group)] <- toupper(names(reg_group)[2:ncol(reg_group)])
    names(reg_group)[3] <- paste("LOG(", names(reg_group)[3], ")", sep = "")
    write.csv(reg_group, paste0(nameDF, "_reg.csv"), row.names = FALSE)
    as.data.frame(model$coefficients) -> coeffs
    toupper(row.names.default(coeffs)) -> coeffs$Variable
    names(coeffs)[1] <- "Coefficient"
    str_replace_all(coeffs$Variable, c("(INTERCEPT)" = "C", "\\(" = "", "\\)" = "")) -> coeffs$Variable
    coeffs %>%
      mutate("NA1" = 0, "NA2" = 0, "NA3" = 0) %>%
      select(Variable, Coefficient, NA1, NA2, NA3) -> coeffs
    coeffstop <- as.data.frame(matrix(data = NA, nrow = 6, ncol = 5))
    names(coeffstop) <- names(coeffs)
    bind_rows(coeffstop, coeffs) -> coeffs
    names(coeffs) <- NULL
    coeffs[6, 1] <- "Variable"
    coeffs[6, 2] <- "Coefficient"
    write.csv(coeffs, paste0(nameDF, "_eq.csv"), row.names = FALSE)
  } else {
    cbind(select(DF, Brand, Month), OKPIVARS, KPIVARS, VARSCOLS) -> reg_group
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[,1] -> reg_group$YEAR
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[,2] -> reg_group$MONTH
    paste0(reg_group$YEAR, "M", reg_group$MONTH) -> reg_group$Month
    reg_group$YEAR <- NULL
    reg_group$MONTH <- NULL
    paste(toupper(reg_group$Brand), reg_group$Month, sep = " - ") -> reg_group$Month
    reg_group$Brand <- NULL
    names(reg_group) <- str_replace_all(names(reg_group), c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "", "\\<A\\>" = "_A_", "\\<S\\>" = "_S_"))
    names(reg_group)[2:ncol(reg_group)] <- toupper(names(reg_group)[2:ncol(reg_group)])
    names(reg_group)[3] <- paste("LOG(", names(reg_group)[3], ")", sep = "")
    write.csv(reg_group, paste0(nameDF, "_reg.csv"), row.names = FALSE)
    as.data.frame(model$coefficients) -> coeffs
    toupper(row.names.default(coeffs)) -> coeffs$Variable
    names(coeffs)[1] <- "Coefficient"
    str_replace_all(coeffs$Variable, c("(INTERCEPT)" = "C", "\\(" = "", "\\)" = "")) -> coeffs$Variable
    coeffs %>%
      mutate("NA1" = 0, "NA2" = 0, "NA3" = 0) %>%
      select(Variable, Coefficient, NA1, NA2, NA3) -> coeffs
    coeffstop <- as.data.frame(matrix(data = NA, nrow = 6, ncol = 5))
    names(coeffstop) <- names(coeffs)
    bind_rows(coeffstop, coeffs) -> coeffs
    names(coeffs) <- NULL
    coeffs[6, 1] <- "Variable"
    coeffs[6, 2] <- "Coefficient"
    write.csv(coeffs, paste0(nameDF, "_eq.csv"), row.names = FALSE)
  }
}

ChartResids <- function(DF, KPI, model, panel_name = FALSE) {
  # DF = DF_Panel_Div
  # KPI = "mur_sales<D>mur_tiv"
  # model = model_Div
  # by_panel = FALSE
  # panel = "ver"
  # individual = TRUE

  if (is.null(DF$Brand)){
    DF$dma_code <- "100"
  } else {
    DF$dma_code <- DF$Brand
  }

  DF <- DF[complete.cases(DF),]
  DF$resid <- model$residuals
  DF$KPI <- DF[, colnames(DF) %in% KPI]
  DF$pred <- DF$KPI - DF$resid

  if (panel_name == "all") {
    DF %>% select(Brand, Month, KPI, pred, resid) %>%
      group_by(Brand, Month) %>%
      summarise(KPI = sum(KPI), pred = sum(pred), resid = sum(resid)) -> tmp1

    brands <- DF$Brand %>% unique(.)
    allp <- list()
    for (i in brands) {
      i -> ttl
      ungroup(tmp1) %>%
        filter(Brand == i) %>%
        rename(Year = Month) %>%
        select(Year, KPI, pred, resid) %>%
        ggplot(aes(Year)) +
        geom_line(aes(y = KPI, colour = "Total Sales")) +
        geom_line(aes(y = pred, colour = "Predicted Total Sales")) +
        geom_line(aes(y = resid, colour = "Residual")) +
        theme(legend.position = "none", plot.title = element_text(size = 10),
              axis.title.x = element_text(size = 5),
              axis.title.y = element_text(size = 5)) +
        ggtitle(as.character(i)) -> allp[[i]]
    }

    # Find number of rows
    round((length(brands)/4) + 0.1) -> i

    # Number of plot spaces
    round(i)*4 -> grid
    layout <- matrix(c(1:grid), nrow = i, ncol = 4, byrow = TRUE)
    multiplot(plotlist = allp, layout = layout)
  } else {

  if (panel_name != FALSE) {
    DF <- filter(DF, dma_code == panel_name)
  } else {
    print("View all panel resids: panels aggregated")
  }

  if (panel_name != FALSE) {
    DF %>% select(dma_code, Month, KPI, pred, resid) -> tmp
    tmp$zeroline <- 0

    ungroup(tmp) %>%
      rename(Year = Month) %>%
      select(Year, KPI, pred, resid) %>%
      ggplot(aes(Year)) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = pred, colour = "Predicted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      theme(legend.position = "bottom")
    ggplotly()

  } else {
    DF %>% select(Month, KPI, pred, resid) %>%
      group_by(Month) %>%
      summarise(KPI = sum(KPI), pred = sum(pred), resid = sum(resid)) -> tmp
    tmp$zeroline <- 0

    ungroup(tmp) %>%
      rename(Year = Month) %>%
      select(Year, KPI, pred, resid) %>%
      ggplot(aes(Year)) +
      geom_line(aes(y = KPI, colour = "Total Sales")) +
      geom_line(aes(y = pred, colour = "Predicted Total Sales")) +
      geom_line(aes(y = resid, colour = "Residual")) +
      theme(legend.position = "bottom")
    ggplotly()

    }
  }
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


firstfill <- function(data) {
  if (sum(is.na(data)) > 0) {
    sum(is.na(data))/2 -> val
    first(data[!is.na(data)]) -> min1
    min1 -> data[c(1:val)]
    return(data)
  } else {return(data)}
}
lastfill <- function(data) {
  if (sum(is.na(data)) > 0) {

    last(data[!is.na(data)]) -> end
    end -> max1
    max1 -> data[is.na(data)]
    return(data)
  } else {return(data)}
}


lm_string_format <- function(x){
  return(str_replace_all(x, c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "")))
}

lm_data_headers_format <- function(x){
  names(x) <- str_replace_all(names(x), c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = ""))
  return(x)
}

is_coeff_negative <- function(x, varname) {
  map_lgl(x,
          function(y){
            if(!any(grepl(varname, y$term))){
              return(FALSE)
            }
            any(y[grepl(varname, y$term), 2] < 0)
          }) %>%
    return()
}

is_significant <- function(x, varname) {
  map_lgl(x,
          function(y){
            if(!any(grepl(varname, y$term))){
              return(FALSE)
            }
            any(y[grepl(varname, y$term), 5] >= 0.05)
          }) %>%
    return()
}

transform_combn <- function(x){
  output <- expand.grid(
    unlist(
      data_apply[[x]], recursive = F, use.names = FALSE)
  ) %>%
    apply(1, function(x){paste(x, collapse = " + ")})
  return(output)
}

stacker <- function(group_num){
  var_combo <- combn(groupings[[group_num]], (length(groupings[[group_num]])-1), simplify)
  var_combo <- as.data.frame(var_combo)
  var_combo %>%
    gather(model_number, vars, 1:length(var_combo)) %>%
    mutate(model_number = sprintf("%s_%s", group_num, model_number)) -> var_combo
  return(var_combo)
}

createAdStock <- function(data, data_piv, var_name, type){

  ifelse(test = type == "Panel",
         yes = data %>%
           bind_cols(select(data_piv, week, brand)) %>%
           select(week, brand, c(1)) %>%
           rename(Weeks = week, DMA = brand) -> data,
         no = data %>%
           bind_cols(select(data_piv, week)) %>%
           mutate(brand = 100) %>%
           select(week, brand, c(1)) %>%
           rename(Weeks = week, DMA = brand) -> data)

  D <- str_extract(var_name, "([D][0-9][0-9])")
  D <- str_replace(D, "D", "")
  D <- as.integer(D)
  D <- D/100
  P <- str_extract(var_name, "([P][0-9])")
  P <- str_replace(P, "P", "")
  P <- as.integer(P)
  decays <- seq(D, D, 1)
  period <- seq(P, P, 1)
  as.matrix(data) -> data1
  names(data)[3] -> names
  totald <- list()
  totalp <- list()
  all <- list()
  alldma <- list()
  allpanel <- list()
  for (dma in unique(data1[,2])) {
    filter(data, DMA == dma) -> d
    d[,3:ncol(d)] -> d
    d <- as.matrix(d)
    for (x in 1:ncol(d)) {
      for (p in period) {
        for (i in 1:length(decays)) {
          rowSums(as.data.frame(embed(c(rep(NA, p), as.numeric(d[,x])), p+1) %*% ((1-decays[i])^seq(0,p,1))), na.rm = F) -> totald[[i]]
        }
        totald -> totalp[[p]]
      }
      totalp -> all[[x]]
    }
    all -> alldma[[dma]]
  }
  alldma <- as_data_frame(Map(unlist, alldma))
  lapply(alldma, unlist) -> alldma
  as.data.frame(alldma) -> alldma

  for (x in 1:ncol(alldma)) {
    matrix(unlist(alldma[,x]), nrow = length(unique(data$Weeks))) -> allpanel[[x]]
  }

  dfs <- lapply(allpanel, data.frame, stringsAsFactors = FALSE)
  bind_rows(dfs) -> dfs
  names(dfs) <- paste(rep(names, each = length(decays)*length(period)), "_", "D", rep(decays*100, length(period)*length(names)), "P", rep(rep(period, each = length(decays)), length(names)), sep = "")

  return(dfs)
}


Reach = function(fa, fb, fc, fGRPs) {
  # Return reach data subject to fitted formula to a value r=a/(1+b*(GRPs/1000)^c)

  # fa = Alpha Coefficient in reach model fb = Beta Coefficient in reach model fc = Gamma Coefficient in reach model
  # fGRPs = single data point of GRPs at which to calculate reach

  fReach = as.numeric(fGRPs > 0) * fa/(1 + fb * (fGRPs/1000)^fc)
  # Return calculated reach value
  return(fReach)

  # Example Use of Reach Function (solution=1.222065) test=Reach(0.79,-1,0.5,125)

}

RollingSum = function(afGRPsMat, fDecay, nPeriod) {
  # Create a rolling sum of an AdStock to a vector of data

  # afGRPsMat = matrix (vertical vector) of GRP data fDecay = single data point, decimal decay rate of media nPeriod
  # = integer value of number of observations to sum over

  weights = vector(length=nPeriod)
  decay = 1 - fDecay
  for(i in 1:nPeriod){
    weights[i] <- decay^(nPeriod - i)
  }

  afRollingSum = roll_sumr(afGRPsMat, weights=weights, normalize=F)
  afRollingSum[1:nPeriod-1] <- afGRPsMat[1:nPeriod-1]

  # Return the rolling sum of data
  return(afRollingSum)

  # Example use of Function test=RollingSum(afGRPs, 0.15, 4)
}


AdStock = function(afGRPsMat, fdecayRate) {
  # Generate matrix of AdStocked/Decayed GRPs as a function of input GRPs and decay rate to a value
  # y(t)=y(t-1)*d + x(t)

  # afGRPs = matrix (vertical vector) of GRP Data
  # fdecayRate = decimal version of decay rate

  # Create output matrix base on size of input
  afAdStockedGRPsMat = matrix(1:nrow(afGRPsMat))

  # first observations are equal
  afAdStockedGRPsMat[1, 1] = afGRPsMat[1, 1]

  # loop through calculating AdStocked GRPs
  decay = 1 - fdecayRate
  value = afGRPsMat[1, 1]
  for (x in 2:nrow(afGRPsMat)) {
    value = value * decay + afGRPsMat[x, 1]
    afAdStockedGRPsMat[x, 1] = value
  }

  # Return AdStocked GRPs matrix
  return(afAdStockedGRPsMat)

  # Example use of AdStock Function test=AdStock(data.matric(GRPs[3]),0.15)

}
adstock =  compiler::cmpfun(AdStock)


createColumns = function(input, column, params = list()){
  #   Create Ad Response Columns
  #
  #   Create reach columns, based upon sequence of parameters.
  #   Function returns a data frame for each new column.
  #
  #   input : List or single column of dataframe or matrix.
  #   column : Column name. Used exclusively to name generated column.
  #   params : List of new parameters. Take 4 possible arguments: EffectiveFrequency,
  #   Recency, Period, and Decay. Each argument takes a vector or three elements: start, stop, and step.
  #
  #   Decay is assumed to be an integer from 0 - 100, e.g. 10.
  #
  #   dataframe
  #
  #
  #
  #
  #   d = matrix(c(125, 200, 200, 300, 23, 34234,324))
  #
  #   createColumns(d, "column name")
  #
  #   params <- list(
  #   "EffectiveFrequency" = c(4, 8, 1),
  #   "Recency" = c(1, 4, 1),
  #   "Period" = c(3, 6, 1),
  #   "Decay" = c(10, 20, 10)
  #   )
  #
  #   createColumns(d, "column name", params)
  #
  #  # Set up default parameters
  defaultParams <- list(
    "EffectiveFrequency" = c(2, 4, 1),
    "Recency" = c(1, 2, 1),
    "Period" = c(2, 6, 1),
    "Decay" = c(20, 20, 10)
  )
  if(!exists("vars")){
    vars = matrix(ncol = 10, nrow=3)
  }
  # Import parameters we choose to pass
  p = modifyList(defaultParams, params)
  p = lapply(p, function(x){ seq(x[1], x[2], x[3]) })

  results = list()
  if((grepl("digital", column) & grepl("t1", column))){
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars_display_t1, freq, rec, period, decay)
          }
        }
      }
    }
  }
  else if((grepl("digital", column) & grepl("t2", column))) {
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars_display_t2, freq, rec, period, decay)
          }
        }
      }
    }
  }
  else if((grepl("social display", column) & grepl("t1", column))) {
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars_display_t1, freq, rec, period, decay)
          }
        }
      }
    }
  }
  else if((grepl("social display", column) & grepl("t2", column))) {
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars_display_t2, freq, rec, period, decay)
          }
        }
      }
    }
  }
  else if(grepl("addressable", column)){
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars_addressable, freq, rec, period, decay)
          }
        }
      }
    }
  }
  else if(grepl("streaming", column)){
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars_streaming, freq, rec, period, decay)
          }
        }
      }
    }
  }
  else if(grepl("social video", column)){
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars_streaming, freq, rec, period, decay)
          }
        }
      }
    }
  }
  else if(!grepl("digital", column) & !grepl("addressable", column) & !grepl("streaming", column)  & !grepl("social", column)){
    for(freq in p[["EffectiveFrequency"]]){
      for(rec in p[["Recency"]]){
        if(rec > freq){
          next;
        }
        for(period in p[["Period"]]){
          for(d in p[["Decay"]]){
            decay = d / 100
            name = sprintf("%s_E%sR%sP%sD%s", column, freq, rec, period*7, decay*100)
            results[[name]] = adresponse(input, vars, freq, rec, period, decay)
          }
        }
      }
    }
  }
  return(as.data.frame(results))
}

AdResponse = function(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay) {

  # Generate the Effective Cover of a vector of input GRPs

  # afGRPsMat = matrix (vertical vector) of GRP data
  # afCoeffsMat = matrix (10 cols by 3 rows) of coefficients for reach models from 1+ to 10+
  # nEffFreq = integer value of Effective Frequency Parameter
  # nRecFreq = integer value of Recency Frequency Parameter
  # nPeriod = integer value of Response Period Parameter
  # fDecay = decimal value of decay rate parameter

  # Define output matrix size
  afGRPsMat = as.matrix(afGRPsMat)
  afAdResponse = matrix(1:nrow(afGRPsMat))
  fEffGRPs = RollingSum(afGRPsMat, fDecay, nPeriod)
  fTotalEffGRPs = adstock(afGRPsMat, fDecay)

  a = afCoeffsMat[[1, nEffFreq]]
  b = afCoeffsMat[[2, nEffFreq]]
  c = afCoeffsMat[[3, nEffFreq]]

  if(nRecFreq == 0){
    afAdResponse[,1] = Reach(a, b, c, fTotalEffGRPs)
    return(afAdResponse);
  }

  if(nRecFreq == nEffFreq){
    afAdResponse[,1] = Reach(a, b, c, fEffGRPs)
    return(afAdResponse);
  }

  fTotalEffGRPs_rounded = round(fTotalEffGRPs[, 1], digits = 6)
  fEffGRPs_rounded = round(fEffGRPs[, 1], digits = 6)
  a_s = afCoeffsMat[1,]
  b_s = afCoeffsMat[2,]
  c_s = afCoeffsMat[3,]
  f = matrix(0, nrow=nrow(afGRPsMat), ncol=1)
  for (k in nRecFreq:(nEffFreq - 1)) {
    one = Reach(a_s[[k]], b_s[[k]], c_s[[k]], fEffGRPs)
    two = Reach(a_s[[k + 1]], b_s[[k + 1]], c_s[[k + 1]], fEffGRPs)
    three = Reach(a_s[[nEffFreq - k]], b_s[[nEffFreq - k]], c_s[[nEffFreq - k]], fTotalEffGRPs - fEffGRPs)
    f = f + (one-two)*three/100
  }


  for (i in 1:nrow(afGRPsMat)) {
    # Calculate the x+y(<x) Model
    if (fTotalEffGRPs_rounded[i] == fEffGRPs_rounded[i]) {
      # There is no extra history outside of the recency window
      afAdResponse[i ,1] = Reach(a, b, c, fEffGRPs[i, 1])
    } else {
      # There is extra history outside of the recency window to be considered
      afAdResponse[i, 1] = f[i, 1] + Reach(a, b, c, fEffGRPs[i, 1])
    }
  }

  # Return the calculated AdResponse
  return(afAdResponse)

  # example use of function test=AdResponse(afGRPs, afCoeffs, 5, 1, 2, 0.25)
}

adresponse = compiler::cmpfun(AdResponse)

createAdR = function(data, data_piv, var_name, type){
  ifelse(test = type == "Panel",
         yes = data %>%
           bind_cols(select(data_piv, week, brand)) %>%
           select(week, brand, c(1)) %>%
           rename(WeekDate = week, DMA = brand) -> data,
         no = data %>%
           bind_cols(select(data_piv, week)) %>%
           mutate(brand = 100) %>%
           select(week, brand, c(1)) %>%
           rename(WeekDate = week, DMA = brand) -> data)

  e <- str_extract(var_name, "([E][0-9])")
  e <- str_replace(e, "E", "")
  e <- as.integer(e)
  r <- str_extract(var_name, "([R][0-9])")
  r <- str_replace(r, "R", "")
  r <- as.integer(r)
  p <- str_extract(var_name, "[P][0-9][0-9]")
  p <- str_replace(p, "P", "")
  p <- as.integer(p)
  p <- p/7
  p <- as.integer(p)
  d <- str_extract(var_name, "[D][0-9][0-9]")
  d <- str_replace(d, "D", "")
  d <- as.numeric(d)
  params <- list( "EffectiveFrequency" = c(e, e, 1), "Recency" = c(r, r, 1), "Period" = c(p, p, 1), "Decay" = c(d, d, 1))
  cmp_createColumns = compiler::cmpfun(createColumns)
  data$date <- as.Date(data$WeekDate, format = "%Y-%m-%d UTC")

  # Ensure data is in correct order
  data %>%
    arrange(DMA, date) ->
    data

  results = list()

  for(dma in unique(data$DMA)) {
    data %>%
      filter(DMA == dma) ->
      tmp

    c = list()
    c[['DMA']] = data.frame("DMA" = rep(dma, nrow(tmp)))
    c[['date']] = data.frame("Date" = tmp$date)
    # Produce list of columns
    ns = names(data)[-grep("DMA|WeekDate|date", names(data))]
    col_indexes <- 1
    for(column in ns[col_indexes]){
      c[[column]] = tmp[column]
      start = Sys.time()
      c[[sprintf("%s_exploded", column)]] = createColumns(tmp[column], column, params)
      end = Sys.time()
      print(sprintf("%s - %s: %s", dma, column, end-start))
    }
    results[[dma]] <- bind_cols(c)
  }
  k = bind_rows(results)
  k %>% select(c(ncol(.))) -> k
  # ifelse(test = type == "Panel",
  #        yes = k %>% select(DMA, Date, c(ncol(k))) %>% rename(brand = DMA) -> k,
  #        no = k %>% select(Date, c(ncol(k))) -> k)
  return(k)
}


reprow <- function(x,n){
  matrix(rep(x,each = n),nrow = n)
}


repcol <- function(x,n){
  matrix(rep(x,each = n), ncol = n, byrow = TRUE)
}

chart_var_resid <- function(kpi, var, modelname, numPanels, kpiDataset, varDataset){


  teststring <- paste(kpi, "~", var)
  chartDF <- StringGen(teststring, media_data = varDataset, nonmedia_data = kpiDataset, Panel = numPanels)
  chartDF$resid <- modelname$resid
  chartDF[[var]] <- chartDF[[var]]/mean(chartDF[[var]])-1


  if(numPanels > 1){
    chartDF[,c("Brand","Month", var, "resid")] %>%
      gather(Type, Val, 3:4) %>%
      ggplot(aes(Month, Val, color = Type)) +
      facet_wrap(~Brand, ncol=NULL, scales="fixed") +
      geom_line() +
      scale_y_continuous("Residuals")
    ggplotly()
  } else{
    chartDF[,c("Month", var, "resid")] %>%
      gather(Type, Val, 2:3) %>%
      ggplot(aes(Month, Val, color = Type)) +
      geom_line() +
      scale_y_continuous("Residuals")
    ggplotly()
  }

}

OutResidDMA <- function(DF, model) {
  DFO <- DF
  DF1 <- DF[,1:2]
  DF$dma_code <- DF$Brand
  DF <- DF[complete.cases(DF),]
  DF$resid <- model$residuals
  SD <- select(DF, dma_code, Month, resid)
  SD_piv <- dcast(SD, Month ~ dma_code, fun.aggregate = sum, value.var = "resid")
  all_sd <- list()
  for (i in 1:(ncol(SD_piv) - 1)) {
    SD1 <- SD_piv[,2:ncol(SD_piv)]
    all_sd[i] <- sd(SD1[,i])
  }
  all_sd <- cbind(DF$dma_code %>% unique(.), all_sd)
  all_sd <- as.data.frame(matrix(unlist(all_sd), nrow = length(unique(DF$dma_code))))
  all_sd <- all_sd %>% mutate(dma_code = as.character(V1), residSD = as.double(as.character(V2))) %>% select(dma_code, residSD)
  all_sd <- left_join(all_sd, SD, by = c("dma_code" = "dma_code"))
  residuals1 <- list()
  for (i in 1:nrow(all_sd)) {
    if (all_sd[i, 4] >= all_sd[i, 2]) {
      residuals1[i] <- 1}
    else if (all_sd[i, 4] <= -all_sd[i, 2]) {
      residuals1[i] <- -1
    }
    else {
      residuals1[i] <- 0
    }
  }
  residuals1 <- t(as.data.frame(residuals1))
  residuals1 <- cbind(DF[,1:2], residuals1)
  names(residuals1)[3] <- "residualdum_dma"
  residuals1 <- left_join(DF1, residuals1)
  residuals1 <- cbind(residuals1, DFO[,3:ncol(DFO)])
  return(residuals1)
}

ReachCurveCalc <- function(Values, input){
  # Look up for EF/RF parameters and
  gather(fit, Param, Reach, 2:ncol(fit)) -> lookup
  gather(fit_displayt1, Param, Reach, 2:ncol(fit)) -> lookup_dt1
  gather(fit_displayt2, Param, Reach, 2:ncol(fit)) -> lookup_dt2
  gather(fit_st, Param, Reach, 2:ncol(fit)) -> lookup_st
  gather(fit_addressable, Param, Reach, 2:ncol(fit)) -> lookup_ad
  AllReach <- list()
  gaAdresponse <- vector("numeric", length = 104)
  totaladresponse <- list()
  Reach <- compiler::cmpfun(Reach)
  LayDown <- 0
  for (col in 2:ncol(input)) {
    if (names(input[col]) == "R") {

      # Store Relevant Input Values
      as.integer(input[1,col]) -> spend
      as.integer(input[2,col]) -> contribution
      as.integer(input[4,col]) -> EF
      as.integer(input[5,col]) -> RF
      as.integer(input[6,col]) -> Period
      as.integer(input[7,col]) -> Decay
      as.character(input[11,col]) -> Channel

      if((grepl("DIGITAL", Channel) & grepl("T1", Channel))){
        Alpha <- createABG(lookup_dt1, "A")
        Beta <- createABG(lookup_dt1, "B")
        Gamma <- createABG(lookup_dt1, "C")
      }else if((grepl("SOCIAL DISPLAY", Channel) & grepl("T1", Channel))){
        Alpha <- createABG(lookup_dt1, "A")
        Beta <- createABG(lookup_dt1, "B")
        Gamma <- createABG(lookup_dt1, "C")
      }
      else if((grepl("SOCIAL DISPLAY", Channel) & grepl("T2", Channel))){
        Alpha <- createABG(lookup_dt2, "A")
        Beta <- createABG(lookup_dt2, "B")
        Gamma <- createABG(lookup_dt2, "C")
      }
      else if((grepl("DIGITAL", Channel) & grepl("T2", Channel))){
        Alpha <- createABG(lookup_dt2, "A")
        Beta <- createABG(lookup_dt2, "B")
        Gamma <- createABG(lookup_dt2, "C")
      }else if(grepl("STREAMING", Channel)){
        Alpha <- createABG(lookup_st, "A")
        Beta <- createABG(lookup_st, "B")
        Gamma <- createABG(lookup_st, "C")
      }
      else if(grepl("SOCIAL VIDEO", Channel)){
        Alpha <- createABG(lookup_st, "A")
        Beta <- createABG(lookup_st, "B")
        Gamma <- createABG(lookup_st, "C")
      }else if(grepl("ADDRESSABLE", Channel)){
        Alpha <- createABG(lookup_ad, "A")
        Beta <- createABG(lookup_ad, "B")
        Gamma <- createABG(lookup_ad, "C")
      }else if(!grepl("STREAMING", Channel) & !grepl("ADDRESSABLE", Channel) & !grepl("DIGITAL", Channel) & !grepl("SOCIAL", Channel)){
        Alpha <- createABG(lookup, "A")
        Beta <- createABG(lookup, "B")
        Gamma <- createABG(lookup, "C")
      }


      Decay/100 -> Decay
      Values[1:104, col] -> gaGRPs
      104 -> gnObs
      for (t in 1:1) {
        # Repeat GRPs
        gaGRPs[1:52,t] -> YagoGRPs
        gaGRPs[53:104,t] -> CYGRPs
        reprow(YagoGRPs, 101) -> YagoGRPs
        reprow(CYGRPs, 101) -> CYGRPs
        matrix(unlist(YagoGRPs), nrow = 52, byrow = FALSE) -> YagoGRPs
        matrix(unlist(CYGRPs), nrow = 52, byrow = FALSE) -> CYGRPs
        # Repeat %
        seq(from = 0, to = 5, by = 0.05) -> Percents
        as.data.frame(t(Percents)) -> Per
        reprow(Per, 52) -> allper
        allper <- data.frame(matrix(unlist(allper), nrow = 52, byrow = FALSE))
        allper1 <- data.frame(matrix(1, nrow = 52, ncol = 101))
        # Multiply GRPs and %
        CYGRPs * allper -> CYGRPs1
        YagoGRPs * allper1 -> YagoGRPs1
        rbind(YagoGRPs1, CYGRPs1) -> total
        names(total) <- Per

        # start = Sys.time()
        for (x in 1:ncol(total)) {
          for (i in Period:gnObs) {
            # Calculate the Effective GRPs
            fEffGRPs = 0
            fTotEffGRPs = 0
            for (j in 1:i) {
              if (j <= Period) {
                fEffGRPs <- fEffGRPs + total[i - j + 1, x][[1]] * (1 - Decay) ^ (j - 1)
              }
              fTotEffGRPs <- fTotEffGRPs + total[i - j + 1, x][[1]] * (1 - Decay) ^ (j - 1)
            }
            # Calculate response function
            # if(grepl("DIGITAL", Channel) & grepl("T1", Channel)){
            #   Alpha <- createABG(lookup_dt1, "A")
            #   Beta <- createABG(lookup_dt1, "B")
            #   Gamma <- createABG(lookup_dt1, "C")
            # }else if(grepl("DIGITAL", Channel) & grepl("T2", Channel)){
            #   Alpha <- createABG(lookup_dt2, "A")
            #   Beta <- createABG(lookup_dt2, "B")
            #   Gamma <- createABG(lookup_dt2, "C")
            # }else if(grepl("STREAMING", Channel)){
            #   Alpha <- createABG(lookup_st, "A")
            #   Beta <- createABG(lookup_st, "B")
            #   Gamma <- createABG(lookup_st, "C")
            # }else if(grepl("ADDRESSABLE", Channel)){
            #   Alpha <- createABG(lookup_ad, "A")
            #   Beta <- createABG(lookup_ad, "B")
            #   Gamma <- createABG(lookup_ad, "C")
            # }else if(!grepl("STREAMING", Channel) & !grepl("ADDRESSABLE", Channel) & !grepl("DIGITAL", Channel)){
            #   Alpha <- createABG(lookup, "A")
            #   Beta <- createABG(lookup, "B")
            #   Gamma <- createABG(lookup, "C")
            # }

            fCalc <- Reach(Alpha(EF), Beta(EF), Gamma(EF), fTotEffGRPs)
            if ((RF > 0 & RF < EF)) {
              if (fTotEffGRPs - fEffGRPs == 0) {
                if (fCalc > Reach(Alpha(RF), Beta(RF), Gamma(RF), fEffGRPs)) {
                  fCalc <- Reach(Alpha(RF), Beta(RF), Gamma(RF), fEffGRPs)
                }
              }
              else {
                fCalc <- 0
                for (k in RF:(EF - 1)) {
                  fCalc <- fCalc + (Reach(Alpha(k), Beta(k), Gamma(k), fEffGRPs) - Reach(Alpha(k + 1), Beta(k + 1), Gamma(k + 1), fEffGRPs)) * Reach(Alpha(EF - k), Beta(EF - k), Gamma(EF - k), fTotEffGRPs - fEffGRPs) / 100
                }
                fCalc <- fCalc + Reach(Alpha(EF), Beta(EF), Gamma(EF), fEffGRPs)
              }
            }
            if (RF == EF) {
              fCalc <- Reach(Alpha(RF), Beta(RF), Gamma(RF), fEffGRPs)
            }
            gaAdresponse[i] <- fCalc
          }
          totaladresponse[[x]] <- gaAdresponse
        }
        # end = Sys.time()
        # print(end - start)

        matrix(unlist(totaladresponse), nrow = 104, byrow = FALSE) -> TotalAdResponse
        colSums((TotalAdResponse)[53:nrow(TotalAdResponse),1:ncol(TotalAdResponse)]) -> VarReach
        VarReach - VarReach[[1]] -> VarReach

        # Find contributions through % of VarReach
        as.data.frame(VarReach) -> VarReach
        rbind(0,VarReach) -> VarReach1
        VarReach1[1:101,] -> VarReach1

        VarReach1/VarReach -> ContrPer
        as.data.frame(ContrPer[1:21,]) -> ContrPer
        VarReach/VarReach1 -> ContrPer1
        as.data.frame(ContrPer1[22:101,]) -> ContrPer1
        names(ContrPer) <- "Percent"
        names(ContrPer1) <- "Percent"

        rbind(ContrPer, ContrPer1) -> ContrPer

        ContrPer$Contribution <- NA
        ContrPer$Contribution[[21]] <- contribution

        for (x in 21:1) {
          ContrPer$Percent[[x]]*ContrPer$Contribution[[x]] -> ContrPer$Contribution[x - 1]
        }

        for (x in 22:nrow(ContrPer)) {
          ContrPer$Percent[[x]]*ContrPer$Contribution[x - 1] -> ContrPer$Contribution[x]
        }

        # Calculate Spend
        t(Per*spend) -> Spend
        cbind(Spend, VarReach, ContrPer$Contribution) -> Final
        names(Final) <- c("Spend", "Reach", "Contribution")
        list(Final) -> test
        print(paste(names(Values)[col], "done"))
      }
      test -> AllReach[[col]]
    } else if (names(input[col]) == "L") {

      gnObs <- nrow(Values)
      contribution <- as.integer(input[2, (col)])
      spend <- as.integer(input[1, (col)])
      period <- as.integer(input[6, (col)])
      decay <- as.integer(input[7, (col)])
      decay <- decay / 100
      gaGRPs <- Values[1:104, col]
      LayDown <- 0
      # Find Decay rates
      for (aInd in 1:period) {
        if (aInd == 1) {
          LayDown[aInd] <- aInd
        } else {
          LayDown[aInd] <-  (1 - decay) ^ (aInd - 1)
        }
      }
      # Repeat GRPs
      gaGRPs[1:52, 1] -> YagoGRPs
      gaGRPs[53:104, 1] -> CYGRPs
      repcol(YagoGRPs, 101) -> YagoGRPs
      repcol(CYGRPs, 101) -> CYGRPs
      matrix(unlist(YagoGRPs), nrow = 52, byrow = FALSE) -> YagoGRPs
      matrix(unlist(CYGRPs), nrow = 52, byrow = FALSE) -> CYGRPs
      # Repeat %
      seq(from = 0, to = 5, by = 0.05) -> Percents
      as.data.frame(t(Percents)) -> Per
      reprow(Per, 52) -> allper
      allper <- data.frame(matrix(unlist(allper), nrow = 52, byrow = FALSE))
      allper1 <- data.frame(matrix(1, nrow = 52, ncol = 101))
      # Multiply GRPs and %
      CYGRPs * allper -> CYGRPs1
      YagoGRPs * allper1 -> YagoGRPs1
      rbind(YagoGRPs1, CYGRPs1) -> total
      names(total) <- Per

      CalcStg <- matrix(NA, nrow = 104, ncol = 4)


      FinalAll <- list()
      for (i in 1:ncol(total)) {
        cInd <- 0
        Output <- matrix(0, nrow = 104, ncol = 1)
        CalcStg <- as.data.frame(outer(as.matrix(total[,i]), as.matrix(LayDown)))
        for (aInd in 1:nrow(total)) {
          for (bInd in 1:period) {
            cInd = aInd + bInd - 1
            if (cInd <= gnObs) {
              Output[cInd] <- Output[cInd] + CalcStg[aInd, bInd]
            }
          }
          Output + 1 -> Output1
          log(Output1) -> Final
        }
        Final -> FinalAll[[i]]
      }
      matrix(unlist(FinalAll), nrow = 104, ncol = 101) -> total1


      colSums((total1)[53:nrow(total1),1:ncol(total1)]) -> VarReach
      VarReach - VarReach[[1]] -> VarReach

      # Find contributions through % of VarReach
      as.data.frame(VarReach) -> VarReach
      rbind(0,VarReach) -> VarReach1
      VarReach1[1:101,] -> VarReach1

      VarReach1/VarReach -> ContrPer
      as.data.frame(ContrPer[1:21,]) -> ContrPer
      VarReach/VarReach1 -> ContrPer1
      as.data.frame(ContrPer1[22:101,]) -> ContrPer1
      names(ContrPer) <- "Percent"
      names(ContrPer1) <- "Percent"

      rbind(ContrPer, ContrPer1) -> ContrPer

      ContrPer$Contribution <- NA
      ContrPer$Contribution[[21]] <- contribution

      for (x in 21:1) {
        ContrPer$Percent[[x]]*ContrPer$Contribution[[x]] -> ContrPer$Contribution[x - 1]
      }

      for (x in 22:nrow(ContrPer)) {
        ContrPer$Percent[[x]]*ContrPer$Contribution[x - 1] -> ContrPer$Contribution[x]
      }

      # Calculate Spend
      t(Per*spend) -> Spend
      cbind(Spend, VarReach, ContrPer$Contribution) -> Final
      names(Final) <- c("Spend", "Reach", "Contribution")
      list(Final) -> Final
      Final -> AllReach[[col]]
      print(paste(names(Values)[col], "done"))
    } else {print("Please select whether you are inputting a Adresponse or Adstock variable.")
    }
  }
  data.frame(matrix(unlist(AllReach), nrow = 101, byrow = FALSE)) -> AllReach
  # Name the reach curves
  names(Values)[-1] -> names
  rep(names, each = 3 ) -> names

  matrix(names, ncol = 3, byrow = TRUE) -> testing
  as.data.frame(testing) -> names1
  paste(testing[,1], "Spend", sep = " ") -> names1$Spend
  paste(testing[,2], "Reach", sep = " ") -> names1$Reach
  paste(testing[,3], "Contribution", sep = " ") -> names1$Contribution
  select(names1, Spend:Contribution) -> names1
  as.matrix(t(names1)) -> names1
  as.vector(names1) -> names1
  names(AllReach) <- names1
  return(AllReach)
}

# ABCsReachGen <- function(Contrib, fit,  fit_displayt1, fit_displayt2, fit_st, fit_addressable,fmi_data, panel, spend, multiplier, year, From, To, revise = "NO", revise_table = Ctable_Revised) {
#
#   tolower(names(fmi_data)) -> names(fmi_data)
#
#   # Through Decomp
#   Contrib %>%
#     gather("Year", "Contributions",  5:ncol(.)) %>%
#     filter(Year == year) %>%
#     group_by(variable, Categories, Model, Year) %>%
#     summarise(Contributions = sum(Contributions)) %>%
#     mutate(TMP = 1) %>%
#     filter(grepl("_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])", variable) | grepl("_([D][0-9][0-9][P][0-9])", variable)) %>%
#     arrange(variable) %>%
#     mutate(variable1 = variable) -> Contrib1
#
#   Contrib1 %>%
#     filter(grepl("_([D][0-9][0-9][P][0-9])", variable)) %>%
#     mutate(Type = "L") -> Contrib1a
#   Contrib1a$variable -> stockvars
#   tolower(stockvars) -> Contrib1a$variable1
#
#   Contrib1 %>%
#     filter(grepl("_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])", variable)) %>%
#     mutate(Type = "R") -> Contrib1b
#
#   rbind(Contrib1a, Contrib1b) -> Contrib1
#
#   spend %>% filter(model_agg == panel | model_agg == toupper(panel)) -> spend
#   inner_join(Contrib1, spend, by = c("Categories" = "media_agg")) -> Contrib1
#   Contrib1 %>% filter(!is.na(Spend)) %>% filter(Contributions > 0) -> Contrib1
#
#
#   # Timeframe
#   timeframe <- as.data.frame(seq.Date(from = as.Date(From), to = as.Date(To), by = "week"))
#   names(timeframe) <- "week"
#   if (length(timeframe) > 104) stop("You need to select only 104 weeks")
#
#   # Parameters
#   param1 <- "_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])" # formatting for adresponse
#   param2 <- "_([D][0-9][0-9][P][0-9])" # formatting for adstock
#   param3 <- "_([d][0-9][0-9][p][0-9])" # formatting for adstock
#
#
#   Contrib1$variable1 %>%
#     str_replace_all(param1, "") %>%
#     str_replace_all(param3, "") %>%
#     str_replace_all(., c("_log" = "", "PANEL_[A-Z][A-Z][A-Z]_M_" = "")) %>%
#     tolower(.) %>%
#     str_replace_all(paste(panel, "_", sep = ""), "") -> Allparams
#
#   Allparams -> Contrib1$LU
#
#   # Select Data
#   AllData <- as.data.frame(fmi_data[, colnames(fmi_data) %in% Allparams]) # grab columns to adresponse
#   AllData <- cbind(select(fmi_data, vehicle, week), AllData)
#   AllData <- AllData %>% filter(vehicle == panel)
#   AllData <- gather(AllData, "Category", "Value", 3:ncol(AllData))
#
#   DataJoin <- left_join(Contrib1[,c("variable", "Categories", "Model", "LU")], AllData, by = c("LU" = "Category"))
#   Data <- dcast(DataJoin, week  ~ Model + variable, fun.aggregate = sum, value.var = "Value")
#
#   # 104 Weeks
#   Data <- left_join(timeframe, Data)
#   names(Data)[1] <- "Week"
#   Data$Week <- NULL
#   neg_num <- as.data.frame(paste("Week", seq(from = -52, to = -1)))
#   names(neg_num) <- "Week"
#   pos_num <- as.data.frame(paste("Week", seq( from = 1, to = 52)))
#   names(pos_num) <- "Week"
#   Week <- rbind(neg_num, pos_num)
#   Data <- cbind(Week, Data)
#
#
#   #Parameters
#   Contrib1$E <- str_extract(Contrib1$variable1, "([E][0-9])")
#   Contrib1$E <- str_replace(Contrib1$E, "E", "")
#   Contrib1$R <- str_extract(Contrib1$variable1, "([R][0-9])")
#   Contrib1$R <- str_replace(Contrib1$R, "R", "")
#   Contrib1$P <- str_extract(Contrib1$variable1, "[P][0-9][0-9]")
#   Contrib1$P <- str_replace(Contrib1$P, "P", "")
#   Contrib1$P <- as.integer(Contrib1$P)
#   Contrib1$P <- Contrib1$P/7
#   Contrib1$D <- str_extract(Contrib1$variable1, "[D][0-9][0-9]")
#   Contrib1$D <- str_replace(Contrib1$D, "D", "")
#   Contrib1$p <- str_extract(Contrib1$variable1, "[p][0-9]")
#   Contrib1$p <- str_replace(Contrib1$p, "p", "")
#   Contrib1$d <- str_extract(Contrib1$variable1, "[d][0-9][0-9]")
#   Contrib1$d <- str_replace(Contrib1$d, "d", "")
#
#   # Input
#   paste(Contrib1$Model, Contrib1$variable, sep = "_") -> Contrib1$TableName
#   Contrib1 %>% arrange(TableName) -> Contrib1
#
#   # Revised
#   if (revise == "NO") {
#     Contrib1 -> Contrib1
#   } else {
#     revise_table -> Contrib1
#   }
#
#   Contrib1[,c("TableName", "Spend", "Contributions", "Type", "E", "R", "P", "D", "p", "d", "Categories")] -> Input
#   Input$p[c(!is.na(Input$p))] -> Input$P[c(is.na(Input$P))]
#   Input$d[c(!is.na(Input$d))] -> Input$D[c(is.na(Input$D))]
#   rbind(Input$Type, Input$Spend,  Input$Contributions, Input$E, Input$R, Input$P, Input$D, Input$Categories, Input$TableName) %>%
#     as.data.frame(.) -> Input
#
#
#   rbind(Input[c(1:3),], NA, Input[c(4:7),], NA, NA, NA, Input[c(8:9),]) -> Input
#
#   Col <- rbind("R/L/P", "100% Spend", "100% Contribution", "DMA", "EF", "RF", "Period", "Decay", "Alpha", "Beta", "Power", "Category", "Var Name")
#
#   Input <- cbind(Col, Input)
#
#   row.names(Input) <- NULL
#   Input <- as.data.frame(Input)
#   names(Input) <- as.character(unlist(Input[1,]))
#   Input = Input[-1,]
#   row.names(Input) <- NULL
#
#
#   fit <- as.data.frame(fit)
#   row.names(fit) <- NULL
#
#   # Write out
#   if (revise != "NO") {
#     write_csv(Contrib1, sprintf("%s RC Info Revised.csv", year))
#   } else {
#     write_csv(Contrib1, sprintf("%s RC Info.csv", year))
#   }
#
#   write_csv(Input, "Input.csv")
#   write_csv(Data, "Data.csv")
#   write_csv(fit, "fit.csv")
#   write_csv(fit_displayt1, "fit_displayt1.csv")
#   write_csv(fit_displayt2, "fit_displayt2.csv")
#   write_csv(fit_st, "fit_st.csv")
#   write_csv(fit_addressable, "fit_addressable.csv")
#
#   # Read in the template files
#   read_csv("Data.csv") -> Data
#   read.csv("Input.csv", check.names = FALSE, stringsAsFactors = FALSE) -> Input
#   read_csv("fit.csv") -> fit
#   read_csv("fit_displayt1.csv") ->fit_displayt1
#   read_csv("fit_displayt2.csv") ->fit_displayt2
#   read_csv("fit_st.csv") ->fit_st
#   read_csv("fit_addressable.csv") -> fit_addressable
#
#   # Generate Reach
#   names(fit)[1] <- "NTV"
#
#   ReachCurveCalc(Data, Input) -> AllReach
#   c(0:100) -> Percent
#   cbind(Percent, AllReach) -> AllReach1
#
#   gather(AllReach1, "Category", "Value", 2:ncol(AllReach1)) %>%
#     mutate(TableName = str_split_fixed(Category, " ", n = 2)[,1], Category = str_split_fixed(Category, " ", n = 2)[,2]) -> AllReach1
#
#   left_join(select(Contrib1, Categories, TableName), AllReach1, c("TableName" = "TableName")) %>%
#     spread(Category, Value) %>%
#     group_by(Categories, Percent) %>%
#     summarise(Contribution = sum(Contribution), Spend = mean(Spend), Reach = mean(Reach)) %>%
#     gather("Category", "Value", 3:5) %>%
#     arrange(desc(Category)) %>%
#     filter(Categories != "TV COMP") -> AllReach2
#
#   # Start ABCs
#   dcast(AllReach2, Percent ~ Categories + Category, fun.aggregate = sum, value.var = "Value") -> AllReach3
#   #AllReach3[c(1:101),] -> AllReach3
#   AllReach3$Percent <- NULL
#
#   # Reach
#   write_csv(AllReach3, "Reach Curve.csv")
#
#
#   AllReach3 -> totalmatrix
#   abcs <- list()
#   names(totalmatrix) <- rep(c("Contribution", "Reach", "Spend"), times = ncol(totalmatrix)/3)
#
#
#   dummy <- tribble(
#     ~term, ~estimate, ~std.error, ~statistic, ~p.value,
#     'a', 0, 0, 0, 0,
#     'b', 0, 0, 0, 0,
#     'c', 0, 0, 0, 0
#   )
#
#   # Loop Reach Curves to find ABCs
#   for (x in 1:(ncol(totalmatrix)/3)) {
#     # Find your ABCs
#     print(x)
#     data <- totalmatrix[,(1 + 3*(x - 1)):(3 + 3*(x - 1))]
#     model <- try(onls(Contribution ~ a/ (1 + b * ((Spend/1000)) ^ c),
#                       start  = list(a = 80, b = 1, c = -1),
#                       data = data,
#                       lower = c(a = 0.1, b = 0.1, c = -2)))
#
#     if(class(model) == "try-error") {
#       abcs[[x]] <- dummy
#       next;
#     }
#
#     m <- try({m <- broom::tidy(model)})
#     if(class(m) == "try-error") {
#       abcs[[x]] <- dummy
#       next;
#     }
#
#     abcs[[x]] <- m
#
#   }
#
#
#   abcs %>% bind_rows %>% select(estimate) -> abcs
#   data.frame(matrix(unlist(abcs), nrow = 3)) -> abcs
#   names(abcs) <- unique(AllReach2$Categories)
#
#   # Find real contributions
#   AllReach2 %>%
#     filter(Category == "Contribution") %>%
#     filter(Percent == 20) -> realcontributions
#
#   t(realcontributions[,4]) -> realcontributions
#
#   AllReach2 %>%
#     filter(Category == "Spend") %>%
#     filter(Percent == 20) -> realspend
#
#   t(realspend[,4]) -> realspend
#
#   # Adjust the A
#   mapply(Reach, abcs[1,], abcs[2,], abcs[3,], realspend[1,]) -> FirstContr
#   as.data.frame(t(FirstContr)) -> FirstContr
#   (realcontributions/FirstContr)*abcs[1,] -> AdjA
#   names(AdjA) <- names(abcs)
#   names(realcontributions) <- names(abcs)
#   names(realspend) <- names(abcs)
#   rbind(abcs, AdjA) -> ABCs
#   ABCs$Parameters <- c("A", "B", "C", "Adjusted A")
#   ABCs %>%
#     select(Parameters, 1:(ncol(ABCs) - 1)) -> ABCs
#   ABCs[,2:ncol(ABCs)] -> ABCs
#
#   mapply(Reach, ABCs[4,], ABCs[2,], ABCs[3,], realspend[1,]) -> FinalContr
#   as.data.frame(t(FinalContr)) -> FinalContr
#   names(FinalContr) <- names(ABCs)
#   rbind(ABCs, FinalContr) -> ABCs
#   as.data.frame(realspend) -> realspend
#   names(realspend) <- names(ABCs)
#   rbind(ABCs, realspend) -> ABCs
#   colABCs <- c("A", "B", "C", "Adjusted A", "Contribution", "Spend")
#   cbind(colABCs, ABCs) -> ABCs
#
#   if (multiplier > 0) {
#     # Revenue Calc
#     ABCs[5, c(2:ncol(ABCs))]*multiplier -> Revenue
#     rbind(ABCs[,c(2:ncol(ABCs))], Revenue) -> ABCs
#     # ROI Calc
#     ABCs[c(7),]/ABCs[c(6),] -> ROIs
#     rbind(ABCs, ROIs) -> ABCs
#     # mROI
#     mROIs <- ((mapply(Reach, ABCs[4,], ABCs[2,], ABCs[3,], ABCs[6,] + 10000)*multiplier) - (mapply(Reach, ABCs[4,], ABCs[2,], ABCs[3,], ABCs[6,])*multiplier))/10000
#     rbind(ABCs, mROIs) -> ABCs
#     # Labels
#     colABCs <- c("A", "B", "C", "Adjusted A", "Contribution", "Spend", "Revenue", "ROI", "mROI")
#     cbind(colABCs, ABCs) -> ABCs
#   } else {ABCs -> ABCs}
#
#   row.names(ABCs) <- NULL
#   return(ABCs)
# }




