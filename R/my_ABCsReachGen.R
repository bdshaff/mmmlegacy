ABCsReachGen <- function(Contrib,
                         fit,
                         fit_displayt1,
                         fit_displayt2,
                         fit_st,
                         fit_addressable,
                         fmi_data,
                         panel,
                         spend,
                         multiplier,
                         year,
                         From,
                         To) {
  names(fmi_data) <- tolower(names(fmi_data))

  # Through Decomp
  Contrib1 <-
    Contrib %>%
    gather("Year", "Contributions", 5:ncol(.)) %>%
    filter(Year == year) %>%
    group_by(variable, Categories, Model, Year) %>%
    summarise(Contributions = sum(Contributions, na.rm = TRUE)) %>%
    mutate(TMP = 1) %>%
    filter(grepl("_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])", variable) | grepl("_([D][0-9][0-9][P][0-9])", variable)) %>%
    arrange(variable) %>%
    mutate(variable1 = variable)

  Contrib1a <-
    Contrib1 %>%
    filter(grepl("_([D][0-9][0-9][P][0-9])", variable)) %>%
    mutate(Type = "L")

  stockvars <- Contrib1a$variable
  Contrib1a$variable1 <- tolower(stockvars)

  Contrib1b <-
    Contrib1 %>%
    filter(grepl("_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])", variable)) %>%
    mutate(Type = "R")

  Contrib1 <- rbind(Contrib1a, Contrib1b)

  spend <-
    spend %>% filter(model_agg == panel | model_agg == toupper(panel))

  Contrib1 <- inner_join(Contrib1, spend, by = c("Categories" = "media_agg"))
  Contrib1 <- Contrib1 %>%
    filter(!is.na(Spend)) %>%
    filter(Contributions > 0)


  # Timeframe
  timeframe <- as.data.frame(seq.Date(from = as.Date(From), to = as.Date(To), by = "week"))
  names(timeframe) <- "week"
  if (length(timeframe) > 104) {
    stop("You need to select only 104 weeks")
  }

  # Parameters
  param1 <- "_([E][0-9][R][0-9][P][0-9][0-9][D][0-9][0-9])" # formatting for adresponse
  param2 <- "_([D][0-9][0-9][P][0-9])" # formatting for adstock
  param3 <- "_([d][0-9][0-9][p][0-9])" # formatting for adstock


  Allparams <-
    Contrib1$variable1 %>%
    str_replace_all(param1, "") %>%
    str_replace_all(param3, "") %>%
    str_replace_all(., c("_log" = "", "PANEL_[A-Z][A-Z][A-Z]_M_" = "")) %>%
    tolower(.) %>%
    str_replace_all(paste(panel, "_", sep = ""), "")

  Contrib1$LU <- Allparams

  # Select Data
  AllData <- as.data.frame(fmi_data[, colnames(fmi_data) %in% Allparams]) # grab columns to adresponse
  AllData <- cbind(select(fmi_data, vehicle, week), AllData)
  AllData <- AllData %>% filter(vehicle == panel)
  AllData <- gather(AllData, "Category", "Value", 3:ncol(AllData))

  DataJoin <- left_join(Contrib1[, c("variable", "Categories", "Model", "LU")], AllData, by = c("LU" = "Category"))
  Data <- dcast(DataJoin, week ~ Model + variable, fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "Value")

  # 104 Weeks
  Data <- left_join(timeframe, Data)
  names(Data)[1] <- "Week"
  Data$Week <- NULL
  neg_num <- as.data.frame(paste("Week", seq(from = -52, to = -1)))
  names(neg_num) <- "Week"
  pos_num <- as.data.frame(paste("Week", seq(from = 1, to = 52)))
  names(pos_num) <- "Week"
  Week <- rbind(neg_num, pos_num)
  Data <- cbind(Week, Data)


  # Parameters
  Contrib1$E <- str_extract(Contrib1$variable1, "([E][0-9])")
  Contrib1$E <- str_replace(Contrib1$E, "E", "")
  Contrib1$R <- str_extract(Contrib1$variable1, "([R][0-9])")
  Contrib1$R <- str_replace(Contrib1$R, "R", "")
  Contrib1$P <- str_extract(Contrib1$variable1, "[P][0-9][0-9]")
  Contrib1$P <- str_replace(Contrib1$P, "P", "")
  Contrib1$P <- as.integer(Contrib1$P)
  Contrib1$P <- Contrib1$P / 7
  Contrib1$D <- str_extract(Contrib1$variable1, "[D][0-9][0-9]")
  Contrib1$D <- str_replace(Contrib1$D, "D", "")
  Contrib1$p <- str_extract(Contrib1$variable1, "[p][0-9]")
  Contrib1$p <- str_replace(Contrib1$p, "p", "")
  Contrib1$d <- str_extract(Contrib1$variable1, "[d][0-9][0-9]")
  Contrib1$d <- str_replace(Contrib1$d, "d", "")

  # Input
  Contrib1$TableName <- paste(Contrib1$Model, Contrib1$variable, sep = "_")
  Contrib1 <- Contrib1 %>% arrange(TableName)

  # # Revised
  # if (revise == "NO") {
  #   Contrib1 = Contrib1
  # } else {
  #   Contrib1 = revise_table
  # }

  Input <- Contrib1[, c("TableName", "Spend", "Contributions", "Type", "E", "R", "P", "D", "p", "d", "Categories")]
  Input$P[c(is.na(Input$P))] <- Input$p[c(!is.na(Input$p))]
  Input$D[c(is.na(Input$D))] <- Input$d[c(!is.na(Input$d))]
  Input <-
    rbind(
      Input$Type,
      Input$Spend,
      Input$Contributions,
      Input$E,
      Input$R,
      Input$P,
      Input$D,
      Input$Categories,
      Input$TableName
    ) %>% as.data.frame(.)


  Input <- rbind(Input[c(1:3), ], NA, Input[c(4:7), ], NA, NA, NA, Input[c(8:9), ])

  Col <- rbind("R/L/P", "100% Spend", "100% Contribution", "DMA", "EF", "RF", "Period", "Decay", "Alpha", "Beta", "Power", "Category", "Var Name")

  Input <- cbind(Col, Input)

  row.names(Input) <- NULL
  Input <- as.data.frame(Input)
  names(Input) <- as.character(unlist(Input[1, ]))
  Input <- Input[-1, ]
  row.names(Input) <- NULL


  fit <- as.data.frame(fit)
  row.names(fit) <- NULL

  # # Write out
  # if (revise != "NO") {
  #   write_csv(Contrib1, sprintf("%s RC Info Revised.csv", year))
  # } else {
  #   write_csv(Contrib1, sprintf("%s RC Info.csv", year))
  # }

  # write_csv(Input, "Input.csv")
  # write_csv(Data, "Data.csv")
  # write_csv(fit, "fit.csv")
  # write_csv(fit_displayt1, "fit_displayt1.csv")
  # write_csv(fit_displayt2, "fit_displayt2.csv")
  # write_csv(fit_st, "fit_st.csv")
  # write_csv(fit_addressable, "fit_addressable.csv")



  # Read in the template files
  Data <- Data %>%
    mutate(Week = as.character(Week)) %>%
    as_tibble()
  # Data = read_csv("Data.csv")
  Input <- apply(Input, 2, function(x) as.character(x)) %>% as.data.frame(stringsAsFactors = FALSE)
  # Input = read.csv("Input.csv", check.names = FALSE, stringsAsFactors = FALSE)



  # fit = read_csv("fit.csv")
  # fit_displayt1 = read_csv("fit_displayt1.csv")
  # fit_displayt2 = read_csv("fit_displayt2.csv")
  # fit_st = read_csv("fit_st.csv")
  # fit_addressable = read_csv("fit_addressable.csv")
  #
  # Generate Reach
  names(fit)[1] <- "NTV"


  ################################################################################################
  ################################################################################################
  ################################################################################################

  AllReach <- ReachCurveCalc(Data, Input)
  Percent <- c(0:100)
  AllReach1 <- cbind(Percent, AllReach)

  AllReach1 <-
    gather(AllReach1, "Category", "Value", 2:ncol(AllReach1)) %>%
    mutate(
      TableName = str_split_fixed(Category, " ", n = 2)[, 1],
      Category = str_split_fixed(Category, " ", n = 2)[, 2]
    ) %>%
    filter(!is.na(Value))

  AllReach2 <-
    left_join(select(Contrib1, Categories, TableName), AllReach1, c("TableName" = "TableName")) %>%
    spread(Category, Value) %>%
    group_by(Categories, Percent) %>%
    summarise(Contribution = sum(Contribution, na.rm = TRUE), Spend = mean(Spend, na.rm = TRUE), Reach = mean(Reach, na.rm = TRUE)) %>%
    gather("Category", "Value", 3:5) %>%
    arrange(desc(Category)) %>%
    filter(Categories != "TV COMP")

  # Start ABCs
  AllReach3 <- dcast(AllReach2, Percent ~ Categories + Category, fun.aggregate = function(x) sum(x, na.rm = TRUE), value.var = "Value")
  # AllReach3[c(1:101),] -> AllReach3
  AllReach3$Percent <- NULL

  # Reach
  # write_csv(AllReach3, "Reach Curve.csv")


  totalmatrix <- AllReach3
  abcs <- list()
  names(totalmatrix) <- rep(c("Contribution", "Reach", "Spend"), times = ncol(totalmatrix) / 3)


  dummy <- tribble(
    ~term, ~estimate, ~std.error, ~statistic, ~p.value,
    "a", 0, 0, 0, 0,
    "b", 0, 0, 0, 0,
    "c", 0, 0, 0, 0
  )

  # Loop Reach Curves to find ABCs
  for (x in 1:(ncol(totalmatrix) / 3)) {
    # Find your ABCs
    print(x)
    data <- totalmatrix[, (1 + 3 * (x - 1)):(3 + 3 * (x - 1))]
    model <- try(onls(Contribution ~ a / (1 + b * ((Spend / 1000))^c),
      start = list(a = 80, b = 1, c = -1),
      data = data,
      lower = c(a = 0.1, b = 0.1, c = -2)
    ))

    if (class(model) == "try-error") {
      abcs[[x]] <- dummy
      next
    }

    m <- try({
      m <- broom::tidy(model)
    })
    if (class(m) == "try-error") {
      abcs[[x]] <- dummy
      next
    }

    abcs[[x]] <- m
  }


  abcs <- abcs %>%
    bind_rows() %>%
    select(estimate)
  abcs <- data.frame(matrix(unlist(abcs), nrow = 3))
  names(abcs) <- unique(AllReach2$Categories)

  # Find real contributions
  realcontributions <-
    AllReach2 %>%
    filter(Category == "Contribution") %>%
    filter(Percent == 20)

  realcontributions <- t(realcontributions[, 4])

  realspend <-
    AllReach2 %>%
    filter(Category == "Spend") %>%
    filter(Percent == 20)

  realspend <- t(realspend[, 4])

  # Adjust the A
  FirstContr <- mapply(Reach, abcs[1, ], abcs[2, ], abcs[3, ], realspend[1, ])
  FirstContr <- as.data.frame(t(FirstContr))
  AdjA <- (realcontributions / FirstContr) * abcs[1, ]



  names(AdjA) <- names(abcs)
  names(realcontributions) <- names(abcs)
  names(realspend) <- names(abcs)
  ABCs <- rbind(abcs, AdjA)
  ABCs$Parameters <- c("A", "B", "C", "Adjusted A")
  ABCs <-
    ABCs %>%
    select(Parameters, 1:(ncol(ABCs) - 1))
  ABCs <- ABCs[, 2:ncol(ABCs)]

  if (is.null(dim(ABCs))) {
    ABCs <- as.data.frame(ABCs)
    names(ABCs) <- names(abcs)
  }

  FinalContr <- mapply(Reach, ABCs[4, ], ABCs[2, ], ABCs[3, ], realspend[1, ])
  FinalContr <- as.data.frame(t(FinalContr))
  names(FinalContr) <- names(ABCs)
  ABCs <- rbind(ABCs, FinalContr)
  realspend <- as.data.frame(realspend)
  names(realspend) <- names(ABCs)
  ABCs <- rbind(ABCs, realspend)
  colABCs <- c("A", "B", "C", "Adjusted A", "Contribution", "Spend")
  ABCs <- cbind(colABCs, ABCs)

  if (multiplier > 0) {
    # Revenue Calc
    Revenue <- ABCs[5, c(2:ncol(ABCs))] * multiplier
    ABCs <- rbind(ABCs[, c(2:ncol(ABCs))], Revenue)
    # ABCs = rbind(ABCs, Revenue)
    # ROI Calc
    ROIs <- ABCs[c(7), ] / ABCs[c(6), ]
    ABCs <- rbind(ABCs, ROIs)
    # mROI
    mROIs <- ((mapply(Reach, ABCs[4, ], ABCs[2, ], ABCs[3, ], ABCs[6, ] + 10000) * multiplier) - (mapply(Reach, ABCs[4, ], ABCs[2, ], ABCs[3, ], ABCs[6, ]) * multiplier)) / 10000
    ABCs <- rbind(ABCs, mROIs)
    # Labels
    colABCs <- c("A", "B", "C", "Adjusted A", "Contribution", "Spend", "Revenue", "ROI", "mROI")
    rownames(ABCs) <- colABCs
    # ABCs[,1] = colABCs
  } else {
    ABCs <- ABCs
  }


  return(list(
    ABCs = ABCs,
    Data = Data,
    Input = Input,
    Reach = AllReach3,
    Contribution = Contrib1
  ))
}
