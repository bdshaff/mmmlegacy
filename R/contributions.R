contributions <- function(reg_name, cat_table="var categorization table.xlsx",lag_rollback=FALSE,
                          min_ref = NULL, max_ref = NULL, mean_ref = NULL, print_summary=TRUE,
                          remove_base=FALSE, remove_constant=FALSE, start_date=NULL,end_date=NULL,
                          imputeMissingValues=FALSE,aggregation_period="CalendarYear",FY_Begins=4,
                          custom_period_table="customPeriod.csv",mQED=FALSE,using_plm=FALSE,plm_coefficents=NULL,
                          plm_is_additive=TRUE,displayMeanKPI=FALSE){
  options("scipen"=100, "digits"=4)
  reg_group_file<-paste0(reg_name,"_reg.csv",sep="")
  reg_eq_file<-paste0(reg_name,"_eq.csv",sep="")
  tailsList <- list()


  # test if aggregation_period is valid
  if(!aggregation_period %in% c("CalendarYear", "FiscalYear", "HalfYear", "QuarterYear", "CustomPeriod")){
    stop('aggregation_period must be one of: "CalendarYear" "FiscalYear" "HalfYear" "QuarterYear" "CustomPeriod"')
  }

  # IMPORT OF REGRESSOR GROUP
  if(!using_plm && !mQED){
    suppressMessages(suppressWarnings(reg_gr<-readr::read_csv(reg_group_file,n_max=3)))
    # if(names(reg_gr[,2])==names(reg_gr[,3]))names(reg_gr[,3])<-paste0(names(reg_gr[,3]),"word",sep="")
    if(stringr::str_detect(names(reg_gr)[3],"LOG\\(")){model.type<-"LOG" } else {model.type<-"LIN" }
    n<-base::rep("d",ncol(reg_gr)-1)
    n<-base::paste0(n,collapse="")
    c<-base::paste0("c",n,collapse="")
    suppressMessages(suppressWarnings(reg_gr<-readr::read_csv(reg_group_file,col_types =c )))
    reg_gr<-reg_gr[,-3]
  }
  if(using_plm){
    reg_gr <- reg_name[,-3]
    reg_gr <- reg_gr[,c(1:2,base::ncol(reg_gr),3:(base::ncol(reg_gr)-1))]
    names(reg_gr) <- stringr::str_to_upper(base::names(reg_gr))
    if(plm_is_additive){model.type<-"LOG" }else{model.type<-"LIN"}
  }
  if(mQED){
    reg_group_file<-paste0(reg_name,".xlsx",sep="")

    reg_gr <- read_excel(reg_group_file, sheet = "Transformed Data", skip = 5)%>%
      filter(`Included Period`=="Yes")%>%
      select(-`Fixed Independents`,-`Included Period`)
    names(reg_gr) <- stringr::str_to_upper(base::names(reg_gr))
    model.type<-"LIN"
  }




  if(base::max(min_ref,max_ref,mean_ref,0)>base::ncol(reg_gr)){
    stop("Error: Reference points indicated are not in dataset.")
  }

  #if custom period is used, import custom_period_table
  if(aggregation_period=="CustomPeriod"){

    if(!base::class(custom_period_table) %in% c("character","data.frame")){
      stop("custom_period_table must be either a data.frame or path to a .csv file")}


    if(base::class(custom_period_table)=="character"){
      cPdf <- suppressMessages(suppressWarnings(readr::read_csv(custom_period_table))) %>%
        base::as.data.frame(stringsAsFactors=FALSE) %>%
        dplyr::transmute(Date=as.Date(lubridate::parse_date_time(.[[1]],orders=c("dmy","mdy"))),
                         Period=as.character(.[[2]]))}

    if(base::class(custom_period_table)=="data.frame"){
      cPdf <- custom_period_table %>%
        dplyr::transmute(Date=as.Date(.[[1]]),
                         CustomPeriod=as.character(.[[2]]))}


    if(base::class(cPdf) != "data.frame" || base::dim(cPdf)[2]!=2){
      stop("custom_period_table failed to read
it must be a data.frame with 2 columns
or a path to a .csv file with two columns
of which the first is dates that match model
dates and the second is the name of period
which the date belongs.
current custom_period_table has ",base::dim(cPdf)[2]," columns
and is a ",base::class(cPdf))}

  }

  #Figure out if the model was pooled across groups and if so,
  #seperate dataset based on the groups
  #add them seperated to a list

  if(!using_plm && !mQED){
    if(base::sum(stringr::str_detect(reg_gr[[1]]," - "))==base::length(reg_gr[[1]])){
      panel_marker<-1
      reg_gr<-base::cbind(base::t(base::as.data.frame(stringr::str_split(reg_gr[[1]]," - ",2))),reg_gr[,2:base::ncol(reg_gr)])
      base::row.names(reg_gr)<-NULL
      base::names(reg_gr)<-c("Group","Date",base::names(reg_gr[3:base::ncol(reg_gr)]))
      groups<-base::unique(reg_gr$Group)
      reg_gr_list<-base::list()
      for(p in 1:base::length(groups)){
        reg_gr[reg_gr$Group==groups[p],-1]%>%
          base::list()->reg_gr_list[p]}
    }else{
      panel_marker<-0
      reg_gr_list<-base::list(reg_gr)
      groups<-"TOTAL"
    }}


  if(mQED){
    panel_marker<-0
    reg_gr_list<-base::list(reg_gr)
    groups<-"TOTAL"

  }


  if(using_plm){
    # only for plm model
    panel_marker<-1
    base::names(reg_gr)<-c("Group","Date",stringr::str_to_upper(base::names(reg_gr[3:base::ncol(reg_gr)])))
    # reg_gr[reg_gr[,3]==0,3] <- 1

    groups<-base::unique(reg_gr$Group)
    reg_gr_list<-base::list()
    for(p in 1:base::length(groups)){
      reg_gr[reg_gr$Group==groups[p],-1]%>%
        base::list()->reg_gr_list[p]}

  }
  missingValuesList <-base::list() #create list for missing values, used later on..

  for(q in 1:base::length(reg_gr_list)){
    reg_gr<-base::as.data.frame(reg_gr_list[q])
    base::names(reg_gr)<-base::names(reg_gr_list[[q]])

    # Assume first column is date
    base::names(reg_gr)[1]<-"Date"
    if(base::sum(base::colSums(base::sapply(reg_gr,is.na))==base::nrow(reg_gr)))next()

    #Convert Date field depending on panel model or not and weekly/monthly (eviews exports dates differently depending on this)

    if(!using_plm && !mQED){
      reg_gr$Date <- base::as.Date(lubridate::parse_date_time(reg_gr$Date,orders=c("dmy","mdy","ym")))
      if(lubridate::day(reg_gr$Date+1) %>% base::mean()==1)  reg_gr$Date <- reg_gr$Date+1
    }else{reg_gr$Date <- base::as.Date(reg_gr$Date)}
    # print(names(reg_gr))
    # peel away data at beginning and end that is missing
    reg_gr <- reg_gr[base::min(base::which( reg_gr[,2] != 0 )) : base::max( base::which( reg_gr[,2] != 0 )),]
    # for(r in 3:ncol(reg_gr)) reg_gr <- reg_gr[min( which ( !is.na(reg_gr[,r]))) : max( which( !is.na(reg_gr[,r]) )),]

    #Reduce the dataframe to only include dates that are wanted, if specified
    if(!base::is.null(start_date))reg_gr <- reg_gr[reg_gr[,"Date"]>=start_date,]
    if(!base::is.null(end_date))reg_gr <- reg_gr[reg_gr[,"Date"]<=end_date,]


    # This replaces missing values with the nearest present value in ther same column

    missingValues <- base::list() #to track what values are missing in the dataset
    counterMissingValues <- 1
    for(b in 2:base::ncol(reg_gr)){

      incomplete<-base::is.na(reg_gr[,b])
      complete<-!incomplete
      for(i in (1:base::length(incomplete))[incomplete]){
        k<-base::which(base::abs(reg_gr$Date[complete]-reg_gr$Date[i]) == base::min(base::abs(reg_gr$Date[complete] - reg_gr$Date[i])))
        base::suppressWarnings(k <- min(k,na.rm = T))

        j<-reg_gr[complete,b]
        if(imputeMissingValues) reg_gr[i,b]<-j[k]
        missingValues[[counterMissingValues]] <- base::as.data.frame(base::t(base::c(base::as.character(groups)[q],
                                                                                     base::as.Date(reg_gr$Date[i]),
                                                                                     base::names(reg_gr)[b],j[k])),
                                                                     stringsAsFactors=FALSE)
      }
    }
    if(!imputeMissingValues)reg_gr <- reg_gr[stats::complete.cases(reg_gr),]
    #create dataframe with missing values to display at end
    if(base::length(missingValues)>1) {
      missingValues <- dplyr::bind_rows(missingValues)
      base::names(missingValues) <- base::c("Group","Date","Variable","ImputeValue")
      missingValues$Date <- base::as.Date(base::as.numeric(missingValues$Date),origin = "1970-01-01")
      missingValuesList[[q]] <-missingValues
    }
    if(base::length(missingValues)==1) {
      missingValues <- missingValues[[1]]
      base::class(missingValues)
      base::names(missingValues) <- base::c("Group","Date","Variable","ImputeValue")
      missingValues$Date <- base::as.Date(base::as.numeric(missingValues$Date),origin = "1970-01-01")
      missingValuesList[[q]] <-missingValues
    }


    #Take out minimum values for adjustment of base
    if(model.type=="LIN"){
      ref_base_correction <- 0
      if(!base::is.null(min_ref)) ref_base_correction <- base::c(ref_base_correction,base::apply(base::as.data.frame(dplyr::select(reg_gr,min_ref)),MARGIN=2,FUN=min))
      if(!base::is.null(max_ref)) ref_base_correction <- base::c(ref_base_correction,base::apply(base::as.data.frame(dplyr::select(reg_gr,max_ref)),MARGIN=2,FUN=max))
      if(!base::is.null(mean_ref)) ref_base_correction <- base::c(ref_base_correction,base::apply(base::as.data.frame(dplyr::select(reg_gr,mean_ref)),MARGIN=2,FUN=mean))
      if(base::length(ref_base_correction)!=1){ref_base_correction <- ref_base_correction[-1]}}

    # MIN
    reg_gr = constraint(reg_gr, min_ref, min)
    # MAX
    reg_gr = constraint(reg_gr, max_ref, max)
    # MEAN
    reg_gr = constraint(reg_gr, mean_ref, mean)



    # IMPORT OF EQUATION
    # This reads in the equation a first time in order to establish the number of rows to skip for second read

    if(q==1){ ##seperate calc for first loop
      if(!using_plm && !mQED){
        suppressMessages(suppressWarnings(initial_eq <- readr::read_csv(reg_eq_file, skip = 0)))
        # Find position of the 'Variable' column header in the first colum
        variable_row = base::grep("Variable", initial_eq[[1]])
        # Read in the equation but skips the rows before 'Variable' appears in first column
        #       read_csv(reg_eq_file, skip = variable_row) %>%
        #         dplyr::select(1:2)%>%
        #         na.omit ->
        #         eq
        #       variable_n = grep("R-squared", eq[,1])
        #       eq<-eq[1:(min(variable_n)-1),]
        #       eq[[2]]<-as.double(eq[[2]])
        suppressMessages(suppressWarnings(readr::read_csv(reg_eq_file, skip = variable_row))) %>%
          stats::na.omit() ->
          eq
        eq[[2]]<-base::as.double(eq[[2]])
      }
      if(using_plm){
        # for plm only
        eq <-  plm_coefficents %>%
          dplyr::transmute(Variable=stringr::str_to_upper(base::rownames(plm_coefficents)),
                           Coefficient=base::as.double(plm_coefficents[[1]]))}
      if(mQED){
        # read_excel(reg_group_file, sheet = "Transformed Data", skip = 5)
        eq <-     read_excel(reg_group_file, sheet = "Model Definition",col_names=F)%>%
          slice(7)%>%
          select(-1,-2)%>%
          t()%>%
          as_tibble()%>%
          transmute(Coefficient=as.numeric(V1))%>%
          filter(!is.na(Coefficient))%>%
          mutate(Variable=names(reg_gr)[-(1:2)])

        eq <-  read_excel(reg_group_file, sheet = "Model Definition",col_names=F)%>%
          filter(X0=="Constant")%>%
          select(X1)%>%
          transmute(Coefficient=as.numeric(X1))%>%
          mutate(Variable="C")%>%
          bind_rows(eq)


      }

      # }
      # Categorization of variables from cat_table
      if("character" %in% base::class(cat_table)){
        if(stringr::str_sub(cat_table,-3,-1)=="csv"){
          categorization <- suppressMessages(suppressWarnings(readr::read_csv(cat_table))) %>%
            dplyr::select(1:2)
        }else{
          categorization <- readxl::read_excel(cat_table, sheet = 1) %>%
            dplyr::select(1:2)
        }}

      if("data.frame" %in% base::class(cat_table)){
        categorization <- cat_table
      }

      base::names(categorization) <- base::c("Variable","Agg1")

      categorization%>%
        dplyr::arrange(Variable) %>%
        dplyr::mutate(variable = stringr::str_trim(stringi::stri_trans_toupper(Variable,locale="")),
                      Agg1=stringr::str_trim(stringi::stri_trans_toupper(Agg1,locale=""))) ->
        categorization

      var_cat <- base::as.data.frame(names(reg_gr), stringsAsFactors = F)

      var_cat = base::apply(var_cat, 1, function(x){
        # If the column is Date, return Date.
        if(x == "Date"){
          return("Date")
        }
        #This function first uses string matching, but if there are more than one matches it uses fuzzy matching for closest match
        new_name = categorization$Agg1[stringr::str_detect(x, fixed(categorization$variable))]

        # print(new_name)
        # new_name = categorization$Agg1[str_detect(x, fixed(categorization$variable))][1]
        if(base::length(new_name)>1)new_name = categorization$Agg1[stringdist::amatch(x,categorization$variable, maxDist=Inf)]
        new_name=new_name[1]
        # print(new_name)
        # Otherwise, if we don't have a category call it "BASE".
        result = if(base::is.na(new_name)) "BASE" else new_name;
        return(result);
      })



      # Create coefficient DF
      eq_Coeff <- base::matrix(eq$Coefficient, ncol = 1, dimnames = base::list(eq$Variable))
    }##end  seperate calc for first loop

    # Initialize the contribution DF
    contribution <- reg_gr #%>% as.data.frame

    # Apply decomp equation
    # print(eq_Coeff)
    # print(reg_gr)
    if(model.type=="LIN"){
      if(remove_constant){reg_gr$C <- 0} else{reg_gr$C <- 1}
      # print(head(reg_gr))
      #Base adjustment for refrences
      # print(min_ref-1)
      coef_base_correction <- 0
      # eq_CoeffVec <- as.vector(eq_Coeff)
      if(!base::is.null(min_ref)) coef_base_correction <- base::c(coef_base_correction,eq_Coeff[min_ref-1])
      if(!base::is.null(max_ref)) coef_base_correction <- base::c(coef_base_correction,eq_Coeff[max_ref-1])
      if(!base::is.null(mean_ref)) coef_base_correction <- base::c(coef_base_correction,eq_Coeff[mean_ref-1])
      if(base::length(coef_base_correction)!=1){coef_base_correction <- coef_base_correction[-1]}
      # print("coef_base_correction")
      # print(coef_base_correction)
      # print(class(coef_base_correction))
      addToBase <- base::sum(coef_base_correction*ref_base_correction)
    } else{addToBase <- 0}
    # print(addToBase)
    for (i in base::rownames(eq_Coeff)) {
      if(model.type=="LIN"){
        # if(i != "C")
        contribution[, i] <- reg_gr[, i] * eq_Coeff[i,1]
        # print(i)
        # print(eq_Coeff[i,1])
        # print(head(reg_gr[, i]))
      }else{
        # View(reg_gr)
        # print(i)
        # print(names(reg_gr[, 2]))
        # print(names(reg_gr[, i]))
        # print(names(eq_Coeff[i,1]))
        if(i != "C") contribution[, i] <- reg_gr[, 2] - (reg_gr[, 2]/exp(reg_gr[, i] * eq_Coeff[i,1]))

      }
    }
    #Remove model bugs that create huge/tiny values
    infin<-purrr::map_df(contribution,is.infinite)
    contribution[base::as.matrix(infin)]<-0

    litenstor<-base::as.matrix(purrr::map_df(contribution,function(x){x>sum(reg_gr[,2])|x<(-sum(reg_gr[,2]))}))
    litenstor[,1]<-FALSE
    contribution[litenstor]<-0
    #XXXRemove constant if specified
    # Add column for base and calculate base
    # if(remove_constant) contribution$C <- NULL
    cols <- 3:base::ncol(contribution)
    if(model.type=="LOG"){
      contribution$BASE <- (contribution[, 2] - base::rowSums(contribution[, cols]))
      cols <- 3:base::ncol(contribution)}
    # Create second contribution dataframe for percentage
    # contribution2 <- contribution
    # contribution2[, cols] <- contribution[, cols]/contribution[, 2]

    if(q==1){ ##seperate calc for first loop
      if(model.type=="LOG"){
        var_cat_tbl = base::data.frame(variable = names(contribution),
                                       modelType=model.type,
                                       Categories = base::c(var_cat, "BASE"),
                                       stringsAsFactors = F)}
      else {
        var_cat_tbl = base::data.frame(variable = names(contribution),
                                       modelType=model.type,
                                       Categories = base::c(var_cat,"BASE"),
                                       stringsAsFactors = F)
      }


      # Indicate which variables have which refrence points in the summary categorization table
      var_cat_tbl$ReferencePoints <- ""
      var_cat_tbl$ReferencePoints[min_ref] <- "Min"
      var_cat_tbl$ReferencePoints[mean_ref] <- "Average"
      var_cat_tbl$ReferencePoints[max_ref] <- "Max"
      if(base::class(reg_name)=="character"){var_cat_tbl$Model<-reg_name} else{
        # var_cat_tbl$Model<-deparse(substitute(reg_name))
        var_cat_tbl$Model<-base::names(contribution)[2]
      }


    }##end seperate calc for first loop
    ############################################################
    #indicate lag in var_cat_tbl
    var_cat_tbl$LagRollback<-0
    if(lag_rollback==TRUE){
      if(q==1){ ##seperate calc for first loop
        lag_str<-c("(-1)","(-2)","(-3)","(-4)",
                   "(-5)","(-6)","(-7)","(-8)",
                   "(-9)","(-10)","(-11)",
                   "(-12)","(-13)","(-14)",
                   "(-15)","(-16)","(-17)","(-18)")

        for(p in 1:base::length(lag_str)){ #print(p)}
          var_cat_tbl$LagRollback[stringr::str_detect(var_cat_tbl$variable,lag_str[p])]<-p}
        var_cat_tbl$LagRollback[base::is.na(var_cat_tbl$LagRollback)]<-0

      }##end seperate calc for first loop
      ##Roll back lags
      for(f in 3:base::length(var_cat_tbl$LagRollback)){
        contribution[1:(base::nrow(contribution)-var_cat_tbl$LagRollback[f]),f]<-contribution[(1+var_cat_tbl$LagRollback[f]):base::nrow(contribution),f]
        contribution[(base::nrow(contribution)-var_cat_tbl$LagRollback[f]):base::nrow(contribution),f]<-contribution[(base::nrow(contribution)-var_cat_tbl$LagRollback[f]),f]

      }
    }

    ################################################################

    #Create order for presenting variables
    if(q==1){ ##seperate calc for first loop
      var_order<-base::unique(var_cat_tbl$Categories)
      var_order<-var_order[!stringr::str_detect(var_order,"BASE")]
      var_order<-base::c("agg_period_",var_order[1:2],"BASE",var_order[-(1:2)])
    }##end seperate calc for first loop

    if(q==1){ ##seperate calc for first loop
      var_order2<-base::unique(var_cat_tbl$variable)
      #var_order2<-var_order2[!stringr::str_detect(var_order,"BASE")]
      var_order2<-base::c("agg_period_",var_order2[1:2], var_order2[-(1:2)])
    }

    # "CalendarYear" "FiscalYear" "HalfYear" "QuarterYear" "CustomPeriod"

    #Set period for aggregation
    contribution <- contribution %>%
      tidyr::gather(variable, value, -Date) %>%
      dplyr::mutate(variable = base::as.character(variable))%>%
      dplyr::left_join(var_cat_tbl, by=c("variable"))  %>%
      dplyr::mutate(FiscalYear = FY(Date,FY_Begins)) %>%
      dplyr::mutate(HY = base::ifelse(lubridate::quarter(Date)<3,"1","2"),
                    QY=lubridate::quarter(Date))%>%
      dplyr::mutate(HalfYear = base::paste0(lubridate::year(Date)," H",HY),
                    QuarterYear = base::paste0(lubridate::year(Date)," Q",QY),
                    CalendarYear = lubridate::year(Date))
    contribution$QY <- NULL
    contribution$HY <- NULL


    #append custom period if set

    if(aggregation_period=="CustomPeriod"){
      contRow <- base::nrow(contribution)
      contribution <- dplyr::inner_join(contribution,cPdf, by="Date")

      if(base::nrow(contribution)!=contRow) stop("custom_period_table dates do not match model dates!")
    }

    contribution$agg_period_ <- contribution[,aggregation_period]
    # View(contribution)
    if(!displayMeanKPI){
      summary <- contribution %>%
        reshape2::dcast(agg_period_ ~ Categories, fun.aggregate=sum,value.var="value")%>%
        .[,var_order[-2]]}else{

          summary <- contribution %>%
            reshape2::dcast(Date+agg_period_ ~ Categories, fun.aggregate=sum,value.var="value")%>%
            .[,var_order[-2]] %>%
            dplyr::group_by(agg_period_)%>%
            dplyr::summarise_all(mean)

        }

    if(base::length(groups)==1){
      summary10 <- contribution
      summary10$Group <- "Brand"
      summary10 <- summary10 %>% group_by(Group, variable, FiscalYear) %>%
        summarise(cont = sum(value))%>%
        reshape2::dcast(Group+variable ~ FiscalYear, fun.aggregate=sum,value.var="cont")

      var_cat_tblPG <- var_cat_tbl %>%
        left_join(summary10, by = c(variable = "variable"))

      summary11 <- contribution %>%
        dplyr::select(-ReferencePoints) %>%
        dplyr::group_by(agg_period_, Date, variable) %>%
        dplyr::summarise(total=base::sum(value)) %>%
        tidyr::spread(variable, total)%>%
        .[,var_order2]
      # dplyr::mutate(BASE= BASE+addToBase)#%>%
      #rename(as.name(aggregation_period) = agg_period_)
      #contribution <- renameAggPeriod(contribution,aggregation_period)



      # if(remove_base) contribution$BASE <- 0
      #  contribution2<-contribution
      pg <- summary11
      summary11 <- summary11[, -(3:3)]

      summary11[,-(1:2)]<-summary11[,-(1:2)]/base::rowSums(summary11[,-(1:2)])
      summary11 <- gather(summary11, "variable", "value", 3:ncol(summary11))
      summary11$Group <- "Brand"
      summary11 <- summary11 %>% group_by(Group, variable, agg_period_) %>%
        summarise(cont = sum(value)/12)%>%
        reshape2::dcast(Group+variable ~ agg_period_, fun.aggregate=sum,value.var="cont")

      var_cat_tblPG2 <- var_cat_tbl %>%
        left_join(summary11, by = c(variable = "variable"))

      # summary11[,-(1:3)]<- summary11[,-(1:3)]/rowSums(summary11[,-(1:3)])
      #summary11$Group <- "Brand"
      #summary11 <- summary11 %>% group_by(Group, variable, FiscalYear) %>%
      # summarise(cont = sum(value)/12)%>%
      #reshape2::dcast(Group+variable ~ FiscalYear, fun.aggregate=sum,value.var="cont")

    }else{

      if(q==1){
        var_tbl <- vector("list", length = length(groups))
        var_tblPG <- vector("list", length = length(groups))
      }
      summary10 <- contribution
      summary10$Group <- groups[q]
      summary10 <- summary10 %>%  group_by(Group, variable, FiscalYear) %>%
        summarise(cont = sum(value))%>%
        reshape2::dcast(Group+variable ~ FiscalYear, fun.aggregate=sum,value.var="cont")

      pa_tb <- var_cat_tbl %>%
        left_join(summary10, by = c(variable = "variable"))
      var_tbl[[q]] <- pa_tb


      summary11 <- contribution %>%
        dplyr::select(-ReferencePoints) %>%
        dplyr::group_by(agg_period_, Date, variable) %>%
        dplyr::summarise(total=base::sum(value)) %>%
        tidyr::spread(variable, total) %>%
        .[,var_order2]

      #summary11 <-  summary11 %>% dplyr::mutate(BASE= BASE+addToBase)#%>%
      #rename(as.name(aggregation_period) = agg_period_)
      #contribution <- renameAggPeriod(contribution,aggregation_period)

      summary11 <- summary11[, -(3:3)]
      pg <- summary11
      # if(remove_base) contribution$BASE <- 0
      #  contribution2<-contribution

      summary11[,-(1:2)]<-summary11[,-(1:2)]/base::rowSums(summary11[,-(1:2)])

      summary11 <- gather(summary11, "variable", "value", 3:ncol(summary11))
      summary11$Group <- groups[q]
      summary11 <- summary11 %>% group_by(Group, variable, agg_period_) %>%
        summarise(cont = sum(value)/12)%>%
        reshape2::dcast(Group+variable ~ agg_period_, fun.aggregate=sum,value.var="cont")

      pa_tb2 <- var_cat_tbl %>%
        left_join(summary11, by = c(variable = "variable"))
      var_tblPG[[q]] <- pa_tb2

    }



    paultable <- contribution








    renameAggPeriod <- function(df, aggregation_period){
      varOrder2 <- base::c(aggregation_period,base::names(df)[-(1)])
      df[,aggregation_period] <- df$agg_period_
      df <- df[,varOrder2]
      return(df)
    }
    summary <- renameAggPeriod(summary,aggregation_period)
    if(remove_base) summary$BASE <- 0
    summary2<-summary
    # print(summary)
    summary2[,-(1:2)]<-summary2[,-(1:2)]/base::rowSums(summary2[,-(1:2)])

    #########################prepping for tails######################################################
    tailsList[[groups[q]]] <-  contribution
    tailsList[[groups[q]]]$Group <- groups[q]
    ###############################################################################











    contribution<- contribution %>%
      dplyr::select(-variable, -ReferencePoints) %>%
      dplyr::group_by(agg_period_, Date, Categories) %>%
      dplyr::summarise(total=base::sum(value)) %>%
      tidyr::spread(Categories, total)%>%
      .[,var_order] %>%
      dplyr::mutate(BASE= BASE+addToBase)#%>%
    #rename(as.name(aggregation_period) = agg_period_)
    contribution <- renameAggPeriod(contribution,aggregation_period)

    if(remove_base) contribution$BASE <- 0
    contribution2<-contribution

    contribution2[,-(1:3)]<-contribution2[,-(1:3)]/base::rowSums(contribution2[,-(1:3)])






    if(q==1){
      contribution_return<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],nrow=nrow(contribution),ncol=1,dimnames = base::list(base::c(),
                                                                                                                                                 base::c("Group"))),stringsAsFactors =FALSE),contribution)

      contribution2_return<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],nrow=nrow(contribution2),ncol=1,dimnames = base::list(base::c(),base::c("Group"))),stringsAsFactors =FALSE),
                                             contribution2)
      summary_return<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],
                                                                        nrow=nrow(summary),ncol=1,dimnames = base::list(base::c(),base::c("Group"))),stringsAsFactors =FALSE),summary)
      summary2_return<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],
                                                                         nrow=nrow(summary2),ncol=1,dimnames = base::list(base::c(),base::c("Group"))),stringsAsFactors =FALSE),summary2)
      if(print_summary==TRUE)base::print(var_cat_tbl)
    }else{
      contribution<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],
                                                                      nrow=nrow(contribution),ncol=1,dimnames = base::list(base::c(),base::c("Group"))),stringsAsFactors =FALSE),contribution)
      contribution2<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],
                                                                       nrow=nrow(contribution2),ncol=1,dimnames = base::list(base::c(),base::c("Group"))),stringsAsFactors =FALSE),contribution2)
      summary<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],
                                                                 nrow=nrow(summary),ncol=1,dimnames = base::list(base::c(),base::c("Group"))),stringsAsFactors =FALSE),summary)
      summary2<-dplyr::bind_cols(base::as.data.frame(base::matrix(data=groups[q],
                                                                  nrow=nrow(summary2),ncol=1,dimnames = base::list(base::c(),base::c("Group"))),stringsAsFactors =FALSE),summary2)

      contribution_return<-base::rbind(contribution_return,contribution)
      contribution2_return<-base::rbind(contribution2_return,contribution2)
      summary_return<-base::rbind(summary_return,summary)
      summary2_return<-base::rbind(summary2_return,summary2)
    }


  }##This ends the loop for every reg_gr in reg_gr_list
  if(print_summary==TRUE) base::print(summary2_return)

  if(base::length(groups)!=1){
    grouping1 <- base::names(summary_return)[2]
    summary_return[,-1]%>%
      dplyr::group_by_(grouping1)%>%
      dplyr::summarise_each(funs(sum))%>%
      base::cbind("Group"="TOTAL",.)->tmp
    if(print_summary==TRUE) base::print(tmp)
    tmp[,-(1:3)]<-tmp[,-(1:3)]/tmp[[3]]
    if(print_summary==TRUE) base::print(tmp)
  }

  #Display messages for missing values and dates

  if(base::length(missingValuesList)>0) {
    if(imputeMissingValues){
      base::message("The following datapoints were imputed due to missing values:\n",
                    printAndCapture(dplyr::bind_rows(missingValuesList)))

    }
    if(!imputeMissingValues){ base::message("The following periods were excluded due to missing values:\n",
                                            printAndCapture(dplyr::bind_rows(missingValuesList)),
                                            "\n\nRerun the function with imputeMissingValues=TRUE to impute the values above from nearest period.\n")
    }
  }

  base::message("\nDecomp start period:  ", base::min(contribution_return$Date))
  base::message("Decomp end period:    ", base::max(contribution_return$Date))

  #if(base::is.null(start_date) && base::is.null(end_date)) base::message("Set start_date and end_date to change the period.")
  #suppressWarnings(var_cat_tbl <- findAdStock(var_cat_tbl))


  # tailsPrep <- dplyr::bind_rows(tailsList) %>%
  #   dplyr::left_join(var_cat_tbl[,c("variable","adStock")],by="variable") %>%
  #   calculateTailsPlease() %>%
  #   dplyr::select(agg_period_ ,Group,Date,Categories,variable,Model,
  #          modelType,ReferencePoints,LagRollback,value,periodSumWithTails)
  # tailsPrep$KPI <- tailsList[[1]]$Categories[1]
  # tailsPrep <- renameAggPeriod(tailsPrep,aggregation_period)
  tailsPrep <- NULL

  if(base::length(groups)>1){
    var_cat_tblPG <- rbindlist(var_tbl)
    var_cat_tblPG2  <- rbindlist(var_tblPG)
  }
  var_cat_tblPG <- as.data.frame(var_cat_tblPG)
  var_cat_tblPG2 <- as.data.frame(var_cat_tblPG2)

  var_cat_tblPG <- var_cat_tblPG[, -c(2,4,6)]
  var_cat_tblPG2 <- var_cat_tblPG2[, -c(2,4,6)]

  return(base::list(contributionsTbl=contribution_return,
                    contributionsTblPerc=contribution2_return,
                    summary=summary_return,
                    summaryPerc=summary2_return,
                    variableTbl=var_cat_tbl,
                    varCont=var_cat_tblPG,
                    varContPct=var_cat_tblPG2,
                    tailsDF=tailsPrep))  #Return contribution DF from function
}
