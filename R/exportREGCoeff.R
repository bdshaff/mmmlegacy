exportREGCoeff <- function(Formula, DF, Panel, model, nameDF, kpi_var) {

  # Formula = stringformat
  # DF = DF
  # Panel = 1
  # model = model
  # nameDF = "alt_hisp1"
  # kpi_var = "ALT_retail_sales_HISP"

  str_split_fixed(Formula, pattern = " ~ ", n = 2)[1] -> KPI
  kpi_var -> OKPI
  as.data.frame(DF[, colnames(DF) %in% KPI]) -> KPIVARS
  KPI -> names(KPIVARS)
  DF %>%
    left_join(select(nonfmi_dataP, region, month, kpi_var), by = c("Brand" = "region", "Month" = "month")) %>%
    select(kpi_var) -> OKPIVARS
  OKPI -> names(OKPIVARS)
  str_split_fixed(Formula, pattern = " ~ ", n = 2)[2] -> VARS
  unlist(strsplit(VARS, " \\+ ")) -> VARS
  as.data.frame(DF[, colnames(DF) %in% VARS]) -> VARSCOLS

  if (Panel == 1) {
    cbind(select(DF, Month), OKPIVARS, KPIVARS, VARSCOLS) -> reg_group
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[, 1] -> reg_group$YEAR
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[, 2] -> reg_group$MONTH
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
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[, 1] -> reg_group$YEAR
    str_split_fixed(reg_group$Month, pattern = "-", n = 3)[, 2] -> reg_group$MONTH
    paste0(reg_group$YEAR, "M", reg_group$MONTH) -> reg_group$Month
    reg_group$YEAR <- NULL
    reg_group$MONTH <- NULL
    paste(toupper(reg_group$Brand), reg_group$Month, sep = " - ") -> reg_group$Month
    reg_group$Brand <- NULL
    names(reg_group) <- str_replace_all(names(reg_group), c("\\<D\\>" = "_D_", "\\<\\*>" = "_M_", "\\{" = "", "\\}" = "", "\\<A\\>" = "_A_", "\\<S\\>" = "_S_"))
    names(reg_group)[2:ncol(reg_group)] <- toupper(names(reg_group)[2:ncol(reg_group)])
    # names(reg_group)[3] <- paste("LOG(", names(reg_group)[3], ")", sep = "")
    write.csv(reg_group, paste0(nameDF, "_reg.csv"), row.names = FALSE)
    as.data.frame(model$coefficients) -> coeffs
    toupper(row.names.default(coeffs)) -> coeffs$Variable
    names(coeffs)[1] <- "Coefficient"
    str_replace_all(coeffs$Variable, c("(INTERCEPT)" = "C", "\\(" = "", "\\)" = "")) -> coeffs$Variable
    coeffs %>%
      mutate("NA1" = 0, "NA2" = 0, "NA3" = 0) %>%
      select(Variable, Coefficient, NA1, NA2, NA3) -> coeffs
    coeffstop <- as.data.frame(matrix(data = NA, nrow = 7, ncol = 5))
    names(coeffstop) <- names(coeffs)
    bind_rows(coeffstop, coeffs) -> coeffs
    names(coeffs) <- NULL
    coeffs[6, 1] <- "Variable"
    coeffs[6, 2] <- "Coefficient"
    coeffs[7, ] <- c("C", 0, 0, 0, 0)
    write.csv(coeffs, paste0(nameDF, "_eq.csv"), row.names = FALSE)
  }
}
