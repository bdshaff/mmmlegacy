DateConvertMean = function (x, DMA, dma, Date, j, Month)
{
  DMAs = list()
  for (dma in unique(x$DMA)) {
    daily = list()
    d <- x %>% filter(DMA == dma) %>% select(-DMA)
    dates = d$Date
    nums = d %>% select(-Date)
    daily[[1]] = nums
    daily[[1]]$Date = dates
    for (j in 1:6) {
      daily[[j + 1]] = nums
      daily[[j + 1]]$Date = dates + j
    }
    result = bind_rows(daily)
    result$DMA = dma
    DMAs[[dma]] = result
  }
  everything = bind_rows(DMAs)
  Monthly <-
    everything %>%
    mutate(Month = floor_date(Date, "month")) %>%
    select(-Date) %>%
    group_by(DMA, Month) %>%
    summarise_if(is.numeric, function(x) mean(x, na.rm = TRUE))
  return(Monthly)
}
