library(nycflights13)
library(tidyverse)
  
  head(flights)
  flights %>% names
  flights %>% filter(arr_delay > 120) 
  flights %>% filter(dest %in% c('IAH','HOU'))
  
  flights %>% filter(between(month, 7, 9)) 
  
  summary(flights)
  
  flights %>% arrange(desc(is.na(dep_time)))
  flights %>% arrange(desc(arr_delay))
  
  flights %>% select(starts_with('dep'), starts_with('arr'))
  flights %>% select(ends_with('time'), ends_with('delay'),-starts_with('sched_dep'))
  
  vars <- c("year", "month", "day", "dep_delay", "arr_delay")
  flights %>% select(vars)
  
  select(flights, contains("TIME", ignore.case = FALSE))
  
  
  #differences x - lag(x)
  #when values change (x =! lag(x))
  #cumsum(), cumprod(), cummin(), cummax(), cummean()

  x <- 1:10
  cumsum(x)
  cumprod(x)
  cummin(x)
  cummax(x)
  cummean(x)
  
  min_rank(x)
  min_rank(desc(x))
#row_number(), dense_rank(), percent_rank(), cume_dist(), ntile()

##### mutate
  flights %>% mutate(
    dep_time = (dep_time%/%100)*60 + (dep_time%%100),
    sched_dep_time = (sched_dep_time %/%100)*60 + (sched_dep_time%%100)
    )
  
  flights$air_time 
  flights %>% transmute(arr_time - dep_time)
  
  flights %>% filter(min_rank(desc(dep_delay))<=10)
  flights %>% top_n(n = 10, wt = dep_delay)
  
##### summarise  
  flights %>% summarise(delay = mean(dep_delay, na.rm = TRUE))
  flights %>% group_by(year, month, day) %>% dplyr::summarise(delay = mean(dep_delay, na.rm =TRUE))
  
  # count
  flights %>% group_by(dest) %>% 
    dplyr::summarise(
      count = n(),
      dist = mean(distance, na.rm = TRUE),
      delay = mean(arr_delay, na.rm = TRUE)
      ) %>% 
  filter(delay, count > 20, dest != "HNL") %>% 
    ggplot(aes(x = dist, y = delay)) +
    geom_point(aes(size = count, alpha = 0.5)) +
    geom_smooth(se=FALSE)
  
  not_cancelled <- flights %>% 
    filter(!is.na(dep_delay), !is.na(arr_delay))
  
  delays <- not_cancelled %>% 
    group_by(tailnum) %>% 
    dplyr::summarise(
      delay = mean(arr_delay,na.rm = TRUE),
      n = n()
    )
  
  # geom_freqpoly
  ggplot(data = delays, mapping = aes(x = delay)) + 
    geom_freqpoly(binwidth = 10)
  
  # conditional
  not_cancelled %>% 
    group_by(year, month, day) %>% 
    dplyr::summarise(
      avg_delay1 = mean(arr_delay),
      avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
    )
  
  # SD
  summarise(distance_sd = sd(distance))
  
  # min max
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
  
  # first, last
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )
  
  flights %>% dplyr::count(dest)
  
  # n distinct
  flights %>% summarise(carriers = n_distinct(carrier))
  
  # tally
  not_cancelled  %>% 
    dplyr::count(tailnum, wt = distance)
  
  not_cancelled  %>% 
    dplyr::group_by(tailnum) %>% 
    dplyr::summarise(sum = sum(distance))

  not_cancelled  %>% 
    dplyr::group_by(tailnum) %>% 
    dplyr::tally()
  
  not_cancelled %>% 
    dplyr::group_by(year, month, day) %>% 
    dplyr::summarise(n_early = sum(dep_time < 500))
  
  flights %>% group_by(flight) %>% 
    dplyr::summarise(n = n(), 
                     percentage_on_time = sum(arr_time == sched_arr_time, na.rm = TRUE)/n(),
                     percentage_early = sum(arr_time < sched_arr_time, na.rm = TRUE)/n()
                     )
  
  
  flights %>%
    dplyr::mutate(dep_date = lubridate::make_datetime(year, month, day)) %>%
    dplyr::group_by(dep_date) %>% 
    dplyr::summarise(cancelled = sum(is.na(dep_delay)),
              n = n(),
              meandelay = mean(dep_delay, na.rm = T),
              propcancelled = cancelled/n
              ) %>% 
    ggplot(aes(x= propcancelled, y = meandelay)) +
    geom_point()
  
  ## cumany 
  cumany (c(FALSE, TRUE, FALSE, FALSE))
  cumany (c(FALSE, FALSE, TRUE, FALSE))
    
  flights %>%
    group_by(tailnum) %>%
    filter(!cumany(arr_delay>60)) %>%
    tally(sort = TRUE) %>% arrange(tailnum)
  
  flights %>% group_by(tailnum) %>% 
    filter(!cumany(arr_delay>60)) %>%
    dplyr::summarise(sum = sum(!cumany(arr_delay>60)))%>% 
    arrange(tailnum)

##### mutate
  flights_sml %>% 
    group_by(year, month, day) %>%
    filter(rank(desc(arr_delay)) < 10)
  
  popular_dests <- flights %>% 
    group_by(dest) %>% 
    filter(n() > 365)
  
  popular_dests %>% 
    filter(arr_delay > 0) %>% 
    mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
    select(year:day, dest, arr_delay, prop_delay)
  
  flights %>% group_by(tailnum) %>% 
    dplyr::summarise(propontime = sum(arr_delay < 30, !is.na(arr_delay))/n()) %>% 
    arrange((propontime))
                                    
