if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, lubridate, janitor, data.table, hrbrthemes)
base_url = "https://eugene.craigslist.org/search/sss?query=speakers&sort=rel&srchType=T"
craiglist = read_html(base_url)
speakers =
  craiglist %>%
  html_elements(".result-hood , .result-date , .result-price , .hdrlnk")
speakers = html_text(speakers)
head(speakers, 20) 
head(as.data.frame(t(matrix(speakers, nrow=5))))
dates = as.Date(speakers, format = '%b %d') 
idates = which(!is.na(dates))
speakers_dt =
  rbindlist(lapply(
    seq_along(idates),
    function(i) {
      start = idates[i]
      end = ifelse(i!=length(idates), idates[i+1]-2, tail(idates, 1))
      data.table(t(speakers[start:end]))
    }
  ), fill = TRUE)
speakers_dt
names(speakers_dt) = c('date', 'description', 'price', 'location')
speakers_dt[, ':=' (date = as.Date(date, format = '%b %d'),
                    price = as.numeric(gsub('\\$|\\,', '', price)))]
speakers_dt[date>Sys.Date(), date := date - years(1)]
speakers_dt = speakers_dt[!is.na(price)]
speakers_dt
ggplot(speakers_dt, aes(date, price)) +
  geom_point(aes(fill = price), show.legend = FALSE,
             shape = 21, colour = 'black', size = 2, stroke = 0.1) +
  scale_y_comma() +
  scale_fill_viridis_c(option = 'magma', begin = 0.3, end = 0.9) +
  labs(title = 'Speakers for sale near Eugene, OR',
       caption = 'Source: Craigslist',
       x = 'Listing date', y = 'Price (USD)') +
  theme_modern_rc()
