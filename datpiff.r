library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(googleVis)
raw.data = read.csv('./datpiff.csv')

str(raw.data)

# Graphs to plot:
# Frequencies of mixtape award levels over time (official v unofficial)
# Top artists by award winning mixtapes
# Top users by award winning mixtapes (official v unofficial)
# Top mixtape year by year (official v unnoficial)
# Number of artists with award winning mixtapes by year


# Additional Cleaning
data = raw.data %>% 
  mutate(., artist = trimws(artist),
         title = trimws(title),
         host = trimws(host),
         official = ifelse(official==1,'official', 'unofficial'),
         release_date = mdy(release_date), 
         listens = as.numeric(gsub(',','',listens)),
         year = year(release_date),
         promoted = ifelse(banner=='','Unpromoted','Promoted')) %>% 
  select(., added_by, artist, award, banner, downloads, listens, official, rating_score, release_date, title, tracks, views, year, promoted)


# Adding missing data for significant mixtapes (gold or above)
x = data %>% filter(., is.na(release_date)) %>% 
  arrange(., desc(downloads)) %>% 
  select(., artist, award, downloads, title, release_date, year)

rd = mdy(c('11/26/2015', 
      '12/25/2017', 
      '12/09/2015', 
      '09/02/2016', 
      '01/26/2018',
      '09/18/2017',
      '10/11/2016',
      '09/26/2016',
      '02/28/2017',
      rep(NA,5)))

y = year(rd)

v = c(11924119,
      11782002,
      6026125,
      9075182,
      8605041,
      2100770,
      1929806,
      1678848,
      1477422,
      rep(NA,5))

x = x %>% 
  mutate(., release_date = rd,
         views = v,
         year = y)

for (i in c(1:length(data$title))){
  if (is.na(data$release_date[i])){
    data$release_date[i] <- x$release_date[match(data$title[i],x$title)]
    data$views[i] <- x$views[match(data$title[i],x$title)]
    data$year[i] <- year(data$release_date[i])
  }
}

# Frequencies of mixtape awards by year
award.trend = data %>% 
  group_by(., award, year) %>% 
  summarise(., freq=n(), na.rm=T) %>% 
  ggplot(., mapping = aes(x = year, y = freq)) +
  geom_point(aes(color = award)) + 
  geom_line(aes(color = award))
award.trend

# Top mixtape by year - plot
top.annual = data %>% 
  group_by(., year, official) %>% 
  slice_max(order_by = downloads, n = 1) %>% 
  data.frame(.) %>% 
  ggplot(., mapping = aes(x = year, y = downloads)) +
  geom_bar(aes(fill = official),stat='identity')

top.annual

# table
top.annual.df = data %>% 
  group_by(., year) %>% 
  slice_max(order_by = downloads, n = 5) %>%
  arrange(., year) %>% 
  data.frame(.) 
top.annual.df

 # Top artists by awarded mixtapes (official only)
top.artists = data %>% 
  filter(., official == 1) %>% 
  group_by(., artist) %>% 
  summarise(., total.dl = sum(downloads)) %>% 
  arrange(., desc(total.dl)) %>% 
  top_n(.,10)
top.artists

# Top 10 artists for every year (official only)
top.artist.dl.ann = data %>%
  filter(., official == "official") %>%
  group_by(., artist, year) %>% 
  summarise(., total_dl = sum(downloads),mixtapes=n()) %>% 
  group_by(., year) %>% 
  slice_max(order_by = total_dl, n = 3)
data.frame(top.artist.dl.ann)

# Top 10 artists annually by average downloads per mixtape
top.avg.dl.ann = data %>%
  filter(., official == 1) %>%
  group_by(., artist, year) %>% 
  summarise(., avg_dl = mean(downloads), mixtapes=n()) %>%
  group_by(., year) %>% 
  slice_max(order_by = avg_dl, n = 3)


top.artist.dl.ann %>% 
  ggplot(., mapping = aes(x = year, y = total_dl)) +
  geom_point(aes(color = artist))


top.avg.dl.ann %>% 
  ggplot(., mapping = aes(x = year, y = avg_dl)) +
  geom_point(aes(color = artist))

# Median Downloads for mixtapes (given bronze+ status) by upload year
med.dl.trend = data %>% 
  group_by(., year, official) %>% 
  summarise(., med_dl = median(downloads)) %>% 
  ggplot(., mapping = aes(x = year, y = med_dl)) +
  geom_point(aes(color = official)) +
  geom_line(aes(color = official))

med.dl.trend

# Box Plots of download distribution annually (official only)
dl.box = data %>% 
  ungroup(.) %>% 
  filter(., official=="official", !is.na(year)) %>% 
  ggplot(., aes(x = factor(year), y = downloads, fill = factor(promoted))) + 
  geom_boxplot() + 
  geom_jitter(width = 0.25, alpha = 0.1) +
  coord_cartesian(ylim=c(25000,1000000))
  
dl.box

# Ratio of downloads to listens over time
dl.listen <- data %>% 
  group_by(., year) %>% 
  summarise(., dl = mean(downloads, na.rm=TRUE), listens = mean(listens, na.rm=TRUE)) %>% 
  mutate(., dl_to_listen = dl/listens) %>% 
  ggplot(., mapping = aes(x = year, y = dl_to_listen)) +
  geom_line()

dl.listen

# downloads and listens by number of views
views_per_dl <- data %>% 
  ggplot(., aes(x = views, y = downloads)) +
  geom_point(aes(color = year))

views_per_dl

data %>% filter(., year==2012, banner=='') %>% 
  arrange(desc(downloads))

data %>% ggplot(., aes(x = release_date, y = downloads)) +
  geom_point(aes(color = official))
   

# Download counts by percentile ranges over time
ptile_trend <- data %>% 
  filter(., official == 'official') %>% 
  group_by(., year) %>% 
  mutate(., percentile = ntile(desc(downloads), 100)) %>% 
  mutate(., percentile = cut(percentile, breaks = c(0,1,10,25,50,75,100))) %>% 
  group_by(., year, percentile) %>% 
  summarise(., Avg_Downloads = mean(downloads)) %>% 
  ggplot(aes(x=year, y = Avg_Downloads)) +
  geom_point(aes(color = factor(percentile))) + 
  geom_smooth(aes(color = factor(percentile)), se = F)

ptile_trend
  
# Downloads and Listens per view over time
dl_list_view_trend <- data %>% 
  mutate(dl_per_view = downloads/views, listens_per_view = listens/views) %>%
  filter(official == 'official', downloads > 0, listens > 0, views > 0, dl_per_view <=1, listens_per_view<=1) %>% 
  arrange(desc(dl_per_view)) %>% 
  ggplot(., aes(x = release_date)) + 
  geom_point(aes(y = dl_per_view, color = 'red'), alpha = 0.3) + 
  geom_smooth(aes(y = dl_per_view, color = 'red'), se = F) + 
  geom_point(aes(y = listens_per_view, color = 'blue'), alpha = 0.3) +
  geom_smooth(aes(y = listens_per_view, color = 'blue'), se = F) 

dl_list_view_trend

# Downloads of an artists most recent mixtape
addcount = data %>%
  filter(official=='official') %>% 
  group_by(artist) %>% 
  arrange(artist,release_date) %>% 
  mutate(., tape_num = row_number()) %>% 
  top_n(1,release_date) %>% 
  top_n(1,downloads) %>% 
  mutate(., tape_num = ifelse(tape_num >= 5,'5+', as.character(tape_num))) %>% 
  ggplot(aes(x=release_date)) +
  geom_point(aes(y = downloads, color = factor(tape_num))) + 
  geom_smooth(aes(y = downloads, color = factor(tape_num)), se=F) +
  coord_cartesian(xlim = as.Date(c('2014-01-01','2019-12-31')), ylim = c(25000,1000000))

addcount

# Percentage breakdown of award levels year over year
award_dist <- data %>% 
  mutate(award = ifelse(downloads >=5E5, 'Double Platinum', award)) %>% 
  group_by(year, award, promoted) %>% 
  summarise(count=n()) %>% 
  group_by(year, promoted) %>% 
  mutate(portion = count / sum(count), 
         award = factor(award , levels=c('Double Platinum', 'Platinum', 'Gold', 'Silver', 'Bronze') )) %>% 
  ggplot(aes(x=year)) +
  geom_area(aes(y = portion, fill = factor(award))) +
  facet_wrap(~promoted) +
  coord_cartesian(xlim=c(2014,2019))

award_dist




