library(XML)
library(dplyr)
library(lubridate)

setwd("/Users/irinabusurkina/Downloads/xml")
getwd()


xml <- xmlParse(file = "07_09_2019_03_32_56.xml")
xml_data <- xmlToList(data)


title        <- xpathApply(xml,"//item/title",xmlValue)
description  <- xpathApply(xml,"//item/description",xmlValue)
pubDate      <- xpathApply(xml,"//item/pubDate",xmlValue)
source       <- xpathApply(xml,"//item/source",xmlValue)
place        <- xpathApply(xml,"//item/place",xmlValue)
url          <- xpathApply(xml,"//item/url",xmlValue)
genre        <- xpathApply(xml,"//item/genere",xmlValue)
type         <- xpathApply(xml,"//item/type",xmlValue)
period       <- xpathApply(xml,"//item/period",xmlValue)


df <- data.frame(cbind(title, description, pubDate, source, 
                       place, url, genre, type, period))

class(df)
df$title <- vapply(df$title, paste, collapse = ", ", character(1L))
df$description <- vapply(df$description, paste, collapse = ", ", character(1L))
df$pubDate <- vapply(df$pubDate, paste, collapse = ", ", character(1L))
df$source <- vapply(df$source, paste, collapse = ", ", character(1L))
df$place <- vapply(df$place, paste, collapse = ", ", character(1L))
df$url <- vapply(df$url, paste, collapse = ", ", character(1L))
df$genre <- vapply(df$genre, paste, collapse = ", ", character(1L))
df$type <- vapply(df$type, paste, collapse = ", ", character(1L))
df$period <- vapply(df$period, paste, collapse = ", ", character(1L))

write.csv(all_2011, file = "2011.csv")

tenth <- read.csv(file = "10th.csv")
eleventh <- read.csv(file = "11th.csv")
thirteen <- read.csv(file = "13th.csv")
twelth <- read.csv(file = "12th.csv")
#2018
a_2018 <- first %>% slice(1:389)
#2017
b_2017 <- first %>% slice(390:485)
a_2017 <- second %>% slice(1:419)
all_2017 <- rbind(b_2017, a_2017)
#2016
a_2016 <- second %>% slice(420:500)
b_2016 <- third %>% slice(1:488)
c_2016 <- forth %>% slice(1:38)
all_2016 <- rbind(a_2016, b_2016, c_2016)
#2015
a_2015 <- third %>% slice(489:500)
b_2015 <- forth %>% slice(39:500)
c_2015 <- fiths %>% slice(1:500)
d_2015 <- sixth %>% slice(1:211)
all_2015 <- rbind(a_2015, b_2015, c_2015, d_2015)
#2014
a_2014 <- sixth %>% slice(212:500)
b_2014 <- seventh %>% slice(1:500)
c_2014 <- eigth %>% slice(1:500)
d_2014 <- ninth %>% slice(1:140)
all_2014 <- rbind(a_2014, b_2014, c_2014, d_2014)
#2013
a_2013 <- ninth %>% slice(141:500)
b_2013 <- tenth %>% slice(1:500)
c_2013 <- eleventh %>% slice(1:8)
all_2013 <- rbind(a_2013, b_2013, c_2013)
#2012
a_2012 <- eleventh %>% slice(9:439)
#2011
a_2011 <- eleventh %>% slice(439:500)
b_2011 <- twelth %>% slice(1:500)
c_2011 <- thirteen %>% slice(1:248)
all_2011 <- rbind(a_2011, b_2011, c_2011)

#хз чего там
temp = list.files(pattern = ".csv")
myfiles = lapply(temp, read.delim)
head(myfiles)

data  <- lapply(files, function(x) {
  temp <- read_xml(x) %>% xml_find_all("//ns2:opendataField")
  cols <- xml_attr(xml_find_all(temp, "//ns2:opendataField"), "key")
  rows <- xml_attr(xml_find_all(temp, "//ns2:opendataField"), "value")
  out  <- data.frame(rows, row.names = cols)
  names(out) <- x
  out
})
do.call(cbind, data)

temp = list.files(pattern="*.xml")
myfiles = lapply(temp, read.delim)
head(myfiles)
xml <- xmlParse(myfiles) 
