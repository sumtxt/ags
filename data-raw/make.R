library(devtools)
library(tidyverse)
library(wiesbaden)
library(lubridate)
library(readxl)


# Bundesland Lookup Table 
#########################

bl <- read_csv("./data-raw/rawdata/bundeslaender.csv")
bl <- as.data.frame(bl)

# Example Data: BTW in Saxony
##############################

btw_sn <- retrieve_data("14111KJ001", 
    genesis=c("db"="regio"))

btw_sn <- btw_sn %>% 
    filter(str_sub(KREISE,0,2)=='14') %>% 
    select(KREISE,STAG,WAHL01_val,WAHL09_val) %>% 
    rename(
        district=KREISE,
        year=STAG,
        voters=WAHL01_val, 
        valid=WAHL09_val
        ) %>% 
    mutate(
        year=year(dmy(year)),
        district=as.character(district),
        )

btw_sn <- btw_sn %>% 
    filter(year < 2021) %>% 
    filter( (year < 2008) & (str_sub(district, 0,3) %in% c(141:143)) |
            (year > 2008) & (str_sub(district, 0,3) %in% c(145:147)) )


# Umstiegsschlüssel BBSR to AGS 2019
#####################################

source("./data-raw/xd19.R")
source("./data-raw/xd20.R")
source("./data-raw/xm19.R")
source("./data-raw/xm20.R")

use_data(btw_sn, internal=FALSE, overwrite = TRUE)
use_data(bl,xd19,xd20,xm19,xm20,internal=TRUE, overwrite = TRUE)
