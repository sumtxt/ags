library(devtools)
library(readr)

bl <- read_csv("./data-raw/bundeslaender.csv")
bl <- as.data.frame(bl)

# Essential not to break get_bundesland()
bl <- bl[sort(bl$id),]

use_data(bl, internal=TRUE, overwrite=TRUE)
