library(tidyverse)

D_individual = read_csv("http://johnpalmer.github.io/MICA2018-Slides/data/NYC_Crime_Map_2017_NovDec.csv")

D = D_individual %>% group_by(ADDR_PCT_CD, OFNS_DESC) %>% count()

