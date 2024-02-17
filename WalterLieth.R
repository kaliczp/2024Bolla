## Import
library(readxl)
rawstation <- read_excel("csapadék és hőmérséklet.xlsx", range = "M01!D10:O48", col_names = FALSE, col_types = "numeric")
library(climatol)
diagwl(as.data.frame(rawstation[c(1,39,38),]),
       cols=NULL,est="M01",alt=100,per="1999-2022",mlab="en")
