## Import
library(readxl)
library(climatol)
excelsheets <- c("M01", "M03", "M09", "M15", "M16", "M17", "M19", "M21")
pdf()
for(currsheet in excelsheets) {
stationname <- as.character(read_excel("csapadék és hőmérséklet.xlsx", range = paste0(currsheet,"!C1"), col_names = FALSE))
rawstation <- read_excel("csapadék és hőmérséklet.xlsx", range = paste0(currsheet,"!D7:O48"), col_names = FALSE, col_types = "numeric")
diagwl(as.data.frame(rawstation[c(4,42,41),]),cols=NULL,stname=stationname,per="1999-2022",mlab="en")
}
dev.off()
