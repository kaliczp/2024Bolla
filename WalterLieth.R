## Import 1999–2022
library(readxl)
library(climatol)
excelsheets <- c("M01", "M03", "M09", "M15", "M16", "M17", "M19", "M21")
pdf()
for(currsheet in excelsheets) {
stationname <- as.character(read_excel("csapadék és hőmérséklet.xlsx", range = paste0(currsheet,"!C1"), col_names = FALSE))
rawstation <- read_excel("csapadék és hőmérséklet.xlsx", range = paste0(currsheet,"!D7:O48"), col_names = FALSE, col_types = "numeric")
mydiagwl(as.data.frame(rawstation[c(4,42,41),]),cols=NULL,stname=paste(currsheet,stationname),per="1999-2022",mlab="en", addtemp=as.numeric(rawstation[37,]), addprec=as.numeric(rawstation[1,])/2)
}
dev.off()

## Import 2017-21 és 2022
## Temperature
excelcols <- c("U", "X", "AA", "AD", "AG")
MonthlyTemp <- list()
for(tti in 1:length(excelcols)) {
    currcol <- excelcols[tti]
## Read raw cols
rawtemp <- read_excel("csapadék és hőmérséklet.xlsx", range = paste0("Havi T!",currcol,"2:",currcol,"73"), col_names = FALSE, col_types = "numeric")
## 
    MonthlyTemp[[tti]] <- matrix(as.numeric(rawtemp[,1,drop = TRUE]), ncol = 12, byrow = TRUE)
    colnames(MonthlyTemp[[tti]]) <- month.abb
    row.names(MonthlyTemp[[tti]]) <- 2017:2022
}

## Precipitation
excelcols <- c("K", "N", "Q", "T", "W")
MonthlyPrec <- list()
for(tti in 1:length(excelcols)) {
    currcol <- excelcols[tti]
## Read raw cols
rawtemp <- read_excel("csapadék és hőmérséklet.xlsx", range = paste0("Havi Csapadék!",currcol,"2:",currcol,"73"), col_names = FALSE, col_types = "numeric")
## 
    MonthlyPrec[[tti]] <- matrix(as.numeric(rawtemp[,1,drop = TRUE]), ncol = 12, byrow = TRUE)
    colnames(MonthlyPrec[[tti]]) <- month.abb
    row.names(MonthlyPrec[[tti]]) <- 2017:2022
}

## WL table
WLlist <- list()
for(tti in 1:length(MonthlyPrec)) {
WLlist[[tti]] <- rbind(colMeans(MonthlyPrec[[tti]][1:5,]),
                       apply(MonthlyTemp[[tti]][1:5,], 2, max),
                       apply(MonthlyTemp[[tti]][1:5,], 2, min)
                       )
}
StationNames <- c("M01 Beech and M03 Oak", "M15 Pine and M16 Oak", "M17 Beech", "M19 Black locust", "M21 Turkey oak")
names(WLlist) <- StationNames

pdf(width = 15.5 / 2.54, height = 6.5 / 2.54, pointsize = 9)
par(family = "serif")
tti <- 1
par(mfrow = c(1,2),mar = c(2, 4, 4, 4), las = 1)
mydiagwl(WLlist[[tti]], cols=NULL, stname = names(WLlist)[tti], per="2017-2021", mlab="en")
## Keep
par(new = TRUE)
plot(1:12, , type = "n",
     xlim = c(0,12), ylim = c(-10,60),
     xaxs = "i", yaxs = "i", 
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n",
     bty = "n")
Prec2022Akt <- MonthlyPrec[[tti]]["2022",]/2
Prec2022Akt[Prec2022Akt > 50] <- 50 + (Prec2022Akt[Prec2022Akt > 50] - 50) / 20
lines(1:12-0.5, Prec2022Akt, col = "#005ac8", lwd = 3)
lines(1:12-0.5, MonthlyTemp[[tti]]["2022",], col = "#e81800", lwd = 3)
dev.off()

## figures without axis label
pdf("Remainingwoaxislab.pdf", width = 13 / 2.54, height = 13 / 2.54, pointsize = 9)
par(mfrow = c(2,2), mar = c(2, 4, 4, 4), las = 1, family = "serif")
for(tti in 2:length(WLlist)){
    par(mar = c(2, 2.5, 4, 2.5), las = 1)
    mydiagwl(WLlist[[tti]], cols=NULL, stname = names(WLlist)[tti], per="2017-2021", mlab="en",
             axislabels = FALSE)
    ## Keep
    par(new = TRUE)
plot(1:12, , type = "n",
     xlim = c(0,12), ylim = c(-10,60),
     xaxs = "i", yaxs = "i", 
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n",
     bty = "n")
Prec2022Akt <- MonthlyPrec[[tti]]["2022",]/2
Prec2022Akt[Prec2022Akt > 50] <- 50 + (Prec2022Akt[Prec2022Akt > 50] - 50) / 20
lines(1:12-0.5, Prec2022Akt, col = "#005ac8", lwd = 3)
lines(1:12-0.5, MonthlyTemp[[tti]]["2022",], col = "#e81800", lwd = 3)
}
dev.off()
