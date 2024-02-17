## From Jose A. Guijarro's diagwl function from climatol package
## https://cran.r-project.org/web/packages/climatol
## https://climatol.eu/
mydiagwl <- function (dat, cols = 1:6, format = "%Y-%m-%d", yeari = NA, yearf = NA, 
    stname = "", alt = NA, per = "", mlab = "", shem = FALSE, 
    p3line = FALSE, ...) 
{
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(mar = c(4, 4, 5, 4), las = 1, new = FALSE)
    pcol = "#005ac8"
    tcol = "#e81800"
    pfcol = "#79e6e8"
    sfcol = "#09a0d1"
    if (length(mlab) != 12) {
        if (mlab == "es") 
            mlab = c("E", "F", "M", "A", "M", "J", "J", "A", 
                "S", "O", "N", "D")
        else if (mlab == "en") 
            mlab = c("J", "F", "M", "A", "M", "J", "J", "A", 
                "S", "O", "N", "D")
        else mlab = c(1:12)
    }
    if (is.null(cols)) {
        if (ncol(dat) < 12) 
            stop("Data frame has less than 12 columns!")
        if (ncol(dat) == 13) 
            dat <- dat[, 1:12]
        nr <- nrow(dat)
        switch(nr, stop("At least two monthly rows (average precipitation and temperature)\n  must be supplied"), 
            cat("Warning: When only monthly precipitation and mean temperature\n         are provided, no frost risk will be drawn.\n"), 
            cat("Warning: When no absolute minimum temperatures are provided,\n         likely frost will not be drawn.\n"))
    }
    else {
        if (length(cols) == 4) {
            dates <- as.Date(dat[, cols[1]], format = format)
            dat <- data.frame(YY = as.integer(strftime(dates, 
                "%Y")), MM = as.integer(strftime(dates, "%m")), 
                DD = as.integer(strftime(dates, "%d")), dat[, 
                  cols[2:4]])
        }
        else dat <- dat[, cols]
        z <- range(dat[, 1])
        if (is.na(yeari)) 
            yeari <- z[1]
        else if (yeari < z[1]) 
            yeari <- z[1]
        else if (yeari > z[1]) 
            d <- d[d[, 1] >= yeari, ]
        if (is.na(yearf)) 
            yearf <- z[2]
        else if (yearf > z[2]) 
            yearf <- z[2]
        else if (yearf < z[1]) 
            d <- d[d[, 1] <= yearf, ]
        per = sprintf("%d-%d", yeari, yearf)
        ny <- yearf - yeari + 1
        datcli <- matrix(NA, 4, 12)
        datcli[1, ] <- round(aggregate(dat[, 4], list(dat[, 2]), 
            sum)$x/ny, 1)
        datcli[2, ] <- round(aggregate(dat[, 5], list(dat[, 2]), 
            mean)$x, 1)
        datcli[3, ] <- round(aggregate(dat[, 6], list(dat[, 2]), 
            mean)$x, 1)
        datcli[4, ] <- round(aggregate(dat[, 6], list(dat[, 2]), 
            min)$x, 1)
        dat <- datcli
        nr <- nrow(dat)
    }
    dat <- as.matrix(dat)
    if (shem) {
        m1 <- dat[, 1:6]
        m2 <- dat[, 7:12]
        dat <- cbind(m2, m1)
        mlab <- c(mlab[7:12], mlab[1:6])
    }
    p <- dat[1, ]
    if (nr == 2) 
        tm <- dat[2, ]
    else tm <- apply(dat[2:3, ], 2, mean)
    pmax <- max(p)
    ymax <- 60
    if (pmax > 300) 
        ymax <- 50 + 10 * floor((pmax + 100)/200)
    ymin <- min(-1.5, min(tm))
    if (ymin < -1.5) {
        ymin = floor(ymin/10) * 10
        labT <- paste(ymin)
        labP <- ""
        if (ymin < -10) {
            for (i in (ymin/10 + 1):-1) {
                labT <- c(labT, i * 10)
                labP <- c(labP, "")
            }
        }
        labT <- c(labT, "0", "10", "20", "30", "40", "50", "")
        labP <- c(labP, "0", "20", "40", "60", "80", "100", "300")
    }
    else {
        labT <- c("0", "10", "20", "30", "40", "50", "")
        labP <- c("0", "20", "40", "60", "80", "100", "300")
    }
    if (ymax > 60) {
        for (i in 6:(ymax/10 - 1)) {
            labT <- c(labT, "")
            labP <- c(labP, 100 * (2 * i - 7))
        }
    }
    plot(0:13 - 0.5, c(tm[12], tm[1:12], tm[1]), xlim = c(0, 
        12), ylim = c(ymin, ymax), type = "n", xaxs = "i", yaxs = "i", 
        xaxp = c(0, 12, 12), xlab = "", ylab = "", xaxt = "n", 
        yaxt = "n", bty = "n")
    lmin <- ymin
    if (lmin == -1.5) 
        lmin = 0
    axis(2, ((lmin/10):(ymax/10)) * 10, labels = labT, col.axis = tcol)
    axis(4, ((lmin/10):(ymax/10)) * 10, labels = labP, col.axis = pcol)
    mtext("C", 2, col = tcol, las = 1, line = 3, adj = 0, at = 55)
    mtext("mm", 4, col = pcol, las = 1, line = 3, adj = 1, at = 55)
    abline(0, 0)
    abline(50, 0)
    if (is.na(alt)) 
        mtext(stname, line = 2, adj = 0)
    else mtext(paste(stname, " (", alt, " m)", sep = ""), line = 2, 
        adj = 0)
    mtext(per, line = 1, adj = 0)
    mtext(paste(round(mean(tm), 1), " C        ", round(sum(p)), 
        " mm", sep = ""), line = 1, adj = 1)
    x <- 0:13 - 0.5
    p2 <- c(p[12], p[1:12], p[1])
    if (p3line) {
        yl3 <- c(p[12], p[1:12], p[1])/3
        yl3[yl3 > 50] <- 50
    }
    if (pmax <= 100) {
        xl <- x
        yl <- c(p[12], p[1:12], p[1])/2
        n2 <- 14
    }
    else {
        xp <- numeric(30)
        yp <- numeric(30)
        xl <- numeric(25)
        yl <- numeric(25)
        n <- 0
        n2 <- 0
        gr <- FALSE
        if (p2[1] > 100) {
            n <- n + 1
            xp[n] <- x[1]
            yp[n] <- 50
            n <- n + 1
            xp[n] <- x[1]
            yp[n] <- 50 + (p2[1] - 100)/20
            n2 <- n2 + 1
            xl[n2] <- x[1]
            yl[n2] <- 50
            gr <- TRUE
        }
        else {
            n2 <- n2 + 1
            xl[n2] <- x[1]
            yl[n2] <- p2[1]/2
        }
        for (i in 2:14) {
            if (gr) {
                n <- n + 1
                if (p2[i] > 100) {
                  xp[n] <- x[i]
                  yp[n] <- 50 + (p2[i] - 100)/20
                }
                else {
                  xp[n] <- x[i - 1] + (100 - p2[i - 1])/(p2[i] - 
                    p2[i - 1])
                  yp[n] <- 50
                  n2 <- n2 + 1
                  xl[n2] <- xp[n]
                  yl[n2] <- 50
                  n <- n + 1
                  xp[n] <- NA
                  yp[n] <- NA
                  n2 <- n2 + 1
                  xl[n2] <- x[i]
                  yl[n2] <- p2[i]/2
                  gr <- FALSE
                }
            }
            else {
                if (p2[i] > 100) {
                  n <- n + 1
                  xp[n] <- x[i - 1] + (100 - p2[i - 1])/(p2[i] - 
                    p2[i - 1])
                  yp[n] <- 50
                  if (xl[n2] != x[i - 1]) {
                    n2 <- n2 + 1
                    xl[n2] <- x[i - 1]
                    yl[n2] <- p2[i - 1]/2
                  }
                  n2 <- n2 + 1
                  xl[n2] <- xp[n]
                  yl[n2] <- 50
                  n <- n + 1
                  xp[n] <- x[i]
                  yp[n] <- 50 + (p2[i] - 100)/20
                  gr <- TRUE
                }
                else {
                  n2 <- n2 + 1
                  xl[n2] <- x[i]
                  yl[n2] <- p2[i]/2
                }
            }
        }
        if (!is.na(yp[n])) {
            n <- n + 1
            xp[n] <- xp[n - 1]
            yp[n] <- 50
            n2 <- n2 + 1
            xl[n2] <- 12.5
            yl[n2] <- 50
        }
        polygon(xp[1:n], yp[1:n], col = pcol, border = pcol)
    }
    pi <- approx(xl[1:n2], yl[1:n2], n = 66)$y
    ti <- approx(x, c(tm[12], tm[1:12], tm[1]), n = 66)$y
    ti[ti < 0] <- 0
    d <- pi - ti
    xi <- (1:66)/5 - 0.7
    xw <- subset(xi, d > 0)
    y1 <- subset(pi, d > 0)
    y2 <- subset(ti, d > 0)
    if (length(xw) > 0) 
        segments(xw, y1, xw, y2, col = pcol, lty = 1, lwd = 1)
    xw <- subset(xi, d < 0)
    y1 <- subset(pi, d < 0)
    y2 <- subset(ti, d < 0)
    if (length(xw) > 0) 
        segments(xw, y1, xw, y2, col = tcol, lty = 3, lwd = 2)
    if (nr > 2) {
        for (i in 1:12) if (dat[3, i] <= 0) 
            rect(i - 1, -1.5, i, 0, col = sfcol)
        if (nr > 3) 
            for (i in 1:12) {
                if (dat[4, i] <= 0 && dat[3, i] > 0) 
                  rect(i - 1, -1.5, i, 0, col = pfcol)
            }
        else mtext("(Likely frost months not provided)", 1, line = 1.5)
    }
    else mtext("(No monthly frost risk provided)", 1, line = 1.5)
    lines(xl[1:n2], yl[1:n2], col = pcol, lwd = 2)
    if (p3line) 
        lines(x, yl3)
    lines(x, c(tm[12], tm[1:12], tm[1]), col = tcol, lwd = 2)
    if (nr > 2) {
        mtext(formatC(max(as.matrix(dat[2, ])), digits = 1, format = "f"), 
            2, las = 1, line = 2, at = 35)
        mtext(formatC(min(as.matrix(dat[3, ])), digits = 1, format = "f"), 
            2, las = 1, line = 2, at = 15)
    }
    for (i in 0:13) segments(i, 0, i, -1.5)
    mtext(mlab, 1, las = 1, line = 0.5, adj = 0.5, at = x[2:13])
    invisible()
}
