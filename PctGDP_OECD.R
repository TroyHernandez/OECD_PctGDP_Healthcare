# PctGDP_OECD.R

# Data from:
# https://stats.oecd.org/viewhtml.aspx?datasetcode=SHA&lang=en
dat <- read.csv("PctGDP_OECD.csv")
dat2 <- dat[which(dat$Measure == "Share of gross domestic product" &
                    dat$Unit == "Percentage" &
                    dat$Provider == "All providers" &
                    dat$Function == "Current expenditure on health (all functions)" &
                    dat$Financing.scheme == "All financing schemes"),]
dat2 <- dat2[order(dat2$Country, dat2$Year),]
dat3 <- unique(dat2[, c("Country", "Year", "Value")])
# dat3 <- unique(dat2[, c("LOCATION", "TIME", "Value")])
Countries <- sort(unique(dat3$Country))
Years <- sort(unique(dat3$Year))
dat.final <- data.frame(matrix(NA, nrow = length(Countries), ncol = 11))
colnames(dat.final) <- Years
rownames(dat.final) <- Countries
for(i in 1:length(Countries)){
  for(j in 1:length(Years)){
    if(length(dat3[which(dat3$Country == Countries[i] & dat3$Year == Years[j]), "Value"])){
      dat.final[Countries[i], j] <- dat3[which(dat3$Country == Countries[i] & dat3$Year == Years[j]), "Value"]
    }
  }
}

for(i in 1:10){
  png(filename = paste0("hist_", Years[i], ".png"))
  hist(dat.final[,i], main = paste0("Industrialized Nations' Health Care Expenditures ", Years[i]),
       xlab = "% GDP Spent on Health Care (OECD)", ylab = "# of Countries", nclass = 15,
       xlim = c(min(dat3$Value), max(dat3$Value)), ylim = c(0, 10), col = c(rep(4, 11), rep(2, 2)))
  legend("topright",
         legend = c("Universial Health Care Model", "US Health Care Model"),
         fill = c(4, 2), col = c(4, 2))
  dev.off()
}

num.col <- ncol(dat.final)
png(filename = "OECD_GDP_Healthcare_lines.png")
plot(y = c(dat.final[1, -num.col]), x = c(as.numeric(colnames(dat.final))[-num.col]),
     main = paste0("Industrialized Nations' Health Care Expenditures "),
     ylab = "% GDP Spent on Health Care (OECD)", xlab = "Year",
     xlim = c(2010, 2019),
     ylim = c(min(dat3$Value), max(dat3$Value)), type = "l", col = 4)
for(i in 2:nrow(dat.final)){
  lines(y = c(dat.final[i, -num.col]), x = c(as.numeric(colnames(dat.final))[-num.col]),
        col = c(rep(4, 37), 2)[i], lwd = c(rep(1, 37), 4)[i])
}
lines(y = colMeans(dat.final[1:37, -num.col]), x = c(as.numeric(colnames(dat.final))[-num.col]),
      col = 1, lwd = 4)
legend(x = 2015.5, y = 15.5,
       legend = c("US Health Care Model", "Universial Health Care Model", "Non-US Mean"),
       fill = c(2, 4, 1), col = c(2, 4, 1))
dev.off()

system("convert hist_*.png hist.gif")
