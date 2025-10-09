# Day 1 Example Plot

viremia <- read.csv("viremia_data_full.csv")

View(viremia)

colnames(viremia) <- c("bird","n","species","family","order","1","2","3","4")

cols <- c("black","gray",rainbow(26)[4:26])

plot(c(1,3,4,6),as.numeric(viremia[i,6:9]),lwd =2,