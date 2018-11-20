setwd(getwd())

remove(list = ls())

# dataset path
mortgage.path <- file.path("..", "datasets", "mortgage.csv")

# read in the CSV file.
mortgage <- read.csv(mortgage.data)

typeof(mortgage)

# convert it to a data.frame.
mortgage <- as.data.frame(mortgage)

# subset version 1
mortgage.temp <- subset(mortgage, FICO_orig_time >= 500)

# subset version two, functionally equal ways of predication logic.
mortgage.temp2 <- mortgage[mortgage$default_time == 1,]

FICO_cat <- vector(length = length(mortgage$FICO_orig_time))

head(FICO_cat)

# determine the Fico Category by the value of Fico_orig_time.
for (i in 1:length(mortgage$FICO_orig_time)) {
	FICO_cat[i] <- if ((mortgage$FICO_orig_time[i] > 500) &
		(mortgage$FICO_orig_time[i] <= 700)) 1
	else if (mortgage$FICO_orig_time[i] > 700) 2
	else 0
}

# add the vector to the mortage data.
mortgage[, "FICO_cat"] <- FICO_cat

head(mortgage)

# delete the status_time column.

mortgage$status_time <- NULL

select.cols <- subset(mortgage, select = c(default_time, FICO_orig_time,
	   LTV_orig_time, gdp_time))

# summary statistics

n.mortgage <- apply(select.cols, 2, length)
mean.mortgage <- apply(select.cols, 2, mean)
st.dev.mortgage <- apply(select.cols, 2, sd)
min.mortgage <- apply(select.cols, 2, min)
max.mortgage <- apply(select.cols, 2, max)

# alternatively, summary is equally as useful.

summary(mortgage)

# combined the calculated statistics in a matrix.

