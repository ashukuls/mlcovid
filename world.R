require(data.table)
require(ggplot2)
require(segmented)

#######################
# US states data
#######################

dt.covid <- fread("covid-19-data/us-states.csv", header=TRUE, sep=",")
dt.covid[, date := as.Date(date,  format = "%Y-%m-%d")]
dt.covid[, id := as.factor(state)]

# For now treat cases = confirmed because recovered is small.
dt.covid[, confirmed := cases]

setkey(dt.covid, id, date.date)

states.selected <- c("New York", "New Jersey", "California")


#######################
# COUNTRY data
#######################

dt.covid <- fread("worldcovid-19/data/countries-aggregated.csv", header=TRUE, sep=",")
setnames(dt.covid, "Date", "date")
setnames(dt.covid, "Confirmed", "confirmed")
setnames(dt.covid, "Recovered", "recovered")
setnames(dt.covid, "Deaths", "deaths")
setnames(dt.covid, "Country", "country")

dt.covid[, cases := confirmed - recovered - deaths]

dt.covid[, date := as.Date(date,  format = "%Y-%m-%d")]
dt.covid[, id := as.factor(country)]

