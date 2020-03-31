library(reshape2)


jhu.dir <- "jhu-data/COVID-19/csse_covid_19_data/csse_covid_19_time_series"


jhu.clean <- function(jhu.dt, value.name) {
  setnames(jhu.dt, "Country/Region", "country")
  setnames(jhu.dt, "Province/State", "state")
  # jhu.dt[, id := paste(country, state, sep=".")]
  jhu.dt[, c("Lat", "Long") := NULL]
  jhu.dt.long <- melt(jhu.dt, id.vars=c("country", "state"), variable.name="date", value.name="value")
  jhu.dt.sum <- jhu.dt.long[,  list(value.sum=sum(value)), by=list(country, date)]
  setkey(jhu.dt.sum, "country", "date")
  setnames(jhu.dt.sum, "value.sum", value.name)
  return (jhu.dt.sum)
}

jhu.confirmed <- fread(file.path(jhu.dir, "time_series_covid19_confirmed_global.csv"), header=TRUE, sep=",")
jhu.deaths <- fread(file.path(jhu.dir, "time_series_covid19_deaths_global.csv"), header=TRUE, sep=",")
jhu.recovered <- fread(file.path(jhu.dir, "time_series_covid19_recovered_global.csv"), header=TRUE, sep=",")

jhu.confirmed.long <- jhu.clean(jhu.confirmed, "confirmed")
jhu.deaths.long <- jhu.clean(jhu.deaths, "deaths")
jhu.recovered.long <- jhu.clean(jhu.recovered, "recovered")

jhu.merged <- merge(jhu.confirmed.long, jhu.deaths.long, all=TRUE)
dt.jhu <- merge(jhu.merged, jhu.recovered.long, all=TRUE)


dt.jhu[, date := as.Date(date,  format = "%m/%d/%y")]
dt.jhu[, id := as.factor(country)]
dt.jhu[, cases := confirmed - recovered - deaths]


dt.covid <- dt.jhu

