require(data.table)
require(ggplot2)


#######################
# US states data
#######################

dt.states <- fread("../covid-19-data/us-states.csv", header=TRUE, sep=",")
dt.states[, date := as.Date(date,  format = "%Y-%m-%d")]
dt.states[, id := as.factor(state)]

# For now treat cases = confirmed because recovered is small.
dt.covid[, confirmed := cases]

setkey(dt.covid, id, date.date)




dt.states <- fread("../covid-19-data/us-states.csv", header=TRUE, sep=",")
dt.states[, date.date := as.Date(date,  format = "%Y-%m-%d")]
dt.states[, id := as.factor(state)]
setkey(dt.states, id, date.date)


dt.states[, new.cases := c(NA,diff(cases)), by=list(id)]

dt.states[, cases.log10 := log10(cases)]
dt.states[, new.cases.log10 := log10(new.cases)]

dt.cases.state <- dt.states[, list(max.cases = max(cases)), by=id][order(-max.cases)]

states.selected <- dt.cases.state[max.cases > 3000]$id
states.selected <- c("New York", "New Jersey", "California")

dt.states.sel <- dt.states[cases > 5 & id %in% states.selected]












#######################

ggplot(dt.states.sel, aes(date.date, cases, col=state)) + geom_line() + scale_y_log10()


ggplot(dt.states.sel, aes(cases.log10, new.cases.log10, col=state)) +
  geom_point() +
  geom_line(linetype="dashed") +
  geom_smooth(method=loess, se=FALSE)


ggplot(dt.states.sel[cases > 10 & new.cases > 0,], aes(cases, new.cases, col=state)) +
  geom_point() +
  geom_line() +
  scale_x_log10() + scale_y_log10()


model <- lm(new.cases.log10 ~ 0 + cases.log10 + id, data=dt.states[new.cases > 0])
summary(model)

#######################

dt.counties <- fread("covid-19-data/us-counties.csv", header=TRUE, sep=",")
dt.counties[, date.date := as.Date(date,  format = "%Y-%m-%d")]
dt.counties[, id := as.factor(paste(state, county, sep="."))]

setkey(dt.counties, id, date.date)

dt.counties[, new.cases := c(NA, diff(cases)), by=list(id)]

dt.cases.counties <- dt.counties[, list(max.cases = max(cases)), by=id][max.cases > 300]


dt.counties.sel <- dt.counties[cases > 5 & id %in% dt.cases.counties$id]

dt.counties.sel[, cases.log10 := log10(cases)]
dt.counties.sel[, new.cases.log10 := log10(new.cases)]



#######################

ggplot(dt.counties.sel, aes(date.date, cases, col=id)) + geom_line() +
  scale_y_log10() + theme(legend.position = "none")

ggplot(dt.counties.sel, aes(cases, new.cases, col=id)) +
  geom_point() +
  geom_line() +
  scale_x_log10() + scale_y_log10() +
  theme(legend.position = "none")


ggplot(dt.counties.sel, aes(cases.log10, new.cases.log10, col=id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")
  

ggplot(dt.counties.sel[state %in% c("California", "New York")], aes(cases, new.cases, col=id)) +
  geom_point() +
  # geom_line(aes(linetype=state)) +
  geom_smooth(, method=loess, se=FALSE) +
  scale_x_log10() + scale_y_log10() +
  facet_grid(state ~ ., scales="fixed")

model <- lm(new.cases.log10 ~ 0 + cases.log10, data=dt.counties.sel[new.cases > 0])
summary(model)
