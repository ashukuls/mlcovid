
setkey(dt.covid, id, date)

dt.covid[, new.cases := c(NA, diff(confirmed)), by=list(id)]
dt.covid[, cases.log10 := log10(cases)]
dt.covid[, new.cases.log10 := log10(new.cases)]
dt.covid[, rate.log10 := new.cases.log10 - cases.log10]
dt.covid[, rate := new.cases / cases]

#######################
# select interesting cases

dt.max.cases <- dt.covid[, list(max.cases = max(cases)), by=id][order(-max.cases)]


id.selected <- dt.max.cases[max.cases > 1000]$id

dt.covid.sel <- dt.covid[id %in% id.selected & cases > 10]


#######################

covid.model <- lm(new.cases.log10 ~ cases.log10, data=dt.covid.sel[new.cases > 5])
summary(covid.model)
covid.coeff <- coefficients(covid.model)

ids.selected <- c("California", "New York")

ids.selected <- c("China", "Korea, South", "Spain", "US", "Japan")

# time series plot of cases
ggplot(dt.covid.sel[id %in% ids.selected], aes(date, cases, col=id)) +
  geom_point() +
  geom_line(size=0.5)

# time series plot of rate
ggplot(dt.covid.sel[id %in% ids.selected], aes(date, rate, col=id)) +
  geom_point() +
  geom_smooth(se=FALSE)

# new case ~ case with date label
ggplot(dt.covid.sel[id %in% ids.selected & new.cases > 0], aes(cases.log10, new.cases.log10, col=id, label=date)) +
  geom_point() +
  geom_line(size=0.5) +
  geom_smooth(method=loess, se=FALSE, size=0.5) +
  geom_text(position = position_dodge(width=0.9), size=2)

# faceted plot log(new cases) ~ log(cases) with regression model
ggplot(dt.covid.sel, aes(cases.log10, new.cases.log10, col=id)) +
  geom_point(size=0.5) +
  # geom_line(linetype="dashed") +
  geom_smooth(method=loess, se=FALSE, size=0.5) +
  geom_abline(intercept=covid.coeff[1], slope=covid.coeff[2], linetype="dotted") +
  facet_wrap(id ~ ., scales="fixed") +
  theme(legend.position = "none")

# faceted plot log(rate) ~ log(cases)
ggplot(dt.covid.sel, aes(cases.log10, rate, col=id)) +
  geom_point(size=0.5) +
  # geom_line(linetype="dashed") +
  geom_smooth(method=loess, se=FALSE, size=0.5) +
  facet_wrap(id ~ ., scales="fixed") +
  theme(legend.position = "none")

# log(new cases) ~ cases on the same plot
ggplot(dt.covid.sel[id %in% ids.selected & new.cases > 0], aes(cases.log10, new.cases.log10, col=id)) +
  # geom_point(size=0.5) +
  geom_smooth(method=loess, se=FALSE, size=0.5) +
  geom_abline(intercept=covid.coeff[1], slope=covid.coeff[2], linetype="dashed")

# log(new cases) ~ cases on the same plot
ggplot(dt.covid.sel[id %in% ids.selected & new.cases > 0], aes(cases.log10, rate.log10, col=id)) +
  # geom_point(size=0.5) +
  geom_smooth(method=loess, se=FALSE, size=0.5) +
  geom_abline(intercept=covid.coeff[1], slope=covid.coeff[2], linetype="dashed")



#############################
# segmented 

ids.selected <- c("Spain", "Korea, South", "US", "India")
ids.selected <- c("Korea, South")

dt.covid.india <- dt.covid.sel[id %in% ids.selected]
dt.covid.india[cases > 0, growth.log10 := new.cases.log10 - cases.log10]

ggplot(dt.covid.india, aes(date, growth.log10, col=id)) +
  geom_point() + geom_line() + geom_smooth()

ggplot(dt.covid.india, aes(cases.log10, new.cases.log10, col=id)) +
  # geom_point(size=0.5) +
  geom_line(linetype="dashed") +
  geom_smooth(method=loess, se=FALSE, size=0.5)

ggplot(dt.covid.india[cases > 50,], aes(cases.log10, growth.log10, col=id)) +
  geom_point() + geom_line() +
  geom_smooth(se=FALSE)


covid.lm <- lm(new.cases.log10 ~ cases.log10, data=dt.covid.india)

