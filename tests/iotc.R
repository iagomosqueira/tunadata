# iotc.R - DESC
# tunadata/vignette/iotc.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ggplot2)
library(tunadata)

data(iotc)

theme_set(theme_bw())

# CATCH per gear group in 2014
dat <- ionc[year == 2014, .(catch=sum(catch)), by = fishgr]
dat[, percent := (catch / sum(catch)) * 100]
dat[, fishgr := factor(fishgr, levels=rev(fishgr))]

ggplot(dat, aes(y=fishgr, x=percent)) + geom_point(size=3) +
  geom_segment(aes(x=0, xend=percent, y=fishgr, yend=fishgr)) +
  ylab("") + xlab("Percentage catch in weight, 2012") + theme_bw()


# TS of CATCH per gear group
dat <- ionc[, .(catch=sum(catch)), by = list(fishgr, year)]
dat[, fishgr := factor(fishgr, levels=unique(fishgr))]

ggplot(dat, aes(x=year, y=catch, group=fishgr)) + geom_line(aes(colour=fishgr)) +
  theme(legend.position="bottom") +
  labs(colour="Gear Group") + xlab("") + ylab("Catch per gear (t)") +
  geom_text(data=subset(dat, year==2014), aes(x=2020, y=catch, label=fishgr)) +
  xlim(c(NA, 2023))


# CATCH per CPC (20 largest) in 2012
dat <- ionc[year == 2012, .(catch=sum(catch/1000)),
  by = list(cpccde, year)][order(-catch), .SD[1:20]]
dat[, cpccde := factor(cpccde, levels=rev(cpccde))]

ggplot(dat, aes(y=cpccde, x=catch)) + geom_point(size=3) +
  geom_segment(aes(x=0, xend=catch, y=cpccde, yend=cpccde)) +
  ylab("") + xlab("Catch (000s t)") 


# CATCH per CPC (40 largest) by decade
dat <- ionc[, .(catch=sum(catch/1000)),
  by = list(cpccde, decade=floor(year/10)*10)][order(-catch), .SD[1:40]]
dat[, cpccde := factor(cpccde, levels=rev(unique(cpccde)))]

ggplot(dat, aes(y=cpccde, x=catch)) + geom_point(size=3) +
  geom_segment(aes(x=0, xend=catch, y=cpccde, yend=cpccde)) +
  ylab("") + xlab("Catch (000s t)") + facet_wrap(~decade) 
