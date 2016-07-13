# iotc.R - DESC
# tunadata/vignette/iotc.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ggplot2)
library(tunadata)

data(iotc)

theme_set(theme_bw())

# CATCH per gear group in 2014
dat <- ionc[year == 2014, .(catch=sum(catch)), by = grgrp]
dat[, percent := (catch / sum(catch)) * 100]
dat[, grgrp := factor(dat$grgrp, levels=rev(dat$grgrp))]

ggplot(dat, aes(y=grgrp, x=percent)) + geom_point(size=3) +
  geom_segment(aes(x=0, xend=percent, y=grgrp, yend=grgrp)) +
  ylab("") + xlab("Percentage catch in weight, 2012") + theme_bw()


# TS of CATCH per gear group
dat <- ionc[, .(catch=sum(catch)), by = list(grgrp, year)]
dat[, grgrp := factor(dat$grgrp, levels=unique(dat$grgrp))]

ggplot(dat, aes(x=year, y=catch, group=grgrp)) + geom_line(aes(colour=grgrp)) +
  theme(legend.position='bottom') +
  labs(colour="Gear Group") + xlab("") + ylab("Catch per gear (t)") +
  geom_text(data=subset(dat, year==2014), aes(x=2020, y=catch, label=grgrp)) +
  xlim(c(NA, 2023))


# CATCH per SPP


# CATCH per CPC (20 largest) in 2012
dat <- ionc[year == 2012, .(catch=sum(catch/1000)),
  by = list(cpcde, year)][order(-catch), .SD[1:20]]
dat[, cpcde := factor(dat$cpcde, levels=rev(dat$cpcde))]

ggplot(dat, aes(y=cpcde, x=catch)) + geom_point(size=3) +
  geom_segment(aes(x=0, xend=catch, y=cpcde, yend=cpcde)) +
  ylab("") + xlab("Catch (000s t)") 


# CATCH per CPC (20 largest) by decade
dat <- ionc[, .(catch=sum(catch/1000)),
  by = list(cpcde, decade=floor(year/10)*10)][order(-catch), .SD[1:40]]
dat[, cpcde := factor(dat$cpcde, levels=rev(unique(dat$cpcde)))]

ggplot(dat, aes(y=cpcde, x=catch)) + geom_point(size=3) +
  geom_segment(aes(x=0, xend=catch, y=cpcde, yend=cpcde)) +
  ylab("") + xlab("Catch (000s t)") + facet_wrap(~decade) 
