# iotc.R - DESC
# tunadata/data/raw/iotc.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(tunadata)

IOTC_KEY <- Sys.getenv("IOTC_KEY")

# NC {{{

# LOAD ionc
ionc <- getNC(key=IOTC_KEY)

# fleet
# GET values in field
fleet <- getCodeList("Fleet", key=IOTC_KEY)
# SUBSET values to those in DB
ionc <- fleet[ionc, on="fleetCode"]

# species
species <- getCodeList("Species", key=IOTC_KEY)
ionc <- species[ionc, on="speciesCode"]

# speciesGroup
speciesGroup <- getCodeList("SpeciesGroup", key=IOTC_KEY)
ionc <- speciesGroup[ionc, on="speciesGroupCode"]

# fisheryType
fisheryType <- getCodeList("FisheryType", key=IOTC_KEY)
ionc <- fisheryType[ionc, on="fisheryTypeCode"]

# fisheryGroup
fisheryGroup <- getCodeList("FisheryGroup", key=IOTC_KEY)
ionc <- fisheryGroup[ionc, on="fisheryGroupCode"]

# fishery
fishery <- getCodeList("Fishery", key=IOTC_KEY)
ionc <- fishery[ionc, on="fisheryCode"]

# quality
quality <- getCodeList("QualityCode", key=IOTC_KEY, name="quality")
ionc <- quality[ionc, on="qualityCode"]

# cpc
cpc <- getCPCCodeList(key=IOTC_KEY, name="fleet")
ionc <- cpc[ionc, on="fleetCode"]

# area
area <- data.table(areaCode=c("IRWESIO", "IREASIO"),
  area=c("Western Indian Ocean", "Eastern Indian Ocean"))
ionc <- area[ionc, on="areaCode"]

# REORDER columns
setcolorder(ionc, c("year", "speciesGroupCode", "speciesGroup",
  "speciesCode", "species", "areaCode", "area", "fleetCode", "fleet",
  "cpc", "cpcCode", "fisheryGroupCode", "fisheryGroup", "fisheryTypeCode",
  "fisheryType", "fisheryCode", "fishery", "qualityCode", "quality", "catches"))

# RENAME columns
setnames(ionc, c("year", "spgcde", "spg",
  "sppcde", "spp", "areacde", "area", "fleetcde", "fleet", "cpc", "cpccde",
  "fishgrcde", "fishgr", "fishtycde", "fishty",
  "fishcde", "fishery", "qualcde", "quality", "catch"))

# SETKEY
setkey(ionc, spgcde, sppcde, areacde, fleetcde, cpccde, fishgrcde,
  fishtycde, fishcde, qualcde, year)

# SAVE iotc
save(ionc, file="../data/iotc.RData", compress="xz") 

# }}}

# TODO CONVERT to web services
# CE {{{

# CE LL
ce <- fread("iotc/IOTC-2016-WPNT06-DATA04-CELongline.csv", sep=",", header=TRUE)

# cen
cen <- ce[, c(names(ce)[1:11], grep("-NO", names(ce), value=TRUE)), with=FALSE]
names(cen) <- sub("-NO", "", names(cen))
cen <- melt(cen, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="no")

# cet
cet <- ce[, c(names(ce)[1:11], grep("-MT", names(ce), value=TRUE)), with=FALSE]
names(cet) <- sub("-MT", "", names(cet))
cet <- melt(cet, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="catch")

# BIND cell
cell <- cbind(cen, cet[,list(catch)])

# SUBSET cols and SET names
cell <- cell[,c("Fleet", "Gear", "Year", "MonthStart", "MonthEnd", "Grid",
  "Effort", "EffortUnits", "spp", "no", "catch"), with=FALSE]
names(cell) <- c("flcde", "grcde", "year", "msta", "mend", "grid",
  "effort", "effunits", "spp", "no", "catch")

# CLEAN flcde
cell[, flcde := gsub(" ", "", flcde)]

# ADD cpcde
cell[, cpcde := ifelse(grepl("EU*", flcde), "EU", flcde)]

# SAVE iotc
# save(ionc, cell, file="../iotc.RData", compress="xz") 


# -- EXAMPLE 1 - Efforty by year for LL (HOOKS)
dat <- cell[effunits == "HOOKS", .(effort=sum(effort)), by=list(flcde, year)]
ggplot(dat, aes(x=year, y=effort)) + geom_line() +
  facet_wrap(~ flcde, scales="free")

# -- EXAMPLE 2 - Effort of JPN by year
dat <- cell[flcde == "JPN", .(effort=sum(effort)/1000), by=list(year)]
ggplot(dat, aes(x=year, y=effort)) + geom_line() +
  geom_point(data=subset(dat, year == 2014), size=2) +
  geom_point(data=subset(dat, year == min(year)), size=2, shape=1) +
  ylab("Effort (000s hooks)") + xlab("")


# CE SURF
ce <- fread("iotc/IOTC-2016-WPNT06-DATA05-CESurface.csv", sep=",", header=TRUE)

# cen
cen <- ce[, c(names(ce)[1:11], grep("-NO", names(ce), value=TRUE)), with=FALSE]
names(cen) <- sub("-NO", "", names(cen))
cen <- melt(cen, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="no")

# cet
cet <- ce[, c(names(ce)[1:11], grep("-MT", names(ce), value=TRUE)), with=FALSE]
names(cet) <- sub("-MT", "", names(cet))
cet <- melt(cet, id.vars=c(1:11), measure.vars=c(12:34),
  variable.name="spp", value.name="catch")

# ce
cell <- cbind(cen, cet[,list(catch)])


# CE COAST
ce <- fread("iotc/IOTC-2016-WPNT06-DATA06-CECoastal.csv", sep=",", header=TRUE)


# FC

nmsfc <- c("flcde", "fleet", "year", "ficde", "fishery",
  "grgrp", "grcde", "gear", "loa", "boats") 

iofc <- fread("iotc/iofc_results_5832c68550a44.csv", sep=",", header=TRUE)

# SELECT cols
cols <- c("FlCde", "Fleet", "Year_An", "TFCde", "TypeFishery", "GrGroup",
  "GrCde", "Gear", "LOA_LHT", "noBoats_nBateaux")

iofc <- iofc[, cols, with=FALSE]
names(iofc) <- nmsfc

# cpc
iofc[, cpcde := ifelse(grepl("EU.*", iofc$fleet), "EU", iofc$flcde)]
iofc[, cpc := ifelse(grepl("EU.*", iofc$fleet), "European Union", iofc$fleet)]

setkey(iofc, "cpcde", "flcde", "ficde", "grgrp", "grcde", "year")

# SAVE iotc
save(ionc, iofc, file="../iotc.RData", compress="xz") 

# }}}
