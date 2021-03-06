# iotc.R - DESC
# tunadata/R/iotc.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# getNC {{{

getNC <- function(key=Sys.getenv("IOTC_KEY"),
  toYear=as.character(as.numeric(format(Sys.time(), "%Y")) - 1)) {

  url <- "http://statistics.iotc.org/rest/services/data/nc/summary/results"

  jfilter = paste(
    '{',
      '"includePreliminary": false, ',
      '"includeConfidential": false, ',
      '"fromYear": 1950, ',
      paste0('"toYear": ', as.character(toYear), ', '),
      '"groupByYear": true, ',
      '"groupByFleet": true, ',
      '"groupByFisheryType": true, ',
      '"groupByFisheryGroup": true, ',
      '"groupByFishery": true, ',
      '"groupBySpeciesGroup": true, ',
      '"groupBySpecies": true, ',
      '"groupByArea": true, ',
      '"groupByQuality": true, ',
      '"species": [] ',
    '}'
  )

  req <- POST(url, accept_json(),
    add_headers("Content-Type" = "application/json"),
    add_headers("X-IOTC-STATS-API-key" = key),
    body = jfilter)
 
  if(http_error(req))
    stop(paste(http_status(req)$message, url, sep = " - "))

  nc <- data.table(fromJSON(content(req, "text", encoding="UTF-8")),
    stringsAsFactors=TRUE)
  setkey(nc, fleetCode)

  return(nc)
}
# }}}

# getCodeList {{{
getCodeList <- function(list, key,
  name=paste0(tolower(substring(list, 1, 1)), substring(list, 2))) {
  
  # CONSTRUCT url
  url <- paste0("http://statistics.iotc.org/rest/services/reference/codelists/retrieve/", list)
  
  # GET codelist
  req <- httr::GET(url, add_headers("X-IOTC-STATS-API-key" = key))

  # STOP if error
  if(http_error(req))
    stop(paste(http_status(req)$message, url, sep = " - "))
 
  # deJSON table
  res <- fromJSON(content(req, "text", encoding="UTF-8"))

  # SUBSET code and nameEn
  res <- data.table(res[["data"]][,c("code", "nameEn")])

  # RENAME to match list
  names(res) <- c(paste0(name, "Code"), name)

  setkeyv(res, paste0(name, "Code"))

  return(res)
} # }}}

# getCPCCodeList {{{
getCPCCodeList <- function(key,
  name=paste0(tolower(substring(list, 1, 1)), substring(list, 2))) {
  
  list <- "Fleet"

  # CONSTRUCT url
  url <- paste0("http://statistics.iotc.org/rest/services/reference/codelists/retrieve/",
    list)
  
  # GET codelist
  req <- httr::GET(url, add_headers("X-IOTC-STATS-API-key" = key))

  if(http_error(req))
    stop(paste(http_status(req)$message, url, sep = " - "))
 
  # deJSON table
  res <- fromJSON(content(req, "text", encoding="UTF-8"), flatten=TRUE)
  res <- data.table(res[["data"]][,c("code", "fleetOfficial.code", "fleetOfficial.nameEn")])

  # RENAME to match list
  names(res) <- c(paste0(name, "Code"), "cpcCode", "cpc")

  setkeyv(res, paste0(name, "Code"))

  return(res)
} # }}}
