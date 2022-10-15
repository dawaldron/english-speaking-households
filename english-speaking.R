library(here)
library(data.table)
library(magrittr)
library(readxl)

# census api function
getData <- function(varlist, dataYear, geog) {
  require(httr)
  require(jsonlite)
  require(magrittr)
  require(data.table)
  req_url <- paste0('https://api.census.gov/data/', dataYear, '/acs/acs5?get=NAME', varlist, '&', geog, '&key=5ac41fffeac338e86d11691565b43d053c6a2c59')
  resp_data <- GET(req_url, verbose(TRUE))
  resp <- resp_data %>%
    content(as = 'text')
  dt <- resp %>%
    fromJSON() %>%
    data.table()
  
  setnames(dt, names(dt), t(dt[1]))
  dt <- dt[-1]
  
  dt <- melt(dt, id.var = c('NAME'), variable.factor = FALSE)
  return(dt)
}

# PUMA names
dt_puma <- getData('', 2019, URLencode('for=public use microdata area:*&in=state:*')) %>%
  dcast(NAME ~ variable, value.var = 'value') %>%
  .[,
    .(STATEFIP = state,
      PUMA = `public use microdata area`,
      PUMAName = NAME)]

# metro names
dt_msa <- read_excel('MSA2013_PUMA2010_match_summary.xlsx') %>%
  data.table() %>%
  .[,
    .(MET2013 = `MSA Code`,
      MSATitle = `MSA Title`)]

# State names
dt_statefip <- fread('STATEFIP.tsv')
dt_statefip[, STATEFIP := sprintf('%02d', STATEFIP)]

# Language names
dt_language <- fread('LANGUAGE.tsv')

dt_acs <- fread('usa_00263.csv.gz')

# does respondent speak mainly english at home?
dt_acs[, parentNonenglish := 'Yes']
dt_acs[LANGUAGE == 1, parentNonenglish := 'No']

# select kids age 5-17
dt_acs.kids <- dt_acs %>%
  .[AGE %in% 5:17,
    .(SERIAL,
      PERNUM,
      MOMLOC,
      MOMLOC2,
      POPLOC,
      POPLOC2,
      SPEAKENG)]

# do kids speak english well?
dt_acs.kids[, childEnglish := 'No']
dt_acs.kids[SPEAKENG %in% c(3,4,5), childEnglish := 'Yes']

# match potential parents 1-4
dt_acs.kids1 <- dt_acs %>%
  .[,
    .(SERIAL,
      MOMLOC = PERNUM,
      parentNonenglish)] %>%
  .[dt_acs.kids, on = c('SERIAL', 'MOMLOC')]

dt_acs.kids2 <- dt_acs %>%
  .[,
    .(SERIAL,
      MOMLOC2 = PERNUM,
      parentNonenglish)] %>%
  .[dt_acs.kids, on = c('SERIAL', 'MOMLOC2')]

dt_acs.kids3 <- dt_acs %>%
  .[,
    .(SERIAL,
      POPLOC = PERNUM,
      parentNonenglish)] %>%
  .[dt_acs.kids, on = c('SERIAL', 'POPLOC')]

dt_acs.kids4 <- dt_acs %>%
  .[,
    .(SERIAL,
      POPLOC2 = PERNUM,
      parentNonenglish)] %>%
  .[dt_acs.kids, on = c('SERIAL', 'POPLOC2')]

# combine all matched parents
dt_acs.kids.all <- dt_acs.kids1 %>%
  rbind(dt_acs.kids2) %>%
  rbind(dt_acs.kids3) %>%
  rbind(dt_acs.kids4)

# summarize by child: any non-english-speaking parent?
dt_acs.kids.sum <- dt_acs.kids.all %>%
  .[,
    .(parentNonenglishAny = ifelse(sum((parentNonenglish == 'Yes') * 1, na.rm = TRUE) > 0, 'Yes', 'No')),
    .(SERIAL,
      PERNUM,
      childEnglish)]

# summarize by household: any English-speaking child with any non-English-speaking parent?
dt_acs.kids.sum2 <- dt_acs.kids.sum %>%
  .[,
    .(engChildNonEngPar = sum((childEnglish == 'Yes' & parentNonenglishAny == 'Yes') * 1, na.rm = TRUE) > 0),
    .(SERIAL)]

# match back to full dataset
dt_acs <- dt_acs.kids.sum2[dt_acs, on = 'SERIAL']
dt_acs[is.na(engChildNonEngPar), engChildNonEngPar := FALSE]

# tabulate by different geographies
# by state
dt_sum.by.st <- dt_acs %>%
  .[PERNUM == 1,
    .(hhNonEngParEngChld = sum(engChildNonEngPar * HHWT),
      hhTotal = sum(HHWT)),
    .(STATEFIP = sprintf('%02d', STATEFIP))] %>%
  dt_statefip[., on = 'STATEFIP'] %>%
  .[, pctNonEngParEngChld := round(100 * hhNonEngParEngChld / hhTotal, 1)] %>%
  .[order(-pctNonEngParEngChld)]

fwrite(dt_sum.by.st, 'sum-by-state.csv')

# by metro
dt_sum.by.msa <- dt_acs %>%
  .[PERNUM == 1,
    .(hhNonEngParEngChld = sum(engChildNonEngPar * HHWT),
      hhTotal = sum(HHWT)),
    .(MET2013 = sprintf('%05d', MET2013))] %>%
  dt_msa[., on = 'MET2013'] %>%
  .[, pctNonEngParEngChld := round(100 * hhNonEngParEngChld / hhTotal, 1)] %>%
  .[order(-pctNonEngParEngChld)]

fwrite(dt_sum.by.msa, 'sum-by-metro.csv')

# by PUMA
dt_sum.by.puma <- dt_acs %>%
  .[PERNUM == 1,
    .(hhNonEngParEngChld = sum(engChildNonEngPar * HHWT),
      hhTotal = sum(HHWT)),
    .(STATEFIP = sprintf('%02d', STATEFIP),
      PUMA = sprintf('%05d', PUMA))] %>%
  dt_statefip[., on = 'STATEFIP'] %>%
  dt_puma[., on = c('STATEFIP','PUMA')] %>%
  .[, pctNonEngParEngChld := round(100 * hhNonEngParEngChld / hhTotal, 1)] %>%
  .[order(-pctNonEngParEngChld)]

fwrite(dt_sum.by.puma, 'sum-by-puma.csv')

