# https://data.world/liz-friedman/covid-19-in-germany/workspace/file?filename=cases-rki-by-state.csv
# https://en.wikipedia.org/wiki/ISO_3166-2:DE

germanyLatLong <- read_csv("./data/duitsland-latlong.csv") 
germany <- read_csv("./data/duitsland.csv") %>%
  rename(
    dateRep = `time_iso8601`,
    `Schleswig-Holstein` = de_sh  ,
    `Hamburg` = de_hh  ,
    `Niedersachsen` = de_ni  ,
    `Bremen` = de_hb  ,
    `Nordrhein-Westfalen`  = de_nw  ,
    `Hessen` = de_he  ,
    `Rheinland-Pfalz` =de_rp  ,
    `Baden-Wurttemberg` = de_bw  ,
    `Bayern` = de_by  ,
    `Saarland` = de_sl  ,
    `Brandenburg`= de_bb  ,
    `Mecklenburg-Vorpommern` = de_mv  ,
    `Sachsen` =de_sn  ,
    `Sachsen-Anhalt` = de_st  ,
    `Thuringen` = de_th  ,
    `Berlin` = de_be  ,
    SumCases = sum_cases
    ) %>%
  pivot_longer(-c(dateRep, SumCases ), names_to = "Provinces", values_to = "Cases") %>%
  select(-c("SumCases")) %>%
  left_join(germanyLatLong) 
germany_long <- germany
germany <- germany %>%
  pivot_wider(names_from = dateRep, values_from = Cases )

rm(germanyLatLong)
