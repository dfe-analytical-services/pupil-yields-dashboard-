tidy_yields <- function() {
  dfYieldsWide <- read.csv("data/TestTotalYieldsTable.csv",
    stringsAsFactors = FALSE
  )
  dfYieldsWide <- dfYieldsWide %>%
    rename(
      local_authority = `Local.Authority`,
      phase_type = `PHASE.OR.TYPE`
    )
  dfNumDevs <- dfYieldsWide %>%
    select(local_authority, phase_type,
      ndev_10to49 = `Oct.49`, ndev_50to99 = `X50.99`,
      ndev_100to199 = `X100.199`, ndev_200to499 = `X200.499`,
      ndev_500to999 = `X500.999`, ndev_1000 = `X1000.`
    ) %>%
    tidyr::pivot_longer(
      cols = -c("local_authority", "phase_type"),
      names_to = "number_developments",
      values_to = "pupil_yield"
    )
  dfAfford <- dfYieldsWide %>% pivot_subset("affordability", c("AFFORDABLE.HOUSING", "MARKET.HOUSING"))
  dfRurality <- dfYieldsWide %>% pivot_subset("rurality", c("Rural.town.and.fringe", "Rural.village.and.dispersed", "Urban.city.and.town"))
  dfHousingType <- dfYieldsWide %>% pivot_subset("housing_type", c("Detached", "Flats", "Semi.detached", "Terraced..inc..end."))
  dfHiMidLow <- dfYieldsWide %>% pivot_subset("himidlow", c("HIGH", "MID", "LOW"))
  dfBeds <- dfYieldsWide %>% pivot_subset("number_beds", c("X1.BED", "X2.BED", "X3.BED", "X4..BED"))
  dfAffordabilityHimidlow <- dfYieldsWide %>%
    pivot_subset_combi(
      c("affordability", "himidlow"),
      c(
        "affordable.housing...high", "affordable.housing...mid", "affordable.housing...low",
        "market.housing...high", "market.housing...mid", "market.housing...low"
      )
    )
  dfBedsAffordability <- dfYieldsWide %>%
    pivot_subset_combi(
      c("number_beds", "affordability"),
      c(
        "X1.bed...market.housing", "X2.bed...market.housing",
        "X3.bed...market.housing", "X4..bed...market.housing",
        "X1.bed...affordable.housing", "X2.bed...affordable.housing",
        "X3.bed...affordable.housing", "X4..bed...affordable.housing"
      )
    )

  # Now bring it all together
  dfYieldsTidy <- bind_rows(
    dfAfford, dfNumDevs, dfRurality, dfHousingType, dfHiMidLow, dfBeds,
    dfAffordabilityHimidlow, dfBedsAffordability
  ) %>%
    select(
      local_authority, phase_type,
      affordability, number_developments, rurality, housing_type, himidlow, number_beds,
      pupil_yield
    )
  dfYieldsTidy[is.na(dfYieldsTidy)] <- "Total"
  return(dfYieldsTidy)
}

pivot_subset <- function(df, filternames, entries, primary_filters = c("local_authority", "phase_type")) {
  df %>%
    select(c(primary_filters, entries)) %>%
    tidyr::pivot_longer(
      cols = -all_of(primary_filters),
      names_to = filternames,
      values_to = "pupil_yield"
    )
}

pivot_subset_combi <- function(df, filtername, entries, primary_filters = c("local_authority", "phase_type")) {
  dfPivot <- df %>%
    select(c(primary_filters, entries)) %>%
    tidyr::pivot_longer(
      cols = -all_of(primary_filters),
      names_to = filtername,
      names_pattern = "(.*)\\.\\.\\.(.*)",
      values_to = "pupil_yield"
    )
  return(dfPivot)
}
