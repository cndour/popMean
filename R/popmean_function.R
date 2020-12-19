#' Title calculates the national prevalence by year and by intervention
#'
#' @param data a dataset with columns "zone", "population", "year", "none", "IRS", "ITN", "IRS.ITN"
#' @param intervention intervention type. It must be one of the following interventions:  "none", "IRS", "ITN", "IRS.ITN".
#' @param years years for wich the national prevalence have to be calculated.
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise rename
#' @export
popmean = function(data, intervention = "IRS", years = 2020:2022){

  meltData = melt(data = data,
                  id.vars = c("zone", "population", "year"),
                  variable.name = "interventionType",
                  value.name = "PR"
  )
  
  intervention = match.arg(arg = intervention, 
                           choices = unique(meltData$interventionType)
  )
  stopifnot(years%in%unique(meltData$year))

  tabMeans = meltData %>%
    filter(interventionType == intervention, year %in% years) %>%
    group_by(year, interventionType) %>%
    summarise(mean = sum(PR*population)/sum(population)) %>%
    rename(intervention = interventionType) %>%
    data.frame()

  return(tabMeans)
}
