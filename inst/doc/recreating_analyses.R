## ----setup, include = FALSE---------------------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE
)

## ----libraries----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
library(salem)

## -----------------------------------------------------------------------------
salem_theme <-  theme(axis.text.x = element_text(angle = 45, vjust = .5))

## ----fig.width=5--------------------------------------------------------------
ggplot(data = accused_witches) + 
       aes(x = Month.of.Accusation.Name) +
       geom_bar() +
       labs(title = "Accusations by month", x = "Month")  +
       scale_x_discrete(drop = FALSE) +
       salem_theme

table(accused_witches$Month.of.Accusation.Name, useNA = "ifany", 
      dnn = "Month") |> kable(caption = "Accusations by Month")


## ----fig.width=5--------------------------------------------------------------
ggplot(data = accused_witches) + 
       aes(x = as.numeric(Month.of.Accusation.Name)) +
       stat_ecdf(na.rm = TRUE, geom = "line") +
       labs(title = "Cumulative accusations by month", x = "Month", 
            y = "Cumulative Accusations")  +
       scale_x_continuous(breaks = c(1:12), 
                          labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                     "Jun", "Jul","Aug", "Sept", "Oct", "Nov", "Dec") )


cumsum(table(accused_witches$Month.of.Accusation.Name)) |> kable()


## ----message=FALSE, fig.width=5-----------------------------------------------
monthly_accusations <- accused_witches |> 
                      filter(!is.na(Month.of.Accusation)) |>
                      group_by(Month.of.Accusation.Name) |>
                      summarize(number = n()) |> 
                      mutate(cumulative = cumsum(number) )

monthly_accusations |> kable(caption = "Accusations by Month")

ggplot(data = monthly_accusations) +
             aes(x = Month.of.Accusation.Name, y = cumulative, group =1) +
             geom_col(aes(y = number)) +
             geom_line() +
             labs(title = "Frequency and Cumulative Frequency of Accusations by Month") +
             salem_theme


 monthly_accusations |> 
             arrange(desc(number))  |> 
            mutate(Month.of.Accusation.Name = 
                   factor(Month.of.Accusation.Name, 
                         levels=Month.of.Accusation.Name)) |>
            mutate(pareto_cumulative = cumsum(number) ) |>
        ggplot() +
             aes(x = Month.of.Accusation.Name, y = number) +
             geom_col() +
             labs(title = "Pareto Histogram of Accusations by Month") +
             salem_theme


monthly_accusations |> 
            arrange(desc(number))  |> 
            mutate(Month.of.Accusation.Name = 
                   factor(Month.of.Accusation.Name, 
                         levels=Month.of.Accusation.Name)) |>
            mutate(pareto_cumulative = cumsum(number) ) |>
        ggplot() +
             aes(x = Month.of.Accusation.Name, y = pareto_cumulative) +
             geom_point( ) +
             geom_path(aes(y=pareto_cumulative, group=1), colour="blue", lty=3, 
                       linewidth= 0.9) +
             labs(title = "Pareto Cumulative Distribution of Accusations by Month") +
             salem_theme


 monthly_accusations |> 
             arrange(desc(number))  |> 
            mutate(Month.of.Accusation.Name = 
                   factor(Month.of.Accusation.Name, 
                         levels = Month.of.Accusation.Name)) |>
            mutate(pareto_cumulative = cumsum(number) ) |>
        ggplot() +
             aes(x = Month.of.Accusation.Name, y = number) +
             geom_col() +
             geom_path(aes(y=pareto_cumulative, group = 1), colour = "blue", 
                       lty = 3, linewidth = 0.9) +
             labs(title = "Pareto Histogram of Accusations by Month") +
             salem_theme



## ----fig.width=5--------------------------------------------------------------
ggplot(data = na.omit(accused_witches)) + 
       aes(x = Month.of.Execution.Name) +
       geom_bar() +
       labs(title = "Executions by month", x = "Month")  +
       scale_x_discrete(drop = FALSE) +
       salem_theme


table(accused_witches$Month.of.Execution.Name, useNA = "ifany", 
      dnn = "Month") |> kable(caption = "Executions by Month")


## ----fig.width=6--------------------------------------------------------------

monthly_accused <- accused_witches |> 
                      filter(!is.na(Month.of.Accusation)) |>
                      group_by(Month.of.Accusation.Name) |>
                      summarize(accusations = n())  |>
                      rename(Month = Month.of.Accusation.Name)
monthly_executed <- accused_witches |> 
                      filter(!is.na(Month.of.Execution)) |>
                      group_by(Month.of.Execution.Name) |>
                      summarize(executions = n()) |>
                      rename(Month = Month.of.Execution.Name)
monthly_data <- left_join(monthly_accused, monthly_executed) |>
                          mutate(executions = 
                                   ifelse(is.na(executions), 0, executions))


ggplot(monthly_data) +
     aes(x = Month, y = accusations, group = 1) +
     geom_path( color = "blue" ) +
     geom_line(aes(y = executions, x = Month, group = 1), color = "red") +
     labs(title = "Executions and Accusations by Month",
          subtitle = "Accusations in blue and executions in red") +
     salem_theme

## -----------------------------------------------------------------------------
addmargins(table( accused_witches$Month.of.Accusation.Name, accused_witches$Residence,
        dnn = c("Month", "Residence") )) 

## -----------------------------------------------------------------------------
table(accused_witches$Residence, dnn = "Residence") |> 
  kable(caption = "Residences of Accused")

accused_witches |> group_by(Residence) |> summarize(number = n()) |> 
     kable(caption = "Residences of Accused")


## ----fig.width=5--------------------------------------------------------------

accused_witches |> filter(Residence == "Andover") |>
   ggplot() +
   aes(x = Month.of.Accusation.Name) +
   geom_bar() +
   scale_x_discrete(drop=FALSE) +
   salem_theme

## ----fig.width=5--------------------------------------------------------------

accused_witches |> filter(Residence == "Andover") |>
   ggplot() +
   aes(x = Month.of.Accusation.Name) +
   geom_bar() +
   geom_vline(xintercept = 6.5, color = "red") +
   scale_x_discrete(drop = FALSE) +
  labs(title = "Accusations by Month", 
       subtitle = "Red line indicates reconvening of the Court of Oyer and Terminer",
       x = "Month of accusation") +
   salem_theme


## ----fig.width=6, fig.height=8------------------------------------------------

accused_witches |> 
   ggplot() +
   aes(x = Month.of.Accusation.Name) +
   geom_bar() +
   geom_vline(xintercept = 6.5, color = "red") +
   scale_x_discrete(drop = FALSE) +
   facet_wrap(facets = vars(Residence), ncol = 4) +
   labs(title = "Number of accusations by month",
        subtitle = "Red line indicates reconvening of the Court of Oyer and Terminer",
        x = "Month of Accusation") +
   theme(axis.text.x = element_text(angle = 90, vjust = .5))


## -----------------------------------------------------------------------------

newdata <- salem_region |> mutate(February.Any = February > 0, March.Any = March > 0,
                                   April.Any = April > 0, May.Any = May > 0,
                                   June.Any = June > 0, July.Any = July > 0,
                                   August.Any = August > 0, 
                                   September.Any = September >0,
                                   October.Any = October > 0,
                                   November.Any = November > 0)

newdata$TOWN_LABEL <- ifelse(newdata$n_accused == 0, NA,  newdata$TOWN_LABEL)


## -----------------------------------------------------------------------------



## ----eval=FALSE, fig.width=8, fig.height=8------------------------------------
# 
# if (requireNamespace("sf", quietly = TRUE)) {
#      library(sf)
#       p1 <- ggplot(newdata)
#       p2 <- geom_sf_text(aes(label = TOWN_LABEL), color = "blue", size = 2,
#                               nudge_x = 5,
#                                nudge_y = 5, na.rm = TRUE)
#       p3 <-       scale_fill_manual(values = c( "grey", "red"), na.value = "white")
# }
# 

## ----eval=FALSE,fig.width=8, fig.height=8-------------------------------------
# if (requireNamespace("sf", quietly = TRUE)) {
# 
#       p1  + geom_sf(data = newdata,
#                     aes(geometry = newdata$geometry,  fill = February.Any
#                         ), color = "black",
#                     size = .1) +
#         p3 + p2 + labs(title = "Location of Accusations in February")
# }
# 
# if (requireNamespace("sf", quietly = TRUE)) {
#       library(sf)
#       p1 +  geom_sf(data = newdata,
#                     aes(geometry = geometry, fill = July.Any),
#                     color = "black", size = .1) +
#         p3 + p2 + labs(title = "Location of Accusations in July")
# }

## ----eval=FALSE,fig.width=8, fig.height=8-------------------------------------
#  if (requireNamespace("sf", quietly = TRUE)) {
#       p1 + geom_sf(data = newdata,
#                    aes(fill = August.Any, geometry = geometry), color = "black",
#                    size = .1) +
#         p3 + p2  + labs(title = "Location of Accusations in August")
#  }
# if (requireNamespace("sf", quietly = TRUE)) {
#       p1 + geom_sf(data = newdata,  aes(fill = November.Any, geometry = geometry), color = "black",
# 
#                    size = .1) +
#         p3 + p2 + labs(title = "Location of Accusations in November")
# }

## -----------------------------------------------------------------------------

table(parris_social$Sex, parris_social$Identification, parris_social$view,
      dnn = c("Sex", "Identification", "View"))

ftable(parris_social$Sex, parris_social$Identification, parris_social$view,
      dnn = c("Sex", "Identification", "View"))



## -----------------------------------------------------------------------------

table(parris_social$view, parris_social$Identification, parris_social$Sex, 
      dnn = c("View", "Identification", "Sex"))

ftable( parris_social$Identification, parris_social$view,  parris_social$Sex,
      dnn = c("View", "Identification", "Sex"))



## -----------------------------------------------------------------------------
table(salem_village$Petition, salem_village$Church.to.1696, 
      dnn = c( "Membership", "View"))

addmargins( table(salem_village$Petition, salem_village$Church.to.1696, 
      dnn = c( "Membership", "View")))

## -----------------------------------------------------------------------------

table(committee_list$Petition, committee_list$Year, dnn = c("Petition", "Year"))


## -----------------------------------------------------------------------------
table( committee_list$Petition, committee_list$Social, committee_list$Year )

## -----------------------------------------------------------------------------

table( committee_list$Petition, committee_list$Social )


## -----------------------------------------------------------------------------
committee_list2 <-  committee_list |> group_by(Committee.Members) |> 
                             summarize(Social = first(Social), 
                                       Petition = first(Petition),
                                       Terms = n())

addmargins(table( committee_list2$Petition, committee_list2$Social ))

## -----------------------------------------------------------------------------
table(tax_comparison$Year, dnn = c("Year")) |> kable(caption = "Tax payers by year")

## -----------------------------------------------------------------------------
tax_comparison |> group_by(Year) |>
            summarize(mean = round(mean(Tax), 1), median = round(median(Tax), 1),
                                   minimum = min(Tax),
                      maximum =  max(Tax), range = maximum - minimum, 
                      sum = sum(Tax), Count = n()) |> 
                      kable(caption = "Taxes paid by year")



tax_comparison |> group_by(Year, Petition) |>
            summarize(mean = round(mean(Tax), 1), median = round(median(Tax), 1),
                                   minimum = min(Tax),
                      maximum =  max(Tax), range = maximum - minimum, 
                      sum = sum(Tax), Tax_payers = n()) |> 
                      kable(caption = "Taxes paid by year and petition signed")

## ----fig.width=5--------------------------------------------------------------
yearly_tax <- tax_comparison |> group_by(Year, Petition) |>
            summarize(mean = round(mean(Tax), 1), median = round(median(Tax), 1),
                                   minimum = min(Tax),
                      maximum =  max(Tax), range = maximum - minimum, 
                      sum = sum(Tax), Count = n())

ggplot(yearly_tax) + 
        aes(x = Year, y = median, group = Petition, color = Petition) +
        geom_line() +
        labs(title = "Tax paid by petition", y = "Shillings", x = "Year") 


## ----fig.width=5--------------------------------------------------------------

ratios <- yearly_tax |> filter(Petition %in% c("Pro-P", "Anti-P")) |>
                   select(Year, Petition, median) |> 
                   pivot_wider(id_cols = c(Year), values_from = median, 
                               names_from = Petition) |> 
                    mutate(median_ratio = `Pro-P`/`Anti-P`)
ratios |> kable()
ggplot(ratios, 
        aes(x = Year, y = median_ratio)) +
        geom_point() +
        geom_line(group = 1) +
        geom_hline(yintercept = 1, color = "blue") +
        labs(title = "Ratio of Pro- to Anti- Parris median tax",
             y = "Ratio", x = "Year") 


## -----------------------------------------------------------------------------
ratios <- yearly_tax |> filter(Petition %in% c("Pro-P", "Anti-P")) |>
                   select(Year, Petition, mean) |> 
                   pivot_wider(id_cols = c(Year), values_from = mean, 
                               names_from = Petition) |> 
                    mutate(mean_ratio = `Pro-P`/`Anti-P`)
ratios |> kable()
ggplot(ratios, 
        aes(x = Year, y = mean_ratio)) +
        geom_point() +
        geom_line(group = 1) +
        geom_hline(yintercept = 1, color = "blue") +
        labs(title = "Ratio of Pro- to Anti- Parris mean tax",
             y = "Ratio", x = "Year") 

## -----------------------------------------------------------------------------
persisters <- tax_comparison |> pivot_wider(id_cols = c(Name, Petition),
                                             names_from = Year,
                                             names_prefix = "X",
                                             values_from = Tax
                                             ) |>
                                 relocate(X1681, .before = X1690) 


## -----------------------------------------------------------------------------

comparison <- persisters |> 
                    mutate(across(starts_with("X"), 
                                               list(
                                                    rank = min_rank
                                                    )
                                        )) |>
                   filter(!is.na(X1690) & !is.na(X1681)) |>
                   mutate(
                         X1690_ptile = 100 * (X1690_rank - 1)/100,
                         X1681_ptile = 100* (X1681_rank - 1)/94,
                         compare_1681_1690 =
                         case_when(
                          X1690_ptile -X1681_ptile >=  10 ~ "Up",
                          X1690_ptile -X1681_ptile <= -10 ~ "Down",
                          TRUE ~ "No Change"
                        )  
                    ) 

table(comparison$compare_1681_1690, comparison$Petition, 
      dnn = c("Change", "Petition")) |>
      kable(caption = "Change in position 1681-1690")



