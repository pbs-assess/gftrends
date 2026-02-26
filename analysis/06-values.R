library(dplyr)
end_year <- 2025

refpt <- readRDS(here::here("data-generated/p-thresh.rds"))
ts_mcmc <- readRDS(here::here("data-generated/x_t_posterior.rds")) |>
  filter(year == end_year)
ts_lrp <- dplyr::filter(ts_mcmc, ratio == "B/LRP") %>%
  pull(.value)
ts_usr <- dplyr::filter(ts_mcmc, year == end_year, ratio == "B/USR") %>%
  pull(.value)
ts_bmsy <- dplyr::filter(ts_mcmc, year == end_year, ratio == "B/B[MSY]") %>%
  pull(.value)
q_lrp <- sprintf("%.1f", round(quantile(ts_lrp, probs = c(0.025, 0.5, 0.975)), 1))

make_quant_text <- function(x) {
  paste0(x[2], " (95% CI: ", x[1], "--", x[3], ")")
}

(q_usr <- sprintf("%.1f", round(quantile(ts_usr, probs = c(0.025, 0.5, 0.975)), 1)))

(q_msy <- sprintf("%.1f", round(quantile(ts_bmsy, probs = c(0.025, 0.5, 0.975)), 1)))

# Section 29.4
# We estimated the overall mean B/LRP (biomass divided by the LRP) in 2021 to be
make_quant_text(q_lrp)
# The overall mean B/USR and B/B~MSY~ (biomass divided by biomass at maximum sustainable yield) in 2021 was
make_quant_text(q_usr)
# and
make_quant_text(q_msy)
# respectively (Figure \@ref(fig:pa-framework), \@ref(fig:ridges)).

# Considering the USR instead of the LRP,
sum(refpt$p_usr > 0.25)
# /
nrow(refpt)
# of the stocks in Figure \@ref(fig:ridges) had \> 25% probability of being below their USR as of their most recent assessment.
