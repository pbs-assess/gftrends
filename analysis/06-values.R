library(dplyr)
refpt <- readRDS(here::here("data-generated/p-thresh.rds"))
ts_mcmc <- readRDS(here::here("data-generated/x_t_posterior.rds"))
ts_2021_lrp <- dplyr::filter(ts_mcmc, year == 2022, ratio == "B/LRP") %>%
  pull(.value)
ts_2021_usr <- dplyr::filter(ts_mcmc, year == 2022, ratio == "B/USR") %>%
  pull(.value)
ts_2021_bmsy <- dplyr::filter(ts_mcmc, year == 2022, ratio == "B/B[MSY]") %>%
  pull(.value)
q2021lrp <- sprintf("%.1f", round(quantile(ts_2021_lrp, probs = c(0.025, 0.5, 0.975)), 1))

make_quant_text <- function(x) {
  paste0(x[2], " (95% CI: ", x[1], "--", x[3], ")")
}

(q2021usr <- sprintf("%.1f", round(quantile(ts_2021_usr, probs = c(0.025, 0.5, 0.975)), 1)))

(q2021msy <- sprintf("%.1f", round(quantile(ts_2021_bmsy, probs = c(0.025, 0.5, 0.975)), 1)))

# We estimated the overall mean B/LRP (biomass divided by the LRP) in 2021 to be
make_quant_text(q2021lrp)
# The overall mean B/USR and B/B~MSY~ (biomass divided by biomass at maximum sustainable yield) in 2021 was
make_quant_text(q2021usr)
# and
make_quant_text(q2021msy)
# respectively (Figure \@ref(fig:pa-framework), \@ref(fig:ridges)).

# Considering the USR instead of the LRP,
sum(refpt$p_usr > 0.25)
# /
nrow(refpt)
# of the stocks in Figure \@ref(fig:ridges) had \> 25% probability of being below their USR as of their most recent assessment.
