
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Australian Gross Domestic Product (GDP) application, Di Fonzo and Girolimetto (2021)

The cross-temporal forecast reconciliation for 95 Australian Quarterly
National Accounts time series is applied within the same forecasting
experiment designed by Athanasopoulos et al. (2019) extended in order to
consider semi-annual and annual forecasts as well.

*Keywords:* Linearly constrained multiple time series, Combining
forecasts, Heuristic techniques, Evaluating forecasts, GDP from Income
and Expenditure side

#### R scripts:

-   **00Aus\_base.R**: aggregating and forecasting Australian GDP Time
    series by quarter, semester and year (input: `AusGDP_inpdata.RData`,
    output: `Aus_basef.RData`);

-   **01AusHTS\_recf.R**: creating an RData file of base and
    cross-sectional reconciled forecasts for the Australian GDP’s system
    (input: `Aus_basef.RData`, output: `AusHTS_recf.RData`)

-   **02AusHTS\_scores.R**: Accuracy indices for the cross-sectional
    reconciled forecasts for Australian GDP time series by quarter,
    semester and year. (input: `AusHTS_recf.RData`, output:
    `AusHTS_scores.RData`)

-   **03AusTMP\_recf.R**: Creating an RData file of base and reconciled
    forecasts for each time series in the Australian GDP’s system
    (input: `Aus_basef.RData`, output: `AusTMP_recf.RData`)

-   **04AusTMP\_scores.R**: Accuracy indices for the temporal reconciled
    forecasts for Australian GDP time series by quarter, semester and
    year (input: `AusTMP_recf.RData`, output: `AusTMP_scores.RData`)

-   **05AusCTR\_recf.R**: Reconcile forecasts with the heuristic of
    Kourentzes & Athanasopoulos (2019) and the Optimal Cross-Temporal
    approach. (input: `AusGDP_inpdata.RData` & `Aus_basef.RData`,
    output: `AusCTR_recf.RData`)

-   **06AusCTR\_scores.R**: Accuracy indices for the cross-temporal
    reconciled forecasts for Australian GDP time series by quarter,
    semester and year (input: `AusCTR_recf_part1.RData` &
    `AusCTR_recf_part2.RData`, output: `AusCTR_scores.RData`)

-   **07Aus\_horizon.R**: Focus on performance by forecast horizon
    (input: `AusCTR_recf.RData`, `AusHTS_recf.RData` and
    `AusTMP_recf.RData`, output: `Aus_horizon.RData`)

-   **08Aus\_mcb.R**: Model Comparison with the Best Dataset (input:
    `AusCTR_recf.RData`, `AusHTS_recf.RData` and `AusTMP_recf.RData`,
    output: `Aus_mcb.RData`)

-   **09Aus\_mcbPlot.R**: Model Comparison with the Best (input:
    `Aus_mcb.RData`)

-   **10Aus\_plotWP.R**: Plots of the paper (input:
    `AusCTR_scores.RData`, `AusHTS_scores.RData` &
    `AusTMP_scores.RData`)

#### References:

Athanasopoulos, G., Gamakumara, P., Panagiotelis, A., Hyndman, R.J.,
Affan, M., 2019. Hierarchical Forecasting, in: Fuleky, P. (Ed.),
*Macroeconomic Forecasting in the Era of Big Data.* Springer, Cham,
pp. 689–719.
[doi:10.1007/978-3-030-31150-6\_21](https://dx.doi.org/10.1007/978-3-030-31150-6_21).

Di Fonzo, T., Girolimetto, D. (2021a), Cross-temporal forecast
reconciliation: Optimal combination method and heuristic alternatives,
*International Journal of Forecasting*, *in press* (draft version:
[arXiv:2006.08570](https://arxiv.org/abs/2006.08570)).
