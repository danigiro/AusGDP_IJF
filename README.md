
<!-- README.md is generated from README.Rmd. Please edit that file -->

AusGDP
======

R scripts for Australian GDP forecast and reconciliation:

-   **00Aus\_base.R**: aggregating and forecasting Australian GDP Time
    series by quarter, semester and year (in: `AusGDP_inpdata.RData`,
    out: `Aus_basef.RData`);

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
