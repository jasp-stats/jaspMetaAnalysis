<div align="right">

[![Unit Tests](https://github.com/jasp-stats/jaspMetaAnalysis/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspMetaAnalysis/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspMetaAnalysis/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspMetaAnalysis)
<br>
<b>Maintainer:</b> <a href="https://github.com/FBartos/">František Bartoš</a>

</div>

# The Meta-Analysis Module

## Overview

<img src='inst/icons/meta-analysis.svg' width='149' height='173' align='right'/>

**JASP Meta-Analysis module** is an add-on module for JASP that provides comprehensive tools for synthesizing evidence across multiple studies. The Meta-Analysis module offers a wide range of functionalities, including (but not limited to) classical and Bayesian meta-analytic approaches, multilevel and multivariate models, meta-regression, publication bias adjustment, and robust meta-analytic methods. Specifically, it comprises analysis tools for effect size computation, random-effects and fixed-effects meta-analysis, moderator analysis, heterogeneity assessment, and publication bias detection. Furthermore, the module provides state-of-the-art Bayesian meta-analytic methods through model averaging and hypothesis testing. The module integrates comprehensive visualizations including forest plots, funnel plots, and bubble plots, along with extensive diagnostic tools that assist researchers in understanding, documenting, and communicating their meta-analytic results.

## Articles

The main functions of the Meta-Analysis module are comprehensively documented in two key manuscripts that serve as the primary references for understanding its functionality and statistical methodology:

**Part I: Classical Approaches** - This article provides detailed coverage of the classical meta-analytic methods implemented in the module, including effect size computation, random-effects models, meta-regression, multilevel and multivariate models.
[![arXiv](https://img.shields.io/badge/arXiv-2509.09845-b31b1b.svg)](https://doi.org/10.48550/arXiv.2509.09845)

**Part II: Bayesian Approaches** - This article focuses on the advanced Bayesian meta-analytic functionality, covering Bayesian parameter estimation, hypothesis testing through Bayes factors, Bayesian model averaging, and robust Bayesian meta-analysis.
[![arXiv](https://img.shields.io/badge/arXiv-2509.09850-b31b1b.svg)](https://doi.org/10.48550/arXiv.2509.09850)


## R Packages

<img src='https://www.r-project.org/logo/Rlogo.svg' width='100' height='78' align='right'/>

The meta-analytic functionality is served by several R packages

- **metafor** - The primary package for classical meta-analytic methods ([metafor on CRAN](https://cran.r-project.org/package=metafor))
- **RoBMA** -  The primary package for Bayesian meta-analytic methods ([RoBMA on CRAN](https://cran.r-project.org/package=RoBMA))
- **pema** -  The package for penalized meta-analysis ([pema on CRAN](https://cran.r-project.org/package=pema))
- **metamisc** -  Package for meta-analysis of prediction model performance ([metamisc on CRAN](https://cran.r-project.org/package=metamisc))
- **metaSEM** - Package for meta-analytic SEM and SEM-based meta-analysis ([metaSEM on CRAN](https://cran.r-project.org/package=metaSEM))

## Analyses

The organization of the analyses within the Meta-Analysis module in JASP is as follows:

```
--- Meta-Analysis
    -- Miscellaneous
       - Effect Size Computation
       - Funnel Plot
    -- Classical
       - Meta-Analysis
       - Meta-Analysis (Multilevel/Multivariate)
       - Mantel-Haenszel / Peto
       - Prediction Model Performance
       - WAAP-WLS
       - PET-PEESE
       - Selection Models
       - Meta-Analytic SEM
       - SEM-Based Meta-Analysis
    -- Bayesian
       - Meta-Analysis
       - Meta-Analysis (Deprecated)
       - Binomial Meta-Analysis
       - Penalized Meta-Analysis
       - Prediction Model Performance
       - Robust Bayesian Meta-Analysis
```

### Key Features

**Effect Size Computation**: Calculate standardized effect sizes from raw data including standardized mean differences, odds ratios, correlations, and risk ratios with automatic standard error computation.

**Classical Meta-Analysis**: Comprehensive implementation of random-effects and fixed-effects models with support for meta-regression, subgroup analysis, heterogeneity assessment, and publication bias detection through funnel plot asymmetry tests.

**Advanced Models**: Multilevel models for dependent effect sizes, multivariate models for correlated outcomes, location-scale models for heterogeneity moderation, and cluster-robust standard errors.

**Bayesian Methods**: State-of-the-art Bayesian meta-analysis with model averaging, hypothesis testing via Bayes factors, prior specification options, and robust publication bias adjustment.

**Visualization**: Publication ready forest plots, funnel plots, bubble plots for meta-regression, and comprehensive diagnostic plots including Baujat plots and profile likelihood plots.

**Publication Bias**: Multiple approaches including trim-and-fill, PET-PEESE, selection models, and robust Bayesian meta-analysis.
