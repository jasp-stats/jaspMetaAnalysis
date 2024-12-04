Effect Size Computation
==========================
--------------------------
This analysis allows users to compute effect sizes based on the design and measurement of the studies. In case multiple types of designs and measurements are included in the data set, the user can specify the order in which the effect sizes are calculated (the effect size from the following option is filled in only if it was computed in the previous step). 

Already included effect sizes can be passed forward using the Reported effect sizes option. 

The selected effect size can be computed only for a subset of the dataset using the Subset indicator variable.. 

See [metafor's documentation](https://wviechtb.github.io/metafor/reference/escalc.html) for more detail about the effect sizes.


#### Design

The design dropdown allows users to select the type of effect size based on the design of the original studies.

- Independent groups: This option is for analyzing data comparing two independent groups. The groups may be experimentally defined or naturally occurring.
- Variable association: This option is for examining the direction and strength of the association between two variables measured concurrently and/or without manipulation by experimenters.
- Single group: This option is for summarizing characteristics of individual groups based on either quantitative or dichotomous variables.
- Repeated measures (or matched groups): This option is for assessing change within a single group over time or comparing two matched samples.
- Other: This option includes specific effect sizes that do not fit into the other categories, such as reliability or partial correlations.
- Reported effect sizes: This option allows to directly pass effect sizes and their standard errors or condifidence interval if they were already reported in the original studies. (When confidence interval is passed, normal approximation is used to compute the standard error.)

#### Measurement

The measurement dropdown allows users to select the type effect size based on the measurement in the original studies.

- Quantitative: This option is for analyzing data where the measurement is on a continuous scale (e.g., means and standard deviations).
- Binary: This option is for analyzing data where the measurement is dichotomous (e.g., success/failure, yes/no).
- Counts per time: This option is for analyzing data where the measurement is an event count over a specific time period (e.g., number of incidents per year).
- Mixed: This option is for analyzing data that combines different types of measurements (e.g., a mix of continuous and dichotomous variables).

The available measurement options depend on the selected design type:
- Independent groups: Quantitative / Binary / Counts per time / Mixed
- Variable association: Quantitative / Binary / Mixed 
- Single group: Quantitative / Binary / Counts per time 
- Repeated measures/matched groups: Quantitative / Binary

The measurement dropdown is enabled only when a design type other than "Other" is selected.


#### Effect size
The Effect size dropdown allows users to select the specific effect size or outcome measure to be calculated based on the chosen design and measurement type. The available options are dynamically adjusted according to the selected design and measurement type.

#### Independent groups
- Quantitative:
  - MD: Mean Difference
  - SMD: Standardized Mean Difference (bias corrected, i.e., Hedges' g)
  - SMDH: Standardized Mean Difference with heteroscedastic variances (bias corrected)
  - SMD1: Standardized Mean Difference using standard deviation of second group (bias corrected)
  - SMD1H: Standardized Mean Difference using standard deviation of second group with heteroscedastic variances (bias corrected)
  - ROM: Ratio of Means
  - CVR: Coefficient of Variation Ratio
  - VR: Variability Ratio
- Binary:
  - RR: Log Risk Ratio
  - OR: Log Odds Ratio
  - RD: Risk Difference
  - AS: Arcsine Square Root Transformed Risk Difference
  - PETO: Log Odds Ratio estimated with Peto's method
- Counts per time:
  - IRR: Log Incidence Rate Ratio
  - IRD: Incidence Rate Difference
  - IRSD: Square Root Transformed Incidence Rate Difference
- Mixed:
  - D2ORN: transformed Standardized Mean Difference to Log Odds Ratio assuming normal distributions
  - D2ORL: transformed Standardized Mean Difference to Log Odds Ratio assuming logistic distributions
  - PBIT: Probit Transformed Risk Difference
  - OR2DN: Transformed Log Odds Ratio to Standardized Mean Difference assuming normal distributions
  - OR2DL: Transformed Log Odds Ratio to Standardized Mean Difference assuming logistic distributions

For SMD (including D2ORN, D2ORL) the Cohen's d, T-Statistics from an independent samples t-test, or (signed) P-Values together with the group sizes are sufficient statistics.

For Binary designs (and the corresponding Mixed designs) the table frequencies (Group 1/Outcome +, Group 1/Outcome -, Group 2/Outcome +, and Group 2/Outcome -) or the first column (Group 1/Outcome + and Group 2/Outcome +) with sample sizes (Sample Size Group 1 and Sample Size Group 2) are sufficient statistics.

The Binary design uses the corresponding table:
|              |  **Outcome +**    |  **Outcome -**    |  **Sample Size**    |
| :---         |       :----:      |     :----:        |                ---: |
| **Group 1**  | Group 1/Outcome + | Group 1/Outcome - | Sample Size Group 1 |
| **Group 2**  | Group 2/Outcome + | Group 2/Outcome - | Sample Size Group 2 |


#### Variable association
- Quantitative:
  - COR: Raw Correlation Coefficient
  - UCOR: Bias-Corrected Correlation Coefficient
  - ZCOR: Fisher's r-to-z Transformed Correlation Coefficient
- Binary:
  - OR: Log Odds Ratio
  - PHI: Phi Coefficient
  - YUQ: Yule's Q
  - YUY: Yule's Y
  - RTET: Tetrachoric Correlation Coefficient
  - ZPHI: Fisher's r-to-z Phi Coefficient
  - ZTET: Fisher's r-to-z Tetrachoric Correlation Coefficient
- Mixed:
  - RPB: Point-Biserial Correlation Coefficient
  - RBIS: Biserial Correlation Coefficient
  - ZPB: Fisher's r-to-z Transformed Point-Biserial Correlation Coefficient
  - ZBIS: Fisher's r-to-z Transformed Biserial Correlation Coefficient

For PHI, ZPHI, RPB, and ZPB the Sampling Variance Type specifies ST = stratified vs CS = cross-sectional design, the Mixed option allows passing of character vector specifying ST/CS for each study.

For Binary designs the table frequencies (Outcome +/+, Outcome +/-, Outcome -/+, and Outcome -/-) or the first column (Outcome +/+ and Outcome -/+) with the total +/. and -/. outcomes (Outcome +/+ and Outcome +/-, and Outcome -/+ and Outcome -/-) are sufficient statistics. 

For RPB, RBIS, ZPB, and ZBIS the Cohen's d, T-Statistics from an independent samples t-test, or (signed) P-Values together with the group sizes are sufficient statistics.

The Binary design uses the corresponding table:
|                            | **Variable 2, Outcome +** | **Variable 2, Outcome +** |  **Sample Size**    |
| :---                       |       :----:              |       :----:              |               ---:  |
| **Variable 1, Outcome +**  | Outcome +/+               | Outcome +/-               | Outcome +/+ and +/- |
| **Variable 1, Outcome -**  | Outcome -/+               | Outcome -/-               | Outcome -/+ and -/- |


#### Single group
- Quantitative:
  - MN: Raw Mean
  - SMN: Single-Group Standardized Mean
  - MNLN: Log Transformed Mean
  - CVLN: Log Transformed Coefficient of Variation
  - SDLN: Log Transformed Standard Deviation
- Binary:
  - PR: Raw Proportion
  - PLN: Log Transformed Proportion
  - PLO: Logit Transformed Proportion
  - PAS: Arcsine Square Root Transformed Proportion
  - PFT: Freeman-Tukey Double Arcsine Transformed Proportion
- Counts per time:
  - IR: Raw Incidence Rate
  - IRLN: Log Transformed Incidence Rate
  - IRS: Square Root Transformed Incidence Rate
  - IRFT: Freeman-Tukey Transformed Incidence Rate

For Binary designs the Events and Sample Size or the Events and Non-Events are sufficient statistics.


#### Repeated measures (matched groups)
- Quantitative:
  - MC: Mean Change
  - SMCC: Standardized Mean Change using Change Score Standardization
  - SMCR: Standardized Mean Change using Raw Score Standardization
  - SMCRH: Standardized Mean Change using Raw Score Standardization with heteroscedastic variances
  - SMCRP: Standardized Mean Change using Raw Score Standardization with pooled standard deviations
  - SMCRPH: Standardized Mean Change using Raw Score Standardization with pooled standard deviations and heteroscedastic variances
  - ROMC: Log Transformed Ratio of Means
  - CVRC: Log Transformed Coefficient of Variation Ratio
  - VRC: Log Transformed Variability Ratio
- Binary:
  - MPRR: Matched Pairs Marginal Log Risk Ratio
  - MPOR: Matched Pairs Marginal Log Odds Ratio
  - MPRD: Matched Pairs Marginal Risk Difference
  - MPORC: Conditional Log Odds Ratio
  - MPPETO: Conditional Log Odds Ratio estimated with Peto's method
  - MPORM: Marginal Log Odds Ratio using known/guestimated correlations
- Binary (marginal):
  - MPORM: Matched Pairs Marginal Log Odds Ratio estimated from marginal table

Correlation refers to between measures or between groups correlation.

For SMCC the Cohen's d, T-Statistics from paired-samples t-test, or (signed) P-Values together with the group sizes and correlation are sufficient statistics.

The Binary design uses the corresponding table (Time can reffer to different treatments or matched groups):
|                        | **Time 2, Outcome +** | **Time 2, Outcome +** |
| :---                   |       :----:          |                  ---: |
| **Time 1, Outcome +**  | Outcome +/+           | Outcome +/-           |
| **Time 1, Outcome -**  | Outcome -/+           | Outcome -/-           |

The Binary design can be also reported marginally which results in the following table:
|             | **Outcome +**    | **Outcome -**    |
| :---        |       :----:     |         ---:     |
| **Time 1**  | Time 1/Outcome + | Time 1/Outcome - |
| **Time 2**  | Time 2/Outcome + | Time 2/Outcome - |

In the Binary Marginal design the user also has to supply either the Correlation or Proportion of +/+ outcomes in the binary design. If an impossible value is supplied (the correlation/proportion is restricted by the possible binary tables) the effect size is not calculated. 


#### Other
- Reliability
  - ARAW: Raw Cronbach's Alpha
  - AHW: Transformed Cronbach's Alpha (Hakstian & Whalen)
  - ABT: Transformed Cronbach's Alpha (Bonett)
- Partial and Semi-Partial Correlations
  - PCOR: Partial Correlation Coefficient
  - ZPCOR: Fisher's r-to-z Transformed Partial Correlation Coefficient
  - SPCOR: Semi-Partial Correlation Coefficient
  - ZSPCOR: Fisher's r-to-z Transformed Semi-Partial Correlation Coefficient
- Model fit
  - R2: Raw Coefficient of Determination
  - ZR2: r-to-z Transformed Coefficient of Determination
- Heterozygosity
  - REH: Relative Excess Heterozygosity

Note that (Semi)Partial Correlation in the input does NOT correspond to the raw correlation between the variables. The input can be used when e.g., the partial eta2 is known.

For Partial and Semi-Partial Correlations the P-Value can be supplied instead of the T-Statistic. 

For Model fit only one of the R-Squared, F-Statistic, and P-Value is required.

--------------------------

### Variable Inputs
The specific variable inputs are based on selected effect sizes.

Note that users can supply "signed" P-Value (e.g., p = -0.01, 0.95) where the sign determines the sign of the resulting  effect size (p = -0.01 leads to a negative effect size and p = 0.95 leads to a positive effect size). Sign of T-Statistics is used in the same manner.

#### Independent Groups

- Quantitative Measurement
  - Mean Group 1: Means for group 1.
  - Mean Group 2: Means for group 2.
  - SD Group 1: Standard deviations for group 1.
  - SD Group 2: Standard deviations for group 2.
  - Sample Size Group 1: Total sample sizes of group 1.
  - Sample Size Group 2: Total sample sizes of group 2.
  - T-Statistic: T-test statistics for the variables (for SMD only).
  - P-Value: P-values from a t-test (for SMD only).
  - Cohen's d: Already reported Cohen's d (for SMD only).

- Binary Measurement
  - Group 1/Outcome +: Number of individuals in Group 1 with Outcome +.
  - Group 1/Outcome -: Number of individuals in Group 1 with Outcome -.
  - Group 2/Outcome +: Number of individuals in Group 2 with Outcome +.
  - Group 2/Outcome -: Number of individuals in Group 2 with Outcome -.
  - Sample Size Group 1: Total sample size of Group 1.
  - Sample Size Group 2: Total sample size of Group 2.

- Counts Per Time Measurement
  - Person-Time Group 1: Total person-time for Group 1.
  - Person-Time Group 2: Total person-time for Group 2.
  - Events Group 1: Number of events in Group 1.
  - Events Group 2: Number of events in Group 2.

- Mixed Measurement
  - For Effect Sizes PBIT, OR2DN, OR2DL:
    - Group 1/Outcome +: Number of individuals in group 1 with outcome +.
    - Group 1/Outcome -: Number of individuals in group 1 with outcome -.
    - Group 2/Outcome +: Number of individuals in group 2 with outcome +.
    - Group 2/Outcome -: Number of individuals in group 2 with outcome -.
    - Group 1: Total sample size of group 1.
    - Group 2: Total sample size of group 2.
  - For Effect Sizes D2ORN, D2ORL:
    - Mean Group 1: Means for group 1.
    - Mean Group 2: Means for group 2.
    - SD Group 1: Standard deviations for group 1.
    - SD Group 2: Standard deviations for group 2.
    - Sample Size Group 1: Total sample sizes of group 1.
    - Sample Size Group 2: Total sample sizes of group 2.
    - T-Statistic: T-test statistics for the variables.
    - P-Value: P-values from a t-test.
    - Cohen's d: Already reported Cohen's d.

#### Variable Association

- Quantitative Measurement
  - Correlation Coefficient: Correlation coefficients between the two variables.
  - Sample Size: Total sample sizes.
  - T-Statistic: T-test statistics for the variables.
  - P-Value: P-values for the correlation coefficients.

- Binary Measurement
  - Variable 1/Outcome +: Number of individuals with outcome + for variable 1.
  - Variable 1/Outcome -: Number of individuals with outcome - for variable 1.
  - Variable 2/Outcome +: Number of individuals with outcome + for variable 2.
  - Variable 2/Outcome -: Number of individuals with outcome - for variable 2.

#### Single Group

- Quantitative Measurement
  - Mean: Means.
  - Standard Deviation: Standard deviations.
  - Sample Size: Total sample sizes.

- Binary Measurement
  - Events: Frequencies of the event of interest.
  - Non-Events: Frequencies of the complement event or group means.
  - Sample Size: Total sample sizes.

- Counts Per Time Measurement
  - Events: Number of events.
  - Person-Time: Total person-times.

#### Repeated Measures/Matched Groups

- Quantitative Measurement
  - Mean Time 1: Means at time point 1.
  - Mean Time 2: Means at time point 2.
  - SD Time 1: Standard deviations at time point 1.
  - SD Time 2: Standard deviations at time point 2.
  - Sample Size: Total sample sizes. 

- Binary Measurement
  - Time 1/Outcome +: Number of individuals with outcome + at time point 1.
  - Time 1/Outcome -: Number of individuals with outcome - at time point 1.
  - Time 2/Outcome +: Number of individuals with outcome + at time point 2.
  - Time 2/Outcome -: Number of individuals with outcome - at time point 2.


#### Other

- Reliability
  - Cronbach's alpha: Cronbach's alpha.
  - Items: Items or replications or parts of the measurement instrument.
  - Sample Size: Total sample sizes.

- Partial and Semi-Partial Correlations
  - Predictors: Number of regression predictors.
  - Sample Size: Total sample sizes.
  - R-Squared: R-squared values (for semi-partial correlation only).
  - T-Statistic: T-test statistics for the regression coefficient.
  - P-Value: P-values for the regression coefficient.
  - (Semi)Partial Correlation: Semi(partial) correlations of the regression coefficient.

- Model fit
  - Predictors: Number of regression predictors.
  - Sample Size: Total sample sizes.
  - R-Squared: R-squared values.
  - F-Statistic: F-test statistics for the regression coefficients.
  - P-Value: P-values for the F-test of regression coefficients.

- Relative Excess Heterozygosity (REH)
  - Homozygous Dominant Alleles: Number of individuals with homozygous dominant alleles.
  - Heterozygous Alleles: Number of individuals with heterozygous alleles.
  - Homozygous Recessive Alleles: Number of individuals with homozygous recessive alleles.


#### Reported effect sizes
- Effect size: The reported effect sizes.
- Standard error: Standard errors of the reported effect sizes.
- Sampling Variance: Sampling variances of the reported effect sizes.
- 95% Confidence Interval: Lower and upper bound of the 95% CI of the reported effect sizes. 


--------------------------

#### Frequency/event cell adjustment 
Available only for:
- Independent groups design with Binary measurement.
- Independent groups design with Counts per time measurement.
- Independent groups design with Mixed measurement and effect sizes `PBIT`, `OR2DN`, or `OR2DL`.
- Variable association with Binary measurement.
- Single group designs with Binary measurement.
- Single group designs with Counts per time measurement.

#### Add
The Add input field allows you to specify a small constant to add to zero cells, counts, or frequencies when computing effect sizes, as many effect sizes are undefined when one of the cells, counts or frequencies is equal to zero.

Default Value:
- 0.5: Default value for most effect sizes.
- 0: Used for AS, PHI, ZPHI, RTET, ZTET, IRSD, PAS, PFT, IRS, IRFT


#### To
The To dropdown allows you to specify when the values under the Add option should be added to 

Options
- All: The value of Add is added to each cell/count/frequency of all studies.
- Only zero: The value of Add is added to each cell/count/frequency of a study with at least one cell/count/frequency equal to 0.
- If any zero: The value of Add is added to each cell/count/frequency of all studies, but only when there is at least one study with a zero cell/count/frequency.
- None: No adjustment to the observed table frequencies is made.


#### Drop studies with no cases/events
The `Drop Studies with No Cases/Events` radio button group allows you to specify whether studies with no cases or events should be dropped when calculating the effect sizes.

Options:
- Yes: Drop studies with no cases/events.
- No: Do not drop studies with no cases/events.

#### Sampling variance type
The Sampling variance type dropdown allows you to specify the type of sampling variances for the effect size. The options available depend on the design, measurement, and effect size values.

Options:
- LS: Large-sample approximation.
- LS2: Alternative large-sample approximation.
- UB: Unbiased estimates of the sampling variances.
- AV: Sampling variances with the sample-size weighted average.
- HO: Homoscedastic variances assumption.
- AVHO: Homoscedasticity variances assumption for both groups across studies.

### R Packages
---
- metafor