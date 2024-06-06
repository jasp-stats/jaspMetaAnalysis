Effect Size Computation
==========================
--------------------------
This analysis allows users to compute effect sizes based on the design and measurement of their experiment. In case multiple types of designs and measurements are included in the data set, the user can specify the order in which the effect sizes are calculated (the effect size from the following option is filled in only if it was computed in the previous step).

See [metafor's documentation](https://wviechtb.github.io/metafor/reference/escalc.html) for more detail about the effect sizes.


#### Design

The design dropdown allows users to select the type of effect size based on the design of the original studies.

- Independent groups: This option is for analyzing data comparing two independent groups. The groups may be experimentally defined or naturally occurring.
- Variable association: This option is for examining the direction and strength of the association between two variables measured concurrently and/or without manipulation by experimenters.
- Single group: This option is for summarizing characteristics of individual groups based on either quantitative or dichotomous variables.
- Repeated measures/matched groups: This option is for assessing change within a single group over time or comparing two matched/paired samples.
- Other: This option includes specific effect sizes that do not fit into the other categories, such as reliability or partial correlations.

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

##### Independent groups
- Quantitative:
  - MD: Mean Difference
  - SMD: Standardized Mean Difference
  - SMDH: Standardized Mean Difference with heteroscedastic variances
  - SMD1: Standardized Mean Difference using standard deviation of second group
  - SMD1H: Standardized Mean Difference using standard deviation of second group with heteroscedastic variances
  - ROM: Ratio of Means
  - CVR: Coefficient of Variation Ratio
  - VR: Variability Ratio
- Binary:
  - RR: Risk Ratio
  - OR: Odds Ratio
  - RD: Risk Difference
  - AS: Arcsine Square Root Transformed Risk Difference
  - PETO: Log Odds Ratio estimated with Peto's method
- Counts per time:
  - IRR: Incidence Rate Ratio
  - IRD: Incidence Rate Difference
  - IRSD: Square Root Transformed Incidence Rate Difference
- Mixed:
  - D2ORN: Transformed Standardized Mean Difference assuming normal distributions
  - D2ORL: Transformed Standardized Mean Difference assuming logistic distributions
  - PBIT: Probit Transformed Risk Difference
  - OR2DN: Transformed Odds Ratio assuming normal distributions
  - OR2DL: Transformed Odds Ratio assuming logistic distributions

##### Variable association
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
- Mixed:
  - RPB: Point-Biserial Correlation Coefficient
  - RBIS: Biserial Correlation Coefficient
  - ZPB: Fisher's r-to-z Transformed Point-Biserial Correlation Coefficient
  - ZBIS: Fisher's r-to-z Transformed Biserial Correlation Coefficient

##### Single group
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

##### Repeated measures/matched groups
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

##### Other
- ARAW: Raw Cronbach's Alpha
- AHW: Transformed Cronbach's Alpha (Hakstian & Whalen)
- ABT: Transformed Cronbach's Alpha (Bonett)
- PCOR: Partial Correlation Coefficient
- ZPCOR: Fisher's r-to-z Transformed Partial Correlation Coefficient
- SPCOR: Semi-Partial Correlation Coefficient
- ZSPCOR: Fisher's r-to-z Transformed Semi-Partial Correlation Coefficient
- R2: Raw Coefficient of Determination
- ZR2: r-to-z Transformed Coefficient of Determination
- REH: Relative Excess Heterozygosity


--------------------------

### Variable Inputs

#### Independent Groups

- Quantitative Measurement
  - N: Group 1: Total sample size of Group 1.
  - N: Group 2: Total sample size of Group 2.
  - Mean: Group 1: Mean value for Group 1.
  - Mean: Group 2: Mean value for Group 2.
  - SD: Group 1: Standard deviation for Group 1.
  - SD: Group 2: Standard deviation for Group 2.

- Binary Measurement
  - N: Group 1/Outcome 1: Number of individuals in Group 1 with Outcome 1.
  - N: Group 1/Outcome 2: Number of individuals in Group 1 with Outcome 2.
  - N: Group 2/Outcome 1: Number of individuals in Group 2 with Outcome 1.
  - N: Group 2/Outcome 2: Number of individuals in Group 2 with Outcome 2.
  - Events: Group 1: Number of events in Group 1.
  - Events: Group 2: Number of events in Group 2.

- Counts Per Time Measurement
  - Total Person-Time: Group 1: Total person-time for Group 1.
  - Total Person-Time: Group 2: Total person-time for Group 2.
  - Events: Group 1: Number of events in Group 1.
  - Events: Group 2: Number of events in Group 2.

- Mixed Measurement
  - For Effect Sizes D2ORN, D2ORL:
    - N: Group 1/Outcome 1: Number of individuals in Group 1 with Outcome 1.
    - N: Group 1/Outcome 2: Number of individuals in Group 1 with Outcome 2.
    - N: Group 2/Outcome 1: Number of individuals in Group 2 with Outcome 1.
    - N: Group 2/Outcome 2: Number of individuals in Group 2 with Outcome 2.
    - N: Group 1: Total sample size of Group 1.
    - N: Group 2: Total sample size of Group 2.
  - For Effect Sizes PBIT, OR2DN, OR2DL:
    - Mean: Group 1: Mean value for Group 1.
    - Mean: Group 2: Mean value for Group 2.
    - SD: Group 1: Standard deviation for Group 1.
    - SD: Group 2: Standard deviation for Group 2.
    - N: Group 1: Total sample size of Group 1.
    - N: Group 2: Total sample size of Group 2.

#### Variable Association

- Quantitative Measurement
  - Raw Correlation Coefficients: Correlation coefficients between two variables.
  - Sample/Group Sizes: Total sample sizes.
  - T-Test Statistics: T-test statistics for the variables.
  - P-Values: P-values for the correlation coefficients.
  - F-Test Statistics: F-test statistics for the variables.

- Binary Measurement
  - N: Variable 1/Outcome +: Number of individuals with Outcome + for Variable 1.
  - N: Variable 1/Outcome -: Number of individuals with Outcome - for Variable 1.
  - N: Variable 2/Outcome +: Number of individuals with Outcome + for Variable 2.
  - N: Variable 2/Outcome -: Number of individuals with Outcome - for Variable 2.

#### Single Group

- Quantitative Measurement
  - Mean: Sample 1: Mean value for Sample 1.
  - Standard Deviations: Standard deviations for the samples.
  - Sample/Group Sizes: Total sample sizes.
  - Observed Effect Sizes/Outcomes: Observed effect sizes or outcomes.
  - Sampling Variances: Sampling variances for the effect sizes.
  - Standard Errors: Standard errors for the effect sizes.

- Binary Measurement
  - Event Frequencies: Frequencies of the event of interest.
  - Complement Frequencies / Group Means: Frequencies of the complement event or group means.
  - Sample/Group Sizes: Total sample sizes.
  - Observed Effect Sizes/Outcomes: Observed effect sizes or outcomes.
  - Sampling Variances: Sampling variances for the effect sizes.
  - Standard Errors: Standard errors for the effect sizes.

- Counts Per Time Measurement
  - Events: Sample 1: Number of events in Sample 1.
  - Total Person-Times: Total person-times for the samples.

#### Repeated Measures/Matched Groups

- Quantitative Measurement
  - N: Time Point 1: Total sample size at Time Point 1.
  - N: Time Point 2: Total sample size at Time Point 2.
  - Mean: Time Point 1: Mean value at Time Point 1.
  - Mean: Time Point 2: Mean value at Time Point 2.
  - SD: Time Point 1: Standard deviation at Time Point 1.
  - SD: Time Point 2: Standard deviation at Time Point 2.

- Binary Measurement
  - N: Treatment 1 + Outcome 1 / Treatment 2 + Outcome 1: Number of individuals with Treatment 1 + Outcome 1 and Treatment 2 + Outcome 1.
  - N: Treatment 1 + Outcome 2 / Treatment 2 + Outcome 1: Number of individuals with Treatment 1 + Outcome 2 and Treatment 2 + Outcome 1.
  - N: Treatment 2 + Outcome 1 / Treatment 1 + Outcome 2: Number of individuals with Treatment 2 + Outcome 1 and Treatment 1 + Outcome 2.
  - N: Treatment 2 + Outcome 2 / Treatment 1 + Outcome 1: Number of individuals with Treatment 2 + Outcome 2 and Treatment 1 + Outcome 1.
  - Events: Time Point 1: Number of events at Time Point 1.
  - Events: Time Point 2: Number of events at Time Point 2.

#### Other

- Reliability (ARAW, AHW, ABT)
  - Alpha: Reliability (Cronbach's alpha) value.
  - Beta: Beta value.
  - Gamma: Gamma value.
  - Delta: Delta value.

- Relative Excess Heterozygosity (REH)
  - N: Homozygous Dominant Alleles: Number of individuals with homozygous dominant alleles.
  - N: Homozygous Recessive Alleles: Number of individuals with homozygous recessive alleles.
  - N: Heterozygous Alleles: Number of individuals with heterozygous alleles.
  - N: Non-Events: Number of non-events.

- R-Squared Values (R2, ZR2)
  - R-Squared Values: R-squared values.
  - Sample/Group Sizes: Total sample sizes.

--------------------------

#### Frequency/event cell adjustment 

##### Add
The Add field allows you to specify the amount to add to zero cells, counts, or frequencies when calculating effect sizes or outcomes.

Default Values:
- 0.5 for:
  - Independent groups with binary measurement (RR, OR).
  - Independent groups with counts per time measurement (IRR, IRD, IRSD).
  - Repeated measures/matched groups with binary measurement (MPRR, MPOR, MPRD, MPORC, MPPETO, MPORM).
  - Single group with binary measurement (PR, PFT).
  - Single group with counts per time measurement (IR, IRLN, IRS, IRFT).
  - Variable association with binary measurement (OR, PHI, YUQ, YUY, RTET).
  - Independent groups with mixed measurement (D2ORN, D2ORL).
- 0 for:
  - Independent groups with binary measurement (RD, AS, PETO).
  - Single group with binary measurement (PLN, PLO, PAS).

##### To
The To dropdown allows you to specify when the values under the Add option should be added to 

Options
- All: The value of Add is added to each cell of all tables.
- Only zero: The value of Add is added only to cells of tables with at least one cell equal to 0.
- If any zero: The value of Add is added to each cell of all tables, but only when there is at least one table with a zero cell.
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
- HO*: Homoscedastic variances assumption.


### R Packages
---
- metafor
