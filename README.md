# FactorFactory R Library

Please submit your proposal to GSoC 2024 here: ([FactorFactory: GSoC 2024](https://github.com/rstats-gsoc/gsoc2024/wiki/FactorFactory:-Financial-Factor-Replication)) to improve the library. If your proposal is selected, students can get a stipend over the summer.


## Introduction
FactorFactory is an R library designed to replicate financial factors documented by ([Bryan Kelly](https://github.com/bkelly-lab/ReplicationCrisis)) and ([Chen and Zimmermann](https://github.com/OpenSourceAP/CrossSection)) library. 

## Related Work
While many R packages cater to a broad spectrum of financial analysis and factor modeling, FactorFactory focuses on providing a specialized set of functionalities tailored for the precise process of financial factor replication.

## Core Functions
- **Factor Construction**: Develop functions to create well-known financial factors (e.g., Fama-French three-factor model) from the ground up.
- **Data Management**: Create utilities for efficient data preprocessing, normalization, and alignment, specifically designed for financial time series data.
- **Statistical Analysis**: Implement robust statistical frameworks for empirically testing factor models. This includes backtesting methodologies (e.g., using the R packages FactorAnalytics and ExpectedReturns), significance evaluations, and comparative performance analysis.

## GSoC 2024 Timeline
- **June 1**: Introduction to factor replication 
- **June 7**: Create R factors from Chen and Zimmermann's library
- **June 14**: Start with a small universe: check volume, splits and end_of_day values (e.g., dolvol and returns) and check your factors 
- **June 21**: Larger universe robust tests 
- **July 01**: Restructure and modularize naming conventions for different providers (e.g., eodhd, Bloomberg prioritized) 
- **July 07**: Documentation for all data providers
- **July 14**: Creation of vignettes
- **July 21**: Integration with FactorAnalytics + Vignettes
- **July 28**: Integration with ExpectedReturns + Vignettes
- **August 05**: Creation of R CRAN package and pass all tests

## Tests
Commits every 2 days to indicate project progress.

### Easy
- Document features in Chen’s library not available in Bryan Kelly’s.

### Medium
- Code some factors from Chen’s library not in Bryan Kelly’s documentation.

### Hard
- Generate output for Chen and Zimmerman's factors.
- Establish a solid framework to ensure FactorFactory features correspond to those of Chen and Zimmerman's (unit test).

## How to run the code
To run the Factor Factory script, use the following bash command:

```bash
R -f factor_factory_run.R
```

Output:

[1] "94.33% of features passed the test"

Features to check:
  - sale_nwc
  - xido_at
  - ncol_gr1a
  - f_score
  - cop_bev
  - gp_bev
  - ol_gr1a
  - noa_at
  - txp_gr1a
  - pstk_gr1
  - nri_at
  - ol_gr3a
  - lti_gr1a
  - lti_gr3a
  - div_ni
  - rd5_at
  - ni_ivol
