# FactorFactory R Library

## Introduction
FactorFactory is an R library designed to replicate financial factors documented by Bryan Kelly ([GitHub Link](https://github.com/bkelly-lab/ReplicationCrisis)) and Chen's library ([GitHub Link](https://github.com/OpenSourceAP/CrossSection)).

## Related Work
While many R packages cater to a broad spectrum of financial analysis and factor modeling, FactorFactory focuses on providing a specialized set of functionalities tailored for the precise process of financial factor replication.

## Core Functions
- **Factor Construction**: Develop functions to create well-known financial factors (e.g., Fama-French three-factor model) from the ground up.
- **Data Management**: Create utilities for efficient data preprocessing, normalization, and alignment, specifically designed for financial time series data.
- **Statistical Analysis**: Implement robust statistical frameworks for empirically testing factor models. This includes backtesting methodologies (e.g., using the R packages FactorAnalytics and ExpectedReturns), significance evaluations, and comparative performance analysis.

## Timeline
- **June 1**: Introduction to factor replication 
- **June 7**: Create R factors from Chen and Zimmermann's library
- **June 14**: Start with a small universe: check volume, splits and end_of_day values (e.g., dolvol and returns) and check your factors 
- **June 28**: Restructure and modularize naming conventions for different providers (e.g., eodhd, Bloomberg prioritized) 
- **July 10**: Documentation for all data providers
- **August 10**: Creation of R CRAN package and pass all tests
- **August 17**: Integration with FactorAnalytics + Vignettes
- **August 24**: Integration with ExpectedReturns + Vignettes

## Tests
Commits every 2 days to indicate project progress.

### Easy
- Document features in Chen’s library not available in Bryan Kelly’s.

### Medium
- Code some factors from Chen’s library not in Bryan Kelly’s documentation.

### Hard
- Generate output for Chen and Zimmerman's factors.
- Establish a solid framework to ensure FactorFactory features correspond to those of Chen and Zimmerman's (unit test).
