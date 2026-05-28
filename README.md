# ComputationalValidity

Companion R package for **Frischkorn, Rebmann, & Oberauer (submitted): "Validity is a Theoretical Problem — A Computational Psychometrics Perspective on How to Measure Cognition."**

## Overview

This package implements simulation-based validity analyses that demonstrate how generative cognitive models can assess the validity of behavioral indicators independently of cross-task correlations. The manuscript focuses on two models of conflict processing:

- **DMC** (Diffusion Model for Conflict) — temporal conflict between automatic and controlled evidence accumulation
- **SSP** (Shrinking Spotlight) — attentional narrowing from broad to focused processing

The simulations reveal four measurement phenomena: process impurity, selective validity, reliability–validity dissociations, and correlation transfer failure.

The repository also contains simulation code and results for the **M3** (Memory Measurement Model) applied to complex span tasks, which extends the framework to working memory measurement with categorical data. These M3 materials are not included in the current manuscript but are available for reuse.

## Repository Structure

```text
R/                 Package functions (simulation, analysis, plotting)
scripts/           Standalone scripts to run simulations and generate figures
reports/           Quarto manuscript (apaquarto format, renders to .docx and .pdf)
figures/           Generated manuscript figures
data/              Simulation input data (not tracked in git — see Data Availability below)
output/            Saved simulation results (not tracked in git)
```

## Using the Package

Install the package to access the simulation and analysis functions in `R/`:

```r
# install.packages("remotes")
remotes::install_github("gidonfrischkorn/ComputationalValidity")
```

The installed package includes all exported functions but not the simulation scripts, reports, figures, or data. The package alone is sufficient if you want to use the functions in your own analyses.

## Reproducing the Analyses

To reproduce the full analyses and manuscript, clone the repository and follow these steps:

```r
# install.packages("remotes")
remotes::install_github("gidonfrischkorn/ComputationalValidity")
```

1. **Download the simulation data** from the [OSF repository](https://osf.io/6qnrv/) by running `scripts/download_data.R`.
2. Run the simulation scripts in `scripts/` (e.g., `Simulation_DMC_Recovery.R`).
3. Run the analysis and plotting scripts (e.g., `Analyze_DMC_Recovery.R`, `Generate_DMC_Plots.R`).
4. Render the manuscript: `quarto render reports/ValidityComputaionalModelling.qmd`

## Data Availability

Simulation datasets (~318 MB total) are hosted on [OSF](https://osf.io/6qnrv/) because the largest file exceeds GitHub's per-file size limit. Run `scripts/download_data.R` to download all `.rda` files into `data/` using the `osfr` package.

## License

MIT
