# ComputationalValidity

Companion R package for **Frischkorn, Rebmann, & Oberauer (submitted): "Validity is a Theoretical Problem — A Computational Psychometrics Perspective on Cognitive Task Validity."**

## Overview

This package implements simulation-based validity analyses that demonstrate how generative cognitive models can assess the validity of behavioral indicators independently of cross-task correlations. Simulations use three cognitive models:

- **DMC** (Diffusion Model for Conflict) — temporal conflict between automatic and controlled evidence accumulation
- **SSP** (Shrinking Spotlight) — attentional narrowing from broad to focused processing
- **M3** (Memory Measurement Model) — multinomial processing tree model for complex span tasks

The simulations reveal four measurement phenomena: process impurity, selective validity, reliability–validity dissociations, and correlation transfer failure.

## Repository Structure

```
R/                 Package functions (simulation, analysis, plotting)
scripts/           Standalone scripts to run simulations and generate figures
reports/           Quarto manuscript (apaquarto format, renders to .docx and .pdf)
figures/           Generated manuscript figures
data/              Simulation input data
output/            Saved simulation results
```

## Installation

```r
# install.packages("remotes")
remotes::install_github("gidonfrischkorn/ComputationalValidity")
```

## Reproducing the Analyses

1. Install the package and its dependencies.
2. Run the simulation scripts in `scripts/` (e.g., `Simulation_DMC_Recovery.R`).
3. Run the analysis and plotting scripts (e.g., `Analyze_DMC_Recovery.R`, `Generate_DMC_Plots.R`).
4. Render the manuscript: `quarto render reports/ValidityComputaionalModelling.qmd`

## License

MIT
