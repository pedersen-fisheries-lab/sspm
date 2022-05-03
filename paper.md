---
title: 'The sspm R package: spatial surplus production models for the management of northern shrimp fisheries'
tags:
  - R
  - fisheries
  - management
  - modelling
  - GAM
authors:
  - name: Valentin Lucet^[co-first author]
    orcid: 0000-0003-0268-818X
    affiliation: 1
  - name: Eric Pedersen^[co-first author]
    orcid: 0000-0003-1016-540X
    affiliation: 1
affiliations:
 - name: Concordia University, Montreal, Quebec, CA
   index: 1
date: 13 August 2017
bibliography: paper.bib
---

# Statement of need

1. Population models, in particular fisheries productivity models, rarely integrate important spatially-structured ecosystem drivers
2. The Northern Shrimp stock in the Newfoundland and Labrador Shelves currently lacks a population model 
3. Current SPM models are rarely spatially explicit and usually cannot account for relevant ecosystem drivers
4. Fisheries managers lack user-friendly, flexible tools to implement and apply SSPMs

# Summary

<!-- Pop models are important tools in fisheries science, but most are obsolete as they fail to account for ecosystem variables and spatio-temporal dynamics -->

Population modelling is an exercise of interest within environmental sciences and adjacent fields. Early population models dating back to the blahs (80sm 90s etc) [@] tended to address simple dynamics such as exponential growth and density dependence [@], whereas modern population models increasingly acknowledge the non-stationary nature of wild populations [@thorson_importance_2015; @thorson_using_2017]. More specifically, population models applied to resource management, such as fisheries models, increasingly address how stocks vary across space and time. Resource managers are becoming increasingly interested in how ecosystem factors such as predator abundance and abiotic variables impact the spatio-temporal variability of mechanisms like productivity and density dependence. However, efforts to include spatial dynamics and ecosystem variables in fisheries models are rare. Although the non-stationarity of stocks has been established [@zhang_longer_2021; @szuwalski_climate_2016], and despite the push for more ecosystem-based management methods in fisheries management [@crowder_impacts_2008; @berkes_implementing_2012; @tam_towards_2017], applications are lacking. 

<!-- SPMs are simple, old pop models, in need of updating to account for the non-stationarity of the mechanisms that maintain stocks -->

One family of population models that rarely account for spatial structure is the family of Surplus production models (SPMs). SPMs are well-known tools for single-stock modelling. They model the entire biomass of a stock and are useful in data-poor contexts where the age and sex structure of the population is not accessible [@prager_suite_1994]. Basic SPMs are only based on simple mechanics of logistic growth [@], and therefore are widely viewed as a limited tool for modelling stocks. One main limitation of SPMs is that they usually assume spatially constant productivity. This assumption is a strong handicap in the context of the current global changes that are affecting global fisheries, such as climate change, which is already having an impact on the spatial structure of economically important stocks. One example is that of the Northern Shrimp (Pandalus borealis) in the Newfoundland and Labrador Shelves. This stock currently lacks a population model to predict how fishing pressure and changing environmental conditions may affect future abundance in the region. In the context of climate change and shifting ranges, fisheries productivity is likely to be a moving target [@karp_accounting_2019], and managers need better methods that account for varying productivity [@szuwalski_climate_2016].

<!-- Any pop models, including SPMs, can either be process based or statistical: we decide to implement a statistical approach so that we can benefit from confidence intervals -->

Population models like SPMs usually fall under two categories: process-based models and statistical models. Process based models often rely on differential equations and are based on replicating the underlying processes (predation, recruitment, dispersal) behind population dynamics. Statistical models, on the other hand, rely on fitting a model to data using distributional assumptions, and present the advantage of naturally measuring uncertainty around predictions. This is useful in a management context where uncertainty around decision-making is important information to have on hand. In this paper, we use a statistical approach to fitting SPMs using GAMs (generalized additive models). We apply this approach to the population of northern shrimp of the Newfoundland and Labrador Shelves, leveraging the smoothing properties of GAMs to account for varying productivity across time and space. The resulting model is a spatial SPM (SSPM), implemented via a R package `sspm`.

The R package sspm is designed to make spatially-explicit surplus production models (SSPM) more applicable. The package uses Generalized Additive Models (GAMs) to fit a SSPM to biomass and harvest data. The package includes a range of features to manage biomass and harvest data. Those features are organized in a stepwise workflow, whose implementation is described in more detail in \autoref{fig:workflow}.

1. Ingestion of variables as well as spatial boundaries and discretization into patches, using the user's method of choice (random or custom sampling, voronoi tessellation or Delaunay triangulation).
2. Smoothing data using spatio-temporal GAMs smoothers.
3. Computation of productivity values taking into account harvest information.
4. Fitting of SSPMs to smoothed data with GAMs.
5. Visualization of results, including confidence and prediction intervals.
6. One step ahead prediction of biomass.

Although it was developed in a fisheries context, the package is suitable to model spatially-structured population dynamics in general.

# Package design

The package follows an object oriented design, making use of the S4 class systems. The different classes in the package work together to produce a stepwise workflow  (\autoref{fig:workflow}). 

1. The first pillar of the package's design is the concept of boundary data, the spatial polygons that sets the boundary of the spatial model. The boundary data is ingested into a `sspm_boundary` object with a call to `spm_as_boundary()`.
2. The boundary data is then discretized into a `sspm_discrete_boundary` object with the `spm_discretize()` function, dividing the boundary area into discrete patches.
3. The second pillar is the recognition of 3 types of data: **trawl**, **predictors**, and **catch** (i.e. harvest). The next step in the workflow is to ingest the data into `sspm_dataset` objects via a call to `spm_as_dataset()`.
4. The first proper modelling step is to smooth the biomass and predictors data by combining a `sspm_dataset`, and a `sspm_discrete_boundary`. The user specifies a gam formula with custom smooth terms (see \autoref{tab:formula} for more details). The output is still a `sspm_dataset` object with a `smoothed_data` slot which contains the smoothed predictions for all patches.
5. Then, catch is integrated into the biomass data by calling `spm_aggregate_catch` on the two `sspm_dataset` that contains catch and smoothed biomass. Productivity and (both log and non log) is calculated at this step.
6. The next step consists in combining all relevant datasets for the modelling of productivity (i.e. the newly created productivity dataset and the predictor(s) dataset(s)) with a call to `sspm()`. Additionally, the user may apply lags to the variables with `spm_lag()` and determine the split between testing and training data with `spm_split()`.
7. The second modelling step consists in modelling productivity per se. Once again, a gam formula with custom syntax is used.
8. The resulting object contains the model fit. Predictions can be obtained using the built-in `predict()` method, and plots with the `plot()` method.

![The sspm workflow.\label{fig:workflow}](figures/flowchart.png){width=90%}

<!--
Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }
-->

# Acknowledgements

This research was supported by funding from [...].

# References

<!--
Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

-->
