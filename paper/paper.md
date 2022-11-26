---
title: 'The sspm R package: spatial surplus production models for the management of
  northern shrimp fisheries'
tags:
- R
- fisheries
- management
- modelling
- GAM
date: "13 August 2017"
output:
  pdf_document: default
  word_document: default
authors:
- name: Valentin Lucet^[co-first author]
  orcid: 0000-0003-0268-818X
  affiliation: 1
- name: Eric J. Pedersen^[co-first author]
  orcid: 0000-0003-1016-540X
  affiliation: "1, 2"
bibliography: paper.bib
affiliations:
- name: Department of Biology, Concordia University, Montreal, CA
  index: 1
- name: Department of Biology, Memorial University of Newfoundland, St. John's, CA
  index: 2
---

# Statement of need

1. Population models, in particular fisheries productivity models, rarely integrate important spatially-structured ecosystem drivers
2. The Northern Shrimp stock in the Newfoundland and Labrador Shelves currently lacks a population model 
3. Current SPM models are rarely spatially explicit and usually cannot account for relevant ecosystem drivers
4. Fisheries managers lack user-friendly, flexible tools to implement and apply SSPMs

# Summary

<!-- Pop models are important tools in fisheries science, but most are obsolete as they fail to account for ecosystem variables and spatio-temporal dynamics -->

Population modelling is an exercise of interest within environmental sciences and adjacent fields. Early population models such as the logistic model assumed that while the abundance of a population might change over time, the conditions governing parameters affecting that rate of change, such as the maximum rate of growth or the carrying capacity of the population, stay constant over time [@gotelli_primer_2008]. Modern population models increasingly acknowledge the non-stationary nature of wild populations and work to incorporate environmental fluctuations into dynamic models [@thorson_importance_2015; @thorson_using_2017]. Population models designed to answer applied resource management questions, such as fisheries stock models, increasingly address how the dynamics of stocks vary across space and time. 

Resource managers are becoming increasingly interested in how variation in ecosystem factors such as predator abundance and abiotic variables impact the spatiotemporal variability of population parameters, such as productivity [@zhang_longer_2021; @szuwalski_climate_2016]. Further, treating spatially structured stocks as single unstructured stocks can lead to substantially biased estimates of population change [@thorson_importance_2015]. However, stock models that explicitly incorporate spatial dynamics and time-varying ecosystem variables are still rare in fisheries science, despite the push for more ecosystem-based management methods in fisheries management [@crowder_impacts_2008; @berkes_implementing_2012; @tam_towards_2017]. 

<!-- SPMs are simple, old pop models, in need of updating to account for the non-stationarity of the mechanisms that maintain stocks -->

Surplus production models (SPMs) are one of the classic models used in fisheries, and are based on modelling changes in the total biomass of a stock in a given location over time (i.e. *surplus production*) as a function of current stock abundance and fishing pressure [@walters_surplus_2008]. They are useful in data-poor contexts where the age and sex structure of the population is not accessible, or when age- or length-structure do not change substantially over time [@prager_suite_1994; @punt_extending_2003]. Classically, SPMs assumed single unstructured stocks with purely logistic dynamics [@walters_surplus_2008], and as such have been viewed as a limited tool for modelling more complex stocks. 
  
Two limitations of classic SPMs is the assumption of spatially constant productivity, and that stock productivity is affected only by stock abundance and fishing. These assumptions ignore the effect of global changes that are affecting fisheries, such as climate change, that affect the growth rates of stocks independently of fishing pressure. One example is that of the Northern Shrimp (*Pandalus borealis*) in the Newfoundland and Labrador Shelves, which has undergone several periods of large-scale biomass change in the last two decades, despite a relatively constant harvest regime [@dfo_assessment_2019]. These stocks currently lack a population model to understand the drivers of this change, and to predict how fishing pressure and changing environmental conditions may affect future abundance in the region. In the context of climate change and shifting ranges, fisheries productivity is likely to be a moving target [@karp_accounting_2019], and managers need better methods that account for varying productivity [@szuwalski_climate_2016].

<!-- Any pop models, including SPMs, can either be process based or statistical: we decide to implement a statistical approach so that we can benefit from confidence intervals -->

Population models like SPMs usually fall under two categories: process-based and statistical. Process-based models often rely on differential or difference equations and are based on replicating the underlying processes (e.g., predation, recruitment, dispersal) driving population dynamics. Statistical models instead fit a regression model to time series of population abundances, abundance indices, or productivities, with some assumed error distribution for variation around predictions. This allows for estimation of parameter uncertainty and ranges of model predictions, and for flexibly incorporating potential ecosystem drivers into models [@plaganyi2014Multispecies]. Statistical models also allow for straight-forward estimation of spatial variation in population parameters such as maximum productivity or density dependence from data, in the absence of theory predicting how these parameters should vary. In this paper, we use a statistical approach to fitting SPMs using Generalized Additive Models (GAMS), estimated using the `mgcv` R package [@wood_generalized_2017] as the backend. We apply this approach to the population of Northern Shrimp of the Newfoundland and Labrador Shelves, leveraging the smoothing properties of GAMs to account for varying productivity across time and space. The resulting model is a spatial SPM (SSPM), implemented via a R package `sspm`.

While the initial application of this model was modelling Newfoundland and Labrador Northern Shrimp stocks [@pedersenNewSpatialEcosystembased2021], The R package `sspm` is designed to make spatially-explicit surplus production models (SSPM) simpler to estimate and apply to any spatially structured stock. The package uses GAMs to estimate spatiotemporally varying biomass, and to estimate SSPMs based on changes in fitted biomass, observed catch, and spatially structured environmental predictors. It includes a range of features to manage biomass and harvest data. Those features are organized in a stepwise workflow, whose implementation is described in more detail in \autoref{fig:workflow}.

1. Discretization and aggregation of spatially structured observations into discrete patches, with a range of methods of discretization (random or custom sampling, Voronoi tessellation or Delaunay triangulation).
2. Spatiotemporal smoothing of biomass and environmental predictors using GAMs.
3. Computation of surplus productivity based on biomass density and fishing effort.
4. Fitting of SSPMs to productivity data with GAMs.
5. Visualization of results, including confidence and prediction intervals.
6. One-step-ahead prediction of biomass for model validation and scenario-based forecasting.

Although it was developed in a fisheries context, the package is suitable to model spatially-structured population dynamics in general.

# Package design

The package follows an object oriented design, making use of the S4 class systems. The different classes in the package work together to produce a stepwise workflow  (\autoref{fig:workflow}). 

The key workflow steps are: 

1. Delineation of the boundary of the region of interest for the model. Boundary data is provided as a shapefile and converted into a `sspm_boundary` object with a call to `spm_as_boundary()`.
2. The region within the boundary is discretized into patches with the `spm_discretize()` function, creating a `sspm_discrete_boundary` object.
3. The `spm_as_dataset()` function turns user-provided data frames of raw observations into `sspm_dataset` objects that explicitly track locations, data types, and aggregation scales for each input. `sspm` recognizes three types of data: **trawl** (i.e. biomass estimates from scientific surveys), **predictors**, and **catch** (i.e. harvest). 
4. The `spm_smooth` function use spatiotemporal GAM models to smooth the biomass and predictor data, based on the spatial structure from `sspm_discrete_boundary`. The user specifies a GAM formula with custom smooth terms. The output is another `sspm_dataset` object with a `smoothed_data` slot which contains the smoothed predictions for all patches.
5. The `spm_aggregate_catch` function aggregates catch into patches and years, and calculates patch-specific productivity for each year as the ratio of estimated biomass density plus catch from the next year, divided by estimated biomass density of the current year. The result is returned as a `sspm_dataset`.
6. The `sspm` function combines productivity and predictor datasets into a single dataset. Additionally, the user may create lagged versions of predictors with `spm_lag()` and split data into testing and training sets for model validation with `spm_split()` at this stage.
7. The `spm()` function is used to fit a SSPM model to the output of step 6, using a GAM model with custom syntax able to model a range of SSPMs. The output is an `sspm` object.
8. Predictions from the fitted model can be obtained using the built-in `predict()` method, and plots with the `plot()` method.

![The sspm workflow.\label{fig:workflow}](../man/figures/flowchart.png){width=90%}

# Acknowledgements

This research was supported by the canadian Department of Fisheries of Oceans (DFO) Sustainable fisheries Science Fund, and by a Discovery Grant from the canadian Natural Sciences and Engineering Research Council (NSERC) to E. J.Pedersen. We thank Fonya Irvine and John-Philip Williams for their help in testing the package and providing feedback on model implementation.

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
