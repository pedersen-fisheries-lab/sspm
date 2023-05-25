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

Population models are important tools for making management decisions, especially in fisheries, where predictive methods like Surplus Production Models (SPMs) are widely used. Fisheries analysts and managers often lack user-friendly, flexible tools to implement and apply SPMs. In addition, SPMs are rarely spatially explicit and usually cannot account for relevant ecosystem drivers. Therefore, there is a need for tools that implement spatially explicit surplus production models (SSPMs). The Northern Shrimp stock in the Newfoundland and Labrador Shelves is an example of a stock in need of an SSPM that can integrate important spatially-structured ecosystem drivers.

# Summary

<!-- Pop models are important tools in fisheries science, but most are obsolete as they fail to account for ecosystem variables and spatio-temporal dynamics -->

Population modelling is an exercise of interest within environmental sciences and adjacent fields. Early population models such as the logistic model assumed that while the abundance of a population might change over time, the conditions governing parameters affecting that rate of change, such as the maximum rate of growth or the carrying capacity of the population, stay constant over time [@gotelli_primer_2008]. Modern population models increasingly acknowledge the non-stationary nature of wild populations and work to incorporate environmental fluctuations into dynamic models [@thorson_importance_2015; @thorson_using_2017]. Population models designed to answer applied resource management questions, such as fisheries assessment models, increasingly address how the dynamics of stocks vary across space and time. 

Resource managers are becoming increasingly interested in how variation in ecosystem factors such as predator abundance and abiotic variables impact the spatiotemporal variability of population parameters, such as productivity [@zhang_longer_2021; @szuwalski_climate_2016]. Further, treating spatially structured stocks as single unstructured stocks can lead to substantially biased estimates of population change [@thorson_importance_2015]. However, stock models that explicitly incorporate spatial dynamics and time-varying ecosystem variables are still rare in fisheries science, despite the push for more ecosystem-based management methods in fisheries management [@crowder_impacts_2008; @berkes_implementing_2012; @tam_towards_2017]. 

<!-- SPMs are simple, old pop models, in need of updating to account for the non-stationarity of the mechanisms that maintain stocks -->

Surplus production models (SPMs) are one of the classic models used in fisheries and are based on modelling changes in the total biomass of a stock in a given location over time as a function of current stock abundance and fishing pressure [@walters_surplus_2008]. Classically, SPMs assume single unstructured stocks with purely logistic dynamics [@walters_surplus_2008] and, as such, have been of limited use for modelling more complex stocks. They are useful in data-poor contexts where the age structure of the population is not accessible or when age or length structure do not change substantially over time [@prager_suite_1994; @punt_extending_2003]. SPMs typically model spatially constant productivity. They also assume that populations are only affected by past abundance and fishing, which ignores stressors like climate change which affect growth rates independently of fishing pressure.

In the context of climate change and shifting ranges, fisheries productivity is likely to be a moving target [@karp_accounting_2019], and managers need better methods that account for varying productivity [@szuwalski_climate_2016]. The Northern Shrimp (*Pandalus borealis*) in the Newfoundland and Labrador Shelves, which has undergone several periods of large-scale biomass change in the last two decades, despite a relatively constant harvest regime, is a prime example of a population thought to be affected by environmental conditions [@dfo_assessment_2019]. These populations currently lack a population model to understand the drivers of this change and to predict how fishing pressure and changing environmental conditions may affect future abundance, which managers are advised to account for.

<!-- Any pop models, including SPMs, can either be process based or statistical: we decide to implement a statistical approach so that we can benefit from confidence intervals -->

Population models like SPMs usually fall under the two following categories: process-based and statistical models. Process-based models often rely on differential or difference equations and are based on replicating the underlying processes (e.g., predation, recruitment, dispersal) driving population dynamics. Statistical models instead fit a regression model to time series of population abundances, abundance indices, or productivities, with some assumed error distribution for variation around predictions. 

We have chosen a statistical approach to fitting SPMs. Statistical models allow for estimation of parameter uncertainty and ranges of model predictions and for flexibly incorporating potential ecosystem drivers into models [@plaganyi2014Multispecies]. Statistical models also allow for straight-forward estimation of spatial variation in population parameters such as maximum productivity or density dependence from data, in the absence of theory predicting how these parameters should vary. 

In this paper, we use a statistical approach to fitting SPMs using Generalized Additive Models (GAMS), estimated using the `mgcv` R package [@wood_generalized_2017] as the backend. We apply this approach to the population of Northern Shrimp of the Newfoundland and Labrador Shelves, leveraging the smoothing properties of GAMs to account for varying productivity across time and space. The resulting model is a spatial SPM (SSPM), implemented via an R package: `sspm`.

The R package `sspm` is designed to make spatially-explicit surplus production models (SSPM) simpler to estimate and apply to any spatially structured stock. The basic model it implements was first used to model time-varying production in Newfoundland and Labrador Northern Shrimp stocks [@pedersenNewSpatialEcosystembased2021]. However, the general modelling approach used here will work for any spatially structured fishery with sufficient data. It includes a range of features to manipulate harvest and biomass data. Those features are organized in a stepwise workflow, whose implementation is described in more detail in \autoref{fig:workflow} and in the next section.

Although it was developed in a fisheries context, the package is suited to model spatially-structured population dynamics in general.

# Package design

The package follows an object oriented design, making use of the S4 class systems to model a stepwise workflow: (\autoref{fig:workflow}). 

The key workflow steps are: 

* Discretization and aggregation of spatially structured observations into discrete patches, with a range of methods of discretization (random or custom sampling, Voronoi tessellation, or Delaunay triangulation).
	1. Provided boundary data in the form of a shapefile is converted into a `sspm_boundary` object using `spm_as_boundary()` to define the boundary/region of interest.
	2. The region within the boundary is discretized into patches with the `spm_discretize()` function, creating a `sspm_discrete_boundary` object.

* Spatiotemporal smoothing of biomass and environmental predictors using GAMs.
	3. The `spm_as_dataset()` function turns user-provided data frames of raw observations into `sspm_dataset` objects that explicitly track locations, data types, and aggregation scales for each input. `sspm` recognizes three types of data: **trawl** (i.e. biomass estimates from scientific surveys), **predictors**, and **catch** (i.e., harvest). 
	4. The `spm_smooth()` uses GAMs to calculate spatially smoothed yearly estimates of biomass and environmental predictors for each patch from trawl-level data, based on the spatial structure from the `sspm_discrete_boundary` object. The user specifies a GAM formula with custom smooth terms. The output is another `sspm_dataset` object with a `smoothed_data` slot which contains the smoothed predictions for all patches.

* Computation of surplus production based on biomass density and fishing effort.
	5. The `spm_aggregate_catch()` function aggregates catch into patches and years and calculates patch-specific productivity for each year as the ratio of estimated biomass density plus catch from the next year divided by estimated biomass density of the current year. The result is returned as a `sspm_dataset`.
	6. The `sspm()` function combines productivity and predictor datasets into a single dataset. Additionally, the user may create lagged versions of predictors with `spm_lag()` and split data into testing and training sets for model validation with `spm_split()` at this stage.

* Fitting of SSPMs to productivity estimates with GAMs.
	7. The `spm()` function is used to fit a SSPM model to the output of step 6, using a GAM model with custom syntax able to model a range of SSPMs. The output is an `sspm` object.

* Visualization of results, and one-step-ahead projections of biomass for model validation and scenario-based predictions.
	8. Plots can be generated with the `plot()` method. Predictions from the fitted model can be obtained using the built-in `predict()` method, including confidence and prediction intervals

![The sspm workflow. Gray cylinders represent raw, unprocessed sources of data. Each blue diamond shape represents a function processing a raw input and validating it, or producing an intermediate package object, represented as a green object. Secondary objects like formulas, which must be created by the user, are represented by a purple document shape. Finally, outputs are represented by a red document shape. The steps of the workflow as described above are denoted by dotted lines and corresponding step number. \label{fig:workflow}](../man/figures/flowchart.png){width=90%}


# Connections to other surplus-production based stock assessment approaches

The `sspm` package uses a model-based, random-effects based approach to estimate the effects of ecosystem drivers on surplus production across space and time. Our approach is conceptually related to the stochastic stock assessment approaches used by the R packages `spict` [@pedersen_stochastic_2017] and `jabba` [@winker_jabba_2018] R packages for surplus production modelling, in that we assume that biomass dynamics can be modelled as effectively a logistic growth model with both process and measurement error. While `sspm` does not currently have the capacity to model biomass dynamics as a continuous-time process, as with `spict`, or incorporate prior parameter information on catchability or biomass dynamics as in `jabba`, `sspm` can model spatially and temporally varying productivity, which is currently not possible in these models.

The `sspm` package can be viewed as a spatiotemporal Model of Intermediate Complexity  [a 'MICE-in-space' model; @thorson_spatio-temporal_2019] that can incorporate effects of other species and ecosystem drivers as well as changes in fishing pressure on stock status. Our approach is closely connected to approaches used by other modern model-based spatial abundance estimation software, such as the `VAST` R package [@thorson_spatio-temporal_2019] and the `sdmTMB` R package [@anderson2022sdmTMB]. Our method shares the same approach as both `VAST` and `sdmTMB` of using spatially explicit models to estimate local biomass density (\autoref{fig:workflow} steps 1,2, and 4), then aggregating up from those models to predict aggregate stock-level metrics such as total biomass and productivity (\autoref{fig:workflow} steps 8). The multiplicative surplus production model used by `sspm` is also conceptually similar to the vector-autoregessive model for biomass changes used by these two packages, as both `VAST` and `sdmTMB` can model local temporal changes as autoregressive processes on the link-scale of a generalized linear model. The `sspm` package cannot, however, model the dynamics of multiple species simultaneously; multi-species modelling would require generating a separate surplus production model (\autoref{fig:workflow} steps 5 and 6) for each species of interest.

One major difference between the `sspm` package and other model-based spatiotemporal modelling packages is its special-purpose nature. The default `spatial_smooth` function uses a computationally simpler (although somewhat less flexible) Intrinsic Conditional Autoregressive (ICAR) model [@rueGaussianMarkovRandom2005] for modelling spatial variation in covariates and biomass, as compared to the more complex spatial random effects possible with `VAST` and `sdmTMB`. This has the advantage of computational speed and less user knowledge of how to set up complex spatial grids, although it is less flexible. This means that `sspm` should be easier to adapt to novel fisheries than more complex packages that require more user modelling knowledge. Further, it is possible to specify alternative spatial smoothers than the ICAR model in the `spatial_smooth` function via the `bs=` argument, although this functionality has not been well-tested and should be considered experimental. 

The other benefit of `sspm`, relative to other  modelling packages,  is the ability to model productivity rates directly (\autoref{fig:workflow} steps 5 and 6), rather than implicitly via an auto-regressive processes as used in `VAST` or `sdmTMB`. This means that  it is possible in `sspm` to model nonlinear relationships between environmental covariates and productivity, or to easily include factors such as time-lagged effects of predictors on productivity in a given year. This approach does, however, sacrifice the ability to propagate measurement error into uncertainty about rates of change. One of the future directions for development of this package is to include variance propagation methods into the surplus production modelling step.

# Acknowledgements

This research was supported by the Canadian Fisheries and Oceans Canada's (DFO) Sustainable fisheries Science Fund and by a Discovery Grant from the Canadian Natural Sciences and Engineering Research Council (NSERC) to E. J.Pedersen. We thank Fonya Irvine and John-Philip Williams for their help in testing the package and providing feedback on model implementation.

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
