---
title: 'The sspm R package: spatial surplus production models for the management of northern shrimp fisheries'
tags:
  - R
  - fisheries
  - management
  - modelling
  - GAM
authors:
  - name: Valentin Lucet^[co-first author] # note this makes a footnote saying 'co-first author'
    orcid: 0000-0003-0268-818X
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Eric Pedersen^[co-first author] # note this makes a footnote saying 'co-first author'
    orcid: 0000-0003-1016-540X
    affiliation: 1
affiliations:
 - name: Concordia University, Montreal, Quebec, CA
   index: 1
date: 13 August 2017
bibliography: paper.bib
---

# Abstract

Productivity models such as Surplus Production Models (SPMs) models can be used to inform stock management of fisheries. However, those models often share three main flaws: (1) they are usually not spatially explicit, (2) fail to incorporate ecosystem predictors and therefore are ill-suited to ecosystem-based management of stocks, and (3) their deployment is often limited by code availability, quality and accessibility. To fill this gap, we developed a lag-1 autoregressive spatial SPM based on Generalized Additive Models (GAMs), broadly applicable to spatially-structured populations, and bundled into an R package. We applied this model to one of the most economically important invertebrate populations in Canadian waters, Northern Shrimp (Pandalus borealis) in the Newfoundland and Labrador Shelves. This stock currently lacks a population model to predict how fishing pressure and changing environmental conditions may affect future shrimp abundance in the region. Our model incorporates relevant ecosystem predictors for this stock, such as Atlantic Cod (Gadus morhua) density, alternate predator density, temperature, and stock biomass. In addition, the model is deployed through the R package sspm, a flexible framework aimed at making SSPMs easier to apply to spatially structured populations. The package allows for a repeatable and open workflow and improves the accessibility of SSPMs.

# Summary

The R package sspm is designed to make spatially-explicit surplus production models more applicable. The package uses Generalized Additive Models (GAMs) to fit a surplus production model to biomass and harvest data. The package includes a range of features to manage biomass and harvest data. Those features are organise in a stepwise workflow, whose implementation is described in more detailed in (\autoref{fig:workflow}).

1. Ingestion of variables as well as spatial boundaries and discretization into patches, using the user's method of choice (random or custom sampling, voroinoi tesselation or delaunay triangulation).
2. Smoothing data using spatio-temporal GAMs smoothers.
3. Computation of productivity values taking into account harvest information.
4. Fitting of SPMs to smoothed data with GAMs.
5. Visualization of results, including confidence and prediction intervals.
6. One step ahead prediction of biomass.

Although it was developed in a fisheries context, the package is suitable to model spatially-structured population dynamics in general.

# Statement of need

1. Population models, in particular fisheries productivity models, rarely integrates important spatially-structured ecosystem drivers
2. The Northern Shrimp stock in the Newfoundland and Labrador Shelves currently lacks a population model 
3. Current SPM models are rarely spatially explicit and isually cannot account for relevent ecosystem drivers
4. Fisheries managers lack user-friendly, flexible tools to implement and apply Spatial SPMs

# Introduction

Population modelling is an exercise of interest within environmental sciences and adjacent fields. From early models that addressed simple dynamics such as exponential growth and density dependence, modern models are now ackowledging the non-stationary nature of wild populations. In addition, population models applied to resource management, such as fisheries models, are incresaingly concerned with how stocks varies accross time and space. Resource managers are becoming more and more interested in how ecosystem factors such as predator abundance or abiotic varaibles impact the spatio-temporal variablibity of mechanisms like productivity and density dependence. 

Althought the non-statitionnaity of a wide range of populations has been demonstrated and established, and despite the push for more ecosystem-based management methods in fisheries management, efforts to include spatial dynamics and ecosystem variables in fisheries models are rare. One family of population models that rarely account for spatial structure is the family of Surplus production models (SPMs). SPMs are well-established tools for single-stock modelling. They usually assume spatially constant productivity. This assumption is a strong limitation in the context of the current global changes that are affecting fisheries, such as climate change. The global warming of waters is already having an impact on the spatial structure of stocks, as evidenced by the consistant northward shift of the northern Shrimp biomass (\autoref{fig:shift}). In this context, fisheries producvity is likely to be a mobing target, and managers are in need for better methods that account for varying productivity

Population models in fisheries science usually fall under two categories: process-based models and statistical models. Process based models often rely on differential equations and are based on replicating the underlying processes (predation, recruitment, dispersal) behind popupaltion dynamics. Statistical models, on the other hand, rely on fitting a model to data using distributionnal assumptions, and present the advantage of naturally measuring uncertainty around predictions. This is useful in a management context where uncertainty around decision-making is an important information to have on hand.

In this paper, we implement a statistical model for the population of northern shrimp of the Newfoundland and Labrador Shelves that accounts for varying productivity accross time and space. The model is implemented via a R package designed for this type ps spatial surplus productivity modelling, the sspm package. We exemplify how to successfully implement a spatial model with sspm and discuss the applicability of the framework to the other spatially structured populations.

<!-- ![Northward shift of weighted centroid of biomass trawled.\label{fig:shift}](figures/shift.png){width=75%} -->

# Model

Surplus production models (SPMs), are simply defined as such:

$$b_{t+1} = g(b_{t}) * e^{\epsilon}$$

With $b$ the biomass, $e^{\epsilon}$ an eror term, and $g$ a function of the biomass and time which, in the case of fisheries, usually involves some measure of harvest throught by catch.

Rho => varying prod => covariance matrix => precision => basis functions and Gams

# Results

The GAM biomass estimates are consistent with those of the current tool in use for the assessment of the stock, Ogmap, and provide valuable insights about the drivers of the rapid increase and decline of shrimp in the southern end of the shelf. Our approach demonstrates the model’s ability to become a useful tool for modelling spatially-structured populations like fisheries stocks. The sspm package successfully modularizes each step of the modelling process and implements a range of useful features for modeling spatially-structured populations: spatial discretization, simplified GAM syntax, prediction intervals and scenario based forecasts for longer-term trends. In a fisheries context, It illustrates how our model can be easily used by managers to forecast fisheries productivity under different management regimes. The package is also a tool to think about design choices when conceiving a user interface for managers and on best practices when it comes to adapting research code into management tools. Finally, our approach demonstrates how open source software tools can improve the accessibility and reliability of models for fisheries management.

# Package design

The package follows an object oriented design, making use of the S4 class systems.

![The sspm workflow.\label{fig:workflow}](figures/flowchart.png)<!--{ width=90% }-->

# Application to simulated data

We present an example using simulated biomass and harvest data. Using real trawl and fishing data provided by DFO, we generated fake data for each spatio-temporal units. ...

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
- 

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }
-->

# Acknowledgements

TBD

# References

TBD