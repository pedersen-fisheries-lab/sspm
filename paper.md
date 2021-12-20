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

Spatial Surplus Production Models (SSPMs) are spatially explicit models of fisheries productivity designed to inform stock management. Like other fisheries management tools, such models can be technically convoluted and their deployment is often limited by code availability, quality and accessibility. We present the R package sspm, a flexible framework aimed at making SSPMs easier for managers to apply in the context of the Northern Shrimp (*Pandalus borealis*) fisheries. Although one of the most economically important stocks in Canadian waters, the Northern Shrimp in shrimp fishing areas (SFAs) 4 to 6 currently lack a population model to predict how fishing pressure and changing environmental conditions may affect future shrimp abundance. To fill this gap, we developed a lag-1 autoregressive SSPM that included predictors such as Atlantic Cod (*Gadus morhua*) density, alternate predator density, temperature, and Northern Shrimp biomass. This model was later adapted into the sspm package. We will show how the model design is effectively abstracted by the package design and further demonstrate how the package can be easily used by managers to forecast fisheries productivity under different management regimes. Finally we will discuss choices in the design of the user interface and reflect on best practices when it comes to adapting research code into management tools.

# Summary

TBD

# Statement of need

TBD

# Introduction

TBD

# Package design

TBD

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures
<!--
Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }-->

# Acknowledgements

TBD

# References
