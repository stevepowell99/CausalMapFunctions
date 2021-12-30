# Causal Map Functions

This is a set of functions for manipulating and simplifying causal maps. 

Load example data:

```
devtools::install_github("stevepowell99/CausalMapFunctions")
library(CausalMapFunctions)
example2
```

Visualise it:

```
example2 %>% make_interactive_map
```

These functions are used in the [Causal Map App](https://causalmap.shinyapps.io/CausalMap2/). 

There is more documentation of the functions in our [Vignette](articles/examples.html). 

There is extensive documentation of the app in our [Guide](https://guide.causalmap.app/). 


## Motivation

There are many existing tools for analysing systems diagrams and also undirected graphs, we identified a need for a tool for analysing the kind of causal maps produced in evaluation research such as programme theories where:

- there may be many causal factors, with long text labels 
- the text labels are important because they may be used for searching and filtering
- causal factors may be nested into causal hierarchies
- there may be many co-terminal links between pairs of factors which should sometimes merged and sometimes not
- the main focus is often on a flow of influence from a small set of intervention variables to a small set of focused variables downstream of them, such as key outcomes 

## Features

- As well as an interactive map, the package wraps DiagrammeR and igraph to produce left-to-right layouts which are particularly suitable for directed maps.
- The package provides many new tools for analysing and aggregating causal maps such as `Path robustness`, alongside familiar node and graph metrics.
- For hierarchical coding, tools are provided to zoom in and out of individual hierarchies and entire maps.

