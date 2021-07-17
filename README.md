# Causal Map Functions

This is a set of functions for manipulating and simplifying causal maps. 

Load example data:

```
devtools::install_github("stevepowell99/CausalMapFunctions")
library(CausalMapFunctions)
cashTransferMap
```

Visualise it:

```
cashTransferMap %>% make_vn
```

These functions are used in the [Causal Map Viewer](https://causalmap.shinyapps.io/CausalMapViewer/) and are similar to those used in the [Causal Map app](http://causalmap.app). In our roadmap, these functions will eventually replace their counterparts in the Causal Map app. 

The main functions input and output augmented [tidygraph](https://github.com/thomasp85/tidygraph) graphs. 
The package defines a kind of object called a tidymap which is just a tidygraph but with additional statements, sources and questions tables stored as attributes. This means that a tidymap has class = "tidymap"   "tbl_graph" "igraph", so igraph functions can be applied to it.

Statement tables are used in Causal Map as the underlying data which is to be coded into causal links. So each link refers to a quote from a particular statement. Statements are optional and this package can be used without them.
(If you know Tidygraph, You might expect an `activate(statements)` command but this is not implemented yet.)

The package also contains some UI functions used in the Causal Map Viewer which provide plot and table outputs.

In this package, nodes are called `factors` and the edges are called `links`.

## Motivation

There are many existing tools for analysing systems diagrams and also undirected graphs, we identified a need for a tool for analysing the kind of causal maps produced in evaluation research such as programme theories where:

- there may be many causal factors, with long text labels 
- the text labels are important because they may be used for searching and filtering
- causal factors may be nested into causal hierarchies
- there may be many co-terminal links between pairs of factors which should sometimes merged and sometimes not
- the main focus is often on a flow of influence from a small set of intervention variables to a small set of focused variables downstream of them, such as key outcomes 

## Features

- As well as an interactive map, the package wraps DiagrammeR and igraph to produce left-to-right layouts which are particularly suitable for directed maps.
- The new metric `Path robustness` is implemented alongside familiar node and graph metrics.
- For hierarchical coding, tools are provided to zoom in and out of individual hierarchies and entire maps.

## How it works

A family of functions `pipe_*` are provided (manipulate, calculate, hide, combine...) to implement successive filtering operations on a tidymap. All these operations do not actually change the data. 

So you import/load data as a tidymap and all your work steps are then just applying successive filters.

Each filter returns another tidymap, suitably filtered. 

All of these filters can be produced and edited either in a chain of actual R functions or in the simplified format which is processed by the parser.

If you filter the factors of a tidymap, e.g. show only factors with labels beginning xyz, 

- also the links are filtered (removing links to removed factors)
- the statements are not touched 

If you filter the links of a tidymap, e.g. show only links with hashtags containing xyz, 

- the factors are not filtered (but using a different command you can remove any factors which no longer have any links)
- the statements are not touched 

If you filter the statements of a tidymap, e.g. show only statements with texts containing xyz, 

- also the links are filtered (removing links to removed statements)
- the factors are filtered

So your workflow is always:

Import or load a tidymap / Filter it / Filter it / Filter it / Output an interactive map, a print map or a table.    

## Parser

There is also a parser which takes text strings with a simpler command syntax as input and outputs one of these main functions for each line of text. This parser is used to read text commands from the input window in Causal Map Viewer and manipulate the output map with the corresponding functions. The input text can also consist of several lines, and the commands are applied one by one in sequence, in a pipeline of commands, such that after each command, such as each command starts with the map defined by the previous line and produces a new one. 

## Output functions

There are three output functions which are thin wrappers around visNetwork, DiagrammeR and DT, allowing a graph to be displayed using any of these three visualisation engines. 


### Additional fields

Some of the commands such as path tracing create additional fields or variables for each factor and/or link. For example, when filtering by label, fields are created which can then be used to apply formatting. 



