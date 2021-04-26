# Causal Map Functions

This is a set of functions for manipulating and simplifying causal maps. 

Load example data with:

```
cashTransferMap %>% make_vn
```

These functions are used in the [Causal Map Viewer](https://causalmap.shinyapps.io/CausalMapViewer/) and are similar to those used in the [Causal Map app](http://causalmap.app). In our roadmap, these functions will eventually replace their counterparts in the Causal Map app. 

The main functions input and output [tidygraph](https://github.com/thomasp85/tidygraph) graphs. 

There are many existing tools for analysing systems diagrams and also undirected graphs, we identified a need for a tool for analysing the kind of causal maps produced in evaluation research such as programme theories where

- text labels may be long and are important because they may be used for searching and filtering
- there may be many links between pairs of factors which should sometimes merged and sometimes not
- the main focus is often on a flow of influence from a small set of intervention variables to a small set of focused variables downstream of them, such as key outcomes. 


## Parser

There is also a parser which takes text strings with a simpler command syntax as input and outputs one of these main functions. This parser is used to read text commands from the input window in Causal Map Viewer and manipulate the output map with the corresponding functions. The input text can also consist of several lines, and the commands are applied one by one in sequence, in a pipeline of commands, such that after each command, such as each command starts with the map defined by the previous line and produces a new one. 

## Output functions

There are three output functions which are thin wrappers around visNetwork, DiagrammeR and DT, allowing a graph to be displayed using any of these three visualisation engines. 


## Main functions

### Select top factors or links

Show the most frequently mentioned links and or the most frequently mentioned factors

### Search for factor labels 

You can search for whole labels and also for including keywords or flags which are common to more than one factor. 

Sets of labels separated by the letters OR. 

With this command, just the matching factors identified together with the "ego network" i.e. links between these nodes which have been found. However, by adding the keywords `up` and `down` each followed by a number it is possible to add in factors which are a given number of steps upstream and or downstream of the identified factors. 

### Filter by value

You can filter the map to only show factors and/or links which match either fields in the original data and/or fields created by the app, e.g. frequency.

### Path tracing

This is a powerful command which allows the user to trace paths from one or more upstream factors to one or more downstream factors. Only links which are part of such paths are displayed. 

### Zooming with hierarchical coding

Another important ability of the Causal Map Viewer is to manipulate maps which use hierarchical coding, which is a very powerful way to code causal information. It is possible to globally by roll up all factors into up to a certain level, for example, to roll them all up to their top level. But it is also possible to selectively zoom individual hierarchies while leaving others intact to any given level. 


### Remove

Removes any factors which have no links 


### Hide

A reverse search which allows the user to hide any factor containing one or more specified strings


### Additional fields

Some of the commands such as path tracing create additional fields or variables for each factor and/or link. For example, when filtering by label, fields are created which can then be used to apply formatting. 

### Labels

For example it is possible to add the factor frequency to the factor labels. More than one label can be added: Labels are additive and are applied one after the other. 

Other formatting commands such as factor and link colour can either specify a fixed colour (`links colour blue`) or can be used conditionally, so that a the colour or transparency of links can depend on any custom field existing in the original data such as, say, `gender` but also on fields created by the app e.g. frequency. 


