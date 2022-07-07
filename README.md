# MATSIM R support package

MATSIM <https://matsim.org> is an agent-based microsimulation package for large-scape transport simulations. 

This package mimics the functionality of the [Python matsim-tools](https://pypi.org/project/matsim-tools) package, and will support standard MATSIM networks, plans and events. 

Currently network files are supported.

This work is ongoing and submissions are welcome!

## Install

You can install the package right now using:
`devtools::install_github("matsim-vsp/matsim-r")`
To install the package from specific branch use:
`devtools::install_github("matsim-vsp/matsim-r",ref="{name of the branch}")`

If you don't already have devtools:  `install.packages("devtools")` first!

## Build instructions

`devtools::document()` builds docs and NAMESPACE

