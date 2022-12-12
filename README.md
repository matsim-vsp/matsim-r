# MATSIM R support package

MATSim <https://matsim.org> is an agent-based microsimulation package for large-scape transport simulations. 

This package mimics the functionality of the [Python matsim-tools](https://pypi.org/project/matsim-tools) package, and will eventually support standard MATSim networks, plans and events. 

Currently network files are supported, and many summarization plots based on the MATSim standard output_trips file.

This work is ongoing and submissions are welcome!

## Install

You can install the package right now:

- If you don't already have devtools installed, first run this:  
  - `install.packages("devtools")`

- Then run: 
  - `devtools::install_github("matsim-vsp/matsim-r")`

To install the package from specific branch use:
`devtools::install_github("matsim-vsp/matsim-r",ref="{name of the branch}")`

### Installing from .gz archive

You can also install directly from the source archive instead of using devtools:

- `install.packages("matsim-r.tar.gz", type="source", repos=NULL)`


## Documentation build instructions

`devtools::document()` builds docs and NAMESPACE

or run GNU Make to build the docs as above and also create HTML versions in the `html` folder.



