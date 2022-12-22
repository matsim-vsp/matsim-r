# MATSIM R support package

MATSim <https://matsim.org> is an agent-based microsimulation package for large-scape transport simulations. 

This package mimics the functionality of the [Python matsim-tools](https://pypi.org/project/matsim-tools) package, and will eventually support standard MATSim networks, plans and events. 

Currently network files are supported, and many summarization plots based on the MATSim standard output_trips file.

One very useful command builds a [SimWrapper](https://vsp.berlin/simwrapper) dashboard from the standard MATSim `output_trips.xml.gz` file:

- `R -e "matsim::prepareSimwrapperDashboardFromFolder()"`

This work is preliminary and ongoing -- submissions are welcome!

---

## Install

You can install the package right now:

- If you don't already have devtools installed, first run this:  
  - `install.packages("devtools")`

- Then run: 
  - `devtools::install_github("matsim-vsp/matsim-r")`

To install the package from a specific branch use:
`devtools::install_github("matsim-vsp/matsim-r",ref="{name of the branch}")`

### Installing from .gz archive

Instead of using devtools you can also install directly from the source archive:

- `install.packages("matsim-r.tar.gz", type="source", repos=NULL)`

---

## Documentation build instructions

Run GNU Make to build the docs whenever source files in `R/` change. It will create the man pages as well as the fancy HTML `docs/` folder

Or, build yourself:

- `devtools::document()` builds docs and NAMESPACE
- `pkgdown::build_site()` builds the HTML docs

