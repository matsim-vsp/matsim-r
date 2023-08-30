# MATSIM R support package

[MATSim](https://matsim.org) is an agent-based microsimulation package for large-scale transport simulations. It's free software under the terms of the GNU General Public License. The user guide to MATSim can be found [here](https://matsim.org/docs/userguide/) and more information regarding the license [here](https://github.com/matsim-org/matsim-libs/blob/master/matsim/LICENSE).

This package mimics the functionality of the [Python matsim-tools](https://pypi.org/project/matsim-tools) package, and will eventually support standard MATSim networks, plans and events. 

Currently network files as well as many summarization plots based on the MATSim standard output_trips file, counts file and output_persons file are supported.

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

