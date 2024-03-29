### Makefile --- 

## Author: cnngimenez
## Version: $Id: Makefile,v 0.0 2020/01/16 19:45:18  Exp $
## Keywords: 
## X-URL: 

# Copyright 2020 cnngimenez

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


-include makefile.setup

ifndef prefix
    prefix=$(HOME)/Ada/installs
endif

gprbuild_params=-p
# gprbuild_params+=-d

# # Uncomment for verbose output: shows compilation commands.
# gprbuild_params+=-vh

ifndef optimisation
	optimisation=debug
endif

.PHONY: compile libs libs-static lib-relocatable tools install uninstall install-libs install-tools uninstall-libs uninstall-tools clean clean-relocatable clean-static clean-tools parmas all setup

## Rules
compile: libs tools

libs: libs-relocatable libs-static

libs-static:
	@echo "Compiling libraries in static type"	
	gprbuild $(gprbuild_params) -XLIBRARY_KIND=static -XOPTIMISATION=$(optimisation) -P console_utils.gpr

libs-relocatable:
	@echo "Compiling libraries in relocatable types"
	gprbuild $(gprbuild_params) -XLIBRARY_KIND=relocatable -XOPTIMISATION=$(optimisation) -P console_utils.gpr

tools:
	@echo "Compiling tools"
	gprbuild $(gprbuild_params) -XOPTIMISATION=$(optimisation) console_util_tools.gpr

install: install-libs install-tools

install-libs: uninstall-libs
	@echo Installing into $(prefix)
	gprinstall -p --prefix=$(prefix) console_utils.gpr

install-tools: uninstall-tools
	@echo Installing into $(prefix)
	gprinstall -p --prefix=$(prefix) console_util_tools.gpr

uninstall: uninstall-libs uninstall-tools

uninstall-libs:
	@echo Uninstalling from $(prefix)
	-gprinstall --prefix=$(prefix) --uninstall console_utils.gpr

uninstall-tools:
	@echo Uninstalling from $(prefix)
	-gprinstall --prefix=$(prefix) --uninstall console_util_tools.gpr

clean: clean-relocatable clean-static clean-tools

clean-relocatable:
	gprclean -XLIBRARY_KIND=relocatable console_utils.gpr

clean-static:
	gprclean -XLIBRARY_KIND=static console_utils.gpr

clean-tools:
	gprclean console_util_tools.gpr

params:
	@echo Install into: $(prefix)
	@echo Kind of library (dynamic/static): Both

all: compile install

setup:
	echo "Creating makefile.setup file with current settings..."
	echo

	echo "## ## Makefile personal setup ##" > makefile.setup
	echo "## Edit this file with your own settings" >> makefile.setup

	echo "## Where are the .ads files?" >> makefile.setup
	echo "ADA_INCLUDE_PATH=$(ADA_INCLUDE_PATH)" >> makefile.setup
	echo "GPR_PROJECT_PATH=$(GPR_PROJECT_PATH)" >> makefile.setup

	echo "## ## Install Parameters ##" >> makefile.setup
	echo "## Where should I install the files?" >> makefile.setup
	echo "prefix=$(prefix)" >> makefile.setup

	echo
	echo "Edit the makefile.setup file with your personal parameters."
	echo "Makefile will use it as long as it exists."


### Makefile ends here
