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

## Rules
compile: libs tools

libs: libs-relocatable libs-static

libs-static:
	@echo "Compiling libraries in static type"	
	gprbuild $(gprbuild_params) -XLIBRARY_KIND=static -P console_utils.gpr

libs-relocatable:
	@echo "Compiling libraries in relocatable types"
	gprbuild $(gprbuild_params) -XLIBRARY_KIND=relocatable -P console_utils.gpr

tools:
	@echo "Compiling tools"
	gprbuild $(gprbuild_params) console_util_tools.gpr

install: uninstall
	@echo Installing into $(prefix)
	gprinstall -p --prefix=$(prefix) console_utils.gpr
	gprinstall -p --prefix=$(prefix) console_util_tools.gpr

uninstall:
	@echo Uninstalling from $(prefix)
	-gprinstall --prefix=$(prefix) --uninstall console_utils.gpr
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
