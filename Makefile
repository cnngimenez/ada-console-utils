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

### Makefile ends here

LIBRARY_KIND=dynamic
PREFIX=$(HOME)/Ada/installs

## Rules
compile: libs tools

libs:
	@echo "Compiling libraries"
	@echo "Making $(LIBRARY_KIND) libraries"
	gprbuild console_utils.gpr

tools:
	@echo "Compiling tools"
	gprbuild console_util_tools.gpr

install:
	@echo Installing into $(PREFIX)
	gprinstall -p --prefix=$(PREFIX) console_utils.gpr
	gprinstall -p --prefix=$(PREFIX) console_util_tools.gpr

uninstall:
	@echo Uninstalling from $(PREFIX)
	gprinstall --prefix=$(PREFIX) --uninstall console_utils.gpr
	gprinstall --prefix=$(PREFIX) --uninstall console_util_tools.gpr

clean:
	gprclean console_utils.gpr
	gprclean console_util_tools.gpr

params:
	@echo Install into: $(PREFIX)
	@echo Kind of library (dynamic/static): $(LIBRARY_KIND)

all: compile install
