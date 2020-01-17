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

PREFIX=$(HOME)/Ada/installs

## Rules

compile:
	@echo Compiling...
	gprbuild utils.gpr
	gprbuild util_tools.gpr

install:
	@echo Installing into $(PREFIX)
	gprinstall -p --prefix=$(PREFIX) utils.gpr
	gprinstall -p --prefix=$(PREFIX) util_tools.gpr

params:
	@echo Install into: $(PREFIX)

all: compile install
