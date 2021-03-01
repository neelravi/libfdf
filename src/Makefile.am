# Copyright (C) 2017 Yann Pouillon <devops@materialsevolution.es>
#

#
# Makefile for the FDF library
#

# Core source files
fdf_srcs = \
  prec.F90 \
  utils.F90 \
  parse.F90 \
  io_fdf.F90 \
  fdf.F90

# Fortran modules
# Note: never expect they will have the same name as their source
MODEXT = @ax_cv_f90_modext@
if F90_MOD_UPPERCASE
  fdf_mods = \
    FDF.$(MODEXT) \
    IO_FDF.$(MODEXT) \
    PARSE.$(MODEXT) \
    PREC.$(MODEXT) \
    UTILS.$(MODEXT)
else
  fdf_mods = \
    fdf.$(MODEXT) \
    io_fdf.$(MODEXT) \
    parse.$(MODEXT) \
    prec.$(MODEXT) \
    utils.$(MODEXT)
endif

# Define the FDF library
lib_LTLIBRARIES = libfdf.la

libfdf_la_SOURCES = $(fdf_srcs)
libfdf_la_LDFLAGS = -version-info 0:0:0

# Install Fortran modules
f03moddir = $(includedir)
nodist_f03mod_DATA = $(fdf_mods)
$(fdf_mods): libfdf.la

# Explicit object dependencies (to allow make -j with Fortran)
fdf.$(LTOBJEXT): \
  prec.$(LTOBJEXT) \
  utils.$(LTOBJEXT) \
  parse.$(LTOBJEXT) \
  io_fdf.$(LTOBJEXT)
io_fdf.$(LTOBJEXT): \
  prec.$(LTOBJEXT) \
  utils.$(LTOBJEXT)
parse.$(LTOBJEXT): \
  prec.$(LTOBJEXT) \
  utils.$(LTOBJEXT)
utils.$(LTOBJEXT): \
  prec.$(LTOBJEXT)

# Local cleaning
CLEANFILES = $(fdf_mods)
