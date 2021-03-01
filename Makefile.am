# Copyright (C) 2017 Yann Pouillon <devops@materialsevolution.es>

#
# Top Makefile for LibFDF
#

# Information for aclocal, to rebuild makefiles on the fly when needed
ACLOCAL_AMFLAGS = -I config/m4

# Make sure that 'make distcheck' performs a minimal build
AM_DISTCHECK_CONFIGURE_FLAGS = --disable-debug

# Delegate all build tasks to subdirectories
SUBDIRS = src tests doc

# TODO: write script to generate the environment module
# Files to install for the Environment Modules
# See http://modules.sourceforge.net/ for details
#envmoddir = $(datarootdir)/$(PACKAGE)/modulefiles/libfdf
#nodist_envmod_DATA = config/data/libfdf/$(PACKAGE_VERSION)

# Files to install for pkg-config
# See http://www.freedesktop.org/wiki/Software/pkg-config/ for details
pkgconfigdir = $(libdir)/pkgconfig
nodist_pkgconfig_DATA = config/data/libfdf.pc

# Some files are not removed by default and cause "make distcleancheck" to fail
DISTCLEANFILES = \
  libfdf-*.tar.bz2 \
  libfdf-*.tar.gz \
  libfdf-*.tar.xz \
  config/data/libfdf.pc \
  config/data/libfdf/$(PACKAGE_VERSION) \
  config/data/libfdf/$(PACKAGE_VERSION).in

# We use our own version of Markdown to build Markdown documentation
EXTRA_DIST = config/scripts/markdown
