# Copyright (C) 2017 Yann Pouillon

#
# Makefile for LibFDF tests
#

# Make library modules available globally
AM_FCFLAGS = -I../src

# Define test programs
check_PROGRAMS = sample

sample_SOURCES = sample.F90
sample_LDADD = ../src/libfdf.la

# Define test suite
TESTS = sample

# Distribute remaining test materials
EXTRA_DIST = \
  Coords.dat \
  Otherfile \
  XY.fdf \
  coords.fdf \
  hostfile \
  sample.fdf \
  submit.sh
