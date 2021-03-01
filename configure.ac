# Copyright (C) 2017 Yann Pouillon <devops@materialsevolution.es>

#
# Process this file with autoconf to produce a configure script.
#

# Enhance help message (must be set before anything else)
m4_divert_text([HELP_END], [
More information on how to configure the build of LibFDF is available
in the doc/ subdirectory of the source tree, in particular in the
configuring-libfdf.md file.])

# Init Autoconf
AC_PREREQ([2.69])
AC_INIT([LibFDF], [0.2.2],
  [https://bugs.launchpad.net/libfdf],
  [libfdf],
  [https://launchpad.net/libfdf])
AC_CONFIG_AUX_DIR([config/gnu])
AC_CONFIG_MACRO_DIR([config/m4])
AC_CONFIG_SRCDIR([src/fdf.F90])

# Init Automake (assume GNU Make for now)
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.13 parallel-tests color-tests])
AM_CONFIG_HEADER([config.h])

# Generate timestamp
fdf_timestamp=`date '+%Y%m%dT%H%M%S%z'`
AC_SUBST(fdf_timestamp)

                    # ------------------------------------ #

#
# System utilities
#

# Check for common programs and features
AC_PROG_AWK
AC_PROG_GREP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_SED

# Workaround for the wrong path to install-sh on Mac systems
AX_PROG_MKDIR_P

# Init variables
fdf_core_incs=""
AC_SUBST(fdf_core_incs) 

                    # ------------------------------------ #

#
# Default settings
#

# Note for developers: you may edit these parameters to tune the behaviour
# of the build system. Please do it with care.

# Optional features
fdf_debug_enable_def="no"

                    # ------------------------------------ #

#
# Check consistency of defaults
#

# Default triggers for optional features must be yes or no, and not empty
for fdf_cfg_default in debug; do
  tmp_default=`eval echo \$\{fdf_${fdf_cfg_default}_enable_def\}`
  if test "${tmp_default}" != "no" -a \
          "${tmp_default}" != "yes"; then
    AC_MSG_ERROR([invalid default value: fdf_${fdf_cfg_default}_enable_def = ${tmp_default}])
  fi
done

# Clean-up
unset tmp_default

                    # ------------------------------------ #

#
# Optional features
#

# Debugging
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
    [Enable verbose function output (default: ${fdf_debug_enable_def})])],
  [fdf_debug_enable="${enableval}"; fdf_debug_type="yon"],
  [fdf_debug_enable="${fdf_debug_enable_def}"; fdf_debug_type="def"])
AC_SUBST(enable_debug)

                    # ------------------------------------ #

#
# Check option consistency
#

# All --enable-* options must be yes or no
for fdf_cfg_option in debug; do
  tmp_option=`eval echo \$\{enable_${fdf_cfg_option}\}`
  if test "${tmp_option}" != "" -a \
          "${tmp_option}" != "no" -a \
          "${tmp_option}" != "yes"; then
    AC_MSG_ERROR([--enable-${fdf_cfg_option} must be "yes" or "no"])
  fi
done
unset tmp_option

                    # ------------------------------------ #

#
# Substitute build-system variables
#

# Defaults
AC_SUBST(fdf_debug_enable_def)

# Triggers
AC_SUBST(fdf_debug_enable)

# Initialization types
AC_SUBST(fdf_debug_type)

                    # ------------------------------------ #

#
# C language support
#

# Init C compiler and preprocessor
if test "${CC}" != "" -a ! -x "${CC}"; then
  fdf_cc_probe=`echo "${CC}" | sed -e 's/ .*//'`
  if test ! -x "${fdf_cc_probe}"; then
    AC_PATH_PROG([fdf_cc_path],[${fdf_cc_probe}])
    if test "${fdf_cc_path}" = ""; then
      AC_MSG_ERROR([could not run C compiler "${CC}"])
    fi
  fi
fi
AC_PROG_CC
AC_PROG_CPP

                    # ------------------------------------ #

#
# Fortran language support
#

# Init Fortran compiler
if test "${FC}" != "" -a ! -x "${FC}"; then
  fdf_fc_probe=`echo "${FC}" | sed -e 's/ .*//'`
  if test ! -x "${fdf_fc_probe}"; then
    AC_PATH_PROG([fdf_fc_path], [${fdf_fc_probe}])
    if test "${fdf_fc_path}" = ""; then
      AC_MSG_ERROR([could not run Fortran compiler "${FC}"])
    fi
  fi
fi
AC_PROG_FC

# Enforce strict file extensions
fdf_fc_src_ok="unknown"
AC_FC_SRCEXT([F90], [fdf_fc_src_ok="yes"], [fdf_fc_src_ok="no"])
if test "${fdf_fc_src_ok}" != "yes"; then
  AC_MSG_WARN([Fortran file extension could not be changed])
  AC_MSG_WARN([Fortran tests may fail])
fi

# Look for the iso_c_binding module
fdf_fc_iso_c_binding="unknown"
AC_MSG_CHECKING([for the ISO_C_BINDING Fortran module])
AC_LANG_PUSH([Fortran])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM([], [[use iso_c_binding]])],
  [fdf_fc_iso_c_binding="yes"],
  [fdf_fc_iso_c_binding="no"])
AC_LANG_POP([Fortran])
AC_MSG_RESULT([${fdf_fc_iso_c_binding}])
if test "${fdf_fc_iso_c_binding}" != "yes"; then
  AC_MSG_ERROR([the Fortran compiler does not provide
                the iso_c_binding module.
                This probably happens because this compiler
                is an antiquity.
                Please disable Fortran support or use a modern
                Fortran compiler.])
fi

# Fortran compiler peculiarities
AX_F90_MODULE_EXTENSION
AC_SUBST(ax_cv_f90_modext)
AX_F90_MODULE_CASE

# Language mixing
#AC_FC_WRAPPERS

# Inform Automake
AM_CONDITIONAL([F90_MOD_UPPERCASE],
  [test "${ax_cv_f90_mod_uppercase}" = "yes"])

                    # ------------------------------------ #

#
# Libtool configuration
#

# Init Libtool (must be done once compilers are fully set)
LT_INIT
LT_PREREQ([2.4])
LT_LANG([Fortran])
LTOBJEXT="lo"
AC_SUBST(LTOBJEXT)

                    # ------------------------------------ #

#
# Build-system configuration checkpoint
#

# Write down YAML configuration before any final check
AC_OUTPUT([config/data/libfdf-config.yml])

                    # ------------------------------------ #

#
# Adjust build parameters according to configure options
#

# Debugging support
if test "${fdf_debug_enable}" = "yes"; then
  AC_DEFINE([FDF_DEBUG], 1, [Define to 1 to enable internal debugging.])
fi

                    # ------------------------------------ #

# Output configuration
AC_CONFIG_FILES([
  Makefile
  config/data/libfdf.pc
  doc/Makefile
  src/Makefile
  tests/Makefile])
AC_OUTPUT

# Report configuration
AC_MSG_NOTICE([
    Final build parameters
    ----------------------
    
    TSTAMP       = ${fdf_timestamp}
    
    DEBUG        = ${fdf_debug_enable} (init: ${fdf_debug_type})
    
    CPP          = ${CPP}
    CPPFLAGS     = ${CPPFLAGS}
    CC           = ${CC}
    CFLAGS       = ${CFLAGS}
    FC           = ${FC}
    FCFLAGS      = ${FCFLAGS}
    LDFLAGS      = ${LDFLAGS}
    LIBS         = ${LIBS}
])
