#
# M4 macros for LibFDF (imported from Abinit)
#
# Copyright (C) 2011-2017 Yann Pouillon
#
# This file is part of the Scale-Up software package. For license
# information, please see the COPYING file in the top-level directory of the
# source distribution.



#
# Workarounds for portability issues
#



# AX_PROG_MKDIR_P()
# -----------------
#
# Wrapper for the bugged AC_PROG_MKDIR_P macro.
#
AC_DEFUN([AX_PROG_MKDIR_P],[
  _AC_SRCDIRS([.])
  AC_PROG_MKDIR_P
  ax_tmp_mkdir_p=`echo "${MKDIR_P}" | awk '{print [$]1}'`
  if test "${ax_tmp_mkdir_p}" = "config/gnu/install-sh"; then
    AC_MSG_NOTICE([fixing wrong path to mkdir replacement])
    MKDIR_P="${ac_abs_top_srcdir}/${MKDIR_P}"
  fi
  unset ax_tmp_mkdir_p
]) # AX_PROG_MKDIR_P
