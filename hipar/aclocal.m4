dnl CTK - Additional macros for `autoconf'

dnl ######################################################################
dnl This was stolen from Sven Panne's <Sven.Panne@informatik.uni-muenchen.de>
dnl HOpenGL library.
dnl ######################################################################
dnl GHC syslib versionitis
dnl
dnl CTK_LIB_SYSLIB(Foo, bar baz bing) tests in which of
dnl the syslibs bar, baz, and bing the module Foo resides and
dnl calls AC_SUBST(syslib_Foo) on it.
dnl ######################################################################

AC_DEFUN(CTK_LIB_SYSLIB, [
dnl The syslib variable name.
define(CTK_SYSLIB_NAME, syslib_$1)dnl
dnl The cache variable name.
define(CTK_CV_NAME, ctk_cv_syslib_$1)dnl
AC_CACHE_CHECK(syslib for $1, CTK_CV_NAME, [
for ctk_syslib in $2 ThisIsAMegaHack ; do
rm -rf conftest*
cat > conftest.hs <<EOF
import $1
EOF
$HC -M -optdep-f -optdepconftest.dep -syslib [$]ctk_syslib conftest.hs  > /dev/null 2> /dev/null && break
done
rm -rf conftest*
test "[$]ctk_syslib" = ThisIsAMegaHack && AC_MSG_ERROR(No syslib for $1 found)
CTK_CV_NAME=[$]ctk_syslib
])
CTK_SYSLIB_NAME=$CTK_CV_NAME
AC_SUBST(CTK_SYSLIB_NAME)dnl
undefine([CTK_CV_NAME])dnl
undefine([CTK_SYSLIB_NAME])dnl
])

dnl -- Stolen from FPTOOLS/GHC
dnl
dnl CTK_GHC_VERSION(version)
dnl CTK_GHC_VERSION(major, minor [, patchlevel])
dnl CTK_GHC_VERSION(version, major, minor, patchlevel)
dnl
dnl Test for version of installed ghc.  Uses $GHC.
dnl [original version pinched from c2hs]
dnl
AC_DEFUN(CTK_GHC_VERSION,
[define([CTK_CV_GHC_VERSION], [ctk_cv_ghc_version])dnl
AC_CACHE_CHECK([version of ghc], CTK_CV_GHC_VERSION, [dnl
${GHC-ghc} --version > conftestghc 2>&1
  cat conftestghc >&AC_FD_CC
dnl `Useless Use Of cat' award...
changequote(<<, >>)dnl
  CTK_CV_GHC_VERSION=`cat conftestghc | sed -n -e 's/, patchlevel *\([0-9]\)/.\1/;s/.* version \([0-9.]\+\).*/\1/p'`
changequote([, ])dnl
  rm -fr conftest*
  if test "[$]CTK_CV_GHC_VERSION" = ""
  then
    CTK_CV_GHC_VERSION='unknown'
  fi])
changequote(<<, >>)dnl
CTK_CV_GHC_VERSION<<_major>>=`echo <<$>>CTK_CV_GHC_VERSION | sed -e 's/^\([0-9]\).*/\1/'`
CTK_CV_GHC_VERSION<<_minor>>=`echo <<$>>CTK_CV_GHC_VERSION | sed -e 's/^[0-9]\.\([0-9]*\).*/\1/'`
CTK_CV_GHC_VERSION<<_pl>>=`echo <<$>>CTK_CV_GHC_VERSION | sed -n -e 's/^[0-9]\.[0-9]*\.\([0-9]*\)/\1/p'`
changequote([, ])dnl
if test "[$]CTK_CV_GHC_VERSION[_pl]" = ""
then
  CTK_CV_GHC_VERSION[_all]="[$]CTK_CV_GHC_VERSION[_major].[$]CTK_CV_GHC_VERSION[_minor]"
  CTK_CV_GHC_VERSION[_pl]="0"
else
  CTK_CV_GHC_VERSION[_all]="[$]CTK_CV_GHC_VERSION[_major].[$]CTK_CV_GHC_VERSION[_minor].[$]CTK_CV_GHC_VERSION[_pl]"
fi
ifelse($#, [1], [dnl
[$1]="[$]CTK_CV_GHC_VERSION[_all]"
], $#, [2], [dnl
[$1]="[$]CTK_CV_GHC_VERSION[_major]"
[$2]="[$]CTK_CV_GHC_VERSION[_minor]"
], $#, [3], [dnl
[$1]="[$]CTK_CV_GHC_VERSION[_major]"
[$2]="[$]CTK_CV_GHC_VERSION[_minor]"
[$3]="[$]CTK_CV_GHC_VERSION[_pl]"
], $#, [4], [dnl
[$1]="[$]CTK_CV_GHC_VERSION[_all]"
[$2]="[$]CTK_CV_GHC_VERSION[_major]"
[$3]="[$]CTK_CV_GHC_VERSION[_minor]"
[$4]="[$]CTK_CV_GHC_VERSION[_pl]"
], [AC_MSG_ERROR([wrong number of arguments to [$0]])])dnl
undefine([CTK_CV_GHC_VERSION])dnl
])dnl
