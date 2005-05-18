dnl C->Haskell - Additional macros for `autoconf'


dnl ### Checking compiler characteristics

dnl Currently NOT used anymore.
dnl
dnl C2HS_CHECK_SIZEOF(TYPE [, CROSS-SIZE]) - Compute the size of a type
dnl
dnl * Adapted from `acgeneral.m4'; AC_PROVIDES the feature and sets the 
dnl   shell variable of the same name and calls AC_SUBST on it.

AC_DEFUN(C2HS_CHECK_SIZEOF,
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<C2HS_TYPE_NAME>>, translit(sizeof_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<C2HS_CV_NAME>>, translit(c2hs_cv_sizeof_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(size of $1)
AC_CACHE_VAL(C2HS_CV_NAME,
[AC_TRY_RUN([#include <stdio.h>
main()
{
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  fprintf(f, "%d\n", sizeof($1));
  exit(0);
}], C2HS_CV_NAME=`cat conftestval`, C2HS_CV_NAME=0, 
    ifelse([$2], , , C2HS_CV_NAME=$2))])dnl
AC_MSG_RESULT($C2HS_CV_NAME)
AC_DEFINE_UNQUOTED(C2HS_TYPE_NAME, $C2HS_CV_NAME)
C2HS_TYPE_NAME=$C2HS_CV_NAME
AC_SUBST(C2HS_TYPE_NAME)dnl
AC_PROVIDE($C2HS_TYPE_NAME)
undefine([C2HS_TYPE_NAME])dnl
undefine([C2HS_CV_NAME])dnl
])

dnl Currently NOT used anymore.
dnl
dnl C2HS_CHECK_ALIGNOF(TYPE) - Compute the alignment restriction of a type
dnl
dnl * Adapted from the corresponding test in the Glasgow Haskell Compiler's
dnl   `fptools' configuration written by Simon Marlow(?)
dnl
dnl * This sets the preprocessor symbol ALIGNOF_<type> as well as sets the 
dnl   shell variable of the same name and calls AC_SUBST on it.
dnl
dnl * requires SIZEOF test but AC_CHECK_SIZEOF doesn't call PROVIDE so we 
dnl   can't call REQUIRE)

AC_DEFUN(C2HS_CHECK_ALIGNOF,
[changequote(<<, >>)dnl
dnl The name to #define and to substitute.
define(<<C2HS_TYPE_NAME>>, translit(alignof_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<C2HS_CV_NAME>>, translit(c2hs_cv_alignof_$1, [ *], [_p]))dnl
dnl The name of the corresponding size.
define(<<C2HS_SIZEOF_TYPE_NAME>>, translit(sizeof_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name of the corresponding size.
define(<<C2HS_CV_SIZEOF_NAME>>, translit(c2hs_cv_sizeof_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_REQUIRE($C2HS_SIZEOF_TYPE_NAME)
AC_MSG_CHECKING(alignment of $1)
AC_CACHE_VAL(C2HS_CV_NAME,
[AC_TRY_RUN([
#include <stdio.h>
#if HAVE_STDDEF_H
#include <stddef.h>
#endif
#ifndef offsetof
#define offsetof(ty,field) ((size_t)((char *)&((ty *)0)->field - (char *)(ty *)0))
#endif
int
main()
{
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  fprintf(f, "%d\n", offsetof(struct { char c; $1 ty;},ty));
  exit(0);
}],
C2HS_CV_NAME=`cat conftestval`,
C2HS_CV_NAME=$C2HS_CV_SIZEOF_NAME,
C2HS_CV_NAME=$C2HS_CV_SIZEOF_NAME)])
AC_MSG_RESULT($C2HS_CV_NAME)
AC_DEFINE_UNQUOTED(C2HS_TYPE_NAME, $C2HS_CV_NAME)
C2HS_TYPE_NAME=$C2HS_CV_NAME
AC_SUBST(C2HS_TYPE_NAME)dnl
AC_PROVIDE($C2HS_TYPE_NAME)
undefine([C2HS_TYPE_NAME])dnl
undefine([C2HS_CV_NAME])dnl
undefine([AC_CV_SIZEOF_NAME])dnl
])

dnl -- Pinched from FPTOOLS/GHC
dnl
dnl We keep the CTK_ prefix as this test is shared with CTK and we want to get 
dnl the cache value.
dnl
dnl CTK_GHC_VERSION(version)
dnl CTK_GHC_VERSION(major, minor [, patchlevel])
dnl CTK_GHC_VERSION(version, major, minor, patchlevel)
dnl
dnl Test for version of installed ghc.  Uses $GHC.
dnl [original version pinched from c2hs]
dnl
dnl NB: Don't use `+' in sed regexps; Jonas Svensson reports problems with it
dnl     on Solaris 8.
dnl
AC_DEFUN(CTK_GHC_VERSION,
[define([CTK_CV_GHC_VERSION], [ctk_cv_ghc_version])dnl
AC_CACHE_CHECK([version of ghc], CTK_CV_GHC_VERSION, [dnl
${GHC-ghc} --version > conftestghc 2>&1
  cat conftestghc >&AC_FD_CC
dnl `Useless Use Of cat' award...
changequote(<<, >>)dnl
  CTK_CV_GHC_VERSION=`cat conftestghc | sed -n -e 's/, patchlevel *\([0-9]\)/.\1/;s/.* version \([0-9.][0-9.]*\).*/\1/p'`
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

dnl -- Pinched from Michael Weber's HaskellMPI
dnl
dnl We keep the CTK_ prefix as this test is shared with CTK.
dnl
dnl CTK_PROG_CHECK_VERSION(VERSIONSTR1, TEST, VERSIONSTR2,
dnl                        ACTION-IF-TRUE [, ACTION-IF-FALSE])
dnl
dnl compare versions field-wise (separator is '.')
dnl TEST is one of {-lt,-le,-eq,-ge,-gt}
dnl
dnl quite shell-independent and SUSv2 compliant code
dnl
dnl NOTE: the loop could be unrolled within autoconf, but the
dnl       macro code would be a) longer and b) harder to debug... ;)
dnl
AC_DEFUN(CTK_PROG_CHECK_VERSION,
[if ( IFS=".";
      a="[$1]";  b="[$3]";
      while test -n "$a$b"
      do
              set -- [$]a;  h1="[$]1";  shift 2>/dev/null;  a="[$]*"
              set -- [$]b;  h2="[$]1";  shift 2>/dev/null;  b="[$]*"
              test -n "[$]h1" || h1=0;  test -n "[$]h2" || h2=0
              test [$]{h1} -eq [$]{h2} || break
      done
      test [$]{h1} [$2] [$]{h2}
    )
then ifelse([$4],,[:],[
  $4])
ifelse([$5],,,
[else
  $5])
fi])])dnl

dnl Obtain the value of a C constant.
dnl The value will be `(-1)' if the constant is undefined.
dnl
dnl This is set up so that the argument can be a shell variable.
dnl
AC_DEFUN(C2HS_CHECK_CCONST,
[
eval "def_name=CCONST_$1"
eval "cv_name=ac_cv_cconst_$1"
AC_MSG_CHECKING(value of $1)
AC_CACHE_VAL($cv_name,
[AC_TRY_RUN([#include <stdio.h>
#include <errno.h>
main()
{
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  fprintf(f, "%d\n", $1);
  exit(0);
}], 
eval "$cv_name=`cat conftestval`",
eval "$cv_name=-1",
ifelse([$2], , , eval "$cv_name=$2"))])dnl
eval "c2hs_check_cconst_result=`echo '$'{$cv_name}`"
AC_MSG_RESULT($c2hs_check_cconst_result)
AC_DEFINE_UNQUOTED($def_name, $c2hs_check_cconst_result)
eval "$def_name=$c2hs_check_cconst_result"
unset c2hs_check_cconst_result
])

dnl ** Invoke AC_CHECK_CCONST on each argument (which have to separate with 
dnl    spaces)
dnl
AC_DEFUN(C2HS_CHECK_CCONSTS,
[for ac_const_name in $1
do
C2HS_CHECK_CCONST($ac_const_name)dnl
done
])
