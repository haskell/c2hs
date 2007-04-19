#!/bin/bash

C2HS_CC_LOGDIR=${C2HS_CC_LOGDIR:-/tmp/c2hs-cc-wrapper}
C2HS_CC_WRAPPER=${C2HS_CC_WRAPPER:-cc-wrapper}

if ! which ${C2HS_CC_WRAPPER} > /dev/null 2> /dev/null
then
  echo "cc-wrapper.sh: cc-wrapper binary not found, please set C2HS_CC_WRAPPER"
  exit
fi

if ! [ -d ${C2HS_CC_LOGDIR} ]
then
  echo "cc-wrapper.sh: C2HS_CC_LOGDIR=${C2HS_CC_LOGDIR} does not exist"
  exit
fi

if gcc $@
then
  export C2HS_CC_LOGDIR
  ${C2HS_CC_WRAPPER} $@
else
  exitstatus=$?
  echo "gcc could not compile: $@" >> $C2HS_CC_LOGDIR/cc-wrapper.log
  exit $exitstatus
fi
