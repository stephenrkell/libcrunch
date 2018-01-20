#!/bin/sh

case "$0" in
	(*pbcc) CRUNCH_CONFIG=p ;;
	(*Pbcc) CRUNCH_CONFIG=P ;;
	(*mbcc) CRUNCH_CONFIG=m ;;
	(*Mbcc) CRUNCH_CONFIG=M ;;
	(*tbcc) CRUNCH_CONFIG=t ;;
	(*Tbcc) CRUNCH_CONFIG=T ;;
	(*sbcc) CRUNCH_CONFIG=s ;;
	(*Sbcc) CRUNCH_CONFIG=S ;;
	(*fbcc) CRUNCH_CONFIG=f ;;
	(*Fbcc) CRUNCH_CONFIG=F ;;
	(*bcc)  CRUNCH_CONFIG=p ;;
	(*xcc)  CRUNCH_CONFIG=x ;;
	(*cc)   CRUNCH_CONFIG=default ;;
	(*) echo "Unrecognised config: $0" 1>&2; exit 1 ;;
esac

export CRUNCH_CONFIG
exec \
   gcc \
   -no-integrated-cpp \
   -wrapper "${0%/*}"/wrapper -Wl,-plugin,"${0%/*}"/../../../../liballocs/tools/gold-plugin.so \
   "$@"
