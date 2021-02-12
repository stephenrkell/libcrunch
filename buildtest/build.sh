#! /bin/bash

#cp ../config.mk ubuntu-18.04/config.mk
#cp ../src/Makefile ubuntu-18.04/src_Makefile
#cp ../../liballocs/contrib/libsystrap/src/Makefile ubuntu-18.04/libsystrap_src_Makefile
#cp ../../liballocs/contrib/libsystrap/test/Makefile ubuntu-18.04/libsystrap_test_Makefile

docker build -t libcrunch_ubuntu_18 ubuntu-18.04
#docker run -it --entrypoint /bin/bash libcrunch -s
