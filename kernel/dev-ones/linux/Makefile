obj-m += dev_ones.o
KDIR  ?= /lib/modules/$(shell uname -r)/build
THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

default: dev_ones.ko

dev_ones.ko:
	$(MAKE) -C $(KDIR) M=$(realpath $(dir $(THIS_MAKEFILE))) modules

clean:
	rm -f *.cmd *.o *.ko *.mod.c Module.symvers modules.order
