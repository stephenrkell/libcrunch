case_name := $(notdir $(realpath $(dir $(realpath $(lastword $(MAKEFILE_LIST))))))
CASES_c++ += $(case_name)
