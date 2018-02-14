case_name := $(notdir $(realpath $(dir $(realpath $(lastword $(MAKEFILE_LIST))))))
CASES_c := $(CASES_c) $(case_name)
CASES_CONFIG_x := $(CASES_CONFIG_x) $(case_name)
