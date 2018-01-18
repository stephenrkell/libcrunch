case_name := $(notdir $(realpath $(dir $(realpath $(lastword $(MAKEFILE_LIST))))))
CASES_c := $(CASES_c) $(case_name)
CASES_CONFIG_b := $(CASES_CONFIG_b) $(case_name)
