case_name := $(call get_case_name)
$(call set_frontend_for_case,$(case_name),c)
# FIXME: sb/Sb and tb/Tb are excluded because they don't "fail"-, they "abort"-
$(call set_configs_for_case,$(case_name),x b pb mb fb Pb Mb Fb)
