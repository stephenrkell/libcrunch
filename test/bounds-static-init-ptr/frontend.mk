case_name := $(call get_case_name)
$(call set_frontend_for_case,$(case_name),c)
# sb Sb tb Tb are excluded becase __fetch_bounds_ool_via_dladdr
# finds (correctly) gets a *zero-length* alias symbol for other_vs,
# so thinks the object has zero length.
$(call set_configs_for_case,$(case_name),x b pb mb fb Pb Mb Fb)
