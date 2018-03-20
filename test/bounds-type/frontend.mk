case_name := $(call get_case_name)
$(call set_frontend_for_case,$(case_name),c)
$(call set_configs_for_case,$(case_name),x)
