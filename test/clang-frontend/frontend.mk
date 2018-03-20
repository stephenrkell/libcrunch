case_name := $(call get_case_name)
$(call set_frontend_for_case,$(case_name),clang)
$(call set_configs_for_case,$(case_name),default)
