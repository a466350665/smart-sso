package com.smart.sso.server.dto;

import com.smart.sso.server.model.Permission;

public class RoleDto extends Permission {

    private static final long serialVersionUID = 9191900436619971003L;
    
    private Boolean isChecked = Boolean.valueOf(false);
    
    public Boolean getIsChecked() {
        return isChecked;
    }

    public void setIsChecked(Boolean isChecked) {
        this.isChecked = isChecked;
    }
}