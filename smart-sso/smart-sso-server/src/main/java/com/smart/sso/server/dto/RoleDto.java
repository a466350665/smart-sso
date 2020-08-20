package com.smart.sso.server.dto;

import com.smart.sso.server.model.Permission;

public class RoleDto extends Permission {

    private static final long serialVersionUID = 9191900436619971003L;
    
    private Boolean checked;

    public Boolean getChecked() {
        return checked;
    }

    public void setChecked(Boolean checked) {
        this.checked = checked;
    }
}