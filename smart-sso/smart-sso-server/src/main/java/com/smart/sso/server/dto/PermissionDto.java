package com.smart.sso.server.dto;

import com.smart.sso.server.model.Permission;

public class PermissionDto extends Permission {

    private static final long serialVersionUID = 9191900436619971003L;
    
    private boolean checked;

    public boolean isChecked() {
        return checked;
    }

    public void setChecked(boolean checked) {
        this.checked = checked;
    }
}