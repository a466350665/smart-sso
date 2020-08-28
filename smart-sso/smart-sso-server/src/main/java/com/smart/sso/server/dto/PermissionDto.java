package com.smart.sso.server.dto;

public class PermissionDto extends TreeDto {

    private static final long serialVersionUID = 9191900436619971003L;
    
    private Boolean checked;
    
    public Boolean getChecked() {
        return checked;
    }

    public void setChecked(Boolean checked) {
        this.checked = checked;
    }
}