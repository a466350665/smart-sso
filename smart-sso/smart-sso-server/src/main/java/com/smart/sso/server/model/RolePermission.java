package com.smart.sso.server.model;

import com.smart.mvc.model.PersistentObject;

/**
 * 角色权限映射
 * 
 * @author Joe
 */
public class RolePermission extends PersistentObject {

	private static final long serialVersionUID = 2817362249993235590L;

	/** 应用ID */
	private Integer appId;
	private Integer roleId;
	private Integer permissionId;
	
	public RolePermission() {
		super();
	}
	
	public RolePermission(Integer appId, Integer roleId, Integer permissionId) {
		super();
		this.appId = appId;
		this.roleId = roleId;
		this.permissionId = permissionId;
	}

	public Integer getAppId() {
		return this.appId;
	}

	public void setAppId(Integer appId) {
		this.appId = appId;
	}

	public Integer getRoleId() {
		return this.roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}

	public Integer getPermissionId() {
		return this.permissionId;
	}

	public void setPermissionId(Integer permissionId) {
		this.permissionId = permissionId;
	}
}
