package com.smart.sso.server.model;

import com.baomidou.mybatisplus.annotation.TableName;

import javax.persistence.Table;

/**
 * 角色权限映射
 * 
 * @author Joe
 */
@TableName("sys_role_permission")
public class RolePermission extends PersistentObject {

	private static final long serialVersionUID = 2817362249993235590L;

	/** 应用ID */
	private Integer appId;
	private Integer roleId;
	private Integer permissionId;
	
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
