package com.smart.sso.server.model;

import com.baomidou.mybatisplus.annotation.TableName;

import javax.persistence.Table;

/**
 * 用户角色映射
 * 
 * @author Joe
 */
@TableName("sys_user_role")
public class UserRole extends PersistentObject {

	private static final long serialVersionUID = 4942358338145288018L;

	/** 用户ID */
	private Integer userId;
	/** 角色ID */
	private Integer roleId;

	public Integer getUserId() {
		return this.userId;
	}

	public void setUserId(Integer userId) {
		this.userId = userId;
	}

	public Integer getRoleId() {
		return this.roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}
}
