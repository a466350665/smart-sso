package openjoe.smart.sso.server.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import openjoe.smart.stage.mybatisplus.entity.Entity;

/**
 * 角色权限映射
 * 
 * @author Joe
 */
@TableName("sys_role_permission")
public class RolePermission extends Entity {

	/** 应用ID */
	private Long appId;
	private Long roleId;
	private Long permissionId;
	
	public Long getAppId() {
		return this.appId;
	}

	public void setAppId(Long appId) {
		this.appId = appId;
	}

	public Long getRoleId() {
		return this.roleId;
	}

	public void setRoleId(Long roleId) {
		this.roleId = roleId;
	}

	public Long getPermissionId() {
		return this.permissionId;
	}

	public void setPermissionId(Long permissionId) {
		this.permissionId = permissionId;
	}
}
