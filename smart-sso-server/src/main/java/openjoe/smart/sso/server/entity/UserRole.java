package openjoe.smart.sso.server.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import openjoe.smart.stage.mybatisplus.entity.Entity;

/**
 * 用户角色映射
 * 
 * @author Joe
 */
@TableName("sys_user_role")
public class UserRole extends Entity {

	/** 用户ID */
	private Long userId;
	/** 角色ID */
	private Long roleId;

	public Long getUserId() {
		return this.userId;
	}

	public void setUserId(Long userId) {
		this.userId = userId;
	}

	public Long getRoleId() {
		return this.roleId;
	}

	public void setRoleId(Long roleId) {
		this.roleId = roleId;
	}
}
