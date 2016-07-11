package com.smart.sso.server.dao;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.smart.ssm.dao.Dao;
import com.smart.sso.server.model.UserRole;

/**
 * 管理员角色映射持久化接口
 * 
 * @author Joe
 */
public interface UserRoleDao extends Dao<UserRole, Integer> {

	public UserRole findByUserRoleId(@Param("userId") Integer userId, @Param("roleId") Integer roleId);

	public int deleteByRoleIds(@Param("idList") List<Integer> idList);

	public int deleteByUserIds(@Param("idList") List<Integer> idList, @Param("appId") Integer appId);

	public int deleteByAppIds(@Param("idList") List<Integer> idList);
	
	public int deleteForChangeApp(@Param("userId") Integer userId, @Param("idList") List<Integer> idList);
}
