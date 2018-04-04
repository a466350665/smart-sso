package com.smart.sso.server.service;

import java.util.List;

import com.smart.mvc.service.mybatis.Service;
import com.smart.sso.server.model.UserRole;

/**
 * 用户角色映射服务接口
 * 
 * @author Joe
 */
public interface UserRoleService extends Service<UserRole, Integer> {
	
	/**
	 * 根据用户ID和角色ID查询映射
	 * @param userId 用户ID
	 * @param roleId 角色ID
	 * @return
	 */
	public UserRole findByUserRoleId(Integer userId, Integer roleId);
	
	/**
	 * 根据用户ID给用户分配角色
	 * @param userId 用户ID
	 * @param list 用户角色映射集合
	 * @return
	 */
	public void allocate(Integer userId, Integer appId, List<UserRole> list);
	
	/**
	 * 根据角色ID集合删除映射
	 * @param idList 角色ID集合
	 * @return
	 */
	public void deleteByRoleIds(List<Integer> idList);
	
	/**
	 * 根据用户ID集合删除映射
	 * @param idList 用户ID集合
	 * @return
	 */
	public void deleteByUserIds(List<Integer> idList, Integer appId);
	
	/**
	 * 根据应用ID集合删除映射
	 * @param idList 应用ID集合
	 * @return
	 */
	public void deleteByAppIds(List<Integer> idList);
	
	/**
	 * 分配App时，删除无效的userRole
	 * @param userId 应用ID
	 * @param idList 可用应用ID集合
	 * @return
	 */
	public void deleteForChangeApp(Integer userId, List<Integer> idList);
}
