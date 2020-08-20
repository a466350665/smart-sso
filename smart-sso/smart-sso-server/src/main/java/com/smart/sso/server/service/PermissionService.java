package com.smart.sso.server.service;

import java.util.Collection;
import java.util.List;

import com.smart.mvc.service.Service;
import com.smart.sso.client.model.RpcPermission;
import com.smart.sso.server.model.Permission;

/**
 * 权限服务接口
 * 
 * @author Joe
 */
public interface PermissionService extends Service<Permission> {

	/**
	 * 根据名称和应用ID查询
	 * @param appId 应用ID
	 * @param roleId 角色ID
	 * @param isEnable
	 * @return
	 */
	public List<Permission> selectList(Integer appId, Integer roleId, Boolean isEnable);
	
	/**
	 * 删除权限
	 * @param id 权限ID
	 * @param appId 应用ID
	 * @return
	 */
	public void delete(Integer id, Integer appId);
	
	/**
	 * 删除应用下所有权限
	 * @param idList 应用ID集合
	 * @return
	 */
	public void deleteByAppIds(Collection<Integer> idList);
	
	/**
	 * 根据应用编码和用户ID查权限
	 * @param appCode 应用编码
	 * @param userId 用户ID
	 * @return
	 */
	public List<RpcPermission> selectListByUserId(String appCode, Integer userId);
}
