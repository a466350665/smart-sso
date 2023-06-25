package com.smart.sso.server.service;

import com.smart.sso.server.service.BaseService;
import com.smart.sso.client.dto.RpcPermissionDto;
import com.smart.sso.server.dto.PermissionDto;
import com.smart.sso.server.model.Permission;

import java.util.Collection;
import java.util.List;

/**
 * 权限服务接口
 * 
 * @author Joe
 */
public interface PermissionService extends BaseService<Permission> {

	/**
	 * 根据名称和应用ID查询
	 * @param appId 应用ID
	 * @param roleId 角色ID
	 * @param isEnable
	 * @return
	 */
	public List<PermissionDto> selectTree(Integer appId, Integer roleId, Boolean isEnable);
	
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
	public List<RpcPermissionDto> selectListByUserId(String appCode, Integer userId);
}
