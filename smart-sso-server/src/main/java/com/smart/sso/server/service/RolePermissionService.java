package com.smart.sso.server.service;

import com.smart.sso.server.service.BaseService;
import com.smart.sso.server.model.RolePermission;

import java.util.Collection;
import java.util.List;

/**
 * 角色权限映射服务接口
 * 
 * @author Joe
 */
public interface RolePermissionService extends BaseService<RolePermission> {
	
	/**
	 * 根据角色ID查询映射
	 * @param roleId 角色ID
	 * @return
	 */
	public List<RolePermission> selectByRoleId(Integer roleId);
	
	/**
	 * 根据角色ID给角色授权
	 * @param appId 应用ID
	 * @param roleId 角色ID
	 * @param permissionIdList 权限ID集合
	 * @return
	 */
	public void allocate(Integer appId, Integer roleId, List<Integer> permissionIdList);
	
	/**
	 * 根据权限ID集合删除映射
	 * @param idList 权限ID集合
	 * @return
	 */
	public void deleteByPermissionIds(List<Integer> idList);
	
	/**
	 * 根据角色ID集合删除映射
	 * @param idList 角色ID集合
	 * @return
	 */
	public void deleteByRoleIds(Collection<Integer> idList);
	
	/**
	 * 根据应用ID集合删除映射
	 * @param idList 应用ID集合
	 * @return
	 */
	public void deleteByAppIds(Collection<Integer> idList);
	
	/**
     * 根据用户ID查角色ID集合
     * @param userId
     * @return
     */
    public List<Integer> findPermissionIdListByRoleId(Integer roleId);
}
