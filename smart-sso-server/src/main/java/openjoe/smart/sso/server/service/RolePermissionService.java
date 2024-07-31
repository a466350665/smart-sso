package openjoe.smart.sso.server.service;

import openjoe.smart.sso.server.stage.mybatisplus.service.BaseService;
import openjoe.smart.sso.server.entity.RolePermission;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * 角色权限映射服务接口
 * 
 * @author Joe
 */
public interface RolePermissionService extends BaseService<RolePermission> {
	
	/**
	 * 根据角色ID查询映射
	 * @param roleIdList 角色ID
	 * @return
	 */
	List<RolePermission> selectByRoleIds(List<Long> roleIdList);
	
	/**
	 * 根据角色ID给角色授权
	 * @param appId 应用ID
	 * @param roleId 角色ID
	 * @param permissionIdList 权限ID集合
	 * @return
	 */
	void allocate(Long appId, Long roleId, List<Long> permissionIdList);
	
	/**
	 * 根据权限ID集合删除映射
	 * @param idList 权限ID集合
	 * @return
	 */
	void deleteByPermissionIds(List<Long> idList);
	
	/**
	 * 根据角色ID集合删除映射
	 * @param idList 角色ID集合
	 * @return
	 */
	void deleteByRoleIds(Collection<Long> idList);
	
	/**
	 * 根据应用ID集合删除映射
	 * @param idList 应用ID集合
	 * @return
	 */
	void deleteByAppIds(Collection<Long> idList);
	
	/**
     * 根据用户ID查角色ID集合
     * @param roleIdList
     * @return
     */
	Set<Long> findPermissionIdSetByRoleIds(List<Long> roleIdList);
}
