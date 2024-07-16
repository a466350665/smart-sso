package openjoe.smart.sso.server.service;

import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.server.dto.MenuDTO;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.entity.Permission;
import openjoe.smart.stage.mybatisplus.service.BaseService;

import java.util.Collection;
import java.util.List;
import java.util.Set;

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
	List<PermissionDTO> selectTree(Long appId, Long roleId, Boolean isEnable);
	
	/**
	 * 删除权限
	 * @param id 权限ID
	 * @param appId 应用ID
	 * @return
	 */
	void delete(Long id, Long appId);
	
	/**
	 * 删除应用下所有权限
	 * @param idList 应用ID集合
	 * @return
	 */
	void deleteByAppIds(Collection<Long> idList);

	/**
	 * 获取用户权限信息
	 *
	 * @param userId
	 * @param clientId
	 * @return
	 */
	TokenPermission getUserPermission(Long userId, String clientId);

	/**
	 * 根据应用编码和用户ID查权限
	 * @param clientId 应用编码
	 * @param userId 用户ID
	 * @return
	 */
	List<MenuDTO> getUserMenuList(Long userId, String clientId, Set<String> noPermissionSet);
}
