package openjoe.smart.sso.server.service;

import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.entity.Role;
import openjoe.smart.stage.core.entity.Page;
import openjoe.smart.stage.mybatisplus.service.BaseService;

import java.util.Collection;
import java.util.List;

/**
 * 角色服务接口
 * 
 * @author Joe
 */
public interface RoleService extends BaseService<Role> {
	
	/**
	 * 启用禁用操作
	 * @param isEnable 是否启用
	 * @param idList 角色ID集合
	 * @return
	 */
	void enable(Boolean isEnable, List<Long> idList);
	
	/**
	 * 根据角色名称和应用ID查询分页列表
	 * @param name 角色名称
	 * @param current 分页起始
	 * @param size 分页记录数
	 * @return
	 */
	Page<Role> selectPage(String name, Long current, Long size);
	
	/**
	 * 查询应用可用角色
	 * @param isEnable 是否启用
	 * @return
	 */
	List<Role> selectAll(Boolean isEnable);
	
	List<PermissionDTO> getRoleList(Long userId);

	void deleteByIds(Collection<Long> idList);
}
