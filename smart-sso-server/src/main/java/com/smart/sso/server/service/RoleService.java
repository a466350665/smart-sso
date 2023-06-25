package com.smart.sso.server.service;

import com.smart.sso.server.model.Page;
import com.smart.sso.server.service.BaseService;
import com.smart.sso.server.dto.RoleDto;
import com.smart.sso.server.model.Role;

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
	public void enable(Boolean isEnable, List<Integer> idList);
	
	/**
	 * 根据角色名称和应用ID查询分页列表
	 * @param name 角色名称
	 * @param pageNo 分页起始
	 * @param pageSize 分页记录数
	 * @return
	 */
	public Page<Role> selectPage(String name, Integer pageNo, Integer pageSize);
	
	/**
	 * 查询应用可用角色
	 * @param isEnable 是否启用
	 * @return
	 */
	public List<Role> selectAll(Boolean isEnable);
	
	public List<RoleDto> getRoleList(Integer userId);

	public void deleteByIds(Collection<Integer> idList);
}
