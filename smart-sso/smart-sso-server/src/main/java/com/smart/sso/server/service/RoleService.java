package com.smart.sso.server.service;

import java.util.List;

import com.smart.mvc.model.Pagination;
import com.smart.mvc.service.mybatis.Service;
import com.smart.sso.server.model.Role;

/**
 * 角色服务接口
 * 
 * @author Joe
 */
public interface RoleService extends Service<Role, Integer> {
	
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
	public Pagination<Role> findPaginationByName(String name, Pagination<Role> p);
	
	/**
	 * 查询应用可用角色
	 * @param isEnable 是否启用
	 * @return
	 */
	public List<Role> findByAll(Boolean isEnable);
}
