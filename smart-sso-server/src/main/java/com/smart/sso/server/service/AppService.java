package com.smart.sso.server.service;

import com.smart.sso.server.model.Page;
import com.smart.sso.server.service.BaseService;
import com.smart.sso.server.model.App;

import java.util.Collection;
import java.util.List;

/**
 * 应用服务接口
 * 
 * @author Joe
 */
public interface AppService extends BaseService<App> {
	
	/**
	 * 启用禁用操作
	 * @param isEnable 是否启用
	 * @param idList 应用ID集合
	 * @return
	 */
	public void enable(Boolean isEnable, List<Integer> idList);
	
	/**
	 * 根据名称查询
	 */
	public List<App> selectAll(Boolean isEnable);
	
	/**
	 * 根据名称分页查询
	 * @param name 应用名称
	 * @return
	 */
	public Page<App> selectPage(String name, Integer pageNo, Integer pageSize);
	
	/**
	 * 根据应用编码查询
	 * @param code 应用编码
	 * @return
	 */
	public App selectByCode(String code);

	public void deleteByIds(Collection<Integer> idList);
}