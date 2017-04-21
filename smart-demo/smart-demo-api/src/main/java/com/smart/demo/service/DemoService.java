package com.smart.demo.service;


import com.smart.demo.model.Demo;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.service.mybatis.Service;

/**
 * 测试服务接口
 * 
 * @author Joe
 */
public interface DemoService extends Service<Demo, Integer> {
	
	/**
	 * 根据名称和应用ID查询分页列表
	 * @param name 名称
	 * @param pageNo 分页起始
	 * @param pageSize 分页记录数
	 * @return
	 */
	public Pagination<Demo> findPaginationByName(String name, Pagination<Demo> p);
	
	/**
	 * 根据名称和应用ID查询
	 * @param name 名称
	 * @param appId 应用ID
	 * @return
	 */
	public Demo findByName(String name);
}
