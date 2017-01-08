package com.smart.mvc.mybatis.demo.service;

import com.smart.mvc.model.Pagination;
import com.smart.mvc.service.mybatis.Service;
import com.smart.mvc.mybatis.demo.dao.UserDao;
import com.smart.mvc.mybatis.demo.model.User;

/**
 * 管理员服务接口
 * 
 * @author Joe
 */
public interface UserService extends Service<UserDao, User, Integer> {
	
	/**
	 * 根据登录名和应用ID查询分页列表
	 * @param account 登录名
	 * @param pageNo 分页起始
	 * @param pageSize 分页记录数
	 * @return
	 */
	public Pagination<User> findPaginationByAccount(String account, Pagination<User> p);
	
	/**
	 * 根据登录名和应用ID查询
	 * @param account 登录名
	 * @param appId 应用ID
	 * @return
	 */
	public User findByAccount(String account);
}
