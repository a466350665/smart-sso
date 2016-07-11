package com.smart.base.service;

import com.smart.base.dao.UserDao;
import com.smart.base.model.User;
import com.smart.ssm.model.Pagination;
import com.smart.ssm.service.Service;

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
}
