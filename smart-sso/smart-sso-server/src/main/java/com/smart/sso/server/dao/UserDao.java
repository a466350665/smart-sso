package com.smart.sso.server.dao;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.smart.mvc.dao.mybatis.Dao;
import com.smart.mvc.model.Pagination;
import com.smart.sso.server.model.User;

/**
 * 用户持久化接口
 * 
 * @author Joe
 */
public interface UserDao extends Dao<User, Integer> {
	
	public int enable(@Param("isEnable") Boolean isEnable, @Param("idList") List<Integer> idList);
	
	public int resetPassword(@Param("password") String password, @Param("idList") List<Integer> idList);

	public List<User> findPaginationByAccount(@Param("account") String account, Pagination<User> p);
	
	public User findByAccount(@Param("account") String account);
}
