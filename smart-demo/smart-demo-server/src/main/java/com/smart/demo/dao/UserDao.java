package com.smart.demo.dao;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.smart.demo.model.User;
import com.smart.mvc.dao.mybatis.Dao;
import com.smart.mvc.model.Pagination;

/**
 * 管理员持久化接口
 * 
 * @author Joe
 */
public interface UserDao extends Dao<User, Integer> {
	
	public List<User> findPaginationByAccount(@Param("account") String account, Pagination<User> p);
	
	public User findByAccount(@Param("account") String account);
}
