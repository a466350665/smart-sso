package com.smart.demo.dao;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.smart.demo.model.Demo;
import com.smart.mvc.dao.mybatis.Dao;
import com.smart.mvc.model.Pagination;

/**
 * 测试持久化接口
 * 
 * @author Joe
 */
public interface DemoDao extends Dao<Demo, Integer> {
	
	public List<Demo> findPaginationByName(@Param("name") String name, Pagination<Demo> p);
	
	public Demo findByName(@Param("name") String name);
}
