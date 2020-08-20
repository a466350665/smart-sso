package com.smart.mvc.dao;

import java.io.Serializable;
import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.smart.mvc.model.Condition;
import com.smart.mvc.model.Page;

/**
 * Dao接口
 * 
 * @author Joe
 *
 * @param <T>
 */
public interface Dao<T> {
    
	/**
	 * 查询条件分页
	 * @param condition
	 * @param p
	 * @return
	 */
	 List<T> selectByCondition(@Param("condition") Condition condition, Page<T> p);
	
	/**
	 * 通过主键查询实体
	 * 
	 * @param id
	 * @return
	 */
	T selectById(Serializable id);

	/**
	 * 插入实体
	 * 
	 * @param t
	 * @return
	 */
	int insert(T t);

	/**
	 * 更新实体
	 * 
	 * @param t
	 * @return
	 */
	int update(T t);
	
    /**
     * 条件删除实体
     * 
     * @param condition
     * @return
     */
    int deleteByCondition(@Param("condition") Condition condition);
}
