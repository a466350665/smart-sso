package com.smart.mvc.service;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import com.smart.mvc.model.Page;
import com.smart.mvc.model.PersistentObject;

/**
 * Service接口
 * 
 * @param <T>
 * @param <ID>
 * @author Joe
 */
public interface BaseService<T extends PersistentObject, ID extends Serializable> {
    
	/**
     * 查询所有集合
     * 
     * @return
     */
    List<T> selectAll();

	/**
	 * 查询所有分页
	 * 
	 * @param p
	 * @return
	 */
	Page<T> selectPage(Page<T> p);
	
	/**
	 * 通过主键查询实体
	 * 
	 * @param id
	 * @return
	 */
	T selectById(ID id);

	/**
	 * 通过主键集合查询实体
	 * 
	 * @param ids
	 * @return
	 */
	List<T> selectByIds(Collection<ID> ids);

	/**
	 * 插入/更新实体
	 * 
	 * @param t
	 */
	void save(T t);

	/**
	 * 插入/更新实体集合
	 * 
	 * @param ts
	 */
	void save(Collection<T> ts);
	
	/**
     * 插入实体
     * 
     * @param t
     */
    void insert(T t);

    /**
     * 插入实体集合
     * 
     * @param ts
     */
    void insert(Collection<T> ts);

	/**
	 * 更新实体
	 * 
	 * @param t
	 */
	void update(T t);

	/**
	 * 更新实体集合
	 * 
	 * @param ts
	 */
	void update(Collection<T> ts);

	/**
	 * 删除实体
	 * 
	 * @param t
	 */
	void delete(T t);

	/**
	 * 删除实体集合
	 * 
	 * @param ts
	 */
	void delete(Collection<T> ts);

	/**
	 * 通过主键删除实体
	 * 
	 * @param id
	 */
	void deleteById(ID id);

	/**
	 * 通过主键集合删除实体
	 * 
	 * @param ids
	 */
	void deleteByIds(Collection<ID> ids);
}
