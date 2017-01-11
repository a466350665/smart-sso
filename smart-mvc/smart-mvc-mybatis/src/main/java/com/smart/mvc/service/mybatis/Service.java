package com.smart.mvc.service.mybatis;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import com.smart.mvc.model.Pagination;
import com.smart.mvc.model.PersistentObject;

/**
 * Service接口
 * 
 * @param <DAO>
 * @param <T>
 * @param <ID>
 * @author Joe
 */
public interface Service<T extends PersistentObject, ID extends Serializable> {

	/**
	 * 新建实体
	 * 
	 * @param T
	 *            t
	 */
	public void save(T t);

	/**
	 * 新建实体
	 * 
	 * @param List
	 *            <T> ts
	 */
	public void save(Collection<T> ts);

	/**
	 * 更新实体
	 * 
	 * @param T
	 *            t
	 */
	public void update(T t);

	/**
	 * 更新实体
	 * 
	 * @param List
	 *            <T> ts
	 */
	public void update(Collection<T> ts);
	
	/**
	 * 保存或更新实体
	 * 
	 * @param List
	 *            <T> ts
	 */
	public void saveOrUpdate(T t);

	/**
	 * 删除实体
	 * 
	 * @param T
	 *            t
	 */
	public void delete(T t);

	/**
	 * 删除实体
	 * 
	 * @param List
	 *            <T> ts
	 */
	public void delete(Collection<T> ts);

	/**
	 * 通过主键加载实体
	 * 
	 * @param PK
	 *            pk
	 * @return T
	 */
	public T get(ID pk);

	/**
	 * 通过主键加载实体
	 * 
	 * @param List
	 *            <PK> pks
	 * @return List<T>
	 */
	public List<T> get(Collection<ID> pks);

	/**
	 * 通过主键删除实体
	 * 
	 * @param PK
	 *            pk
	 * @return T
	 */
	public void deleteById(ID id);

	/**
	 * 通过主键删除实体
	 * 
	 * @param List
	 *            <PK> pks
	 * @return List<T>
	 */
	public void deleteById(Collection<ID> idList);

	/**
	 * 查所有分页
	 * 
	 * @param p
	 * @return
	 */
	public Pagination<T> findByAllPagination(Pagination<T> p);
}
