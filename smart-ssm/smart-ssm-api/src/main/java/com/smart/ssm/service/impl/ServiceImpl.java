package com.smart.ssm.service.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.smart.ssm.dao.Dao;
import com.smart.ssm.model.Pagination;
import com.smart.ssm.model.PersistentObject;
import com.smart.ssm.service.Service;

/**
 * Service基类，实现了数据的CRUD
 * 
 * @param <DAO>
 * @param <T>
 * @param <ID>
 * @author Joe
 */
@SuppressWarnings({ "unchecked", "rawtypes" })
public abstract class ServiceImpl<DAO extends Dao, T extends PersistentObject, ID extends Serializable> implements
		Service<DAO, T, ID> {
	/**
	 * 数据访问对象，提供子类使用
	 */
	protected DAO dao;

	public abstract void setDao(DAO dao);

	/**
	 * 如果实体未持久化过则新建实体，否则更新实体
	 * 
	 * @param T
	 *            t
	 * 
	 */
	public int save(T t) {
		return ((Dao) dao).save(t);
	}

	/**
	 * 如果实体未持久化过则新建实体，否则更新实体
	 * 
	 * @param List
	 *            <T> ts
	 */
	public int save(Collection<T> ts) {
		for (T t : ts) {
			save(t);
		}
		return ts.size();
	}

	/**
	 * 更新实体
	 * 
	 * @param T
	 *            t
	 */
	public int update(T t) {
		return ((Dao) dao).update(t);
	}

	/**
	 * 更新实体
	 * 
	 * @param List
	 *            <T> ts
	 */
	public int update(Collection<T> ts) {
		for (T t : ts) {
			update(t);
		}
		return ts.size();
	}
	
	/**
	 * 如果实体未持久化过则新建实体，否则更新实体
	 * 
	 * @param T
	 *            t
	 */
	public int saveOrUpdate(T t) {
		if (t.getId() == null) {
			return this.save(t);
		}
		else {
			return this.update(t);
		}
	}

	/**
	 * 删除实体
	 * 
	 * @param T
	 *            t
	 */
	public int delete(T t) {
		return ((Dao) dao).deleteById(t.getId());
	}

	/**
	 * 删除实体
	 * 
	 * @param List
	 *            <T> ts
	 */
	public int delete(Collection<T> ts) {
		for (T t : ts) {
			delete(t);
		}
		return ts.size();
	}

	/**
	 * 通过主键加载实体
	 * 
	 * @param PK
	 *            pk
	 * @return T
	 */
	public T get(ID pk) {
		return (T) ((Dao) dao).get(pk);
	}

	/**
	 * 通过主键加载实体
	 * 
	 * @param List
	 *            <PK> pks
	 * @return List<T>
	 */
	public List<T> get(Collection<ID> pks) {
		List<T> list = new ArrayList<T>(pks.size());
		for (ID pk : pks) {
			list.add(get(pk));
		}
		return list;
	}

	/**
	 * 通过主键删除实体
	 * 
	 * @param PK
	 *            pk
	 * @return T
	 */
	public int deleteById(ID id) {
		return dao.deleteById(id);
	}

	/**
	 * 通过主键删除实体
	 * 
	 * @param List
	 *            <PK> pks
	 * @return List<T>
	 */
	public int deleteById(List<ID> idList) {
		return dao.deleteById(idList);
	}
	
	/**
	 * 查所有分页
	 * 
	 * @param p
	 * @return
	 */
	public Pagination<T> findByAllPagination(Pagination<T> p){
		dao.findByAll(p);
		return p;
	}
}
