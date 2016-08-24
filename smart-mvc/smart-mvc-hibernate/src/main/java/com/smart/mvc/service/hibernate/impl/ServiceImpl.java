package com.smart.mvc.service.hibernate.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.smart.mvc.dao.hibernate.Dao;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.service.hibernate.Service;

/**
 * Service基类，实现了数据的CRUD
 * 
 * @param <DAO>
 * @param <T>
 * @param <ID>
 */
@SuppressWarnings( { "unchecked", "rawtypes" })
public class ServiceImpl<DAO extends Dao, T extends Serializable, ID extends Serializable>
		implements Service<DAO, T, ID> {
	/**
	 * 数据访问对象，提供子类使用
	 */
	protected DAO dao;

	@Override
	public void setDao(DAO dao) {
		this.dao = dao;
	}

	@Override
	public void save(T t) {
		((Dao) dao).save(t);
	}

	@Override
	public void save(Collection<T> ts) {
		for (T t : ts) {
			save(t);
		}
	}

	@Override
	public void update(T t) {
		((Dao) dao).update(t);
	}

	@Override
	public void update(Collection<T> ts) {
		for (T t : ts) {
			update(t);
		}
	}

	@Override
	public void delete(T t) {
		((Dao) dao).delete(t);
	}

	@Override
	public void delete(Collection<T> ts) {
		for (T t : ts) {
			delete(t);
		}
	}
	
	@Override
	public T deleteById(ID pk) {
		T t = get(pk);
		delete(t);
		return t;
	}

	@Override
	public List<T> deleteById(Collection<ID> pks) {
		List<T> ts = get(pks);
		delete(ts);
		return ts;
	}
	
	public T deleteByProperty(String property, Object values){
		T t=findByProperty(property, values);
		delete(t);
		return t;
	}
	
	public List<T> deleteListByProperty(String property, Object values){
		
		List<T> ts=findListByProperty(property, values);
		delete(ts);
		return ts;
	}
	
	public T deleteByPropertys(String[] property, Object... values){
		T t=findByPropertys(property, values);
		delete(t);
		return t;
	}
	
	public List<T> deleteListByPropertys(String[] property, Object... values){
		List<T> ts=findListByPropertys(property, values);
		delete(ts);
		return ts;
	}

	@Override
	public T get(ID pk) {
		return (T) ((Dao) dao).get(pk);
	}

	@Override
	public List<T> get(Collection<ID> pks) {
		List<T> list = new ArrayList<T>(pks.size());
		for (ID pk : pks) {
			list.add(get(pk));
		}
		return list;
	}
	
	@Override
	public T findByProperty(String property, Object values) {
		return findByPropertys(new String[] { property }, values);
	}
	
	@Override
	public T findByProperty(String sortHql,String property, Object values) {
		List<T> list=dao.findListByPropertys(sortHql, new String[] { property }, new Object[] {values});
		if (list != null && list.size() > 0) {
			return list.get(0);
		}
		else {
			return null;
		}
	}

	@Override
	public T findByPropertys(String[] property, Object... values) {
		List<T> list = dao.findListByPropertys(property, values);
		if (list != null && list.size() > 0) {
			return list.get(0);
		}
		else {
			return null;
		}
	}
	
	@Override
	public T findByProperty(String sortHql,String[] property, Object... values) {
		List<T> list=dao.findListByPropertys(sortHql, property, values);
		if (list != null && list.size() > 0) {
			return list.get(0);
		}
		else {
			return null;
		}
	}

	@Override
	public List<T> findListByPropertys(String[] property, Object... values) {
		return dao.findListByPropertys(property, values);
	}
	
	@Override
	public List<T> findListByPropertys(String sortHql, String[] property,
			Object... values) {
		return dao.findListByPropertys(sortHql, property, values);
	}
	
	@Override
	public List<T> findList() {
		return dao.findByAll(null);
	}

	@Override
	public List<T> findList(String sortHql) {
		return dao.findListByPropertys(sortHql,null);
	}
	
	@Override
	public List<T> findListByProperty(String property, Object values) {
		return dao.findListByPropertys(new String[] { property }, values);
	}
	
	@Override
	public List<T> findListByProperty(String sortHql,String property, Object values) {
		return dao.findListByPropertys(sortHql,new String[] { property }, values);
	}

	@Override
	public Pagination<T> findPagination(Pagination<T> p) {
		dao.findByAll(p);
		return p;
	}

	@Override
	public Pagination<T> findPagination(String sortHql, Pagination<T> p) {
		dao.findByAll(p, null, sortHql);
		return p;
	}
	
	@Override
	public Pagination<T> findPaginationByProperty(Pagination<T> p, String property,
			Object values) {
		dao.findByAll(p, new String[]{property}, null, values);
		return p;
	}
	
	@Override
	public Pagination<T> findPaginationByProperty(String sortHql, Pagination<T> p,
			String property, Object values) {
		dao.findByAll(p, new String[]{property}, sortHql, values);
		return p;
	}
	
	@Override
	public Pagination<T> findPaginationByPropertys(Pagination<T> p, String[] property,
			Object... values) {
		dao.findByAll(p, property, null, values);
		return p;
	}

	@Override
	public Pagination<T> findPaginationByPropertys(String sortHql, Pagination<T> p,
			String[] property, Object... values) {
		dao.findByAll(p, property, sortHql, values);
		return p;
	}

}
