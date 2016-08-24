package com.smart.mvc.service.hibernate;

import java.util.Collection;
import java.util.List;

import com.smart.mvc.model.Pagination;

/**
 * Service接口
 * 
 * @param <DAO>
 * @param <T>
 * @param <ID>
 */
public interface Service<DAO, T, ID> {

	/**
	 * Description:设置数据访问对象
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:17:13 
	 * @param dao
	 */
	public void setDao(DAO dao);

	/**
	 * Description:保存
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:17:21 
	 * @param t
	 */
	public void save(T t);

	/**
	 * Description:保存
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:17:35 
	 * @param ts
	 */
	public void save(Collection<T> ts);

	/**
	 * Description:更新实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:18:14 
	 * @param t
	 */
	public void update(T t);

	/**
	 * Description:更新实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:18:31 
	 * @param ts
	 */
	public void update(Collection<T> ts);

	/**
	 * Description:删除实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:19:07 
	 * @param t
	 */
	public void delete(T t);

	/**
	 * Description:删除实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:19:25 
	 * @param ts
	 */
	public void delete(Collection<T> ts);
	
	/**
	 * Description:通过主键删除实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:20:39 
	 * @param pk
	 * @return
	 */
	public T deleteById(ID pk);

	/**
	 * Description:通过主键删除实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:20:46 
	 * @param pks
	 * @return
	 */
	public List<T> deleteById(Collection<ID> pks);
	
	/**
	 * Description:根据实体某一属性删除实体对象 支持or字句匹配
	 * @author 唐海洋
	 * @Version 1.0 2016-4-11下午3:46:31 
	 * @param property
	 * @param values
	 * @return
	 */
	public T deleteByProperty(String property, Object values);
	
	/**
	 * Description:根据实体某一属性删除实体对象列表 支持or字句匹配
	 * @author 唐海洋
	 * @Version 1.0 2016-4-11下午3:46:31 
	 * @param property
	 * @param values
	 * @return
	 */
	public List<T> deleteListByProperty(String property, Object values);
	
	/**
	 * Description:根据实体N个属性删除实体对象 支持or字句匹配
	 * @author 唐海洋
	 * @Version 1.0 2016-4-11下午3:46:31 
	 * @param property
	 * @param values
	 * @return
	 */
	public T deleteByPropertys(String[] property, Object... values);
	
	/**
	 * Description:根据实体N个属性删除实体对象列表 支持or字句匹配
	 * @author 唐海洋
	 * @Version 1.0 2016-4-11下午3:46:31 
	 * @param property
	 * @param values
	 * @return
	 */
	public List<T> deleteListByPropertys(String[] property, Object... values);

	/**
	 * Description:通过主键加载实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:20:00 
	 * @param pk
	 * @return
	 */
	public T get(ID pk);

	/**
	 * Description:通过主键加载实体
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:20:12 
	 * @param pks
	 * @return
	 */
	public List<T> get(Collection<ID> pks);
	
	/**
	 * Description:根据实体某一属性查询实体对象 支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:43:33
	 * @param property 属性
	 * @param values 属性的值
	 * @return
	 */
	public T findByProperty(String property, Object values);
	
	/**
	 * Description:根据实体某一属性查询实体对象 支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:43:33
	 * @param sortHql  排序语句 不需要写order by
	 * @param property 属性的值
	 * @param values 属性的值
	 * @return
	 */
	public T findByProperty(String sortHql, String property, Object values);

	/**
	 * Description:根据实体N个属性查询实体对象 支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:43:33
	 * @param property 属性
	 * @param values 属性的值
	 * @return
	 */
	public T findByPropertys(String[] property, Object... values);
	
	/**
	 * Description:根据实体N个属性查询实体对象 支持or字句查询
	 * @param sortHql 排序语句 不需要写order by
	 * @param property 属性的值
	 * @param values 属性的值
	 */
	public T findByProperty(String sortHql, String[] property, Object... values);

	/**
	 * Description:查询所有记录，如果大数据量，慎用
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:43:02
	 * @return
	 */
	public List<T> findList();
	
	/**
	 * Description:查询所有记录，如果大数据量，慎用
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:43:02
	 * @return
	 */
	public List<T> findList(String sortHql);
	
	/**
	 * Description:根据实体1个属性查询实体集合 支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:43:33
	 * @param property 属性
	 * @param values 属性的值
	 * @return
	 */
	public List<T> findListByProperty(String property, Object values);
	
	/**
	 * Description:根据实体1个属性查询实体集合 支持or字句查询
	 * @author 唐海洋
	 * @Version 1.0 2014-3-8上午09:43:33
	 * @param sortHql 排序语句
	 * @param property 属性
	 * @param values 属性的值
	 * @return
	 */
	public List<T> findListByProperty(String sortHql,String property, Object values);
	
	/**
	 * Description:根据实体N个属性查询实体集合 支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:43:33
	 * @param property  属性
	 * @param values 属性的值
	 * @return
	 */
	public List<T> findListByPropertys(String[] property, Object... values);

	/**
	 * Description:根据实体N个属性查询实体集合并排序  支持or字句查询
	 * @author 葛传艺 
	 * @version 0.1 2014-7-26 上午11:13:09
	 * @param sortHql 排序hql
	 * @param property 属性
	 * @param values 属性值
	 * @return
	 */
	public List<T> findListByPropertys(String sortHql,String[] property,Object... values);

	/**
	 * Description:查询分页数据
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:30:44 
	 * @param p 分页对象
	 * @return
	 */
	public Pagination<T> findPagination(Pagination<T> p);
	
	/**
	 * Description:查询分页数据
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:31:08 
	 * @param sortHql 排序语句
	 * @param p 分页对象
	 * @return
	 */
	public Pagination<T> findPagination(String sortHql,Pagination<T> p);

	/**
	 * Description:查询分页数据  支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:41:09
	 * @param p 分页对象
	 * @param property 实体属性
	 * @param values 对应属性的值
	 * @return
	 */
	public Pagination<T> findPaginationByProperty(Pagination<T> p, String property, Object values);
	
	/**
	 * Description:查询分页数据  支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:41:09
	 * @param p 分页对象
	 * @param property 实体属性
	 * @param values 对应属性的值
	 * @return
	 */
	public Pagination<T> findPaginationByProperty(String sortHql,Pagination<T> p, String property, Object values);
	
	/**
	 * Description:查询分页数据  支持or字句查询
	 * @author 葛传艺
	 * @Version 1.0 2014-3-8上午09:41:09
	 * @param p 分页对象
	 * @param property 实体属性
	 * @param values 对应属性的值
	 * @return
	 */
	public Pagination<T> findPaginationByPropertys(Pagination<T> p, String[] property, Object... values);
	
	/**
	 * Description:查询分页数据  支持or字句查询
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午5:31:37 
	 * @param sortHql 排序语句
	 * @param p 分页对象
	 * @param property 属性列表
	 * @param values 属性值
	 * @return
	 */
	public Pagination<T> findPaginationByPropertys(String sortHql,Pagination<T> p,String[] property,Object... values);
}