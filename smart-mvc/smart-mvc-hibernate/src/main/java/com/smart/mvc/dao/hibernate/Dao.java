package com.smart.mvc.dao.hibernate;
import java.util.List;

import com.smart.mvc.model.Pagination;

/**
 * Hibernate Dao接口
 */
public interface Dao<T, ID> {

	/**
	 * Description:保存
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:38:40 
	 * @param t
	 */
	public void save(T t);

	/**
	 * Description:更新
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:38:48 
	 * @param t
	 */
	public void update(T t);

	/**
	 * Description:删除
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:00 
	 * @param t
	 */
	public void delete(T t);

	/**
	 * Description:通过id获取
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:13 
	 * @param pk
	 * @return
	 */
	public T get(ID pk);

	/**
	 * Description:获取表中的所有记录
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:27 
	 * @param p
	 * @return
	 */
	public List<T> findByAll(Pagination<T> p);

	/**
	 * Description:根据属性获取分页对象 支持在某属性上进行or查询
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:48 
	 * @param p 分页对象
	 * @param property 要查询的属性列表
	 * @param sortHql 排序语句 例如：sort DESC,createTime DESC
	 * @param values 要匹配的属性值列表   
	 * @return
	 */
	public List<T> findByAll(Pagination<T> p, String[] property,
			String sortHql, Object... values);

	/**
	 * Description:通过属性值查询匹配的实体列表 支持在某属性上进行or查询
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:41:45 
	 * @param property 要查询的属性列表
	 * @param values 要匹配的属性值列表
	 * @return
	 */
	public List<T> findListByPropertys(String[] property, Object... values);	

	/**
	 * Description:根据属性获取分页对象 支持在某属性上进行or查询
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:41:39 
	 * @param sortHql 排序语句
	 * @param property 要查询的属性列表
	 * @param values 要匹配的属性值列表
	 * @return
	 */
	public List<T> findListByPropertys(String sortHql,String[] property, Object... values);
}