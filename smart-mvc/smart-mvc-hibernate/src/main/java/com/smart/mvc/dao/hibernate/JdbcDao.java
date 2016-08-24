package com.smart.mvc.dao.hibernate;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import com.smart.mvc.model.Pagination;

/**
 * JDBC访问结果集DAO层
 */
@SuppressWarnings({"rawtypes"})
public interface JdbcDao extends Serializable {
	
	/**
	 * Description:返回查询结果数量
	 * @author 唐海洋
	 * @Version 1.0 2014-8-13下午05:00:35 
	 * @param sql
	 * @param values
	 * @return
	 */
	int getCount(String sql,Object... values);

	/**
	 * 比如查询SELECT USER_NAME, USER_CODE FROM USER，那么查询返回结果 Object[] value =
	 * list.get(0)取第一条数据; value[0]为user_name的值, value[1]为user_code的值
	 * 
	 * @param String sql
	 * @param Object... values
	 * @return List<Object[]>
	 */
	List findSqlQuery(String sql, Object... values);

	/**
	 * 比如查询SELECT USER_NAME, USER_CODE FROM USER，那么查询返回结果 Map<String, Object>
	 * value = list.get(0)取第一条数据; value.get("user_name")为user_name的值,
	 * value.get("user_code")为user_code的值
	 * 
	 * @param String sql
	 * @param Object... values
	 * @return List<Map<String, Object>>
	 */
	List<Map<String, Object>> findSqlMapQuery(String sql,Object... values);
	
	/**
	 * 比如查询SELECT USER_NAME, USER_CODE FROM USER，那么查询返回结果第一条记录
	 * 
	 * @param String hql
	 * @param Object... values
	 * @return List<Map<String, Object>>
	 */
	Map<String, Object> findSqlMapQueryFirst(String sql,Object... values);
	
	/**
	 * Sql分页查询
	 * @param pagination
	 * @param sql
	 * @param function
	 * @param values
	 * @return List<Map<String, Object>>
	 */
	List<Map<String, Object>> pageSqlMapQuery(Pagination<?> pagination,
			String sql, String function, Object... values);

	/**
	 * 通过参数值执行SQL语句更新数据
	 * 
	 * @param String
	 *            sql
	 * @param Object
	 *            [] values
	 * @return int
	 */
	int executeUpdate(String sql, Object... values);
	
	/**
	 * 查询的数据表必须为包含在实体中且已经注解Hibernate<Model>
	 * @param entityClass 实体类型
	 * @param   String
	 *            hql
	 * @param Object
	 *            ... values
	 * @return List<Model>
	 */
	<T> List<T> findSqlListQuery(Class<T> entityClass,String sql,
			Object... values);
	
	/**
	 * Description:查询的数据表必须为包含在实体中且已经注解Hibernate
	 * @author 葛传艺 
	 * @version 0.1 2014-8-18 下午02:01:28
	 * @param <T>
	 * @param entityClass 实体类型
	 * @param sql
	 * @param pagination
	 * @param function
	 * @param values
	 * @return
	 */
	<T> List<T> pageByRecursionSql(Class<T> entityClass,String sql,
			Pagination<T> pagination, String function, Object... values);
	
	/**
	 * Description:保存
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:38:40 
	 * @param entity 实体
	 */
	<T> T save(T entity);
	
	/**
	 * Description:保存
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:38:40 
	 * @param entityList 实体列表
	 */
	<T> List<T> save(List<T> entityList);

	/**
	 * Description:更新
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:38:48 
	  * @param entity 实体
	 */
	<T> T update(T entity);
	
	/**
	 * Description:更新
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:38:48 
	 * @param entityList 实体列表
	 */
	<T> List<T> update(List<T> entityList);

	/**
	 * Description:删除
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:00 
	 * @param entity 实体
	 */
	<T> void delete(T entity);
	
	/**
	 * Description:删除
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:00 
	 * @param entityList 实体列表
	 */
	<T> void delete(List<T> entityList);
}