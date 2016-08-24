package com.smart.mvc.dao.hibernate.impl;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.transform.ResultTransformer;
import org.hibernate.transform.Transformers;

import com.smart.mvc.dao.hibernate.JdbcDao;
import com.smart.mvc.model.Pagination;
/**
 * JDBC访问结果集的DAO实现
 * 
 */
@SuppressWarnings({"unchecked","rawtypes"})
public abstract class JdbcDaoImpl implements JdbcDao, Serializable {
	
	private static final long serialVersionUID = 3985309538600750401L;
	protected SessionFactory sessionFactory;

	protected abstract void setSessionFactory(SessionFactory sessionFactory);
	
	@Override
	public List findSqlQuery(String sql, Object... values) {
		return getSqlquery(sql, null, values).list();
	}

	@Override
	public List<Map<String, Object>> findSqlMapQuery(String sql,
			Object... values) {
		return getSqlquery(sql, Transformers.ALIAS_TO_ENTITY_MAP, values)
				.list();
	}
	
	@Override
	public Map<String, Object> findSqlMapQueryFirst(String sql,
			Object... values) {
		
		List<Map<String, Object>> list=findSqlMapQuery(sql, values);
		return (list!=null && list.size()>0)?list.get(0):null;
	}
	
	@Override
	public int executeUpdate(String sql, Object... values) {
		SQLQuery query=getSqlQuery(sql, false, values);
		return query.executeUpdate();
	}
	
	@Override
	public List<Map<String, Object>> pageSqlMapQuery(Pagination<?> pagination,
			String sql,String function, Object... values) {
		
		return pageByRecursionSql(sql,pagination, function, values);
	}
	
	@Override
	public <T> List<T> findSqlListQuery(Class<T> entityClass,String sql,
			Object... values){
		return getSqlquery(sql,null,values).addEntity(entityClass).list();
	}
	
	@Override
	public <T> List<T> pageByRecursionSql(Class<T> entityClass,String sql,
			Pagination<T> pagination, String function, Object... values) {
		
		// 为递归查询添加前缀方法
		String functionStr = (function == null ? "" : function).concat(" ");
		String countSql = (functionStr).concat(queryCountBySql(sql));
		
		// 查询总记录数
		long rowCount = Long.valueOf(getSqlQuery(countSql, false, values).uniqueResult().toString());
		if (rowCount <= 0) return null;
		pagination.setRowCount(rowCount);
		
		// 为递归查询添加前缀方法
		String querySql = (functionStr).concat(sql.toString());
		
		// 查询当前页记录
		SQLQuery query =getSqlquery(querySql, Transformers.ALIAS_TO_ENTITY_MAP, values);			
		query.setFirstResult(pagination.getFirstResult());
		query.setMaxResults(pagination.getPageSize());
		//返回实体分页对象
		pagination.setList(query.addEntity(entityClass).list());
		
		return pagination.getList();
	}
	
	@Override
	public int getCount(String sql,Object... values){
		
		Object count=getSqlquery(sql, null, values).uniqueResult();
		return (Integer) (count instanceof BigInteger?((BigInteger)count).intValue():count);
	}

	private Session getSession() {
		return sessionFactory.getCurrentSession();
	}
	
	/**
	 * Description:获取SQLQuery对象
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:25:42 
	 * @param sql
	 * @param transformer
	 * @param values
	 * @return
	 */
	private SQLQuery getSqlquery(String sql, ResultTransformer transformer,
			Object... values) {
		return getSqlQuery(sql, transformer, null, values);
	}
	
	/**
	 * Description:获取SQLQuery对象
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:26:54 
	 * @param sql
	 * @param cacheAble
	 * @param values
	 * @return
	 */
	private SQLQuery getSqlQuery(String sql, boolean cacheAble, Object... values) {
		return getSqlQuery(sql, null, cacheAble, values);
	}
	
	/**
	 * Description:获取SQLQuery对象
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:26:54 
	 * @param sql
	 * @param cacheAble
	 * @param values
	 * @return
	 */
	private SQLQuery getSqlQuery(String sql, ResultTransformer transformer, Boolean cacheAble, Object... values) {
		
		SQLQuery query = getSession().createSQLQuery(sql.toString());
		if (transformer != null){
			query.setResultTransformer(transformer);
		}
		if (cacheAble != null){
			query.setCacheable(cacheAble);
		}
		
		int length;
		if (values != null && (length = values.length) > 0)
			for (int i = 0; i < length; i++) {
				query.setParameter(i, values[i]);
			}
		
		return query;
	}
	
	/**
	 * Description:把SQL语句转换成计数SQL语句，用于分页查询时统计总记录数
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:26:09 
	 * @param sql
	 * @return
	 */
	private String queryCountBySql(String sql) {
		
		Pattern p = Pattern.compile("order\\s*by[\\w|\\W|\\s|\\S]*",Pattern.CASE_INSENSITIVE);
		Matcher m = p.matcher(sql);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			m.appendReplacement(sb, "");
		}
		m.appendTail(sb);
		return "SELECT COUNT(1) FROM( "+sb.toString()+" ) as _count";
	}
	
	private List<Map<String, Object>> pageByRecursionSql(String sql,
			Pagination pagination, String function, Object... values) {
		
		// 添加搜索条件
		// sql = addSearchParams(sql, pagination);
		
		// 为递归查询添加前缀方法
		String functionStr = (function == null ? "" : function).concat(" ");
		String countSql = (functionStr).concat(queryCountBySql(sql));
		
		// 查询总记录数
		long rowCount = Long.valueOf(getSqlQuery(countSql, false, values).uniqueResult().toString());
		if (rowCount <= 0) return null;
		pagination.setRowCount(rowCount);
		
		// 为递归查询添加前缀方法
		String querySql = (functionStr).concat(sql.toString());
		
		// 查询当前页记录
		SQLQuery query =getSqlquery(querySql, Transformers.ALIAS_TO_ENTITY_MAP, values);			
		query.setFirstResult(pagination.getFirstResult());
		query.setMaxResults(pagination.getPageSize());
		
		pagination.setList(query.list());
		
		return pagination.getList();
	}

	@Override
	public <T> T save(T entity) {
		if(entity!=null){
			getSession().save(entity);
		}
		return entity;
	}

	@Override
	public <T> List<T> save(List<T> entityList) {
		
		if(entityList!=null && entityList.size()>0){
			for(T entity:entityList){
				save(entity);
			}
		}
		return entityList;
	}

	@Override
	public <T> T update(T entity) {
		
		if(entity!=null){
			getSession().update(entity);
		}
		return entity;
	}

	@Override
	public <T> List<T> update(List<T> entityList) {
		
		if(entityList!=null && entityList.size()>0){
			for(T entity:entityList){
				update(entity);
			}
		}
		return entityList;
	}

	@Override
	public <T> void delete(T entity) {
		
		if(entity!=null){
			getSession().delete(entity);
		}
	}

	@Override
	public <T> void delete(List<T> entityList) {
		
		if(entityList!=null && entityList.size()>0){
			for(T entity:entityList){
				delete(entity);
			}
		}
	}
}