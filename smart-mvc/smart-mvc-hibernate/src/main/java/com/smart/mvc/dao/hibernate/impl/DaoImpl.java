package com.smart.mvc.dao.hibernate.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

import com.smart.mvc.dao.hibernate.Dao;
import com.smart.mvc.model.Pagination;
import com.smart.mvc.util.hibernate.DaoUtils;
import com.smart.mvc.util.hibernate.ProPertyUtils;
import com.smart.util.StringUtils;

/**
 * Dao基类，实现了数据的CRUD
 * 
 * @param <T>
 * @param <ID>
 */
@SuppressWarnings( { "unchecked", "rawtypes" })
public abstract class DaoImpl<T, ID extends Serializable> implements Dao<T, ID> {
	
	protected Class<?> entityClass;
	protected SessionFactory sessionFactory;

	protected DaoImpl() {
		entityClass = DaoUtils.getSuperClassType(getClass());
	}

	public abstract void setSessionFactory(SessionFactory sessionFactory);

	/**
	 * Description:获取当前线程Session
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:47:23 
	 * @return
	 */
	protected Session getSession() {
		return sessionFactory.getCurrentSession();
	}

	/**
	 * 如果实体未持久化过则新建实体，否则更新实体
	 * @param t
	 */
	@Override
	public void save(T t) {
		getSession().save(t);
	}

	/**
	 * 更新实体
	 * @param t
	 */
	@Override
	public void update(T t) {
		getSession().update(t);
	}

	/**
	 * 删除实体
	 * @param t
	 */
	@Override
	public void delete(T t) {
		getSession().delete(t);
	}

	/**
	 * 获取实体By主键
	 */
	@Override
	public T get(ID pk) {
		return (T) getSession().get(entityClass, (Serializable) pk);
	}

	/**
	 * Description:通过HQL语句和参数值查询实体
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:50:49 
	 * @param hql hql查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param values 查询参数值
	 * @return
	 */
	protected List<T> findByHql(String hql, boolean cacheAble, Object... values) {
		return createHqlQuery(hql, cacheAble, values).list();
	}

	/**
	 * Description:通过SQL语句和参数值查询唯一实体
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:52:05 
	  * @param hql 查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param values 查询参数值
	 * @return
	 */
	protected T findByHqlFirst(String hql, boolean cacheAble, Object... values) {
		return (T) createHqlQuery(hql, cacheAble, values).uniqueResult();
	}

	/**
	 * Description:通过HQL语句和参数值查询实体(分页)
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:52:37 
	 * @param hql 查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param p 分页对象
	 * @param values 查询参数值
	 * @return
	 */
	protected List<T> pageByHql(String hql, boolean cacheAble, Pagination p,
			Object... values) {
		
		// 查询总记录数,hql语句一定要写别名
		long rowCount = (Long) createHqlQuery(queryCountByHql(hql), cacheAble,
				values).uniqueResult();
		if (rowCount <= 0)
			return null;
		p.setRowCount(rowCount);

		// 查询当前页记录
		Query query = createHqlQuery(hql,cacheAble,values);
		query.setFirstResult(p.getFirstResult());
		query.setMaxResults(p.getPageSize());
		p.setList(query.list());
		return p.getList();
	}

	/**
	 * Description:通过SQL语句和参数值查询实体  该方法仅限于sqlserver数据库
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:56:09 
	 * @param sql 查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param function with语句
	 * @param values 查询参数值
	 * @return
	 */
	protected List<T> findByRecursionSql(String sql, boolean cacheAble,
			String function, Object... values) {
		// 为递归查询添加前缀方法
		sql = (function == null ? "" : function).concat(" ").concat(sql);
		return createSqlQuery(sql, entityClass, cacheAble, values).list();
	}

	/**
	 * Description:通过SQL语句和参数值查询实体
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:58:13 
	 * @param sql 查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param values 查询参数值
	 * @return
	 */
	protected List<T> findBySql(String sql, boolean cacheAble, Object... values) {
		return findByRecursionSql(sql, cacheAble, null, values);
	}

	/**
	 * Description:通过SQL语句和参数值查询唯一实体
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:58:52 
	 * @param sql 查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param values 查询参数值
	 * @return
	 */
	protected T findBySqlFirst(String sql, boolean cacheAble, Object... values) {
		return (T) createSqlQuery(sql, entityClass, cacheAble, values).uniqueResult();
	}

	/**
	 * Description:通过SQL语句和参数值查询实体(分页)  该方法仅限于sqlserver数据库
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午5:00:21 
	 * @param sql 查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param p 分页对象
	 * @param function with语句
	 * @param values 查询参数值
	 * @return
	 */
	protected List<T> pageByRecursionSql(String sql, boolean cacheAble,
			Pagination p, String function, Object... values) {

		// 为递归查询添加前缀方法
		String functionStr = (function == null ? "" : function).concat(" ");
		String countSql = (functionStr).concat(queryCountBySql(sql));

		// 查询总记录数
		long rowCount = Long.valueOf(createSqlQuery(countSql, false, values)
				.uniqueResult().toString());
		if (rowCount <= 0)
			return null;
		p.setRowCount(rowCount);

		// 为递归查询添加前缀方法
		String querySql = (functionStr).concat(sql.toString());
		// 查询当前页记录
		SQLQuery query = createSqlQuery(querySql, entityClass, cacheAble,values);
		query.setFirstResult(p.getFirstResult());
		query.setMaxResults(p.getPageSize());
		p.setList(query.list());
		return p.getList();
	}

	/**
	 * Description:通过SQL语句和参数值查询实体(分页)
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:59:32 
	 * @param sql 查询语句
	 * @param cacheAble 是否使用查询缓存
	 * @param p 分页对象
	 * @param values 查询参数值
	 * @return
	 */
	protected List<T> pageBySql(String sql, boolean cacheAble, Pagination p,
			Object... values) {
		return pageByRecursionSql(sql, cacheAble, p, null, values);
	}

	//===================================
	/**
	 * Description:获取表中的所有记录
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:27 
	 * @param p
	 * @return
	 */
	@Override
	public List<T> findByAll(Pagination<T> p) {
		String hql = " from " + entityClass.getSimpleName() + " t  where 1=1 ";
		if (p != null){
			return pageByHql(hql, false, p);
		}
		return findByHql(hql, false);
	}

	/**
	 * Description:根据属性获取分页对象
	 * @author 唐海洋
	 * @Version 1.0 2015-7-23下午4:39:48 
	 * @param p 分页对象
	 * @param property 要查询的属性列表
	 * @param sortHql 排序语句 例如：sort DESC,createTime DESC
	 * @param values 要匹配的属性值列表
	 * @return
	 */
	@Override
	public List<T> findByAll(Pagination<T> p, String[] property,String sortHql, Object... values) {
		
		StringBuffer hql =generateHql(property, values);
		if (!StringUtils.isBlank(sortHql)) {
			hql.append(" order by ").append(sortHql);
		}
		
		try {
			List<Object> objVal = new ArrayList<Object>();
			for (Object o : values) {
				if (o != null){
					if(o.getClass().isArray()){
						objVal.addAll(Arrays.asList((Object[]) o));
					}else{
						objVal.add(o);
					}
				}
			}
			if (p != null){
				return pageByHql(hql.toString(), false, p, objVal.toArray());
			}
			return findByHql(hql.toString(), false, objVal.toArray());
		}catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Description:根据N个属性查询实体集合
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:37:11 
	 * @param property 待查询的属性集合
	 * @param values 查询属性对应的值
	 * @return
	 */
	@Override
	public List<T> findListByPropertys(String[] property, Object... values) {

		return findListByPropertys(null, property,values);
	}

	/**
	 * Description:根据N个属性查询实体集合
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:37:11 
	 * @param sortHql hql 排序语句
	 * @param property 待查询的属性集合
	 * @param values 查询属性对应的值
	 * @return
	 */
	@Override
	public List<T> findListByPropertys(String sortHql,String[] property, Object... values) {
		StringBuffer hql =generateHql(property, values);
		if (StringUtils.isNotBlank(sortHql)) {
			hql.append(" order by ").append(sortHql);
		}
		try {
			List<Object> objVal = new ArrayList<Object>();
			if(values!=null) {
				for (Object o : values) {
					if (o != null){
						if(o.getClass().isArray()){
							objVal.addAll(Arrays.asList((Object[]) o));
						}else{
							objVal.add(o);
						}
					}
				}
			}
			return findByHql(hql.toString(), false, objVal.toArray());
		}catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Description:生成hql查询条件
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:37:11 
	 * @param property 待查询的属性集合
	 * @param values 查询属性对应的值
	 * @return
	 */
	private StringBuffer generateHql(String[] property,Object... values) {
		if (StringUtils.isBlank(property) || values == null){
			return  new StringBuffer(" from "+entityClass.getSimpleName()+" t ");
		}
		if (property.length != values.length) {
			throw new IndexOutOfBoundsException("属性值长度与对应值不匹配");
		}
		String[] cachePro = property;
		StringBuffer hql = new StringBuffer(" from "+ entityClass.getSimpleName() + " t where 1=1 ");
		for (int i = 0; i < property.length; i++) {
			
			String s = ProPertyUtils.valProperty(entityClass, property[i]);
			if (StringUtils.isNotBlank(s)) {
				if(values[i].getClass().isArray()){
					hql.append(" and (");
					Object[] valuesArrayItem=(Object[]) values[i];
					for(int j=0,size=valuesArrayItem.length;j<size;j++){
						if(j==0){
							hql.append(" t.").append(property[i]).append(" =? ");
						}else{
							hql.append(" or t.").append(property[i]).append(" =? ");
						}
					}
					hql.append(" )");
				}else{
					hql.append(" and t.").append(property[i]).append(" =? ");
				}
			}else {
				throw new RuntimeException("在" + entityClass.getName()+ "中不存在属性:" + cachePro[i]);
			}
		}
		return hql;
	}
	
	/**
	 * Description:sql或hql防注入
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午5:08:49 
	 * @param sql
	 * @return
	 */
	protected String filterSql(String sql) {
		String filterText = filterName(sql);
		if (sql.equals(filterText)) {
			return sql;
		}else {
			return filterSql(filterText);
		}
	}

	/**
	 * Description:数据库敏感注入规则
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午5:07:00 
	 * @param filterText 将要过滤的sql语句
	 * @return 过滤后的sql语句
	 */
	private String filterName(String filterText) {
		String[] list = { "and", "exec", "insert", "select", "delete",
				"update", "truncate", "char", "declare", "'", "--", "\"", "<",
				">", "drop", "fetch", "%" };
		Matcher m = null;
		for (int i = 0, len = list.length; i < len; i++) {
			Pattern p = Pattern.compile(list[i], Pattern.CASE_INSENSITIVE);
			StringBuffer sBuffer = new StringBuffer();
			m = p.matcher(filterText);
			while (m.find()) {
				m.appendReplacement(sBuffer, "");
			}
			m.appendTail(sBuffer);
			filterText = sBuffer.toString();
		}
		return filterText;
	}	

	/**
	 * 创建Hql查询语句
	 */
	private Query createHqlQuery(String hql, boolean cacheAble, Object... values) {
		Query query = getSession().createQuery(hql);
		query.setCacheable(cacheAble);
		if (values != null) {
			for (int i = 0; i < values.length; i++) {
				query.setParameter(i, values[i]);
			}
		}
		return query;
	}
	
	/**
	 * 创建Sql查询语句
	 */
	private SQLQuery createSqlQuery(String sql,Class entityClass, boolean cacheAble,
			Object... values) {
		SQLQuery query = createSqlQuery(sql, cacheAble, values);
		query.addEntity(entityClass);
		return query;
	}

	/**
	 * Description:创建Sql查询语句
	 * @author 唐海洋
	 * @Version 1.0 2015-8-13下午4:45:02 
	 * @param sql
	 * @param cacheAble
	 * @param values
	 * @return
	 */
	private SQLQuery createSqlQuery(String sql,boolean cacheAble,Object... values) {
		SQLQuery query = getSession().createSQLQuery(sql.toString());
		int length;
		if (values != null && (length = values.length) > 0)
			for (int i = 0; i < length; i++) {
				query.setParameter(i, values[i]);
			}
		query.setCacheable(cacheAble);
		return query;
	}

	/**
	 * 把HQL语句转换成计数HQL语句，用于分页查询时统计总记录数
	 * 
	 * @param String
	 *            hql
	 * @return String
	 */
	private String queryCountByHql(String hql) {
		hql = Pattern.compile(" fetch ", Pattern.CASE_INSENSITIVE).matcher(hql)
				.replaceAll(" ");
		Pattern p = Pattern.compile("order\\s*by[\\w|\\W|\\s|\\S]*",
				Pattern.CASE_INSENSITIVE);
		Matcher m = p.matcher(hql);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			m.appendReplacement(sb, "");
		}
		m.appendTail(sb);

		String lowerHQL = sb.toString().toLowerCase();
		int selectPos = lowerHQL.indexOf("select");
		int fromPos = lowerHQL.indexOf("from");
		if (selectPos < 0 || selectPos > fromPos) {
			Class clazz = DaoUtils.getSuperClassType(getClass());
			int length = clazz.getSimpleName().length() + 1;
			int entityIndex = sb.indexOf(clazz.getSimpleName() + " ");
			if (entityIndex <= 0) {
				length = clazz.getName().length() + 1;
				entityIndex = sb.indexOf(clazz.getName() + " ");
			}
			int index = entityIndex + length;
			String tmp = sb.substring(index).trim();
			String alian = null;
			if (tmp.indexOf(" ") < 0) {
				alian = tmp;
			}
			else {
				alian = tmp.substring(0, tmp.indexOf(" "));
			}
			sb.insert(0, "SELECT COUNT(" + alian + ") ");
		}else {
			String tmp = sb.substring(selectPos + 7).trim();
			int distinctPos = lowerHQL.indexOf("distinct");
			String alian = null;
			if (distinctPos < 0 || distinctPos > fromPos) {
				alian = tmp.substring(0, tmp.indexOf(" "));
			}
			else {
				tmp = tmp.substring(9).trim();
				int index = tmp.indexOf(" ");
				alian = "DISTINCT " + tmp.substring(0, index);
			}
			sb.delete(0, fromPos);
			sb.insert(0, "SELECT COUNT(" + alian + ") ");
		}
		return sb.toString();
	}


	/**
	 * 把SQL语句转换成计数SQL语句，用于分页查询时统计总记录数
	 * @param String  sql
	 * @return String
	 */
	private String queryCountBySql(String sql) {
		
		Pattern p = Pattern.compile("order\\s*by[\\w|\\W|\\s|\\S]*",
				Pattern.CASE_INSENSITIVE);
		Matcher m = p.matcher(sql);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			m.appendReplacement(sb, "");
		}
		m.appendTail(sb);
		return "SELECT COUNT(1) FROM( "+sb.toString()+" ) as _count";
	}
}
