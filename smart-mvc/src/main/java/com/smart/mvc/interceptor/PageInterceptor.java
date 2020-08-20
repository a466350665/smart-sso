package com.smart.mvc.interceptor;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.ibatis.executor.parameter.ParameterHandler;
import org.apache.ibatis.executor.resultset.ResultSetHandler;
import org.apache.ibatis.executor.statement.StatementHandler;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.plugin.Interceptor;
import org.apache.ibatis.plugin.Intercepts;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.plugin.Plugin;
import org.apache.ibatis.plugin.Signature;
import org.apache.ibatis.reflection.MetaObject;
import org.apache.ibatis.reflection.SystemMetaObject;
import org.apache.ibatis.scripting.defaults.DefaultParameterHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.mvc.model.Page;

/**
 * Mybatis - 分页拦截器
 * 
 * @author Joe
 */
@Intercepts({ @Signature(type = StatementHandler.class, method = "prepare", args = { Connection.class, Integer.class }),
		@Signature(type = ResultSetHandler.class, method = "handleResultSets", args = { Statement.class }) })
public class PageInterceptor implements Interceptor {
	
	private final Logger logger = LoggerFactory.getLogger(getClass());
	
	private final ThreadLocal<Page<?>> localPage = new ThreadLocal<>();

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Object intercept(Invocation invocation) throws Throwable {
		if (invocation.getTarget() instanceof StatementHandler) {
			StatementHandler statementHandler = (StatementHandler) invocation.getTarget();
			Page<?> page = findPageParameter(statementHandler.getBoundSql().getParameterObject());
			if (page == null)
				return invocation.proceed();
			else {
				localPage.set(page);
			}

			MetaObject metaStatementHandler = SystemMetaObject.forObject(statementHandler);
			// 分离代理对象链(由于目标类可能被多个拦截器拦截，从而形成多次代理，通过下面的两次循环
			// 可以分离出最原始的的目标类)
			while (metaStatementHandler.hasGetter("h")) {
				Object object = metaStatementHandler.getValue("h");
				metaStatementHandler = SystemMetaObject.forObject(object);
			}
			// 分离最后一个代理对象的目标类
			while (metaStatementHandler.hasGetter("target")) {
				Object object = metaStatementHandler.getValue("target");
				metaStatementHandler = SystemMetaObject.forObject(object);
			}
			MappedStatement mappedStatement = (MappedStatement) metaStatementHandler
					.getValue("delegate.mappedStatement");
			// 分页信息if (localPage.get() != null) {
			BoundSql boundSql = (BoundSql) metaStatementHandler.getValue("delegate.boundSql");
			// 分页参数作为参数对象parameterObject的一个属性
			String sql = boundSql.getSql();
			// 重写sql
			String pageSql = buildPageSql(sql, page);
			// 重写分页sql
			metaStatementHandler.setValue("delegate.boundSql.sql", pageSql);
			Connection connection = (Connection) invocation.getArgs()[0];
			// 重设分页参数里的总页数等
			setPageParameter(sql, connection, mappedStatement, boundSql, page);
			// 将执行权交给下一个拦截器
			return invocation.proceed();
		}
		else if (invocation.getTarget() instanceof ResultSetHandler) {
			Page<?> page = localPage.get();
			if (page == null)
				return invocation.proceed();
			try {
				Object result = invocation.proceed();
				page.setList((List) result);
				return result;
			}
			finally {
				localPage.remove();
			}
		}
		return null;
	}

	private Page<?> findPageParameter(Object param) {
		if (param instanceof Page<?>) {
			return (Page<?>) param;
		}
		else if (param instanceof Map) {
			for (Object val : ((Map<?, ?>) param).values()) {
				if (val instanceof Page<?>) {
					return (Page<?>) val;
				}
			}
		}
		return null;
	}

	/**
	 * 只拦截这两种类型的 <br>
	 * StatementHandler <br>
	 * ResultSetHandler
	 * 
	 * @param target
	 * @return
	 */
	@Override
	public Object plugin(Object target) {
		if (target instanceof StatementHandler || target instanceof ResultSetHandler) {
			return Plugin.wrap(target, this);
		}
		else {
			return target;
		}
	}

	@Override
	public void setProperties(Properties properties) {
	}

	public String buildPageSql(String sql, Page<?> page) {
		StringBuilder pageSql = new StringBuilder(sql);
		pageSql.append(" LIMIT " + (page.getPageNo() - 1) * page.getPageSize() + "," + page.getPageSize());
		return pageSql.toString();
	}

	/**
	 * 获取总记录数
	 * 
	 * @param sql
	 * @param connection
	 * @param mappedStatement
	 * @param boundSql
	 * @param page
	 */
	private void setPageParameter(String sql, Connection connection, MappedStatement mappedStatement,
			BoundSql boundSql, Page<?> page) {
		// 记录总记录数
		String countSql = "SELECT COUNT(0) FROM (" + sql + ") a LIMIT 1";
		PreparedStatement countStmt = null;
		ResultSet rs = null;
		try {
			countStmt = connection.prepareStatement(countSql);
			BoundSql countBS = new BoundSql(mappedStatement.getConfiguration(), countSql,
					boundSql.getParameterMappings(), boundSql.getParameterObject());
			
            // 解决分页使用<foreach>循环出现的BindingException问题
            MetaObject countBsObject = SystemMetaObject.forObject(countBS);
            MetaObject boundSqlObject = SystemMetaObject.forObject(boundSql);
            countBsObject.setValue("metaParameters", boundSqlObject.getValue("metaParameters"));
            countBsObject.setValue("additionalParameters", boundSqlObject.getValue("additionalParameters"));
			
			setParameters(countStmt, mappedStatement, countBS, boundSql.getParameterObject());
			rs = countStmt.executeQuery();
			int rowCount = 0;
			if (rs.next()) {
				rowCount = rs.getInt(1);
				logger.debug("     Total: {}", rowCount);
			}
			page.setRowCount(rowCount);
		}
		catch (Exception e) {
			logger.error("Ignore this exception", e);
		}
		finally {
			try {
				if (rs != null) {
					rs.close();
				}
			}
			catch (Exception e) {
				logger.error("", e);
			}
			try {
				if (countStmt != null) {
					countStmt.close();
				}
			}
			catch (Exception e) {
				logger.error("", e);
			}
		}
	}

	/**
	 * 代入参数值
	 * 
	 * @param ps
	 * @param mappedStatement
	 * @param boundSql
	 * @param parameterObject
	 * @throws SQLException
	 */
	private void setParameters(PreparedStatement ps, MappedStatement mappedStatement, BoundSql boundSql,
			Object parameterObject) throws SQLException {
		ParameterHandler parameterHandler = new DefaultParameterHandler(mappedStatement, parameterObject, boundSql);
		parameterHandler.setParameters(ps);
	}
}
