package com.smart.mvc.interceptor;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.sql.Connection;
import java.util.Properties;

import org.apache.ibatis.builder.annotation.ProviderSqlSource;
import org.apache.ibatis.executor.statement.StatementHandler;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.mapping.SqlCommandType;
import org.apache.ibatis.plugin.Interceptor;
import org.apache.ibatis.plugin.Intercepts;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.plugin.Plugin;
import org.apache.ibatis.plugin.Signature;
import org.apache.ibatis.reflection.MetaObject;
import org.apache.ibatis.reflection.SystemMetaObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.mvc.provider.DynamicSqlProvider;
import com.smart.mvc.resovler.TableResolver;
import com.smart.mvc.resovler.TableResolver.TableInfo;

/**
 * Mybatis - 动态表名和表字段信息拦截器
 * 
 * @author Joe
 */
@Intercepts({
		@Signature(type = StatementHandler.class, method = "prepare", args = { Connection.class, Integer.class }) })
public class DynamicInterceptor implements Interceptor {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	@Override
	public Object intercept(Invocation invocation) throws Throwable {
		if (invocation.getTarget() instanceof StatementHandler) {
			StatementHandler statementHandler = (StatementHandler) invocation.getTarget();
			MetaObject metaStatementHandler = SystemMetaObject.forObject(statementHandler);
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
			// 这里只处理SELECT和DELETE类型操作，UPDATE和INSERT由DynamicSqlProvider处理
			if (!(mappedStatement.getSqlSource() instanceof ProviderSqlSource)
					|| !(SqlCommandType.SELECT == mappedStatement.getSqlCommandType()
							|| SqlCommandType.DELETE == mappedStatement.getSqlCommandType())) {
				return invocation.proceed();
			}
			Class<?> modelClass = getModelClass(mappedStatement.getId());
			if (modelClass == null) {
				return invocation.proceed();
			}
			TableInfo tableInfo = TableResolver.resolve(modelClass);
			BoundSql boundSql = (BoundSql) metaStatementHandler.getValue("delegate.boundSql");
			String realSql = boundSql.getSql().replaceAll(DynamicSqlProvider.TABLE_NAME, tableInfo.getTableName())
					.replaceAll(DynamicSqlProvider.SELECT_COLUMNS, tableInfo.getSelectColumns());
			metaStatementHandler.setValue("delegate.boundSql.sql", realSql);
			return invocation.proceed();
		}
		return null;
	}

	private Class<?> getModelClass(String statementId) {
		try {
			return getModelClass(Class.forName(statementId.substring(0, statementId.lastIndexOf("."))), 0);
		}
		catch (ClassNotFoundException e) {
			logger.error("get model class fail, statementId: {}", statementId, e);
			return null;
		}
	}

	private Class<?> getModelClass(Class<?> daoClass, int index) {
		Type[] types = daoClass.getGenericInterfaces();
		ParameterizedType parameterizedType = (ParameterizedType) types[index];
		Type type = parameterizedType.getActualTypeArguments()[index];
		return checkType(type, index);
	}

	private static Class<?> checkType(Type type, int index) {
		if (type instanceof Class<?>) {
			return (Class<?>) type;
		}
		else if (type instanceof ParameterizedType) {
			ParameterizedType pt = (ParameterizedType) type;
			Type t = pt.getActualTypeArguments()[index];
			return checkType(t, index);
		}
		else {
			String className = type == null ? "null" : type.getClass().getName();
			throw new IllegalArgumentException(
					"Expected a Class, ParameterizedType" + ", but <" + type + "> is of type " + className);
		}
	}

	@Override
	public Object plugin(Object target) {
		if (target instanceof StatementHandler) {
			return Plugin.wrap(target, this);
		}
		return target;
	}

	@Override
	public void setProperties(Properties properties) {
	}
}
