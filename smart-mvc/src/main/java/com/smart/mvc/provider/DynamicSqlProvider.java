package com.smart.mvc.provider;

import java.io.Serializable;
import java.util.Map;

import javax.persistence.Table;

import com.smart.mvc.model.PersistentObject;
import com.smart.mvc.resovler.TableResolver;
import com.smart.mvc.resovler.TableResolver.TableInfo;

/**
 * 动态Sql提供者
 * 
 * @author Joe
 */
public class DynamicSqlProvider {
	
	public static final String ID = "id";
	
	public static final String TABLE_NAME = "@tableName";
	
	public static final String SELECT_COLUMNS = "@selectColumns";
	
	public String selectById(Serializable id) {
		return new StringBuilder().append("SELECT ").append(SELECT_COLUMNS).append(" FROM ").append(TABLE_NAME)
				.append(" WHERE ").append(ID).append(" = #{").append(ID).append("}").toString();
	}
	
	public String selectByCondition(Map<String, Object> map) {
		return new StringBuilder().append("<script>SELECT ").append(SELECT_COLUMNS).append(" FROM ").append(TABLE_NAME)
				.append(condition()).append("</script>").toString();
	}
	
	public String deleteByCondition(Map<String, Object> map) {
		return new StringBuilder().append("<script>DELETE FROM ").append(TABLE_NAME).append(condition())
				.append("</script>").toString();
	}
	
	public String insert(Object t) {
		return getTable(t.getClass()).getInsertSql();
	}
	
	public String update(Object t) {
		return getTable(t.getClass()).getUpdateSql();
	}
	
	private TableInfo getTable(Class<?> clazz) {
		Class<?> modelClass = getModelClass(clazz);
		if (modelClass == null) {
			throw new IllegalArgumentException("非法持久化class:" + clazz);
		}
		return TableResolver.resolve(modelClass);
	}
	
	private Class<?> getModelClass(Class<?> clazz) {
		if (clazz.getAnnotation(Table.class) != null) {
			return clazz;
		}
		Class<?> parent = clazz.getSuperclass();
		if (parent.equals(PersistentObject.class)) {
			return clazz;
		}
		else if (parent.equals(Object.class)) {
			return null;
		}
		else {
			return getModelClass(parent);
		}
	}
	
	private String condition() {
		StringBuilder c = new StringBuilder();
		c.append("<if test=\"condition != null\">");
		c.append("	<if test=\"condition.criteriaList != null\">");
		c.append("		<where>");
		c.append("			<foreach collection=\"condition.criteriaList\" item=\"criteria\">");
		c.append("				<choose>");
		c.append("					<when test=\"criteria.noValue\">");
		c.append("						and ${criteria.label}");
		c.append("					</when>");
		c.append("					<when test=\"criteria.singleValue\">");
		c.append("						and ${criteria.label} #{criteria.value}");
		c.append("					</when>");
		c.append("					<when test=\"criteria.twoValue\">");
		c.append("						and ${criteria.label} #{criteria.value[0]} and #{criteria.value[1]}");
		c.append("					</when>");
		c.append("					<when test=\"criteria.collectionValue\">");
		c.append("						and ${criteria.label} <foreach close=\")\" collection=\"criteria.value\" item=\"item\" open=\"(\" separator=\",\">#{item}</foreach>");
		c.append("					</when>");
		c.append("				</choose>");
		c.append("			</foreach>");
		c.append("		</where>");
		c.append("	</if>");
		c.append("	<if test=\"condition.orderBy != null\">");
		c.append("		ORDER BY ${condition.orderBy}");
		c.append("	</if>");
		c.append("</if>");
		return c.toString();
	}
}
