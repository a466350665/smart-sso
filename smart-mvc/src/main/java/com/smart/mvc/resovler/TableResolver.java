package com.smart.mvc.resovler;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.concurrent.ConcurrentHashMap;

import javax.persistence.Column;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

import com.smart.mvc.provider.DynamicSqlProvider;

/**
 * Model解析为数据库表Table解析器
 * 
 * @author Joe
 */
public class TableResolver {

	private static final Map<Class<?>, TableInfo> tableInfoMap = new ConcurrentHashMap<>();

	/**
	 * <p>
	 * 解析并获取实体映射表信息
	 * <p>
	 *
	 * @param clazz
	 *            反射实体类
	 * @return
	 */
	public static TableInfo resolve(Class<?> clazz) {
		return tableInfoMap.computeIfAbsent(clazz, TableResolver::initTable);
	}

	private static TableInfo initTable(Class<?> clazz) {
		TableInfo tableInfo = new TableInfo();
		Table tableAnnotation = clazz.getAnnotation(Table.class);
		/* 表名 */
		String tableName = clazz.getSimpleName();
		if (tableAnnotation != null && !StringUtils.isEmpty(tableAnnotation.name())) {
			tableName = tableAnnotation.name();
		}
		else {
			tableName = camelToUnderline(tableName);
		}
		tableInfo.setTableName(tableName);

		List<TableColumn> fieldList = new ArrayList<>();
		List<Field> list = getFieldList(ClassUtils.getUserClass(clazz));
		for (Field field : list) {
			fieldList.add(initTableField(field));
		}

		/* 字段列表 */
		tableInfo.setColumnList(fieldList);
		initSelectColumns(tableInfo);
		initInsertSql(tableInfo);
		initUpdateSql(tableInfo);
		return tableInfo;
	}

	private static List<Field> getFieldList(Class<?> clazz) {
		if (null == clazz) {
			return Collections.emptyList();
		}
		List<Field> fieldList = new ArrayList<>();
		Field[] fields = clazz.getDeclaredFields();
		for (Field field : fields) {
			/* 过滤静态属性 */
			if (Modifier.isStatic(field.getModifiers())) {
				continue;
			}
			/* 过滤 transient关键字修饰的属性 */
			if (Modifier.isTransient(field.getModifiers()) || field.getAnnotation(Transient.class) != null) {
				continue;
			}
			fieldList.add(field);
		}
		/* 处理父类字段 */
		Class<?> superClass = clazz.getSuperclass();
		if (superClass.equals(Object.class)) {
			return fieldList;
		}
		/* 排除重载属性 */
		return excludeOverrideSuperField(fieldList, getFieldList(superClass));
	}

	/**
	 * <p>
	 * 排序重置父类属性
	 * </p>
	 *
	 * @param fieldList
	 *            子类属性
	 * @param superFieldList
	 *            父类属性
	 */
	private static List<Field> excludeOverrideSuperField(List<Field> fieldList, List<Field> superFieldList) {
		// 子类属性
		Map<String, Field> fieldMap = new HashMap<>();
		for (Field field : fieldList) {
			fieldMap.put(field.getName(), field);
		}
		for (Field superField : superFieldList) {
			if (null == fieldMap.get(superField.getName())) {
				// 加入重置父类属性
				fieldList.add(superField);
			}
		}
		return fieldList;
	}

	private static TableColumn initTableField(Field field) {
		/* 获取注解属性，自定义字段 */
		Column tableField = field.getAnnotation(Column.class);
		String columnName;
		if (tableField != null && !StringUtils.isEmpty(tableField.name())) {
			columnName = tableField.name();
		}
		else {
			// columnName = camelToUnderline(field.getName());
			columnName = field.getName();
		}
		return new TableColumn(field.getName(), columnName);
	}

	private static String camelToUnderline(String param) {
		if (StringUtils.isEmpty(param)) {
			return "";
		}
		int len = param.length();
		StringBuilder sb = new StringBuilder(len);
		for (int i = 0; i < len; i++) {
			char c = param.charAt(i);
			if (Character.isUpperCase(c) && i > 0) {
				sb.append("_");
			}
			sb.append(Character.toLowerCase(c));
		}
		return sb.toString();
	}

	private static void initSelectColumns(TableInfo tableInfo) {
		StringJoiner columns = new StringJoiner(",");
		List<TableColumn> list = tableInfo.getColumnList();
		for (TableColumn entityColumn : list) {
			String c = entityColumn.getColumn();
			if (!entityColumn.getColumn().equals(entityColumn.getField())) {
				c += " AS " + entityColumn.getField();
			}
			columns.add(c);
		}
		tableInfo.setSelectColumns(columns.toString());
	}

	private static void initInsertSql(TableInfo tableInfo) {
		StringJoiner columns = new StringJoiner(",", "(", ")");
		StringJoiner values = new StringJoiner(",", "(", ")");
		tableInfo.getColumnList().stream().filter(t -> !DynamicSqlProvider.ID.equals(t.getColumn())).forEach(t -> {
			columns.add("`" + t.getColumn() + "`");
			values.add("#{" + t.getField() + "}");
		});
		tableInfo.setInsertSql(new StringBuilder().append("INSERT INTO ").append(tableInfo.getTableName()).append(" ")
				.append(columns.toString()).append(" VALUES ").append(values.toString()).toString());
	}

	private static void initUpdateSql(TableInfo tableInfo) {
		StringJoiner columns = new StringJoiner(",");
		tableInfo.getColumnList().stream().filter(t -> !DynamicSqlProvider.ID.equals(t.getColumn()))
				.forEach(t -> columns.add(t.getColumn() + "=#{" + t.getField() + "}"));

		tableInfo.setUpdateSql(new StringBuilder().append("UPDATE ").append(tableInfo.getTableName()).append(" SET ")
				.append(columns.toString()).append(" WHERE ").append(DynamicSqlProvider.ID).append(" = #{")
				.append(DynamicSqlProvider.ID).append("}").toString());
	}

	public static class TableInfo {
		private String tableName;
		private String selectColumns;
		private String insertSql;
		private String updateSql;
		private List<TableColumn> columnList;

		public String getTableName() {
			return tableName;
		}

		public void setTableName(String tableName) {
			this.tableName = tableName;
		}

		public List<TableColumn> getColumnList() {
			return columnList;
		}

		public void setColumnList(List<TableColumn> columnList) {
			this.columnList = columnList;
		}

		public String getSelectColumns() {
			return selectColumns;
		}

		public void setSelectColumns(String selectColumns) {
			this.selectColumns = selectColumns;
		}

		public String getInsertSql() {
			return insertSql;
		}

		public void setInsertSql(String insertSql) {
			this.insertSql = insertSql;
		}

		public String getUpdateSql() {
			return updateSql;
		}

		public void setUpdateSql(String updateSql) {
			this.updateSql = updateSql;
		}
	}

	public static class TableColumn {
		private String field;
		private String column;

		public TableColumn(String field, String column) {
			super();
			this.column = column;
			this.field = field;
		}

		public String getColumn() {
			return column;
		}

		public void setColumn(String column) {
			this.column = column;
		}

		public String getField() {
			return field;
		}

		public void setField(String field) {
			this.field = field;
		}
	}
}
