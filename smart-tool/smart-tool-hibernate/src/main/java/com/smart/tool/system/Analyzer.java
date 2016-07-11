package com.smart.tool.system;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

/**
 * 解析器(可分析Sql和db两种解析方式,并支持sql server和mysql两个数据库)
 * 
 * @author Joe
 */
public class Analyzer {
	public static String MYSQL = "mysql";
	public static String SQLSERVER = "sqlserver";
	
	/** 模型名称 */
	private String modelName;
	/** 表名 */
	private String tableName;
	/** 表描述 */
	private String tableComment;
	/** 字段集合 */
	private List<DummyField> fieldList;
	
	public static String ENABLE_NAME = "isEnable";
	private boolean containEnable = false;

	public Analyzer() {
	}

	// db连接解析方式
	public Analyzer(Statement statement, String tableName, String configUrl) {
		this.tableName = tableName;

		String[] subNames = tableName.toLowerCase().split("_");
		StringBuilder sbf = new StringBuilder();
		for (int i = 1; i < subNames.length; i++) {
			if (i == 1 && "re".equals(subNames[i])) {
				continue;
			}
			sbf.append(subNames[i].replaceFirst(subNames[i].substring(0, 1), subNames[i].substring(0, 1).toUpperCase()));
		}
		modelName = sbf.toString().trim();

		fieldList = new ArrayList<DummyField>();
		getTableComment(statement, tableName, configUrl);
		getFieldNames(statement, tableName, fieldList);
		getFieldDesription(statement, tableName, fieldList, configUrl);
	}
	
	private String parse(String all) {
		String comment = null;
		int index = all.indexOf("COMMENT='");
		if (index < 0) {
			return "";
		}
		comment = all.substring(index + 9);
		comment = comment.substring(0, comment.length() - 1);
		return comment;
	}
	
	public void getTableComment(Statement statement, String tableName, String configUrl) {
		try {
			if (configUrl.contains(MYSQL)) {
				ResultSet rs = statement.executeQuery("SHOW CREATE TABLE " + tableName);
				if (rs != null && rs.next()) {
					tableComment = parse(rs.getString(2));
				}
				rs.close();
			}
		}
		catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public void getFieldNames(Statement statement, String tableName, List<DummyField> fieldList) {
		try {
			ResultSet rs = statement.executeQuery("select * from " + tableName);
			ResultSetMetaData rss = rs.getMetaData();
			int columnCount = rss.getColumnCount();
			for (int i = 1; i <= columnCount; i++) {
				String[] ccs = rss.getColumnName(i).split("_");
				StringBuilder ccssbf = new StringBuilder(ccs[0]);
				for (int j = 1; j < ccs.length; j++) {
					ccssbf.append(ccs[j].replaceFirst(ccs[j].substring(0, 1), ccs[j].substring(0, 1).toUpperCase()));
				}
				String fileName = ccssbf.toString();
				if(ENABLE_NAME.equals(fileName)){
					containEnable = true;
				}
				fieldList.add(new DummyField(fileName, getJavaFieldType(rss.getColumnTypeName(i)), rss
						.getColumnName(i)));
			}
		}
		catch (SQLException e) {
			e.printStackTrace();
		}
	}
	
	public String getSqlServerDesription(String tableName){
		StringBuilder sbf = new StringBuilder();
		sbf.append(" SELECT c.name AS columnName, ");
		sbf.append("        convert(varchar(100),ex.value) AS description, ");
		sbf.append("        c.is_nullable AS nullable, ");
		sbf.append("        c.max_length AS maxLength ");
		sbf.append(" FROM sys.columns c LEFT ");
		sbf.append(" JOIN sys.extended_properties ex ON ex.major_id = c.object_id ");
		sbf.append(" AND ex.minor_id = c.column_id ");
		sbf.append(" AND ex.name = 'MS_Description' ");
		sbf.append(" WHERE OBJECTPROPERTY(c.object_id, 'IsMsShipped')=0 ");
		sbf.append("   AND OBJECT_NAME(c.object_id) = '" + tableName + "' ");
		return sbf.toString();
	}
	
	public String getMysqlDesription(String tableName){
		StringBuilder sbf = new StringBuilder();
		sbf.append(" SELECT column_name              AS columnName, ");
		sbf.append("        character_maximum_length AS maxLength, ");
		sbf.append("        CASE ");
		sbf.append("          WHEN is_nullable = 'YES' THEN 1 ");
		sbf.append("          ELSE 0 ");
		sbf.append("        END                      AS nullable, ");
		sbf.append("        column_type              AS columnType, ");
		sbf.append("        column_comment           AS description, ");
		sbf.append("        column_default           AS defaultValue ");
		sbf.append(" FROM   Information_schema.columns ");
		sbf.append(" WHERE  table_Name = '" + tableName + "' ");
		return sbf.toString();
	}

	public void getFieldDesription(Statement statement, String tableName, List<DummyField> fieldList, String configUrl) {
		try {
			String sql = "";
			if(configUrl.contains(MYSQL)){
				sql = getMysqlDesription(tableName);
			}
			else if(configUrl.contains(SQLSERVER)){
				sql = getSqlServerDesription(tableName);
			}
			ResultSet rs = statement.executeQuery(sql);
			while (rs.next()) {
				String columnName = rs.getString("columnName"); // 获取字段名
				for (DummyField dumField : fieldList) {
					if (dumField.getColumnName().equals(columnName)) {
						dumField.setDescription(rs.getString("description"));
						String maxLength = rs.getString("maxLength");
						if (StringUtils.isNotBlank(maxLength)) {
							dumField.setMaxLength(Integer.valueOf(maxLength));
						}
						if("Integer".equals(dumField.getFieldType()) && StringUtils.isNotBlank(rs.getString("columnType"))){
							dumField.setIntMaxLength(Integer.valueOf(rs.getString("columnType").split("\\(")[1].split("\\)")[0]));
						}
						dumField.setNullable(rs.getString("nullable").equals("1") ? true : false);
						String defaultValue = rs.getString("defaultValue");
						if (StringUtils.isNotBlank(defaultValue)) {
							dumField.setDefaultValue(defaultValue);
						}
						break;
					}
				}
			}
		}
		catch (SQLException e) {
			e.printStackTrace();
		}
	}

	// sql文件解析方式
	public Analyzer(String content) {
		String subContent = content.split("create table")[1];
		tableName = subContent.substring(0, subContent.indexOf('(')).toString().trim();

		String[] subNames = tableName.toLowerCase().split("_");
		StringBuilder sbf = new StringBuilder();
		for (int i = 1; i < subNames.length; i++) {
			if (i == 1 && "re".equals(subNames[i])) {
				continue;
			}
			sbf.append(subNames[i].replaceFirst(subNames[i].substring(0, 1), subNames[i].substring(0, 1).toUpperCase()));
		}
		modelName = sbf.toString().trim();

		String property = subContent.substring(subContent.indexOf('(') + 1, subContent.lastIndexOf('('));
		String[] propertys = property.split(",");
		fieldList = new ArrayList<DummyField>();
		for (String pro : propertys) {
			String[] onePropertys = pro.split("\\s+ ");
			if (onePropertys.length > 3) {
				String[] ccs = onePropertys[1].split("_");
				StringBuilder ccssbf = new StringBuilder(ccs[0]);
				for (int i = 1; i < ccs.length; i++) {
					ccssbf.append(ccs[i].replaceFirst(ccs[i].substring(0, 1), ccs[i].substring(0, 1).toUpperCase()));
				}
				fieldList.add(new DummyField(ccssbf.toString(), getJavaFieldType(onePropertys[2]), onePropertys[1]));
			}
		}
	}
	
	public boolean isContainEnable() {
		return containEnable;
	}
	
	public String getTableComment() {
		return tableComment;
	}

	public String getModelName() {
		return modelName;
	}

	public String getLowerModelName() {
		return modelName.substring(0, 1).toLowerCase() + modelName.substring(1, modelName.length());
	}

	public String getTableName() {
		return tableName;
	}

	public List<DummyField> getFieldList() {
		return fieldList;
	}

	private String getJavaFieldType(String type) {
		String fieldType = "";
		if ((type.equalsIgnoreCase("varchar"))) {
			fieldType = "String";
		}
		else if (type.equalsIgnoreCase("int")) {
			fieldType = "Integer";
		}
		else if (type.equalsIgnoreCase("datetime")) {
			fieldType = "Date";
		}
		else if (type.equalsIgnoreCase("bit") || type.equalsIgnoreCase("tinyint")) {
			fieldType = "Boolean";
		}
		else if (type.equalsIgnoreCase("Double") || type.equalsIgnoreCase("float")) {
			fieldType = "Double";
		}
		else {
			fieldType = "String";
		}
		return fieldType;
	}
}