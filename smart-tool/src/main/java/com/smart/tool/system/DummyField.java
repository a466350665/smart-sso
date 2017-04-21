package com.smart.tool.system;

/**
 * 虚拟字段实体
 * 
 * @author Joe
 */
public class DummyField {
	// Java属性名称
	private String fieldName;
	// Java属性类型
	private String fieldType;
	// 数据库字段名
	private String columnName;
	// 描述
	private String description;
	// 最大长度(String)
	private Integer maxLength;
	// 最大长度(Integer)
	private Integer intMaxLength;
	// 是否可为空
	private Boolean nullable;
	// 默认值
	private String defaultValue;

	public DummyField(String fieldName, String fieldType, String columnName) {
		this.fieldName = fieldName;
		this.fieldType = fieldType;
		this.columnName = columnName;
	}

	public String getFieldName() {
		return fieldName;
	}

	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}

	public String getFieldType() {
		return fieldType;
	}

	public void setFieldType(String fieldType) {
		this.fieldType = fieldType;
	}

	public String getColumnName() {
		return columnName;
	}

	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getMaxLength() {
		return maxLength;
	}

	public void setMaxLength(Integer maxLength) {
		this.maxLength = maxLength;
	}
	
	public Integer getIntMaxLength() {
		return intMaxLength;
	}

	public void setIntMaxLength(Integer intMaxLength) {
		this.intMaxLength = intMaxLength;
	}

	public String getMaxLengthStr(){
		return (maxLength != null && maxLength > 0) ? maxLength.toString() : null;
	}
	
	public String getIntMaxLengthStr(){
		return (intMaxLength != null && intMaxLength > 0) ? intMaxLength.toString() : null;
	}

	public Boolean isNullable() {
		return nullable;
	}

	public void setNullable(Boolean nullable) {
		this.nullable = nullable;
	}
	
	public String getNullableStr(){
		return (nullable != null && nullable) ? "true" : "false";
	}
	
	public String getUpperFieldName(){
		return Generator.getUpperStr(fieldName);
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}
}