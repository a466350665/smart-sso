package com.${company!''}.${project!''}.${module!''}.model;

<#if containDate>
import java.util.Date;
</#if>

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
<#if containEnable>
import javax.persistence.Transient;
</#if>

<#if containEnable>
import com.${company!''}.${project!''}.sys.enums.TrueFalse;
</#if>
import com.${company!''}.${project!''}.sys.model.${extendsProject};

<#if tableComment??>
/**
 * ${tableComment}
 */
</#if>
@Entity
@Table(name = "${tableName}")
public class ${model} extends ${extendsProject} {

	private static final long serialVersionUID = ${versionId}L;
	
	<#list fieldList as field>
		<#if field.description??>
	/** ${field.description} */
	  	</#if>
	private ${field.fieldType} ${field.fieldName}<#if field.defaultValue??><#if field.fieldType == "Boolean"> = <#if field.defaultValue == "0">Boolean.FALSE<#else>Boolean.TRUE</#if><#elseif field.fieldType == "Integer"> = Integer.valueOf(${field.defaultValue})<#elseif field.fieldType == "Double"> = ${field.defaultValue}D<#else> = ${field.defaultValue}</#if></#if>;
	</#list>
	
	<#list fieldList as field>
	
	@Column(name = "${field.columnName}"<#if field.nullableStr == "false">, nullable = ${field.nullableStr}</#if><#if field.maxLengthStr??>, length = ${field.maxLengthStr}</#if>)
	public ${field.fieldType} get${field.upperFieldName}() {
		return this.${field.fieldName};
	}
	
	public void set${field.upperFieldName}(${field.fieldType} ${field.fieldName}) {
		this.${field.fieldName} = ${field.fieldName};
	}
	</#list>
	
	<#if containEnable>
	@Transient
	public String getIsEnableStr() {
		return (isEnable != null && isEnable) ? TrueFalse.TRUE.getLabel() : TrueFalse.FALSE.getLabel();
	}
	</#if>
}
