package com.${company!''}.${project!''}.<#if module??>${module}.</#if>model;

<#if containDate>
import java.util.Date;
</#if>

<#if containEnable>
import java.beans.Transient;
</#if>

<#if containEnable>
import com.smart.mvc.enums.TrueFalseEnum;
</#if>
import com.smart.mvc.model.${extendsProject};

<#if tableComment??>
/**
 * ${tableComment}
 */
</#if>
public class ${model} extends ${extendsProject} {

	private static final long serialVersionUID = ${versionId}L;
	
	<#list fieldList as field>
		<#if field.description??>
	/** ${field.description} */
	  	</#if>
	private ${field.fieldType} ${field.fieldName}<#if field.defaultValue??><#if field.fieldType == "Boolean"> = <#if field.defaultValue == "0">Boolean.FALSE<#else>Boolean.TRUE</#if><#elseif field.fieldType == "Integer"> = Integer.valueOf(${field.defaultValue})<#elseif field.fieldType == "Double"> = ${field.defaultValue}D<#else> = ${field.defaultValue}</#if></#if>;
	</#list>
	
	<#list fieldList as field>
	
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
		return (isEnable != null && isEnable) ? TrueFalseEnum.TRUE.getLabel() : TrueFalseEnum.FALSE.getLabel();
	}
	</#if>
}
