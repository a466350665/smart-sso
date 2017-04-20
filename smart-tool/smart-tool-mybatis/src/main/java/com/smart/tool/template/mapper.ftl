<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE mapper PUBLIC "-//mybatis.org//DTD Mapper 3.0//EN" "http://mybatis.org/dtd/mybatis-3-mapper.dtd" >
<mapper namespace="com.${company!''}.${project!''}.<#if module??>${module}.</#if>dao.${model}Dao">
	<select id="findByAll" parameterType="map" resultType="${model}">   
		SELECT a.* 
     	FROM ${tableName} a
	</select>
	
	<select id="get" parameterType="java.lang.Integer" resultType="${model}">
     	SELECT a.* 
     	FROM ${tableName} a
     	WHERE a.`id` = &{id}
    </select>
    
	<insert id="insert" parameterType="${model}" statementType="PREPARED" useGeneratedKeys="true" keyProperty="id">
		INSERT INTO ${tableName}(
			<#list fieldList as field>
			`${field.fieldName}`
			</#list>
		) 
		VALUES (
			<#list fieldList as field>
			&{${field.fieldName}}
			</#list>
		)
	</insert>
	
    <update id="update" parameterType="${model}" statementType="PREPARED">
		UPDATE ${tableName} a SET
			<#list fieldList as field>
			a.`${field.fieldName}` = &{${field.fieldName}}
			</#list>
		WHERE a.`id` = &{id}
	</update>
	
	<delete id="deleteById" parameterType="list" statementType="PREPARED">
		DELETE FROM ${tableName}
		<choose>
			<when test="list == null or list.size() == 0">
				WHERE 1 != 1
			</when>
			<when test="list.size() == 1">
				WHERE `id` = <foreach collection="list" item="id">&{id}</foreach>
			</when>
			<otherwise>
				WHERE `id` in <foreach collection="list" item="id" open="(" separator="," close=")">&{id}</foreach>
			</otherwise>
		</choose>
	</delete>
</mapper>