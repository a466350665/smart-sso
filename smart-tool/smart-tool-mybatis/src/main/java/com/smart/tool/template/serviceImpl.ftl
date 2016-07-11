package com.${company!''}.${project!''}.<#if module??>${module}.</#if>service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.${company!''}.${project!''}.<#if module??>${module}.</#if>model.${model};
import com.${company!''}.${project!''}.<#if module??>${module}.</#if>dao.${model}Dao;
import com.${company!''}.${project!''}.<#if module??>${module}.</#if>service.${model}Service;
import com.smart.ssm.service.impl.ServiceImpl;

@Component("${_model}Service")
public class ${model}ServiceImpl extends ServiceImpl<${model}Dao, ${model}, Integer> implements ${model}Service {

	@Autowired
	public void setDao(${model}Dao dao) {
		this.dao = dao;
	}
}
