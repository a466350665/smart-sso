package com.${company!''}.${project!''}.<#if module??>${module}.</#if>dao.impl;

import org.springframework.stereotype.Component;

import com.${company!''}.${project!''}.<#if module??>${module}.</#if>model.${model};
import com.${company!''}.${project!''}.<#if module??>${module}.</#if>dao.${model}Dao;
import com.smart.ssm.dao.impl.DaoImpl;

@Component("${_model}Dao")
public class ${model}DaoImpl extends DaoImpl<${model}, Integer> implements ${model}Dao {
}
