package com.${company!''}.${project!''}.<#if module??>${module}.</#if>service;

import com.${company!''}.${project!''}.<#if module??>${module}.</#if>model.${model};
import com.${company!''}.${project!''}.<#if module??>${module}.</#if>dao.${model}Dao;
import com.smart.mvc.service.mybatis.Service;

public interface ${model}Service extends Service<${model}Dao, ${model}, Integer> { 
}