package com.${company!''}.${project!''}.<#if module??>${module}.</#if>dao;

import com.${company!''}.${project!''}.<#if module??>${module}.</#if>model.${model};
import com.smart.ssm.dao.Dao;

public interface ${model}Dao extends Dao<${model}, Integer> {
}
