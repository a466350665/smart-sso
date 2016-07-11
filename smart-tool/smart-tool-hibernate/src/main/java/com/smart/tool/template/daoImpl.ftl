package com.${company!''}.${project!''}.${module!''}.dao.impl;

import org.springframework.stereotype.Component;

import com.${company!''}.${project!''}.${module!''}.model.${model};
import com.${company!''}.${project!''}.${module!''}.dao.${model}Dao;
import com.${company!''}.${project!''}.sys.dao.impl.SysDaoImpl;

@Component("${_model}Dao")
public class ${model}DaoImpl extends SysDaoImpl<${model}, String> implements ${model}Dao {
}
