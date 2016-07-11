package com.${company!''}.${project!''}.${module!''}.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.${company!''}.${project!''}.${module!''}.model.${model};
import com.${company!''}.${project!''}.${module!''}.dao.${model}Dao;
import com.${company!''}.${project!''}.${module!''}.service.${model}Service;
import com.${company!''}.${project!''}.sys.service.impl.SysServiceImpl;

@Component("${_model}Service")
public class ${model}ServiceImpl extends SysServiceImpl<${model}Dao, ${model}, String> implements ${model}Service {

	@Autowired
	public void setDao(${model}Dao dao) {
		this.dao = dao;
	}
}
