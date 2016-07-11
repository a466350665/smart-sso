package com.${company!''}.${project!''}.${module!''}.service;

import com.${company!''}.${project!''}.${module!''}.model.${model};
import com.${company!''}.${project!''}.${module!''}.dao.${model}Dao;
import com.${company!''}.${project!''}.sys.service.SysService;

public interface ${model}Service extends SysService<${model}Dao, ${model}, String> { 
}