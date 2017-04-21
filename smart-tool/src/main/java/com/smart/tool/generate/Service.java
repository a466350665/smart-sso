package com.smart.tool.generate;

import java.util.HashMap;
import java.util.Map;

import com.smart.tool.system.FreemarkerUtils;
import com.smart.tool.system.StringUtils;

/**
 * Service
 * 
 * @author Joe
 */
public class Service {

	private Map<String, Object> dataMap;

	public Service(String company, String project, String module, String model) {
		dataMap = new HashMap<String, Object>();
		/** 公司 **/
		dataMap.put("company", company);
		/** 项目 **/
		dataMap.put("project", project);
		/** 模块 **/
		if (StringUtils.isNotBlank(module))
			dataMap.put("module", module);
		/** 模型 **/
		dataMap.put("model", model);
	}
	
	public String getHtml(){
		return FreemarkerUtils.getText("service.ftl", dataMap);
	}
}
