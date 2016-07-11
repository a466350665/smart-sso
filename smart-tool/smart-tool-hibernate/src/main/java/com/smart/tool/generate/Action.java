package com.smart.tool.generate;

import java.util.HashMap;
import java.util.Map;

import com.smart.tool.system.FreemarkerUtils;
import com.smart.tool.system.Generator;

/**
 * Action
 * 
 * @author Joe
 */
public class Action {

	private Map<String, Object> dataMap;

	public Action(String company, String project, String module, String model, boolean containEnable) {
		dataMap = new HashMap<String, Object>();
		/** 公司 **/
		dataMap.put("company", company);
		/** 项目 **/
		dataMap.put("project", project);
		/** 模块 **/
		dataMap.put("module", module);
		/** 模型 **/
		dataMap.put("model", model);
		/** 小写开头模型 **/
		dataMap.put("_model", Generator.getLowerStr(model));
		/** 是否包含启用 **/
		dataMap.put("containEnable", containEnable);
	}
	
	public String getHtml(){
		return FreemarkerUtils.getText("action.ftl", dataMap);
	}
}
