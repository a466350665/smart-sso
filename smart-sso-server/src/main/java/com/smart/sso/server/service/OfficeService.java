package com.smart.sso.server.service;

import java.util.List;

import com.smart.mvc.service.Service;
import com.smart.sso.server.model.Office;

public interface OfficeService extends Service<Office> {

	/**
	 * 启用禁用操作
	 * @param isEnable 是否启用
	 * @param idList 管理员ID集合
	 * @return
	 */
	public void enable(Boolean isEnable, List<Integer> idList);
	
	public List<Office> selectList(Boolean isEnable, Boolean isParent, Integer currentId, String prefix);
	
	public List<Integer> selectIdListByParentId(Integer parentId);
}