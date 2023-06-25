package com.smart.sso.server.service;

import com.smart.sso.server.service.BaseService;
import com.smart.sso.server.model.Office;

import java.util.List;

public interface OfficeService extends BaseService<Office> {

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