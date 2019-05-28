package com.smart.sso.server.service;

import java.util.List;

import com.smart.mvc.service.mybatis.Service;
import com.smart.sso.server.model.Office;

public interface OfficeService extends Service<Office, Integer> {

	/**
	 * 启用禁用操作
	 * @param isEnable 是否启用
	 * @param idList 管理员ID集合
	 * @return
	 */
	public void enable(Boolean isEnable, List<Integer> idList);
	
	public List<Office> findByParams(Boolean isEnable, Boolean isParent, Integer currentId, String prefix);
	
	public List<Integer> findIdListByParentId(Integer parentId);
}