package openjoe.smart.sso.server.service;

import openjoe.smart.sso.server.stage.mybatisplus.service.BaseService;
import openjoe.smart.sso.server.entity.Office;

import java.util.List;

public interface OfficeService extends BaseService<Office> {

	/**
	 * 启用禁用操作
	 * @param isEnable 是否启用
	 * @param idList 管理员ID集合
	 * @return
	 */
	void enable(Boolean isEnable, List<Long> idList);
	
	List<Office> selectList(Boolean isEnable, Boolean isParent, Long currentId, String prefix);
	
	List<Long> selectIdListByParentId(Long parentId);
}