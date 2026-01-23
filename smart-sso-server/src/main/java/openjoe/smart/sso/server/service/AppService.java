package openjoe.smart.sso.server.service;

import openjoe.smart.sso.server.entity.App;
import openjoe.smart.stage.core.entity.Page;
import openjoe.smart.stage.mybatisplus.service.BaseService;

import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * 应用服务接口
 * 
 * @author Joe
 */
public interface AppService extends BaseService<App> {
	
	/**
	 * 启用禁用操作
	 * @param isEnable 是否启用
	 * @param idList 应用ID集合
	 * @return
	 */
	void enable(Boolean isEnable, List<Long> idList);
	
	/**
	 * 根据名称查询
	 */
	List<App> selectAll(Boolean isEnable);
	
	/**
	 * 根据名称分页查询
	 * @param name 应用名称
	 * @return
	 */
	Page<App> selectPage(String name, Long current, Long size);
	
	/**
	 * 根据应用编码查询
	 * @param code 应用编码
	 * @return
	 */
	App selectByCode(String code);

	App selectByClientId(String clientId);

	void deleteByIds(Collection<Long> idList);

	Map<String, App> selectMapByClientIds(Collection<String> clientIdList);

	String generateClientId();
}