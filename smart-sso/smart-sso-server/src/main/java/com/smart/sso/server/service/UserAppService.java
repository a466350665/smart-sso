package com.smart.sso.server.service;

import java.util.List;

import com.smart.mvc.service.mybatis.Service;
import com.smart.sso.server.model.UserApp;

/**
 * 用户角色映射服务接口
 * 
 * @author Joe
 */
public interface UserAppService extends Service<UserApp, Integer> {
	
	/**
	 * 根据用户ID和角色ID查询映射
	 * @param userId 用户ID
	 * @param roleId 角色ID
	 * @return
	 */
	public UserApp findByUserAppId(Integer userId, Integer roleId);
	
	/**
	 * 根据用户ID给用户分配角色
	 * @param userId 用户ID
	 * @param idList 应用ID集合
	 * @param list 用户角色映射集合
	 * @return
	 */
	public void allocate(Integer userId, List<Integer> idList, List<UserApp> list);
	
	/**
	 * 根据用户ID集合删除映射
	 * @param idList 用户ID集合
	 * @return
	 */
	public void deleteByUserIds(List<Integer> idList);
	
	/**
	 * 根据应用ID集合删除映射
	 * @param idList 应用ID集合
	 * @return
	 */
	public void deleteByAppIds(List<Integer> idList);
}
