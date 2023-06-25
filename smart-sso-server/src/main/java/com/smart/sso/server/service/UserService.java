package com.smart.sso.server.service;

import com.smart.sso.server.model.Page;
import com.smart.core.entity.Result;
import com.smart.sso.server.service.BaseService;
import com.smart.sso.server.model.User;

import java.util.Collection;
import java.util.List;

/**
 * 用户服务接口
 * 
 * @author Joe
 */
public interface UserService extends BaseService<User> {
	
	/**
	 * 登录
	 * 
	 * @param account
	 *            登录名
	 * @param password
	 *            密码
	 * @return
	 */
	public Result<User> login(String account, String password);
	
	/**
	 * 启用禁用操作
	 * @param isEnable 是否启用
	 * @param idList 用户ID集合
	 * @return
	 */
	public void enable(Boolean isEnable, List<Integer> idList);
	
	/**
	 * 重置密码
	 * @param password 初始化密码(已加密)
	 * @param idList 
	 */
	public void resetPassword(String password, List<Integer> idList);

	/**
	 * 查询分页列表
	 * @param account 登录名
	 * @param name 姓名
	 * @param officeId 机构ID
	 * @return
	 */
	public Page<User> selectPage(String account, String name, Integer officeId, Integer pageNo, Integer pageSize);
	
	/**
	 * 根据登录名和应用ID查询
	 * @param account 登录名
	 * @return
	 */
	public User selectByAccount(String account);
	
	/**
	 * 更新密码
	 * 
	 * @param id
	 *            用户ID
	 * @param newPassword
	 *            新密码
	 * @return
	 */
	public void updatePassword(Integer id, String newPassword);

	public void deleteByIds(Collection<Integer> idList);
}
