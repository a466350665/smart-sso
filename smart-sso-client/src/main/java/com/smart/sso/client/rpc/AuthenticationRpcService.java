package com.smart.sso.client.rpc;

import java.util.List;

import com.smart.sso.client.dto.RpcPermissionDto;
import com.smart.sso.client.dto.RpcUserDto;


/**
 * 身份认证及授权服务接口
 * 
 * @author Joe
 */
public interface AuthenticationRpcService {
	
	/**
	 * 校验票据是否有效
	 * 
	 * @param ticket 票据
	 * @return
	 */
	public RpcUserDto validate(String ticket);
	
	/**
	 * 获取当前用户所有权限(含菜单)
	 * 
	 * @param userId
	 *            用户ID
	 * @param appCode
	 *            应用编码
	 * @return
	 */
	public List<RpcPermissionDto> selectUserPermissionList(Integer userId, String appCode);
	
	/**
     * 获取当前应用所有权限(含菜单)
     * 
     * @param appCode
     *            应用编码
     * @return
     */
    public List<RpcPermissionDto> selectApplicationPermissionList(String appCode);
}
