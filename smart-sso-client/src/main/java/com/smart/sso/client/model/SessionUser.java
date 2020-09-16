package com.smart.sso.client.model;

import com.smart.sso.client.dto.RpcUserDto;

/**
 * 已登录用户信息
 * 
 * @author Joe
 */
public class SessionUser extends RpcUserDto {

	private static final long serialVersionUID = 1764365572138947234L;

	public SessionUser(Integer id, String account) {
		super(id, account);
	}
}
