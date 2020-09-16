package com.smart.sso.server.dto;

import com.smart.sso.client.dto.RpcUserDto;

/**
 * 登录成功用户对象
 * 
 * @author Joe
 */
public class LoginUserDto extends RpcUserDto {

	private static final long serialVersionUID = 4507869346123296527L;

	public LoginUserDto(Integer id, String account) {
		super(id, account);
	}
}