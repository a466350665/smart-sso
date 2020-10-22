package com.smart.sso.server.service.impl;

import org.springframework.stereotype.Service;

import com.smart.sso.client.dto.Result;
import com.smart.sso.client.dto.SsoUser;
import com.smart.sso.server.service.UserService;

@Service("userService")
public class UserServiceImpl implements UserService {
	
	@Override
	public Result<SsoUser> login(String account, String password) {
	    if("admin".equals(account) && password.equals("123456")) {
	        return Result.createSuccess(new SsoUser(1, "admin"));
	    }
	    else {
	        return Result.create(4001, "用户不存在");
	    }
	}
}
