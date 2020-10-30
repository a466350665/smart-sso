package com.smart.sso.server.service.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.springframework.stereotype.Service;

import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcUser;
import com.smart.sso.server.service.UserService;

@Service("userService")
public class UserServiceImpl implements UserService {
	
	private static Map<String, String> userMap;
	
	static {
		userMap = new HashMap<>();
		userMap.put("admin", "123456");
	}
	
	@Override
	public Result<RpcUser> login(String account, String password) {
		for (Entry<String, String> user : userMap.entrySet()) {
			if (user.getKey().equals(account)) {
				if(user.getValue().equals(password)) {
					return Result.createSuccess(new RpcUser(1, account));
				}
				else {
					return Result.createError("密码有误");
				}
			}
		}
		return Result.createError("用户不存在");
	}
}
