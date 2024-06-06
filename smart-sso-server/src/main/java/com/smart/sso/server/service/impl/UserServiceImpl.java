package com.smart.sso.server.service.impl;

import java.util.ArrayList;
import java.util.List;

import com.smart.sso.server.entity.ServerUser;
import org.springframework.stereotype.Service;

import com.smart.sso.client.entity.Result;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.UserService;

@Service("userService")
public class UserServiceImpl implements UserService {
	
	private static List<User> userList;
	
	static {
		userList = new ArrayList<>();
		userList.add(new User(1, "管理员", "admin", "123456"));
	}
	
	@Override
	public Result<ServerUser> login(String username, String password) {
		for (User user : userList) {
			if (user.getUsername().equals(username)) {
				if(user.getPassword().equals(password)) {
					return Result.createSuccess(new ServerUser(user.getId(), user.getUsername()));
				}
				else {
					return Result.createError("密码有误");
				}
			}
		}
		return Result.createError("用户不存在");
	}
}
