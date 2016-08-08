package com.smart.sso.server.service.impl;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.stereotype.Service;

import com.smart.ssm.provider.PasswordProvider;
import com.smart.sso.rpc.AuthenticationRpcService;
import com.smart.sso.rpc.Menu;
import com.smart.sso.rpc.RpcUser;
import com.smart.sso.server.common.LoginUser;
import com.smart.sso.server.common.TokenManager;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.service.UserService;
import com.smart.util.StringUtils;

@Service("authenticationRpcService")
public class AuthenticationRpcServiceImpl implements AuthenticationRpcService {

	@Resource
	private PermissionService permissionService;
	@Resource
	private UserService userService;

	@Override
	public boolean validate(String token) {
		return TokenManager.validate(token) != null;
	}
	
	@Override
	public RpcUser findAuthInfo(String token, String appCode) {
		LoginUser user = TokenManager.validate(token);
		if (user != null) {
			return new RpcUser(user.getUserName(), user.getProfile());
		}
		return null;
	}
	
	@Override
	public List<Menu> findPermissionList(String token, String appCode) {
		if (StringUtils.isBlank(token)) {
			return permissionService.findListById(appCode, null);
		}
		else {
			LoginUser user = TokenManager.validate(token);
			if (user != null) {
				return permissionService.findListById(appCode, user.getUserId());
			}
			else {
				return new ArrayList<Menu>(0);
			}
		}
	}
	
	@Override
	public boolean updatePassword(String token, String newPassword) {
		LoginUser loginUser = TokenManager.validate(token);
		if (loginUser != null) {
			User user = userService.get(loginUser.getUserId());
			user.setPassword(PasswordProvider.encrypt(newPassword));
			int rows = userService.update(user);
			if (rows == 1)
				return true;
			else
				return false;
		}
		else {
			return false;
		}
	}
}
