package com.smart.sso.server.service.impl;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

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
			return findPermissions(appCode, null);
		}
		else {
			LoginUser user = TokenManager.validate(token);
			if (user != null) {
				return findPermissions(appCode, user.getUserId());
			}
			else {
				return new ArrayList<Menu>(0);
			}
		}
	}
	
	private List<Menu> findPermissions(String appCode, Integer userId) {
		List<Map<String, Object>> list = permissionService.findListById(appCode, userId);
		if (CollectionUtils.isEmpty(list))
			return new ArrayList<Menu>(0);
		return listMapToJavaBean(list, Menu.class);
	}
	
	private <T> List<T> listMapToJavaBean(List<Map<String, Object>> list1, Class<T> beanClass) {
		List<T> list = null;
		try {
			list = new ArrayList<T>();
			for (Map<String, Object> mapdata : list1) {
				T t = beanClass.newInstance();
				mapToBean(mapdata, t);
				list.add(t);
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return list;
	}

	private void mapToBean(Map<String, Object> map, Object obj) {
		try {
			BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
			PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();

			for (PropertyDescriptor property : propertyDescriptors) {
				String key = property.getName();

				if (map.containsKey(key)) {
					Object value = map.get(key);
					Method setter = property.getWriteMethod();
					setter.invoke(obj, value);
				}
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return;
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
