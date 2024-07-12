package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenPermissionDTO;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.PermissionUtils;
import openjoe.smart.sso.client.util.TokenUtils;
import org.springframework.core.annotation.Order;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 权限控制Filter
 * 
 * @author Joe
 */
@Order(30)
public class PermissionFilter extends AbstractClientFilter {

	private ClientProperties properties;
	/**
	 * 缓存当前应用配置的所有权限
	 */
	private Set<String> applicationPermissionSet;

	public PermissionFilter(ClientProperties properties) {
		this.properties = properties;
	}

	/**
	 * 初始化缓存应用所有的菜单及权限
	 */
	public void initApplicationPermissions() {
		Result<List<TokenPermissionDTO>> result = PermissionUtils.getApplicationPermissionList(properties.getServerUrl(),
				properties.getAppKey(), properties.getAppSecret());
		if (!result.isSuccess()) {
			throw new IllegalArgumentException("无法连接到单点登录服务端,请检查配置smart.sso.server-url");
		}
		applicationPermissionSet = result.getData().stream().filter(p -> p.getUrl() != null && !p.getUrl().isEmpty())
				.map(p -> p.getUrl()).collect(Collectors.toSet());
	}

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String path = request.getServletPath();
		if (isPermitted(request, path)) {
			return true;
		}
		else {
			responseJson(response, ClientConstant.NO_PERMISSION, "没有访问权限");
			return false;
		}
	}

	private boolean isPermitted(HttpServletRequest request, String path) {
		// 缓存当前应用配置的所有权限
		if(applicationPermissionSet == null){
			initApplicationPermissions();
		}
		Set<String> permissionSet = getLocalPermissionSet(request);
		if (permissionSet.contains(path)) {
			return true;
		}
		// 如果当前请求没有配置权限控制，也返回为True
		return !applicationPermissionSet.contains(path);
	}

	private Set<String> getLocalPermissionSet(HttpServletRequest request) {
		TokenPermission tokenPermission = TokenUtils.getPermission(request);
		if (tokenPermission == null) {
			tokenPermission = invokePermissionInToken(request);
		}
		return tokenPermission.getPermissionSet();
	}

	/**
	 * 保存权限信息
	 * 
	 * @param request
	 * @return
	 */
	public TokenPermission invokePermissionInToken(HttpServletRequest request) {
		Result<List<TokenPermissionDTO>> result = PermissionUtils.getUserPermissionList(properties.getServerUrl(), TokenUtils.getUserId(request), properties.getAppKey(), properties.getAppSecret());

		List<TokenPermissionDTO> dbList = result.getData();

		List<TokenPermissionDTO> menuList = new ArrayList<>();
		Set<String> operateSet = new HashSet<>();
		for (TokenPermissionDTO menu : dbList) {
			if (menu.getIsMenu()) {
				menuList.add(menu);
			}
			if (menu.getUrl() != null) {
				operateSet.add(menu.getUrl());
			}
		}

		TokenPermission tokenPermission = new TokenPermission();
		// 设置登录用户菜单列表
		tokenPermission.setMenuList(menuList);

		// 保存登录用户没有权限的URL，方便前端去隐藏相应操作按钮
		Set<String> noPermissionSet = new HashSet<>(applicationPermissionSet);
		noPermissionSet.removeAll(operateSet);

		tokenPermission.setNoPermissions(String.join(",", noPermissionSet));

		// 保存登录用户权限列表
		tokenPermission.setPermissionSet(operateSet);
		TokenUtils.setPermission(tokenPermission, request);
		return tokenPermission;
	}

	public void setProperties(ClientProperties properties) {
		this.properties = properties;
	}

	public ClientProperties getProperties() {
		return properties;
	}
}