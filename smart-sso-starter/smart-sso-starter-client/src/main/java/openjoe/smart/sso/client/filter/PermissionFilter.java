package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.TokenUtils;
import org.springframework.core.annotation.Order;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * 权限控制Filter
 * 
 * @author Joe
 */
@Order(30)
public class PermissionFilter extends AbstractClientFilter {

	private ClientProperties properties;

	public PermissionFilter(ClientProperties properties) {
		this.properties = properties;
	}

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
		TokenPermission permission = TokenUtils.getPermission(request);
		String path = request.getServletPath();
		if (isPermitted(permission, path)) {
			return true;
		}
		else {
			responseJson(response, ClientConstant.NO_PERMISSION, "没有访问权限");
			return false;
		}
	}

	private boolean isPermitted(TokenPermission tokenPermission, String path) {
		if (tokenPermission.getPermissionSet().contains(path)) {
			return true;
		}
		if(tokenPermission.getNoPermissionSet().contains(path)){
			return false;
		}
		// 如果当前请求地址没有添加权限管理，也就是不需要做权限控制，请求直接放行
		return true;
	}

	public void setProperties(ClientProperties properties) {
		this.properties = properties;
	}

	public ClientProperties getProperties() {
		return properties;
	}
}