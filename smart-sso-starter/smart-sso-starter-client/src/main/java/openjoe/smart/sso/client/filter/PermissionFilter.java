package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.ClientContextHolder;
import org.springframework.core.annotation.Order;

import jakarta.servlet.http.HttpServletResponse;
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
	public boolean isAccessAllowed() throws IOException {
		TokenPermission permission = ClientContextHolder.getPermission();
		String path = ClientContextHolder.getRequest().getServletPath();
		if (isPermitted(permission, path)) {
			return true;
		}
		else {
			responseJson(HttpServletResponse.SC_FORBIDDEN, ClientConstant.NO_PERMISSION, "没有访问权限");
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
		// 未配置到权限管理的路径无需鉴权，默认允许访问。
		return true;
	}

	public void setProperties(ClientProperties properties) {
		this.properties = properties;
	}

	public ClientProperties getProperties() {
		return properties;
	}
}
