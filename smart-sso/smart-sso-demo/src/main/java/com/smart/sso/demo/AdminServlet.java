package com.smart.sso.demo;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.model.SessionPermission;
import com.smart.sso.client.model.SessionUser;
import com.smart.sso.client.util.SessionUtils;

public class AdminServlet extends HttpServlet {

	private static final long serialVersionUID = -9082570370397324479L;

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		SessionUser sessionUser = SessionUtils.getSessionUser(request);
		// 登录用户名
		request.setAttribute("userName", sessionUser.getAccount());

		// 单点退出地址
		request.setAttribute("ssologoutUrl", new StringBuilder().append(getSsoServerUrl(request))
				.append("/logout?backUrl=").append(getLocalUrl(request)).toString());

		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		if (sessionPermission != null) {
			// 登录用户当前应用的菜单
			request.setAttribute("userMenus", sessionPermission.getMenuList());
			// 登录用户当前应用的权限
			request.setAttribute("userPermissions", sessionPermission.getPermissionSet());
		}
		request.getRequestDispatcher("/admin.jsp").forward(request, response);
	}

	/**
	 * 获取单点登录服务端URL
	 * 
	 * @param request
	 * @return
	 */
	private String getSsoServerUrl(HttpServletRequest request) {
		WebApplicationContext wac = WebApplicationContextUtils
				.getRequiredWebApplicationContext(request.getServletContext());
		SmartContainer container = wac.getBean(SmartContainer.class);
		return container.getSsoServerUrl();
	}

	/**
	 * 获取当前应用访问路径
	 * 
	 * @param request
	 * @return
	 */
	private String getLocalUrl(HttpServletRequest request) {
		StringBuilder url = new StringBuilder();
		url.append(request.getScheme()).append("://").append(request.getServerName());
		if (request.getServerPort() != 80 && request.getServerPort() != 443) {
			url.append(":").append(request.getServerPort());
		}
		url.append(request.getContextPath());
		return url.toString();
	}
}