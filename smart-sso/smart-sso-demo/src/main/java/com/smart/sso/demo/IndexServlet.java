package com.smart.sso.demo;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.smart.sso.client.SessionPermission;
import com.smart.sso.client.SessionUser;
import com.smart.sso.client.SessionUtils;

public class IndexServlet extends HttpServlet {

	private static final long serialVersionUID = -9082570370397324479L;

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		SessionUser sessionUser = SessionUtils.getSessionUser(request);
		// 登录用户名
		request.setAttribute("userName", sessionUser.getAccount());
		
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		if (sessionPermission != null){
			// 登录用户当前应用的菜单
			request.setAttribute("userMenus", sessionPermission.getMenuList());
			// 登录用户当前应用的权限
			request.setAttribute("userPermissions", sessionPermission.getPermissionSet());
		}
		request.getRequestDispatcher("/index.jsp").forward(request, response);
	}
}