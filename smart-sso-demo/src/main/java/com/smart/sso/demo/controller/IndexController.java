package com.smart.sso.demo.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import com.smart.sso.client.model.SessionPermission;
import com.smart.sso.client.model.SessionUser;
import com.smart.sso.client.util.SessionUtils;

@Controller
public class IndexController {

	@Value("${sso.server.url}")
	private String ssoServerUrl;

	@GetMapping("/")
	public String index(Model model, HttpServletRequest request) {

		SessionUser sessionUser = SessionUtils.getUser(request);
		// 登录用户名
		model.addAttribute("userName", sessionUser.getAccount());
		// 单点退出
		model.addAttribute("ssologoutUrl", request.getServletContext().getContextPath() + "/logout");

		SessionPermission sessionPermission = SessionUtils.getPermission(request);
		if (sessionPermission != null) {
			// 登录用户当前应用的菜单
			request.setAttribute("userMenus", sessionPermission.getMenuList());
			// 登录用户当前应用的权限
			request.setAttribute("userPermissions", sessionPermission.getPermissionSet());
		}
		return "index";
	}
}
