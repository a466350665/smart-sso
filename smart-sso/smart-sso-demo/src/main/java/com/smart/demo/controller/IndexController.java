package com.smart.demo.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.sso.client.SessionPermission;
import com.smart.sso.client.SessionUser;
import com.smart.sso.client.SessionUtils;

/**
 * @author Joe
 */
@Controller
@RequestMapping("/index")
public class IndexController {

	@RequestMapping(method = RequestMethod.GET)
	public String execute(HttpServletRequest request, Model model) {
		SessionUser sessionUser = SessionUtils.getSessionUser(request);
		// 登录用户名
		model.addAttribute("userName", sessionUser.getAccount());
		
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		if (sessionPermission != null){
			// 登录用户当前应用的菜单
			model.addAttribute("userMenus", sessionPermission.getMenuList());
			// 登录用户当前应用的权限
			model.addAttribute("userPermissions", sessionPermission.getPermissionSet());
		}
		return "/index";
	}
}