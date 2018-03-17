package com.smart.sso.server.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.model.Result;
import com.smart.sso.client.ApplicationPermission;
import com.smart.sso.client.SessionPermission;
import com.smart.sso.client.SessionUser;
import com.smart.sso.client.SessionUtils;

/**
 * @author Joe
 */
@Api(tags = "首页管理")
@Controller
@RequestMapping("/admin/admin")
public class AdminController {

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(HttpServletRequest request, Model model) {
		SessionUser sessionUser = SessionUtils.getSessionUser(request);
		// 设置登录用户名
		model.addAttribute("userName", sessionUser.getAccount());
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		// 设置当前登录用户没有的权限，以便控制前台按钮的显示或者隐藏
		model.addAttribute("sessionUserNoPermissions",
				sessionPermission == null ? null : sessionPermission.getNoPermissions());
		// 默认首页
		// model.addAttribute("defaultPage", null);
		return "/admin";
	}

	@ApiOperation("菜单")
	@RequestMapping(value = "/menu", method = RequestMethod.GET)
	public @ResponseBody Result menu(HttpServletRequest request) {
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		// 如果配置的权限拦截器，则获取登录用户权限下的菜单，没有权限拦截限制的情况下，获取当前系统菜单呈现
		return Result.createSuccessResult().setData(
				sessionPermission == null ? ApplicationPermission.getApplicationMenuList() : sessionPermission
						.getMenuList());
	}
}