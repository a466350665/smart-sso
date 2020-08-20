package com.smart.sso.server.controller.admin;

import java.util.Collections;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.smart.mvc.model.Result;
import com.smart.sso.client.model.SessionPermission;
import com.smart.sso.client.model.SessionUser;
import com.smart.sso.client.util.SessionUtils;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * @author Joe
 */
@Api(tags = "首页管理")
@Controller
@RequestMapping("/admin/admin")
@SuppressWarnings("rawtypes")
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
		return "/admin";
	}

    @ApiOperation("菜单")
    @ResponseBody
	@RequestMapping(value = "/menu", method = RequestMethod.GET)
	public Result menu(HttpServletRequest request) {
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		// 获取登录用户权限下的菜单列表
        return Result
            .createSuccess(sessionPermission == null ? Collections.emptyList() : sessionPermission.getMenuList());
	}
}