package com.smart.sso.server.controller.admin;

import java.util.Collections;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
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
    
    @Value("${sso.server.url}")
    private String ssoServerUrl;

	@ApiOperation("初始页")
	@RequestMapping(method = RequestMethod.GET)
	public String execute(HttpServletRequest request, Model model) {
		SessionUser sessionUser = SessionUtils.getUser(request);
		// 设置登录用户名
		model.addAttribute("userName", sessionUser.getAccount());
		SessionPermission sessionPermission = SessionUtils.getPermission(request);
		// 设置当前登录用户没有的权限，以便控制前台按钮的显示或者隐藏
		model.addAttribute("sessionUserNoPermissions",
				sessionPermission == null ? null : sessionPermission.getNoPermissions());
		// 单点退出地址
        model.addAttribute("ssologoutUrl", ssoServerUrl + "/logout?service=" + getLocalUrl(request).toString());
		return "/admin";
	}

    @ApiOperation("菜单")
    @ResponseBody
	@RequestMapping(value = "/menu", method = RequestMethod.GET)
	public Result menu(HttpServletRequest request) {
		SessionPermission sessionPermission = SessionUtils.getPermission(request);
		// 获取登录用户权限下的菜单列表
        return Result
            .createSuccess(sessionPermission == null ? Collections.emptyList() : sessionPermission.getMenuList());
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