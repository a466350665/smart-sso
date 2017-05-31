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
	protected String ssoLoginUri() {
        //TODO: 根据配置文件动态拼接成sso的登录地址，更好的做法应该是把ssoLogin单独成一个类，供第三方使用
        // SSO sso = new SSO('appkey', 'appsecrt')
        // sso.setBackUrl();
        // sso.getLoginUrl('http://localhost:8082/');
		return "http://localhost:8080/login?backUrl=http://localhost:8082/&appCode=system_demo";
	}

	@RequestMapping(method = RequestMethod.GET)
	public String execute(HttpServletRequest request, Model model) {
		SessionUser sessionUser = SessionUtils.getSessionUser(request);

		if (sessionUser == null) {
			//未授权，去登录界面
			return "redirect:" + ssoLoginUri();
		}

		model.addAttribute("userName", sessionUser.getAccount());
		
		SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
		if (sessionPermission != null){
			model.addAttribute("userMenus", sessionPermission.getMenuList());
			model.addAttribute("userPermissions", sessionPermission.getPermissionSet());
		}

		return "/index";
	}
}