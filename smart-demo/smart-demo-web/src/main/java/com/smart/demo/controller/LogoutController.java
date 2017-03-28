package com.smart.demo.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.config.ConfigUtils;
import com.smart.sso.client.SsoInterceptor;

/**
 * 远程登出
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/logout")
public class LogoutController {
	
	@RequestMapping(method = RequestMethod.GET)
	public String logout(HttpServletRequest request) {
		request.getSession().invalidate();
		String ssoLogoutUrl = new StringBuilder().append(ConfigUtils.getProperty("sso.server.url"))
				.append("/logout?backUrl=").append(SsoInterceptor.getLocalUrl(request))
				.append("/admin/admin").toString();
		return "redirect:" + ssoLogoutUrl;
	}
}