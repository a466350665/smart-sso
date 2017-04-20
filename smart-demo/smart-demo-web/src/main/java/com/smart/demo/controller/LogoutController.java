package com.smart.demo.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.config.ConfigUtils;
import com.smart.sso.client.SsoInterceptor;

/**
 * @author Joe
 */
@Api(tags = "单点登出")
@Controller
@RequestMapping("/logout")
public class LogoutController {
	
	@ApiOperation("登出")
	@RequestMapping(method = RequestMethod.GET)
	public String logout(HttpServletRequest request) {
		request.getSession().invalidate();
		String ssoLogoutUrl = new StringBuilder().append(ConfigUtils.getProperty("sso.server.url"))
				.append("/logout?backUrl=").append(SsoInterceptor.getLocalUrl(request))
				.append("/admin/admin").toString();
		return "redirect:" + ssoLogoutUrl;
	}
}