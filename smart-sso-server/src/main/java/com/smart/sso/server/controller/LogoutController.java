package com.smart.sso.server.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.client.util.SessionUtils;
import com.smart.sso.server.common.TokenManager;
import com.smart.sso.server.util.CookieUtils;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * @author Joe
 */
@Api(tags = "单点登出")
@Controller
@RequestMapping("/logout")
public class LogoutController {
	
	@Autowired
	private TokenManager tokenManager;

	@ApiOperation("登出")
	@RequestMapping(method = RequestMethod.GET)
	public String logout(@ValidateParam(name = "返回链接") String backUrl, HttpServletRequest request) {
		String token = CookieUtils.getCookie(request, TokenManager.TOKEN);
		if (!StringUtils.isEmpty(token)) {
			tokenManager.remove(token);
		}
		SessionUtils.invalidate(request);
		return "redirect:" + (StringUtils.isEmpty(backUrl) ? "/admin/admin" : backUrl);
	}
}