package com.smart.sso.server.controller;

import com.smart.sso.server.constant.ServerConstant;
import com.smart.sso.server.session.SessionManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * 单点登出
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/logout")
public class LogoutController {

	@Autowired
	private SessionManager sessionManager;

	/**
	 * 登出
	 * 
	 * @param redirectUri
	 * @param request
	 * @param response
	 * @return
	 */
	@RequestMapping(method = RequestMethod.GET)
	public String logout(
			@RequestParam(value = ServerConstant.REDIRECT_URI, required = true) String redirectUri,
	        HttpServletRequest request, HttpServletResponse response) {
		sessionManager.invalidate(request, response);
        return "redirect:" + redirectUri;
	}
}