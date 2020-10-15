package com.smart.sso.server.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.smart.sso.client.util.SessionUtils;
import com.smart.sso.server.common.TicketGrantingTicketManager;
import com.smart.sso.server.constant.AppConstant;
import com.smart.sso.server.util.CookieUtils;

/**
 * 单点登出
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/logout")
public class LogoutController {
	
    @Autowired
    private TicketGrantingTicketManager ticketGrantingTicketManager;

	/**
	 * 登出
	 * @param service
	 * @param request
	 * @param response
	 * @return
	 */
	@RequestMapping(method = RequestMethod.GET)
	public String logout(
	    @RequestParam String service,
	        HttpServletRequest request, HttpServletResponse response) {
		String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
		if (!StringUtils.isEmpty(tgt)) {
		    ticketGrantingTicketManager.remove(tgt);
		    CookieUtils.removeCookie(AppConstant.TGC, "/", response);
		}
		SessionUtils.invalidate(request);
        return "redirect:" + service;
	}
}