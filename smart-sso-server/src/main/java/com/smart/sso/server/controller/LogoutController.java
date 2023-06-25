package com.smart.sso.server.controller;

import com.smart.sso.client.util.SessionUtils;
import com.smart.sso.server.common.TicketGrantingTicketManager;
import com.smart.sso.server.constant.AppConstant;
import com.smart.sso.server.util.CookieUtils;
import com.smart.sso.server.validator.ValidateParam;
import com.smart.sso.server.validator.Validator;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author Joe
 */
@Api(tags = "单点登出")
@Controller
@RequestMapping("/logout")
public class LogoutController {
	
    @Autowired
    private TicketGrantingTicketManager ticketGrantingTicketManager;

	@ApiOperation("登出")
	@RequestMapping(method = RequestMethod.GET)
	public String logout(@ValidateParam(name = "返回链接", value = { Validator.NOT_BLANK }) String service,
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