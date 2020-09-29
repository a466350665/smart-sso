package com.smart.sso.server.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.client.util.SessionUtils;
import com.smart.sso.server.common.TicketGrantingTicketManager;
import com.smart.sso.server.constant.AppConstant;
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
    private TicketGrantingTicketManager ticketGrantingTicketManager;

	@ApiOperation("登出")
	@RequestMapping(method = RequestMethod.GET)
	public String logout(@ValidateParam(name = "返回链接", value = { Validator.NOT_BLANK }) String service,
	        HttpServletRequest request) {
		String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
		if (!StringUtils.isEmpty(tgt)) {
		    ticketGrantingTicketManager.remove(tgt);
		}
		SessionUtils.invalidate(request);
        return "redirect:" + service;
	}
}