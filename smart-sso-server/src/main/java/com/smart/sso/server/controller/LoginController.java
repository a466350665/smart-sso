package com.smart.sso.server.controller;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.Result;
import com.smart.sso.client.dto.SsoUser;
import com.smart.sso.server.common.ServiceTicketManager;
import com.smart.sso.server.common.TicketGrantingTicketManager;
import com.smart.sso.server.constant.AppConstant;
import com.smart.sso.server.dto.UserDto;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.util.CookieUtils;

/**
 * 单点登录管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/login")
public class LoginController{

	@Autowired
	private ServiceTicketManager serviceTicketManager;
	@Autowired
	private TicketGrantingTicketManager ticketGrantingTicketManager;
	@Autowired
	private UserService userService;

	/**
	 * 登录页
	 * 
	 * @param service
	 * @param request
	 * @return
	 */
	@RequestMapping(method = RequestMethod.GET)
	public String login(@RequestParam String service, HttpServletRequest request) {
		String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
		if (StringUtils.isEmpty(tgt) || ticketGrantingTicketManager.exists(tgt) == null) {
			return goLoginPath(service, request);
		}
		return "redirect:" + authService(service, tgt);
	}

	@RequestMapping(method = RequestMethod.POST)
	public String login(@RequestParam String service, @RequestParam String account, @RequestParam String password,
			HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {
		Result<UserDto> result = userService.login(account, password);
		if (!result.isSuccess()) {
			request.setAttribute("errorMessage", result.getMessage());
			return goLoginPath(service, request);
		}
		else {
			String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
			if (StringUtils.isEmpty(tgt) || ticketGrantingTicketManager.exists(tgt) == null) {
				UserDto user = result.getData();
				tgt = ticketGrantingTicketManager.generate(new SsoUser(user.getId(), user.getAccount()));

				// TGT存cookie，和Cas登录保存cookie中名称一致为：TGC
				CookieUtils.addCookie(AppConstant.TGC, tgt, "/", request, response);
			}
			return "redirect:" + authService(service, tgt);
		}
	}

	/**
	 * 设置request的service参数，跳转到登录页
	 * 
	 * @param service
	 * @param request
	 * @return
	 */
	private String goLoginPath(String service, HttpServletRequest request) {
		request.setAttribute("service", service);
		return AppConstant.LOGIN_PATH;
	}

	/**
	 * 根据TGT生成ST，并拼接到回调service中
	 * 
	 * @param service
	 * @param tgt
	 * @return
	 */
	private String authService(String service, String tgt) {
		StringBuilder sbf = new StringBuilder(service);
		if (service.indexOf("?") > 0) {
			sbf.append("&");
		}
		else {
			sbf.append("?");
		}
		sbf.append(SsoConstant.TICKET_PARAMETER_NAME).append("=").append(generateSt(service, tgt));
		try {
			return URLDecoder.decode(sbf.toString(), "utf-8");
		}
		catch (UnsupportedEncodingException e) {
			return sbf.toString();
		}
	}

	/**
	 * 生成并签发ST
	 * 
	 * @param service
	 * @param tgt
	 * @return
	 */
	private String generateSt(String service, String tgt) {
		return ticketGrantingTicketManager.signSt(tgt, serviceTicketManager.generate(tgt), service);
	}
}