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

import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcUser;
import com.smart.sso.server.constant.AppConstant;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.session.CodeManager;
import com.smart.sso.server.session.TicketGrantingTicketManager;
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
	private CodeManager codeManager;
	@Autowired
	private TicketGrantingTicketManager ticketGrantingTicketManager;
	@Autowired
	private UserService userService;
	@Autowired
	private AppService appService;

	/**
	 * 登录页
	 * 
	 * @param redirectUri
	 * @param appId
	 * @param request
	 * @return
	 */
	@RequestMapping(method = RequestMethod.GET)
	public String login(
			@RequestParam String redirectUri,
			@RequestParam String appId,
			HttpServletRequest request) throws UnsupportedEncodingException {
		String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
		if (StringUtils.isEmpty(tgt) || ticketGrantingTicketManager.exists(tgt) == null) {
			return goLoginPath(redirectUri, appId, request);
		}
		return generateCodeAndRedirect(redirectUri, tgt);
	}
	
	/**
	 * 登录提交
	 * 
	 * @param redirectUri
	 * @param appId
	 * @param account
	 * @param password
	 * @param request
	 * @param response
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String login(
			@RequestParam String redirectUri,
			@RequestParam String appId,
			@RequestParam String account, 
			@RequestParam String password,
			HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {

		if(!appService.exists(appId)) {
			request.setAttribute("errorMessage", "非法应用");
			return goLoginPath(redirectUri, appId, request);
		}
		
		Result<RpcUser> result = userService.login(account, password);
		if (!result.isSuccess()) {
			request.setAttribute("errorMessage", result.getMessage());
			return goLoginPath(redirectUri, appId, request);
		}

		String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
		if (StringUtils.isEmpty(tgt) || !ticketGrantingTicketManager.refresh(tgt)) {
			tgt = ticketGrantingTicketManager.generate(result.getData());

			// TGT存cookie，和Cas登录保存cookie中名称一致为：TGC
			CookieUtils.addCookie(AppConstant.TGC, tgt, "/", request, response);
		}
		return generateCodeAndRedirect(redirectUri, tgt);
	}

	/**
	 * 设置request的redirectUri和appId参数，跳转到登录页
	 * 
	 * @param redirectUri
	 * @param request
	 * @return
	 */
	private String goLoginPath(String redirectUri, String appId, HttpServletRequest request) {
		request.setAttribute("redirectUri", redirectUri);
		request.setAttribute("appId", appId);
		return AppConstant.LOGIN_PATH;
	}
	
	/**
	 * 生成授权码，跳转到redirectUri
	 * 
	 * @param redirectUri
	 * @param tgt
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	private String generateCodeAndRedirect(String redirectUri, String tgt) throws UnsupportedEncodingException {
		// 生成授权码
		String code = codeManager.generate(redirectUri, tgt);
		return "redirect:" + authRedirectUri(redirectUri, code);
	}

	/**
	 * 将授权码拼接到回调redirectUri中
	 * 
	 * @param redirectUri
	 * @param code
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	private String authRedirectUri(String redirectUri, String code) throws UnsupportedEncodingException {
		StringBuilder sbf = new StringBuilder(redirectUri);
		if (redirectUri.indexOf("?") > -1) {
			sbf.append("&");
		}
		else {
			sbf.append("?");
		}
		sbf.append(Oauth2Constant.AUTH_CODE).append("=").append(code);
		return URLDecoder.decode(sbf.toString(), "utf-8");
	}

}