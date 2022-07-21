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
import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.SsoUser;
import com.smart.sso.server.constant.AppConstant;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.session.CodeManager;
import com.smart.sso.server.session.SessionManager;

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
	private SessionManager sessionManager;
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
			@RequestParam(value = SsoConstant.REDIRECT_URI, required = true) String redirectUri,
			@RequestParam(value = Oauth2Constant.APP_ID, required = true) String appId,
			HttpServletRequest request) throws UnsupportedEncodingException {
		// TODO: 2022/7/21 其他服务转发发起的登录请求, appid,redirectUri,request(用于检查tgt)
		String tgt = sessionManager.getTgt(request);
		if (StringUtils.isEmpty(tgt)) {
			// TODO: 2022/7/21 没有tgt就开始登录操作了 appid,redirectUri,request
			return goLoginPath(redirectUri, appId, request);
		}
		// TODO: 2022/7/21 有tgt的话就开始授权码登录了 
		return generateCodeAndRedirect(redirectUri, tgt);
	}
	
	/**
	 * 登录提交
	 * 
	 * @param redirectUri
	 * @param appId
	 * @param username
	 * @param password
	 * @param request
	 * @param response
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	@RequestMapping(method = RequestMethod.POST)
	public String login(
			@RequestParam(value = SsoConstant.REDIRECT_URI, required = true) String redirectUri,
			@RequestParam(value = Oauth2Constant.APP_ID, required = true) String appId,
			@RequestParam String username, 
			@RequestParam String password,
			HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {

		// TODO: 2022/7/21 主要是login.html会提交表单,post请求.  redirectUri,appId,username,password
		if(!appService.exists(appId)) {//检查appid是不是正确的
			request.setAttribute("errorMessage", "非法应用");
			return goLoginPath(redirectUri, appId, request);//回显到前端报错一下.
		}
		// TODO: 2022/7/21 模拟一个查数据库用户密码操作,判断登录成功否 
		Result<SsoUser> result = userService.login(username, password);
		if (!result.isSuccess()) {
			request.setAttribute("errorMessage", result.getMessage());
			return goLoginPath(redirectUri, appId, request);
		}

		// TODO: 2022/7/21  登录成功后,创建tgt将其写到客户端的cookie中
		String tgt = sessionManager.setUser(result.getData(), request, response);
		// TODO: 2022/7/21 然后就走生成授权码的那个步骤了.算是登录告捷!
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
		// TODO: 2022/7/21 往request中放一些信息. 
		request.setAttribute(SsoConstant.REDIRECT_URI, redirectUri);
		request.setAttribute(Oauth2Constant.APP_ID, appId);
		// TODO: 2022/7/21 进入登录界面 
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
		// TODO: 2022/7/21 如果cookie中有tgt就生成授权码
		String code = codeManager.generate(tgt, true, redirectUri);
		return "redirect:" + authRedirectUri(redirectUri, code);
		// TODO: 2022/7/21 转发到http://ip:port?code=****;就是回调了
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