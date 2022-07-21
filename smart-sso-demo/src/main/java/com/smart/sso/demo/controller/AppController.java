package com.smart.sso.demo.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcAccessToken;
import com.smart.sso.client.rpc.SsoUser;
import com.smart.sso.client.util.Oauth2Utils;
import com.smart.sso.client.util.SessionUtils;

/**
 * @author Joe
 *
 */
@SuppressWarnings("rawtypes")
@RestController
@RequestMapping("/app")
public class AppController {
	
	@Value("${sso.server.url}")
    private String serverUrl;
    @Value("${sso.app.id}")
    private String appId;
    @Value("${sso.app.secret}")
    private String appSecret;

	/**
	 * 初始页
	 * @param request
//	 * @param model
	 * @return
	 */
    @RequestMapping
	public Result index(HttpServletRequest request) {
		SsoUser user = SessionUtils.getUser(request);
		return Result.createSuccess(user);
	}
	
	/**
	 * 登录提交
	 * 
	 * @param username
	 * @param password
	 * @param request
	 * @return
	 */
	@RequestMapping("/login")
	public Result login(
			@RequestParam(value = Oauth2Constant.USERNAME, required = true) String username,
			@RequestParam(value = Oauth2Constant.PASSWORD, required = true) String password,
			HttpServletRequest request) {
		//发起access_token请求
		Result<RpcAccessToken> result = Oauth2Utils.getAccessToken(serverUrl, appId, appSecret, username, password);
		if (!result.isSuccess()) {
			return result;
		}
		// TODO: 2022/7/21 将得到access_token放到本地的session
		SessionUtils.setAccessToken(request, result.getData());
		return Result.createSuccess().setMessage("登录成功");
	}
}
