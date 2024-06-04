package com.smart.sso.demo.controller;

import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.rpc.ClientAccessToken;
import com.smart.sso.client.rpc.ClientUser;
import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.util.Oauth2Utils;
import com.smart.sso.client.util.SessionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

/**
 * @author Joe
 *
 */
@SuppressWarnings("rawtypes")
@RestController
@RequestMapping("/app")
public class AppController {

	@Autowired
	private ClientProperties smartSsoProperties;

	/**
	 * 初始页
	 * @param request
	 * @return
	 */
    @RequestMapping
	public Result index(HttpServletRequest request) {
		ClientUser user = SessionUtils.getUser(request);
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
		Result<ClientAccessToken> result = Oauth2Utils.getAccessToken(smartSsoProperties.getServerUrl(),
				smartSsoProperties.getAppId(), smartSsoProperties.getAppSecret(), username, password);
		if (!result.isSuccess()) {
			return result;
		}
		SessionUtils.setAccessToken(request, result.getData());
		return Result.createSuccess().setMessage("登录成功");
	}
}
