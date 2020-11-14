package com.smart.sso.demo.controller;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

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
	 * @param model
	 * @return
	 * @throws UnsupportedEncodingException 
	 */
    @RequestMapping
	public Result index(HttpServletRequest request) throws UnsupportedEncodingException {
		SsoUser user = SessionUtils.getUser(request);
		return Result.createSuccess(user);
	}
	
	/**
	 * 登录提交
	 * 
	 * @param account
	 * @param password
	 * @param request
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	@RequestMapping("/login")
	public Result login(
			@RequestParam String account, 
			@RequestParam String password,
			HttpServletRequest request) throws UnsupportedEncodingException {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put("appId", appId);
		paramMap.put("account", account);
		paramMap.put("password", password);
		RpcAccessToken rpcAccessToken = Oauth2Utils.getAccessToken(serverUrl, appId, appSecret, paramMap);
		if (rpcAccessToken == null) {
			return Result.createError("登录失败");
		}
		SessionUtils.setAccessToken(request, rpcAccessToken);
		return Result.createSuccess().setMessage("登录成功");
	}
}
