package com.smart.sso.server.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcAccessToken;
import com.smart.sso.client.rpc.SsoUser;
import com.smart.sso.server.common.CodeContent;
import com.smart.sso.server.common.RefreshTokenContent;
import com.smart.sso.server.enums.ClientTypeEnum;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.session.AccessTokenManager;
import com.smart.sso.server.session.CodeManager;
import com.smart.sso.server.session.RefreshTokenManager;
import com.smart.sso.server.session.TicketGrantingTicketManager;

/**
 * Cas服务管理
 * 
 * @author Joe
 */
@SuppressWarnings("rawtypes")
@RestController
@RequestMapping("/oauth2")
public class Oauth2Controller {
	
	@Autowired
	private AppService appService;
	@Autowired
	private UserService userService;

	@Autowired
	private CodeManager codeManager;
	@Autowired
	private AccessTokenManager accessTokenManager;
	@Autowired
	private RefreshTokenManager refreshTokenManager;
	@Autowired
	private TicketGrantingTicketManager ticketGrantingTicketManager;
	
	/**
	 * 获取授权码（app通过此方式由客户端代理转发http请求到服务端获取授权码）
	 * 
	 * @param appId
	 * @param account
	 * @param password
	 * @return
	 */
	@RequestMapping(value = "/authorize")
	public Result getCode(
			@RequestParam String appId,
			@RequestParam String account, 
			@RequestParam String password) {

		if(!appService.exists(appId)) {
			return Result.createError("非法应用");
		}
		
		Result<SsoUser> result = userService.login(account, password);
		if (!result.isSuccess()) {
			return Result.createError(result.getMessage());
		}
		String tgt = ticketGrantingTicketManager.generate(result.getData());
		return Result.createSuccess(codeManager.generate(tgt, ClientTypeEnum.APP, null));
	}

	/**
	 * 获取accessToken
	 * 
	 * @param appId
	 * @param appSecret
	 * @param code
	 * @return
	 */
	@RequestMapping(value = "/access_token", method = RequestMethod.GET)
	public Result getAccessToken(
			@RequestParam String appId,
			@RequestParam String appSecret,
			@RequestParam String code) {

		Result<Void> result = appService.validate(appId, appSecret);
		if (!result.isSuccess()) {
			return result;
		}

		CodeContent codeContent = codeManager.validate(code);
		if (codeContent == null) {
			return Result.createError("code有误或已过期");
		}

		SsoUser user = ticketGrantingTicketManager.refresh(codeContent.getTgt());
		if (user == null) {
			return Result.createError("服务端session已过期");
		}

		
		String accessToken = accessTokenManager.generate(codeContent.getTgt(), codeContent.getClientType(),
				codeContent.getRedirectUri());

		String refreshToken = refreshTokenManager.generate(codeContent.getTgt(), codeContent.getClientType(),
				codeContent.getRedirectUri(), accessToken, appId);
		
		return Result.createSuccess(new RpcAccessToken(accessToken, accessTokenManager.getExpiresIn(), refreshToken, user));
	}

	/**
	 * 刷新accessToken，并延长TGT周期
	 * 
	 * accessToken刷新结果有两种：
	 * 1. 若accessToken已超时，那么进行refreshToken会获取一个新的accessToken，新的超时时间；
	 * 2. 若accessToken未超时，那么进行refreshToken不会改变accessToken，但超时时间会刷新，相当于续期accessToken。
	 * 
	 * @param appId
	 * @param refreshToken
	 * @return
	 */
	@RequestMapping(value = "/refresh_token", method = RequestMethod.GET)
	public Result refreshToken(
			@RequestParam String appId,
			@RequestParam String refreshToken) {
		if(!appService.exists(appId)) {
			return Result.createError("非法应用");
		}
		
		RefreshTokenContent refreshTokenContent = refreshTokenManager.validate(refreshToken);
		if (refreshTokenContent == null || !appId.equals(refreshTokenContent.getAppId())) {
			return Result.createError("refreshToken有误或已过期");
		}

		SsoUser user = ticketGrantingTicketManager.refresh(refreshTokenContent.getTgt());
		if (user == null) {
			return Result.createError("服务端session已过期");
		}

		String accessToken = refreshTokenContent.getAccessToken();
		if (!accessTokenManager.refresh(accessToken)) {
			accessToken = accessTokenManager.generate(refreshTokenContent.getTgt(), refreshTokenContent.getClientType(),
					refreshTokenContent.getRedirectUri());
		}

		String newRefreshToken = refreshTokenManager.generate(refreshTokenContent.getTgt(),
				refreshTokenContent.getClientType(), refreshTokenContent.getRedirectUri(), accessToken, appId);
		return Result.createSuccess(new RpcAccessToken(accessToken, accessTokenManager.getExpiresIn(), newRefreshToken, user));
	}
}