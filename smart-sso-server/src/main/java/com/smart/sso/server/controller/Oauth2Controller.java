package com.smart.sso.server.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcAccessToken;
import com.smart.sso.server.common.CodeContent;
import com.smart.sso.server.common.RefreshTokenContent;
import com.smart.sso.server.service.AppService;
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
	private CodeManager codeManager;
	@Autowired
	private AccessTokenManager accessTokenManager;
	@Autowired
	private RefreshTokenManager refreshTokenManager;
	@Autowired
	private TicketGrantingTicketManager ticketGrantingTicketManager;

	/**
	 * 获取accessToken
	 * 
	 * @param appId
	 * @param appSecret
	 * @param code
	 * @return
	 */
	@RequestMapping(value = "/access_token", method = RequestMethod.GET)
	public Result accessToken(
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

		if (!ticketGrantingTicketManager.refresh(codeContent.getTgt())) {
			return Result.createError("服务端session已过期");
		}

		String accessToken = accessTokenManager.generate(codeContent.getService(), codeContent.getTgt());

		String refreshToken = refreshTokenManager.generate(accessToken, appId, codeContent.getService(), codeContent.getTgt());
		
		return Result.createSuccess(new RpcAccessToken(accessToken, accessTokenManager.getExpiresIn(), refreshToken));
	}

	/**
	 * 刷新accessToken，并延长TGT周期
	 * 
	 * accessToken刷新结果有两种：
	 * 1. 若accessToken已超时，那么进行refreshToken会获取一个新的accessToken，新的超时时间；
	 * 2. 若accessToken未超时，那么进行refreshToken不会改变accessToken，但超时时间会刷新，相当于续期accessToken。
	 * @param service
	 * @param request
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

		if (!ticketGrantingTicketManager.refresh(refreshTokenContent.getTgt())) {
			return Result.createError("服务端session已过期");
		}

		String accessToken = refreshTokenContent.getAccessToken();
		if (!accessTokenManager.refresh(accessToken)) {
			accessToken = accessTokenManager.generate(refreshTokenContent.getService(), refreshTokenContent.getTgt());
		}
		
		String newRefreshToken = refreshTokenManager.generate(accessToken, appId, refreshTokenContent.getService(),
				refreshTokenContent.getTgt());
		return Result.createSuccess(new RpcAccessToken(accessToken, accessTokenManager.getExpiresIn(), newRefreshToken));
	}
}