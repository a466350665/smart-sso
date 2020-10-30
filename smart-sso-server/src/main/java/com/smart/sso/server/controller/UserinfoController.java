package com.smart.sso.server.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcUser;
import com.smart.sso.server.common.AccessTokenContent;
import com.smart.sso.server.session.AccessTokenManager;
import com.smart.sso.server.session.TicketGrantingTicketManager;

/**
 * 用户信息接口
 * 
 * @author Joe
 */
@SuppressWarnings("rawtypes")
@RestController
@RequestMapping("/userinfo")
public class UserinfoController {

	@Autowired
	private AccessTokenManager accessTokenManager;
	@Autowired
	private TicketGrantingTicketManager ticketGrantingTicketManager;

	/**
	 * @param accessToken
	 * @return
	 */
	@RequestMapping(method = RequestMethod.GET)
	public Result userinfo(@RequestParam String accessToken) {

		AccessTokenContent accessTokenContent = accessTokenManager.validate(accessToken);
		if (accessTokenContent == null) {
			return Result.createError("accessToken有误或已过期");
		}

		RpcUser user = ticketGrantingTicketManager.exists(accessTokenContent.getTgt());
		if (user == null) {
			return Result.createError("服务端session已过期");
		}

		return Result.createSuccess(user);
	}
}