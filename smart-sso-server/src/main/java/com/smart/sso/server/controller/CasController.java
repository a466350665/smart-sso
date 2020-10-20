package com.smart.sso.server.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.smart.sso.client.dto.AccessToken;
import com.smart.sso.client.dto.Result;
import com.smart.sso.client.dto.SsoUser;
import com.smart.sso.server.common.RefreshTokenManager;
import com.smart.sso.server.common.ServiceTicketManager;
import com.smart.sso.server.common.TicketGrantingTicketManager;

/**
 * Cas服务管理
 * 
 * @author Joe
 */
@SuppressWarnings("rawtypes")
@RestController
@RequestMapping("/cas")
public class CasController {

	@Autowired
	private ServiceTicketManager serviceTicketManager;
	@Autowired
	private RefreshTokenManager refreshTokenManager;
	@Autowired
	private TicketGrantingTicketManager ticketGrantingTicketManager;

	/**
	 * 验证ST有效性，一个ST只提供单次验证，无论是否存在都失效ST
	 * 
	 * @param ticket
	 * @return
	 */
	@RequestMapping(value = "/validate", method = RequestMethod.GET)
	public Result validate(@RequestParam String ticket) {
		String tgt = serviceTicketManager.validate(ticket);
		if (StringUtils.isEmpty(tgt)) {
			return Result.createError("ticket有误或已过期");
		}
		SsoUser user = ticketGrantingTicketManager.exists(tgt);
		if (user == null) {
			return Result.createError("TGT有误或已过期");
		}
		else {
			// 每个AccessToken的时效为服务端TGT时效的1/2
			return Result.createSuccess(new AccessToken(ticketGrantingTicketManager.getTimeout() / 2,
					refreshTokenManager.generate(tgt), user));
		}
	}

	/**
	 * 根据refreshToken延长TGT周期，并生成新的refreshToken返回，用于下次refresh
	 * 
	 * @param service
	 * @param request
	 * @return
	 */
	@RequestMapping(value = "/refresh", method = RequestMethod.GET)
	public Result refresh(@RequestParam String refreshToken) {
		String tgt = refreshTokenManager.validate(refreshToken);
		if (tgt == null) {
			return Result.createError("refreshToken有误或已过期");
		}
		if (!ticketGrantingTicketManager.refresh(tgt)) {
			return Result.createError("TGT有误或已过期");
		}
		return Result.createSuccess(refreshTokenManager.generate(tgt));
	}
}