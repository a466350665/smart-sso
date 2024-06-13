package com.smart.sso.server.token;

import com.smart.sso.base.entity.Expiration;
import com.smart.sso.base.entity.LifecycleManager;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.CookieUtils;
import com.smart.sso.server.constant.ServerConstant;
import com.smart.sso.server.entity.TicketGrantingTicketContent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.UUID;

/**
 * 登录凭证（TGT）管理抽象
 * 
 * @author Joe
 */
public abstract class TicketGrantingTicketManager implements LifecycleManager<TicketGrantingTicketContent>, Expiration {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private TokenManager tokenManager;
	private int timeout;

	public TicketGrantingTicketManager(TokenManager tokenManager, int timeout) {
		this.tokenManager = tokenManager;
		this.timeout = timeout;
	}

	/**
     * 登录成功后，根据用户信息生成令牌
     * 
     * @param user
     * @return
     */
	String generate(Userinfo user) {
		String tgt = "TGT-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(tgt, new TicketGrantingTicketContent(user));
		return tgt;
	}

	public String getOrGenerateTgt(Userinfo userinfo, HttpServletRequest request, HttpServletResponse response) {
		String tgt = getCookieTgt(request);
		// cookie中没有
		if (StringUtils.isEmpty(tgt)) {
			tgt = generate(userinfo);

			// TGT存cookie，和Cas登录保存cookie中名称一致为：TGC
			CookieUtils.addCookie(ServerConstant.TGC, tgt, "/", request, response);
		}
		else {
			create(tgt, new TicketGrantingTicketContent(userinfo));
		}
		return tgt;
	}

	public void invalidate(HttpServletRequest request, HttpServletResponse response) {
		String tgt = getCookieTgt(request);
		if (StringUtils.isEmpty(tgt)) {
			return;
		}
		// 删除登录凭证
		removeTgtAndToken(tgt);
		// 删除凭证Cookie
		CookieUtils.removeCookie(ServerConstant.TGC, "/", response);
	}

	public void removeTgtAndToken(String tgt) {
		// 删除登录凭证
		remove(tgt);
		// 删除所有Token
		tokenManager.removeByTgt(tgt);
	}

	public String getTgt(HttpServletRequest request) {
		String tgt = getCookieTgt(request);
		if (StringUtils.isEmpty(tgt) || get(tgt) == null) {
			return null;
		}
		else {
			return tgt;
		}
	}

	private String getCookieTgt(HttpServletRequest request) {
		return CookieUtils.getCookieValue(ServerConstant.TGC, request);
	}

	@Override
	public int getExpiresIn() {
		return 2 * timeout;
	}

    /**
     * 更新过期时间戳
     *
     * @param tgt
     * @return
     */
	public abstract void refresh(String tgt);
}
