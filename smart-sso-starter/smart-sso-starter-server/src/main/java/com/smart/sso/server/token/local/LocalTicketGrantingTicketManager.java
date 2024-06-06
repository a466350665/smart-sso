package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.server.token.TicketGrantingTicketManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地登录凭证管理
 * 
 * @author Joe
 */
public class LocalTicketGrantingTicketManager implements TicketGrantingTicketManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());

    private int timeout;

	private Map<String, TgtWrapper> tgtMap = new ConcurrentHashMap<>();

	public LocalTicketGrantingTicketManager(int timeout) {
		this.timeout = timeout;
	}

	@Override
	public void create(String tgt, Userinfo user) {
		tgtMap.put(tgt, new TgtWrapper(user, System.currentTimeMillis() + getExpiresIn() * 1000));
		logger.info("登录凭证生成成功, tgt:{}", tgt);
	}

	@Override
	public Userinfo getAndRefresh(String tgt) {
		TgtWrapper wrapper = tgtMap.get(tgt);
		long currentTime = System.currentTimeMillis();
		if (wrapper == null || currentTime > wrapper.expired) {
			return null;
		}
		wrapper.expired = currentTime + getExpiresIn() * 1000;
		return wrapper.user;
	}
	
	@Override
	public void set(String tgt, Userinfo user) {
		TgtWrapper wrapper = tgtMap.get(tgt);
		if (wrapper == null) {
			return;
		}
		wrapper.user = user;
	}

	@Override
	public void remove(String tgt) {
		tgtMap.remove(tgt);
		logger.debug("登录凭证删除成功, tgt:{}", tgt);
	}

	@Override
	public void verifyExpired() {
		tgtMap.forEach((tgt, wrapper) -> {
			if (System.currentTimeMillis() > wrapper.expired) {
				tgtMap.remove(tgt);
				logger.debug("登录凭证已失效, tgt:{}", tgt);
			}
		});
	}

	@Override
	public int getExpiresIn() {
		return timeout;
	}
	
	private class TgtWrapper {
		private Userinfo user;
		private long expired;

		public TgtWrapper(Userinfo user, long expired) {
			super();
			this.user = user;
			this.expired = expired;
		}
	}
}
