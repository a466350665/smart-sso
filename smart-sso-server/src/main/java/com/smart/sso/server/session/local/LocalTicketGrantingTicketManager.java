package com.smart.sso.server.session.local;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.smart.sso.client.rpc.SsoUser;
import com.smart.sso.server.common.ExpirationPolicy;
import com.smart.sso.server.session.TicketGrantingTicketManager;

/**
 * 本地登录凭证管理
 * 
 * @author Joe
 */
@Component
@ConditionalOnProperty(name = "sso.session.manager", havingValue = "local")
public class LocalTicketGrantingTicketManager implements TicketGrantingTicketManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());
	
	@Value("${sso.timeout}")
    private int timeout;

	private Map<String, DummyTgt> tgtMap = new ConcurrentHashMap<>();

	@Override
	public void create(String tgt, SsoUser user) {
		tgtMap.put(tgt, new DummyTgt(user, System.currentTimeMillis() + getExpiresIn() * 1000));
	}

	@Override
	public SsoUser get(String tgt) {
		DummyTgt dummyTgt = tgtMap.get(tgt);
		if (dummyTgt == null || System.currentTimeMillis() > dummyTgt.expired) {
			return null;
		}
		return dummyTgt.user;
	}

	@Override
	public void remove(String tgt) {
		tgtMap.remove(tgt);
	}

	@Override
	public SsoUser refresh(String tgt) {
		DummyTgt dummyTgt = tgtMap.get(tgt);
		if (dummyTgt == null) {
			return null;
		}
		dummyTgt.expired = System.currentTimeMillis() + getExpiresIn() * 1000;
		return dummyTgt.user;
	}

	@Scheduled(cron = SCHEDULED_CRON)
	@Override
	public void verifyExpired() {
		tgtMap.forEach((tgt, dummyTgt) -> {
			if (System.currentTimeMillis() > dummyTgt.expired) {
				tgtMap.remove(tgt);
				logger.debug("TGT : " + tgt + "已失效");
			}
		});
	}

	@Override
	public int getExpiresIn() {
		return timeout;
	}
	
	private class DummyTgt {
		private SsoUser user;
		private long expired;

		public DummyTgt(SsoUser user, long expired) {
			super();
			this.user = user;
			this.expired = expired;
		}
	}
}
