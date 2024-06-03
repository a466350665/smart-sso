package com.smart.sso.server.session.local;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
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
public class LocalTicketGrantingTicketManager implements TicketGrantingTicketManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());
	
	@Value("${sso.timeout}")
    private int timeout;

	private Map<String, DummyTgt> tgtMap = new ConcurrentHashMap<>();

	@Override
	public void create(String tgt, SsoUser user) {
		tgtMap.put(tgt, new DummyTgt(user, System.currentTimeMillis() + getExpiresIn() * 1000));
		logger.info("登录凭证生成成功, tgt:{}", tgt);
	}

	@Override
	public SsoUser getAndRefresh(String tgt) {
		DummyTgt dummyTgt = tgtMap.get(tgt);
		long currentTime = System.currentTimeMillis();
		if (dummyTgt == null || currentTime > dummyTgt.expired) {
			return null;
		}
		dummyTgt.expired = currentTime + getExpiresIn() * 1000;
		return dummyTgt.user;
	}
	
	@Override
	public void set(String tgt, SsoUser user) {
		DummyTgt dummyTgt = tgtMap.get(tgt);
		if (dummyTgt == null) {
			return;
		}
		dummyTgt.user = user;
	}

	@Override
	public void remove(String tgt) {
		tgtMap.remove(tgt);
		logger.debug("登录凭证删除成功, tgt:{}", tgt);
	}

	@Scheduled(cron = SCHEDULED_CRON)
	@Override
	public void verifyExpired() {
		tgtMap.forEach((tgt, dummyTgt) -> {
			if (System.currentTimeMillis() > dummyTgt.expired) {
				tgtMap.remove(tgt);
				logger.debug("登录凭证已失效, tgt:{}", tgt);
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
