package com.smart.sso.server.session.local;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.smart.sso.client.rpc.RpcUser;
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

	private final Map<String, DummyTgt> tgtMap = new ConcurrentHashMap<>();

	@Override
	public void create(String tgt, RpcUser user) {
		tgtMap.put(tgt, createDummyTgt(user));
	}

	private DummyTgt createDummyTgt(RpcUser user) {
		DummyTgt dummyTgt = new DummyTgt();
		dummyTgt.expired = System.currentTimeMillis() + getExpiresIn() * 1000;
		dummyTgt.user = user;
		return dummyTgt;
	}

	@Override
	public RpcUser exists(String tgt) {
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
	public boolean refresh(String tgt) {
		DummyTgt dummyTgt = tgtMap.get(tgt);
		if (dummyTgt == null) {
			return false;
		}
		dummyTgt.expired = System.currentTimeMillis() + getExpiresIn() * 1000;
		return true;
	}

	@Scheduled(cron = "0 */1 * * * ?")
	@Override
	public void verifyExpired() {
		for (Entry<String, DummyTgt> entry : tgtMap.entrySet()) {
			String tgt = entry.getKey();
			DummyTgt dummyTgt = entry.getValue();
			// 已过期
			if (System.currentTimeMillis() > dummyTgt.expired) {
				tgtMap.remove(tgt);
				logger.debug("TGT : " + tgt + "已失效");
			}
		}
	}

	@Override
	public int getExpiresIn() {
		return timeout;
	}
	
	private class DummyTgt {
		private RpcUser user;
		private long expired; // 过期时间
	}
}
