package com.smart.sso.server.session.local;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;

import com.smart.sso.client.rpc.RpcUser;
import com.smart.sso.server.common.ExpirationPolicy;
import com.smart.sso.server.common.TimeoutParamter;
import com.smart.sso.server.session.TicketGrantingTicketManager;

/**
 * 本地TGT管理
 * 
 * @author Joe
 */
public class LocalTicketGrantingTicketManager extends TimeoutParamter
		implements TicketGrantingTicketManager, ExpirationPolicy {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	private final Map<String, DummyTgt> tgtMap = new ConcurrentHashMap<>();

	public LocalTicketGrantingTicketManager(int timeout) {
		this.timeout = timeout;
	}

	@Override
	public void generate(String tgt, RpcUser user) {
		tgtMap.put(tgt, createDummyTgt(user));
	}

	private DummyTgt createDummyTgt(RpcUser user) {
		DummyTgt dummyTgt = new DummyTgt();
		dummyTgt.expired = System.currentTimeMillis() + timeout * 1000;
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
		dummyTgt.expired = System.currentTimeMillis() + timeout * 1000;
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

	private class DummyTgt {
		private RpcUser user;
		private long expired; // 过期时间
	}
}
