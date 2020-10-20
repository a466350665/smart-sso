package com.smart.sso.server.common;

import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地RT（refreshToken）管理
 * 
 * @author Joe
 */
public class LocalRefreshTokenManager extends RefreshTokenManager {

	private final Map<String, DummyRt> rtMap = new ConcurrentHashMap<>();
	
    public LocalRefreshTokenManager(int timeout) {
        super(timeout);
    }

    @Override
	public String generate(String tgt) {
	    String resfreshToken = "RT-" + UUID.randomUUID().toString().replaceAll("-", "");
	    DummyRt dummyRt = new DummyRt(tgt, System.currentTimeMillis() + timeout * 1000);
		rtMap.put(resfreshToken, dummyRt);
		return resfreshToken;
	}
	
	@Override
    public String validate(String rt) {
        DummyRt dummyRt = rtMap.remove(rt);
        if (dummyRt == null || System.currentTimeMillis() > dummyRt.expired) {
            return null;
        }
        return dummyRt.tgt;
    }
	
	@Override
    public void verifyExpired() {
        for (Entry<String, DummyRt> entry : rtMap.entrySet()) {
            String resfreshToken = entry.getKey();
            DummyRt dummyRt = entry.getValue();
            // 已过期
            if (System.currentTimeMillis() > dummyRt.expired) {
                rtMap.remove(resfreshToken);
                logger.debug("resfreshToken : " + resfreshToken + "已失效");
            }
        }
    }
	
    private class DummyRt {
        private String tgt;
        private long expired; // 过期时间

        public DummyRt(String tgt, long expired) {
            super();
            this.tgt = tgt;
            this.expired = expired;
        }
    }
}
