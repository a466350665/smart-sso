package com.smart.sso.server.common;

import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地ST管理
 * 
 * @author Joe
 */
public class LocalServiceTicketManager extends ServiceTicketManager {

	// ST存储结构
	private final Map<String, DummySt> stMap = new ConcurrentHashMap<>();
	
	public LocalServiceTicketManager() {
	    // 默认ST失效为10秒
        super(10);
    }
	
	public LocalServiceTicketManager(int timeout) {
        super(timeout);
    }

	@Override
	public String generate(String tgt) {
	    String st = "ST-" + UUID.randomUUID().toString().replaceAll("-", "");
	    DummySt dummySt = new DummySt(tgt, System.currentTimeMillis() + timeout * 1000);
		stMap.put(st, dummySt);
		return st;
	}

	@Override
	public String validate(String st) {
	    DummySt dummySt = stMap.remove(st);
        if (dummySt == null || System.currentTimeMillis() > dummySt.expired) {
            return null;
        }
        return dummySt.tgt;
	}
	
	@Override
    public void remove(String st) {
        stMap.remove(st);
    }
	
	@Override
    public void verifyExpired() {
        for (Entry<String, DummySt> entry : stMap.entrySet()) {
            String st = entry.getKey();
            DummySt dummySt = entry.getValue();
            // 已过期
            if (System.currentTimeMillis() > dummySt.expired) {
                stMap.remove(st);
                logger.debug("ticket : " + st + "已失效");
            }
        }
    }

    private class DummySt {
        private String tgt;
        private long expired; // 过期时间

        public DummySt(String tgt, long expired) {
            super();
            this.tgt = tgt;
            this.expired = expired;
        }
    }
}
