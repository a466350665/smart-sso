package com.smart.sso.server.common;

import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;

import com.google.common.collect.Maps;

/**
 * 本地票据管理
 * 
 * @author Joe
 */
public class LocalServiceTicketManager extends ServiceTicketManager {

	// 票据存储结构
	private final Map<String, DummySt> ticketMap = Maps.newConcurrentMap();

	@Override
	public String generate(String tgt) {
	    String ticket = "ST-" + UUID.randomUUID().toString().replaceAll("-", "");
	    
	    DummySt dummySt = new DummySt();
		dummySt.tgt = tgt;
		dummySt.expired = System.currentTimeMillis() + timeout * 1000;
		ticketMap.put(ticket, dummySt);
		return ticket;
	}

	@Override
	public String validate(String ticket) {
	    DummySt dummySt = ticketMap.remove(ticket);
        if (dummySt == null || System.currentTimeMillis() > dummySt.expired) {
            return null;
        }
        return dummySt.tgt;
	}
	
	@Override
    public void remove(String ticket) {
        ticketMap.remove(ticket);
    }
	
	@Override
    public void verifyExpired() {
        for (Entry<String, DummySt> entry : ticketMap.entrySet()) {
            String ticket = entry.getKey();
            DummySt dummySt = entry.getValue();
            // 已过期
            if (System.currentTimeMillis() > dummySt.expired) {
                ticketMap.remove(ticket);
                logger.debug("ticket : " + ticket + "已失效");
            }
        }
    }

	private class DummySt {
		private String tgt;
		private long expired; // 过期时间
	}
}
