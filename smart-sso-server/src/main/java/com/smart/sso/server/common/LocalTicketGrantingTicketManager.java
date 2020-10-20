package com.smart.sso.server.common;

import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.SsoUser;
import com.smart.sso.client.util.HttpRequestUtils;

/**
 * 本地TGT管理
 * 
 * @author Joe
 */
public class LocalTicketGrantingTicketManager extends TicketGrantingTicketManager {

    private final Map<String, DummyTgt> tgtMap = new ConcurrentHashMap<>();
    
    public LocalTicketGrantingTicketManager(int timeout) {
        super(timeout);
    }
    
    @Override
    public String generate(SsoUser user) {
        String tgt = "TGT-" + UUID.randomUUID().toString().replaceAll("-", "");
        tgtMap.put(tgt, createDummyTgt(user));
        return tgt;
    }
    
    private DummyTgt createDummyTgt(SsoUser user) {
        DummyTgt dummyTgt = new DummyTgt();
        dummyTgt.expired = System.currentTimeMillis() + timeout * 1000;
        dummyTgt.stMap = new ConcurrentHashMap<>();
        dummyTgt.user = user;
        return dummyTgt;
    }
    
    @Override
    public SsoUser exists(String tgt) {
        DummyTgt dummyTgt = tgtMap.get(tgt);
        if (dummyTgt == null || System.currentTimeMillis() > dummyTgt.expired) {
            return null;
        }
        return dummyTgt.user;
    }
    
    @Override
    public void remove(String tgt) {
        DummyTgt dummyTgt = tgtMap.get(tgt);
        if (dummyTgt == null) {
            return;
        }
        Map<String, String> stMap = dummyTgt.stMap;
        for (Entry<String, String> entry : stMap.entrySet()) {
            HttpRequestUtils.get(entry.getValue() + "?" + SsoConstant.LOGOUT_PARAMETER_NAME + "=" + entry.getKey());
        }
        tgtMap.remove(tgt);
    }
    
    @Override
    public String signSt(String tgt, String st, String service) {
        DummyTgt dummyTgt = tgtMap.get(tgt);
        if (dummyTgt == null) {
            return null;
        }
        dummyTgt.stMap.put(st, service);
        return st;
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
        private SsoUser user;
        private Map<String, String> stMap;
        private long expired; // 过期时间
    }
}
