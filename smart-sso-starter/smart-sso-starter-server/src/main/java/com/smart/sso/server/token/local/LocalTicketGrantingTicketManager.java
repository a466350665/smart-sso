package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ExpirationWrapper;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.server.token.TicketGrantingTicketManager;
import com.smart.sso.server.token.TokenManager;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地登录凭证管理
 *
 * @author Joe
 */
public class LocalTicketGrantingTicketManager extends TicketGrantingTicketManager implements ExpirationPolicy {

    private Map<String, ExpirationWrapper<Userinfo>> tgtMap = new ConcurrentHashMap<>();

    public LocalTicketGrantingTicketManager(TokenManager tokenManager, int timeout) {
        super(tokenManager, timeout);
    }

    @Override
    public void create(String tgt, Userinfo userinfo) {
        ExpirationWrapper<Userinfo> wrapper = new ExpirationWrapper<>(userinfo, getExpiresIn());
        tgtMap.put(tgt, wrapper);
        logger.debug("登录凭证创建成功, tgt:{}", tgt);
    }

    @Override
    public Userinfo get(String tgt) {
        ExpirationWrapper<Userinfo> wrapper = tgtMap.get(tgt);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        }
        return wrapper.getObject();
    }

    @Override
    public void remove(String tgt) {
        tgtMap.remove(tgt);
        logger.debug("登录凭证删除成功, tgt:{}", tgt);
    }

    @Override
    public void refresh(String tgt) {
        ExpirationWrapper<Userinfo> wrapper = tgtMap.get(tgt);
        if (wrapper != null) {
            wrapper.setExpired(System.currentTimeMillis() + getExpiresIn() * 1000);
        }
    }

    @Override
    public void verifyExpired() {
        tgtMap.forEach((tgt, wrapper) -> {
            if (wrapper.checkExpired()) {
                remove(tgt);
            }
        });
    }
}
