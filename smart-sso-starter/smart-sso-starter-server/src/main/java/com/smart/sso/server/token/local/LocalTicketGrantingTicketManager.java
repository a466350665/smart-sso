package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ObjectWrapper;
import com.smart.sso.server.entity.TicketGrantingTicketContent;
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

    private Map<String, ObjectWrapper<TicketGrantingTicketContent>> tgtMap = new ConcurrentHashMap<>();

    public LocalTicketGrantingTicketManager(TokenManager tokenManager, int timeout) {
        super(tokenManager, timeout);
    }

    @Override
    public void create(String tgt, TicketGrantingTicketContent tgtContent) {
        ObjectWrapper<TicketGrantingTicketContent> wrapper = new ObjectWrapper<>(tgtContent, getExpiresIn());
        tgtMap.put(tgt, wrapper);
        logger.info("登录凭证生成成功, tgt:{}", tgt);
    }

    @Override
    public TicketGrantingTicketContent get(String tgt) {
        ObjectWrapper<TicketGrantingTicketContent> wrapper = tgtMap.get(tgt);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        }
        return wrapper.getObject();
    }

    @Override
    public void remove(String tgt) {
        tgtMap.remove(tgt);
        logger.info("登录凭证删除成功, tgt:{}", tgt);
    }

    @Override
    public void refresh(String tgt) {
        ObjectWrapper<TicketGrantingTicketContent> wrapper = tgtMap.get(tgt);
        if(wrapper != null){
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
