package com.smart.sso.server.token.local;

import com.smart.sso.base.entity.ExpirationPolicy;
import com.smart.sso.base.entity.ObjectWrapper;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.server.token.TicketGrantingTicketManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地登录凭证管理
 *
 * @author Joe
 */
public class LocalTicketGrantingTicketManager implements TicketGrantingTicketManager, ExpirationPolicy {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private int timeout;

    private Map<String, ObjectWrapper<Userinfo>> tgtMap = new ConcurrentHashMap<>();

    public LocalTicketGrantingTicketManager(int timeout) {
        this.timeout = timeout;
    }

    @Override
    public void create(String tgt, Userinfo userinfo) {
        ObjectWrapper<Userinfo> wrapper = new ObjectWrapper<>(userinfo, System.currentTimeMillis() + getExpiresIn() * 1000);
        tgtMap.put(tgt, wrapper);
        logger.info("登录凭证生成成功, tgt:{}", tgt);
    }

    @Override
    public Userinfo getAndRefresh(String tgt) {
        ObjectWrapper<Userinfo> wrapper = tgtMap.get(tgt);
        long currentTime = System.currentTimeMillis();
        if (wrapper == null || currentTime > wrapper.getExpired()) {
            return null;
        }
        wrapper.setExpired(currentTime + getExpiresIn() * 1000);
        return wrapper.getObject();
    }

    @Override
    public void set(String tgt, Userinfo userinfo) {
        ObjectWrapper<Userinfo> wrapper = tgtMap.get(tgt);
        if (wrapper == null) {
            return;
        }
        wrapper.setObject(userinfo);
    }

    @Override
    public void remove(String tgt) {
        tgtMap.remove(tgt);
        logger.debug("登录凭证删除成功, tgt:{}", tgt);
    }

    @Override
    public void verifyExpired() {
        tgtMap.forEach((tgt, wrapper) -> {
            if (System.currentTimeMillis() > wrapper.getExpired()) {
                tgtMap.remove(tgt);
                logger.debug("登录凭证已失效, tgt:{}", tgt);
            }
        });
    }

    @Override
    public int getExpiresIn() {
        return timeout;
    }
}
