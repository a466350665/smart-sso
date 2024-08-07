package openjoe.smart.sso.server.manager.local;

import openjoe.smart.sso.base.entity.ExpirationPolicy;
import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 本地登录凭证管理
 *
 * @author Joe
 */
public class LocalTicketGrantingTicketManager extends AbstractTicketGrantingTicketManager implements ExpirationPolicy {

    protected final Logger logger = LoggerFactory.getLogger(LocalTicketGrantingTicketManager.class);
    private Map<String, ExpirationWrapper<TokenUser>> tgtMap = new ConcurrentHashMap<>();

    public LocalTicketGrantingTicketManager(int timeout, String cookieName, AbstractTokenManager tokenManager) {
        super(timeout, cookieName, tokenManager);
    }

    @Override
    public void create(String tgt, TokenUser tokenUser) {
        ExpirationWrapper<TokenUser> wrapper = new ExpirationWrapper<>(tokenUser, getTimeout());
        tgtMap.put(tgt, wrapper);
        logger.debug("登录凭证创建成功, tgt:{}", tgt);
    }

    @Override
    public TokenUser get(String tgt) {
        ExpirationWrapper<TokenUser> wrapper = tgtMap.get(tgt);
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
        ExpirationWrapper<TokenUser> wrapper = tgtMap.get(tgt);
        if (wrapper != null) {
            wrapper.setExpired(System.currentTimeMillis() + getTimeout() * 1000);
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
