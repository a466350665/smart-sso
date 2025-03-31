package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.LifecycleManager;
import openjoe.smart.sso.base.util.CookieUtils;
import openjoe.smart.sso.server.entity.TicketGrantingTicketContent;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * 登录凭证（TGT）管理抽象
 *
 * @author Joe
 */
public abstract class AbstractTicketGrantingTicketManager implements LifecycleManager<TicketGrantingTicketContent> {

    private AbstractTokenManager tokenManager;
    private int timeout;
    private String cookieName;

    public AbstractTicketGrantingTicketManager(int timeout, String cookieName, AbstractTokenManager tokenManager) {
        this.timeout = timeout;
        this.cookieName = cookieName;
        this.tokenManager = tokenManager;
    }

    /**
     * 登录成功后，根据用户信息创建令牌
     *
     * @param userId
     * @return
     */
    String create(String tgt, Long userId) {
        create(tgt, new TicketGrantingTicketContent(userId, System.currentTimeMillis()));
        return tgt;
    }

    public String getOrCreate(Long userId, HttpServletRequest request, HttpServletResponse response) {
        String tgt = getCookieTgt(request);
        // cookie中没有
        if (!StringUtils.hasLength(tgt)) {
            tgt = create("TGT-" + UUID.randomUUID().toString().replaceAll("-", ""), userId);

            // TGT存cookie
            CookieUtils.addCookie(cookieName, tgt, "/", request, response);
        } else {
            create(tgt, userId);
        }
        return tgt;
    }

    public void invalidate(HttpServletRequest request, HttpServletResponse response) {
        String tgt = getCookieTgt(request);
        if (!StringUtils.hasLength(tgt)) {
            return;
        }
        invalidate(tgt);
        // 删除凭证Cookie
        CookieUtils.removeCookie(cookieName, "/", response);
    }

    public void invalidate(String tgt) {
        // 删除登录凭证
        remove(tgt);
        // 删除所有Token，通知所有客户端退出，注销其本地Token
        tokenManager.removeByTgt(tgt);
    }

    public String get(HttpServletRequest request) {
        String tgt = getCookieTgt(request);
        if (!StringUtils.hasLength(tgt) || get(tgt) == null) {
            return null;
        } else {
            return tgt;
        }
    }

    private String getCookieTgt(HttpServletRequest request) {
        return CookieUtils.getCookieValue(cookieName, request);
    }

    public AbstractTokenManager getTokenManager() {
        return tokenManager;
    }

    public void setTokenManager(AbstractTokenManager tokenManager) {
        this.tokenManager = tokenManager;
    }

    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }

    public String getCookieName() {
        return cookieName;
    }

    public void setCookieName(String cookieName) {
        this.cookieName = cookieName;
    }

    /**
     * 刷新时效
     *
     * @param tgt
     * @return
     */
    public abstract void refresh(String tgt);

    public abstract Map<String, TicketGrantingTicketContent> getTgtMap(Set<Long> userIds, Long current, Long size);
}
