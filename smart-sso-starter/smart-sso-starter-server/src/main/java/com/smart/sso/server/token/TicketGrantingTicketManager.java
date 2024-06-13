package com.smart.sso.server.token;

import com.smart.sso.base.entity.Expiration;
import com.smart.sso.base.entity.LifecycleManager;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.CookieUtils;
import com.smart.sso.server.constant.ServerConstant;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.UUID;

/**
 * 登录凭证（TGT）管理抽象
 *
 * @author Joe
 */
public abstract class TicketGrantingTicketManager implements LifecycleManager<Userinfo>, Expiration {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    private TokenManager tokenManager;
    private int timeout;

    public TicketGrantingTicketManager(TokenManager tokenManager, int timeout) {
        this.tokenManager = tokenManager;
        this.timeout = timeout;
    }

    /**
     * 登录成功后，根据用户信息创建令牌
     *
     * @param user
     * @return
     */
    String create(Userinfo user) {
        String tgt = "TGT-" + UUID.randomUUID().toString().replaceAll("-", "");
        create(tgt, user);
        return tgt;
    }

    public String getOrCreate(Userinfo userinfo, HttpServletRequest request, HttpServletResponse response) {
        String tgt = getCookieTgt(request);
        // cookie中没有
        if (StringUtils.isEmpty(tgt)) {
            tgt = create(userinfo);

            // TGT存cookie，和Cas登录保存cookie中名称一致为：TGC
            CookieUtils.addCookie(ServerConstant.COOKIE_TGT, tgt, "/", request, response);
        } else {
            create(tgt, userinfo);
        }
        return tgt;
    }

    public void invalidate(HttpServletRequest request, HttpServletResponse response) {
        String tgt = getCookieTgt(request);
        if (StringUtils.isEmpty(tgt)) {
            return;
        }
        // 删除登录凭证
        remove(tgt);
        // 删除所有Token，通知所有客户端退出，注销其本地Token
        tokenManager.removeByTgt(tgt);
        // 删除凭证Cookie
        CookieUtils.removeCookie(ServerConstant.COOKIE_TGT, "/", response);
    }

    public String get(HttpServletRequest request) {
        String tgt = getCookieTgt(request);
        if (StringUtils.isEmpty(tgt) || get(tgt) == null) {
            return null;
        } else {
            return tgt;
        }
    }

    private String getCookieTgt(HttpServletRequest request) {
        return CookieUtils.getCookieValue(ServerConstant.COOKIE_TGT, request);
    }

    @Override
    public int getExpiresIn() {
        return 2 * timeout;
    }

    /**
     * 刷新时效
     *
     * @param tgt
     * @return
     */
    public abstract void refresh(String tgt);
}
