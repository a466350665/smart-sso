package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.LifecycleManager;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.base.util.HttpUtils;
import openjoe.smart.sso.server.entity.CodeContent;
import openjoe.smart.sso.server.entity.TokenContent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.*;

/**
 * 调用凭证AccessToken管理抽象
 *
 * @author Joe
 */
public abstract class AbstractTokenManager implements LifecycleManager<TokenContent> {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * accessToken超时时效
     */
    private int accessTokenTimeout;

    /**
     * refreshToken时效和登录超时时效保持一致
     */
    private int refreshTokenTimeout;

    protected final ExecutorService executorService;

    public AbstractTokenManager(int accessTokenTimeout, int refreshTokenTimeout, int threadPoolSize) {
        this.accessTokenTimeout = accessTokenTimeout;
        this.refreshTokenTimeout = refreshTokenTimeout;
        this.executorService = new ThreadPoolExecutor(
                threadPoolSize,
                threadPoolSize,
                3L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(1000)
        );
    }

    /**
     * 通过AccessToken获取
     *
     * @param accessToken
     * @return
     */
    public abstract TokenContent getByAccessToken(String accessToken);

    /**
     * 通过TGT移除
     *
     * @param tgt
     */
    public abstract void removeByTgt(String tgt);

    /**
     * 创建AccessToken
     *
     * @param tc
     * @return
     */
    public TokenContent create(TokenContent tc) {
        return create(tc.getTokenUser(), tc.getLogoutUri(), tc);
    }

    /**
     * 创建AccessToken
     *
     * @param tokenUser
     * @param codeContent
     * @return
     */
    public TokenContent create(TokenUser tokenUser, String logoutUri, CodeContent codeContent) {
        String accessToken = "AT-" + UUID.randomUUID().toString().replaceAll("-", "");
        String refreshToken = "RT-" + UUID.randomUUID().toString().replaceAll("-", "");
        TokenContent tc = new TokenContent(accessToken, refreshToken, tokenUser, logoutUri, codeContent.getTgt(), codeContent.getClientId());
        create(refreshToken, tc);
        return tc;
    }

    protected void submitRemoveToken(Set<String> refreshTokenSet) {
        // 用于存储所有的Future对象，以便后续等待所有任务完成
        List<Future<?>> futures = new ArrayList<>();

        refreshTokenSet.forEach(refreshToken -> {
            // 发起客户端退出请求，提交任务到线程池并获取Future对象
            Future<?> future = executorService.submit(() -> {
                try {
                    processRemoveToken(refreshToken);
                } catch (Exception e) {
                    logger.error("执行删除Token操作出现异常", e);
                }
            });
            futures.add(future);
        });

        // 等待所有的请求任务都完成
        for (Future<?> future : futures) {
            try {
                future.get();
            } catch (Exception e) {
                logger.error("执行删除Token任务出现异常", e);
            }
        }
    }

    public abstract void processRemoveToken(String refreshToken);

    /**
     * 发起客户端退出请求
     *
     * @param redirectUri
     * @param accessToken
     */
    protected void sendLogoutRequest(String redirectUri, String accessToken) {
        Map<String, String> headerMap = new HashMap<>();
        headerMap.put(BaseConstant.LOGOUT_PARAMETER_NAME, accessToken);
        HttpUtils.postHeader(redirectUri, headerMap);
    }

    public int getAccessTokenTimeout() {
        return accessTokenTimeout;
    }

    public void setAccessTokenTimeout(int accessTokenTimeout) {
        this.accessTokenTimeout = accessTokenTimeout;
    }

    public int getRefreshTokenTimeout() {
        return refreshTokenTimeout;
    }

    public void setRefreshTokenTimeout(int refreshTokenTimeout) {
        this.refreshTokenTimeout = refreshTokenTimeout;
    }
}
