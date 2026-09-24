package openjoe.smart.sso.client;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.core.env.Environment;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

/**
 * 内嵌服务端（本应用同时是 SSO 服务端）地址解析与启动校验
 * <p>
 * smart.sso.server-url 留空即表示"同源"：
 * 页面跳转走相对路径（不依赖域名、端口与反向代理配置），服务端之间的 HTTP 调用走本机推导地址。
 * 若 server-url 留空而本应用并不是 SSO 服务端（例如独立客户端漏配），启动直接失败，
 * 避免静默按同源处理、把用户跳到客户端自己的 /sso/login 上。
 *
 * @author Joe
 */
public class ClientServerAddressResolver implements ApplicationListener<WebServerInitializedEvent> {

    private static final Logger logger = LoggerFactory.getLogger(ClientServerAddressResolver.class);

    /**
     * SSO 服务端标志性 Bean（由 starter-server 提供）。
     * 用类名字符串探测，避免 client 模块反向依赖 server 模块。
     */
    private static final String SERVER_MANAGER_CLASS = "openjoe.smart.sso.server.manager.AbstractTokenManager";

    private final ClientProperties properties;
    private final Environment environment;

    public ClientServerAddressResolver(ClientProperties properties, Environment environment) {
        this.properties = properties;
        this.environment = environment;
    }

    @Override
    public void onApplicationEvent(WebServerInitializedEvent event) {
        if (StringUtils.hasText(properties.getServerUrl())) {
            // 独立客户端：按配置的绝对地址工作，行为与之前保持一致
            return;
        }
        if (!hasSsoServer(event)) {
            throw new IllegalStateException("未配置 smart.sso.server-url，且当前应用不是 SSO 服务端："
                    + "独立客户端必须配置 smart.sso.server-url；只有本应用同时也是 SSO 服务端时才可以留空（同源模式）。");
        }
        properties.setEmbeddedServer(true);
        if (!StringUtils.hasText(properties.getInternalServerUrl())) {
            properties.setInternalServerUrl(buildInternalServerUrl(event));
        }
    }

    /**
     * 当前应用是否包含 SSO 服务端（存在服务端 manager Bean）
     */
    private boolean hasSsoServer(WebServerInitializedEvent event) {
        ClassLoader classLoader = event.getApplicationContext().getClassLoader();
        if (!ClassUtils.isPresent(SERVER_MANAGER_CLASS, classLoader)) {
            // 独立客户端：starter-server 不在 classpath 上
            return false;
        }
        Class<?> managerClass = ClassUtils.resolveClassName(SERVER_MANAGER_CLASS, classLoader);
        // allowEagerInit=false：仅为探测，避免提前实例化 Bean
        return event.getApplicationContext().getBeanNamesForType(managerClass, true, false).length > 0;
    }

    /**
     * 推导本机地址：http(s)://127.0.0.1:{实际端口}{context-path}
     */
    private String buildInternalServerUrl(WebServerInitializedEvent event) {
        String scheme = environment.getProperty("server.ssl.enabled", Boolean.class, Boolean.FALSE) ? "https" : "http";
        String contextPath = environment.getProperty("server.servlet.context-path", "");
        return scheme + "://127.0.0.1:" + event.getWebServer().getPort() + contextPath;
    }
}
