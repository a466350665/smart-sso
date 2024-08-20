package openjoe.smart.sso.client;

import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.filter.AbstractClientFilter;
import openjoe.smart.sso.client.util.ClientContextHolder;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Smart-SSO容器过滤中心
 *
 * @author Joe
 */
public class ClientContainer implements Filter {

    /**
     * 忽略拦截urls
     */
    private String[] excludeUrls;

    private AbstractClientFilter[] clientFilters;

    public ClientContainer(String[] excludeUrls, AbstractClientFilter[] clientFilters) {
        this.excludeUrls = excludeUrls;
        this.clientFilters = clientFilters;
    }

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        if (clientFilters == null || clientFilters.length == 0) {
            throw new IllegalArgumentException("clientFilters不能为空");
        }
    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
            throws IOException, ServletException {
        HttpServletRequest request = (HttpServletRequest) req;
        HttpServletResponse response = (HttpServletResponse) res;
        ClientContextHolder.create(request, response);
        try {
            if (isExcludeUrl(request.getServletPath())) {
                chain.doFilter(req, res);
                return;
            }

            for (AbstractClientFilter filter : clientFilters) {
                if (!filter.isAccessAllowed()) {
                    return;
                }
            }
            chain.doFilter(req, res);
        } finally {
            ClientContextHolder.reset();
        }
    }

    private boolean isExcludeUrl(String url) {
        if (excludeUrls == null || excludeUrls.length == 0) {
            return false;
        }

        Map<Boolean, List<String>> map = Arrays.stream(excludeUrls)
                .collect(Collectors.partitioningBy(u -> u.endsWith(ClientConstant.URL_FUZZY_MATCH)));
        List<String> urlList = map.get(false);
        // 优先精确匹配
        if (urlList.contains(url)) {
            return true;
        }
        urlList = map.get(true);
        // 再进行模糊匹配
        for (String matchUrl : urlList) {
            if (url.startsWith(matchUrl.replace(ClientConstant.URL_FUZZY_MATCH, ""))) {
                return true;
            }
        }
        return false;
    }

    public String[] getExcludeUrls() {
        return excludeUrls;
    }

    public void setExcludeUrls(String[] excludeUrls) {
        this.excludeUrls = excludeUrls;
    }

    public AbstractClientFilter[] getClientFilters() {
        return clientFilters;
    }

    public void setClientFilters(AbstractClientFilter[] clientFilters) {
        this.clientFilters = clientFilters;
    }
}