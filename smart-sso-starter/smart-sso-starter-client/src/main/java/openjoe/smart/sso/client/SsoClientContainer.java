package openjoe.smart.sso.client;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import openjoe.smart.sso.client.filter.AbstractClientFilter;
import openjoe.smart.sso.client.util.ClientContextHolder;

import java.io.IOException;

/**
 * Smart-SSO容器过滤中心
 *
 * @author Joe
 */
public class SsoClientContainer implements Filter {

    private SsoUrlScopeMatcher scopeMatcher;

    private AbstractClientFilter[] clientFilters;

    public SsoClientContainer(SsoUrlScopeMatcher scopeMatcher, AbstractClientFilter[] clientFilters) {
        this.scopeMatcher = scopeMatcher;
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
            if (!scopeMatcher.isControlled(request.getServletPath())) {
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

    public SsoUrlScopeMatcher getScopeMatcher() {
        return scopeMatcher;
    }

    public void setScopeMatcher(SsoUrlScopeMatcher scopeMatcher) {
        this.scopeMatcher = scopeMatcher;
    }

    public AbstractClientFilter[] getClientFilters() {
        return clientFilters;
    }

    public void setClientFilters(AbstractClientFilter[] clientFilters) {
        this.clientFilters = clientFilters;
    }
}
