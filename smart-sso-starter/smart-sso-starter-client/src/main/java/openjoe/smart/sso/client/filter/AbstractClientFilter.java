package openjoe.smart.sso.client.filter;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * Filter基类
 *
 * @author Joe
 */
public abstract class AbstractClientFilter {

    /**
     * 请求是否允许通过
     *
     * @param request
     * @param response
     * @return
     * @throws IOException
     */
    public abstract boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response)
            throws IOException;
}