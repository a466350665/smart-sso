package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.util.JsonUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;

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

    protected void responseJson(HttpServletResponse response, int code, String message) throws IOException {
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(HttpServletResponse.SC_OK);
        try (PrintWriter writer = response.getWriter()) {
            writer.write(JsonUtils.toString(new Result<>(code, message)));
        }
    }
}