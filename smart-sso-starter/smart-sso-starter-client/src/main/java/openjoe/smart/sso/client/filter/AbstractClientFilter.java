package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.client.util.ClientContextHolder;

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
     * @return
     */
    public abstract boolean isAccessAllowed() throws IOException;

    protected void responseJson(int code, String message) throws IOException {
        HttpServletResponse response = ClientContextHolder.getResponse();
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(HttpServletResponse.SC_OK);
        try (PrintWriter writer = response.getWriter()) {
            writer.write(JsonUtils.toString(new Result<>(code, message)));
        }
    }
}