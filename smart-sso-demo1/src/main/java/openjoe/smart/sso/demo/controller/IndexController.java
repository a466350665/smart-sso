package openjoe.smart.sso.demo.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Userinfo;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.TokenUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

@Controller
@RequestMapping("/")
public class IndexController {

    @Value("${server.port}")
    private Integer serverPort;
    @Autowired
    private ClientProperties clientProperties;

    /**
     * 初始页
     *
     * @param request
     * @param model
     * @return
     * @throws UnsupportedEncodingException
     */
    @GetMapping
    public String index(Model model, HttpServletRequest request) throws UnsupportedEncodingException {
        Userinfo userinfo = TokenUtils.getUserinfo(request);
        // 登录用户名
        model.addAttribute("userName", userinfo.getUsername());
        // 当前服务端口号
        model.addAttribute("serverPort", serverPort);
        // 单点退出地址
        model.addAttribute("logoutUrl", clientProperties.getServerUrl() + ClientConstant.LOGOUT_URL + "?" + BaseConstant.REDIRECT_URI + "="
                + URLEncoder.encode(getLocalUrl(request), "utf-8"));
        return "index";
    }


    /**
     * 获取当前应用访问路径
     *
     * @param request
     * @return
     */
    private String getLocalUrl(HttpServletRequest request) {
        StringBuilder url = new StringBuilder();
        url.append(request.getScheme()).append("://").append(request.getServerName());
        if (request.getServerPort() != 80 && request.getServerPort() != 443) {
            url.append(":").append(request.getServerPort());
        }
        url.append(request.getContextPath());
        return url.toString();
    }
}
