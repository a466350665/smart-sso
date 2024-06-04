package com.smart.sso.server.controller;

import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.rpc.ClientUser;
import com.smart.sso.client.util.SessionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

/**
 * 首页管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/")
public class IndexController {
    
	@Value("${server.port}")
	private Integer serverPort;
	@Autowired
    private ClientProperties smartSsoProperties;

	/**
	 * 初始页
	 * @param request
	 * @param model
	 * @return
	 * @throws UnsupportedEncodingException 
	 */
    @GetMapping
	public String execute(Model model, HttpServletRequest request) throws UnsupportedEncodingException {
		ClientUser user = SessionUtils.getUser(request);
		// 设置登录用户名
		model.addAttribute("userName", user.getUsername());
		// 当前服务端口号
		model.addAttribute("serverPort", serverPort);
		// 当前sessionId
		model.addAttribute("sessionId", request.getSession().getId());
		// 单点退出地址
		model.addAttribute("logoutUrl", smartSsoProperties.getServerUrl() + ClientConstant.LOGOUT_URL + "?" + ClientConstant.REDIRECT_URI + "="
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