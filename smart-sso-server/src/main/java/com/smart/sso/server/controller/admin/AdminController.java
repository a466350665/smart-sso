package com.smart.sso.server.controller.admin;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.sso.client.model.SessionUser;
import com.smart.sso.client.util.SessionUtils;

/**
 * 首页管理
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/admin/admin")
public class AdminController {
    
    @Value("${sso.server.url}")
    private String ssoServerUrl;

	/**
	 * 初始页
	 * @param request
	 * @param model
	 * @return
	 */
	@RequestMapping(method = RequestMethod.GET)
	public String execute(HttpServletRequest request, Model model) {
		SessionUser sessionUser = SessionUtils.getUser(request);
		// 设置登录用户名
		model.addAttribute("userName", sessionUser.getAccount());
		// 单点退出地址
        model.addAttribute("ssologoutUrl", ssoServerUrl + "/logout?service=" + getLocalUrl(request).toString());
		return "/admin";
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