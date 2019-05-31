package com.smart.sso.demo.controller;

import com.smart.sso.client.SessionPermission;
import com.smart.sso.client.SessionUser;
import com.smart.sso.client.SessionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@Controller
public class IndexController {

    @Value("${sso.server.url}")
    private String ssoServerUrl;

    @GetMapping("/")
    public String index(Model model, HttpServletRequest request) {

        SessionUser sessionUser = SessionUtils.getSessionUser(request);
        model.addAttribute("userName", sessionUser.getAccount());
        model.addAttribute("ssologoutUrl", new StringBuilder().append(ssoServerUrl)
                .append("/logout?backUrl=").append(getLocalUrl(request)).toString());

        SessionPermission sessionPermission = SessionUtils.getSessionPermission(request);
        if (sessionPermission != null) {
            // 登录用户当前应用的菜单
            request.setAttribute("userMenus", sessionPermission.getMenuList());
            // 登录用户当前应用的权限
            request.setAttribute("userPermissions", sessionPermission.getPermissionSet());
        }
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
