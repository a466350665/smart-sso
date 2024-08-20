package openjoe.smart.sso.demo.controller;

import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.client.util.ClientContextHolder;
import openjoe.smart.sso.client.util.SSOUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.stream.Collectors;

@Controller
@RequestMapping("/")
public class IndexController {

    @Value("${server.port}")
    private Integer serverPort;

    /**
     * 初始页
     *
     * @param model
     * @return
     */
    @GetMapping
    public String index(Model model) {
        TokenUser user = ClientContextHolder.getUser();
        // 登录用户名
        model.addAttribute("username", user.getUsername());

        TokenPermission permission = ClientContextHolder.getPermission();
        // 用户当前应用已分配的菜单
        model.addAttribute("userMenus",
                permission.getMenuList().stream().map(menu -> menu.getName() + ":" + menu.getUrl()).collect(Collectors.toList()));
        // 用户当前应用已分配的权限
        model.addAttribute("userPermissions", permission.getPermissionSet());

        // 当前服务端口号
        model.addAttribute("serverPort", serverPort);

        // 单点退出地址
        model.addAttribute("logoutUrl", SSOUtils.buildLogoutUrl());
        return "index";
    }
}
