package openjoe.smart.sso.demo.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.util.TokenUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.stream.Collectors;

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
     * @param model
     * @return
     * @throws UnsupportedEncodingException
     */
    @GetMapping
    public String index(Model model) throws UnsupportedEncodingException {
        TokenUser user = TokenUtils.getUser();
        // 登录用户名
        model.addAttribute("userName", user.getUsername());

        TokenPermission permission = TokenUtils.getPermission();
        // 用户当前应用已分配的菜单
        model.addAttribute("userMenus",
                permission.getMenuList().stream().map(menu -> menu.getName() + ":" + menu.getUrl()).collect(Collectors.toList()));
        // 用户当前应用已分配的权限
        model.addAttribute("userPermissions", permission.getPermissionSet());

        // 当前服务端口号
        model.addAttribute("serverPort", serverPort);
        // 单点退出地址
        model.addAttribute("logoutUrl", clientProperties.getServerUrl() + BaseConstant.LOGOUT_PATH + "?" + BaseConstant.REDIRECT_URI + "="
                + URLEncoder.encode(TokenUtils.getLocalUrl(), "utf-8"));
        return "index";
    }
}
