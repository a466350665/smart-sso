package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.util.TokenUtils;
import openjoe.smart.stage.core.entity.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

@Controller
@RequestMapping("/admin/admin")
public class AdminController {

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
        TokenUser user = TokenUtils.getUser(request);
        // 登录用户名
        model.addAttribute("userName", user.getUsername());
        TokenPermission permission = TokenUtils.getPermission(request);
        // 设置当前登录用户没有的权限，以便控制前台菜单和按钮隐藏
        model.addAttribute("userNoPermissions",
                CollectionUtils.isEmpty(permission.getNoPermissionSet()) ? "" : String.join(",", permission.getNoPermissionSet()));
        // 单点退出地址
        model.addAttribute("logoutUrl", clientProperties.getServerUrl() + BaseConstant.LOGOUT_PATH + "?" + BaseConstant.REDIRECT_URI + "="
                + URLEncoder.encode(getLocalUrl(request), "utf-8"));
        return "/admin/admin";
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

    @ApiOperation("菜单")
    @ResponseBody
    @RequestMapping(value = "/menu", method = RequestMethod.GET)
    public Result menu(HttpServletRequest request) {
        TokenPermission permission = TokenUtils.getPermission(request);
        // 获取登录用户已分配权限的菜单列表
        return Result.success(permission.getMenuList());
    }
}