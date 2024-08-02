package openjoe.smart.sso.server.controller.admin;

import io.swagger.annotations.ApiOperation;
import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.util.SSOUtils;
import openjoe.smart.sso.server.stage.core.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

@Controller
@RequestMapping("/admin/admin")
public class AdminController {

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
        TokenUser user = SSOUtils.getUser();
        // 登录用户名
        model.addAttribute("username", user.getUsername());
        TokenPermission permission = SSOUtils.getPermission();
        // 设置当前登录用户没有的权限，以便控制前台菜单和按钮隐藏
        model.addAttribute("userNoPermissions",
                CollectionUtils.isEmpty(permission.getNoPermissionSet()) ? "" : String.join(",", permission.getNoPermissionSet()));
        // 单点退出地址
        model.addAttribute("logoutUrl", clientProperties.getServerUrl() + BaseConstant.LOGOUT_PATH + "?" + BaseConstant.REDIRECT_URI + "="
                + URLEncoder.encode(SSOUtils.getLocalUrl(), "utf-8"));
        return "/admin/admin";
    }

    @ApiOperation("菜单")
    @ResponseBody
    @RequestMapping(value = "/menu", method = RequestMethod.GET)
    public Result menu() {
        TokenPermission permission = SSOUtils.getPermission();
        // 获取登录用户已分配权限的菜单列表
        return Result.success(permission.getMenuList());
    }
}