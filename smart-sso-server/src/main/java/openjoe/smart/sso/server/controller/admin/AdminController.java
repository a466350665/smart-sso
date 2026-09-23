package openjoe.smart.sso.server.controller.admin;

import io.swagger.v3.oas.annotations.Operation;
import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.util.ClientContextHolder;
import openjoe.smart.sso.client.util.SSOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/admin/admin")
public class AdminController {

    @Autowired
    private ClientProperties clientProperties;

    @Operation(summary = "当前用户信息")
    @ResponseBody
    @RequestMapping(value = "/userinfo", method = RequestMethod.GET)
    public Result userinfo() throws UnsupportedEncodingException {
        Map<String, Object> data = new HashMap<>();
        TokenUser user = ClientContextHolder.getUser();
        TokenPermission permission = ClientContextHolder.getPermission();

        data.put("username", user.getUsername());
        data.put("userId", user.getId());
        data.put("userNoPermissions",
                CollectionUtils.isEmpty(permission.getNoPermissionSet()) ? "" : String.join(",", permission.getNoPermissionSet()));
        data.put("logoutUrl", clientProperties.getServerUrl() + BaseConstant.LOGOUT_PATH + "?" + BaseConstant.REDIRECT_URI + "="
                + URLEncoder.encode(SSOUtils.getLocalUrl(), "utf-8"));
        return Result.success(data);
    }
    @Operation(summary = "菜单")
    @ResponseBody
    @RequestMapping(value = "/menu", method = RequestMethod.GET)
    public Result menu() {
        TokenPermission permission = ClientContextHolder.getPermission();
        // 获取登录用户已分配权限的菜单列表
        return Result.success(permission.getMenuList());
    }
}
