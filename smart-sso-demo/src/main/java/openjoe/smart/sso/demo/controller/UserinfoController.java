package openjoe.smart.sso.demo.controller;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.client.util.ClientContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/userinfo")
public class UserinfoController {

    /**
     * 用户信息
     *
     * @return
     */
    @GetMapping
    public Result getUserinfo() {
        Map<String, Object> map = new HashMap<>();

        TokenUser user = ClientContextHolder.getUser();
        // 登录用户名
        map.put("username", user.getUsername());

        TokenPermission permission = ClientContextHolder.getPermission();
        // 用户当前应用已分配的菜单
        map.put("userMenus",
                permission.getMenuList().stream().map(menu -> menu.getName() + ":" + menu.getUrl()).collect(Collectors.toList()));
        // 用户当前应用已分配的权限
        map.put("userPermissions", permission.getPermissionSet());
        return Result.success(map);
    }
}
