package openjoe.smart.sso.demo.controller;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.client.util.SSOUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * 前后端分离模式下集成SSO所需的接口
 */
@RestController
@RequestMapping("/auth")
public class AuthController {

    /**
     * 返回SSO登录地址
     *
     * @param redirectUri 登录成功后的回跳地址
     * @return
     */
    @RequestMapping("/login_url")
    public Result<String> getLoginUrl(@RequestParam String redirectUri) {
        return Result.success(SSOUtils.buildLoginUrl(redirectUri));
    }

    /**
     * 返回SSO退出地址
     *
     * @param redirectUri 退出成功后的回跳地址
     * @return
     */
    @RequestMapping("/logout_url")
    public Result<String> getLogoutUrl(@RequestParam String redirectUri) {
        return Result.success(SSOUtils.buildLogoutUrl(redirectUri));
    }

    /**
     * 通过授权码获取SSO调用凭证
     *
     * @param code 授权码
     * @return
     */
    @RequestMapping(value = "/access-token", method = RequestMethod.GET)
    public Result<Token> getAccessToken(@RequestParam String code) {
        Result<Token> result = SSOUtils.getHttpAccessToken(code);
        if (!result.isSuccess()) {
            return result;
        }
        return Result.success(result.getData());
    }

    /**
     * 通过刷新凭证获取新的调用凭证
     *
     * @param refreshToken 刷新凭证
     * @return
     */
    @RequestMapping(value = "/refresh-token", method = RequestMethod.GET)
    public Result<Token> getRefreshToken(String refreshToken) {
        Result<Token> result = SSOUtils.getHttpRefreshToken(refreshToken);
        if (!result.isSuccess()) {
            return result;
        }
        return Result.success(result.getData());
    }
}
