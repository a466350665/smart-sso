package openjoe.smart.sso.server.controller;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.client.util.SsoUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * 前端直接集成SSO所需的接口（认证授权中心管理台自用）
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
        return Result.success(SsoUtils.buildLoginUrl(redirectUri));
    }

    /**
     * 返回SSO退出地址
     *
     * @param redirectUri 退出成功后的回跳地址
     * @return
     */
    @RequestMapping("/logout_url")
    public Result<String> getLogoutUrl(@RequestParam String redirectUri) {
        return Result.success(SsoUtils.buildLogoutUrl(redirectUri));
    }

    /**
     * 通过授权码获取SSO调用凭证
     *
     * @param code 授权码
     * @return
     */
    @RequestMapping(value = "/access-token", method = RequestMethod.GET)
    public Result<Token> getAccessToken(@RequestParam String code) {
        return SsoUtils.getHttpAccessToken(code);
    }

    /**
     * 通过刷新凭证获取新的调用凭证
     *
     * @param refreshToken 刷新凭证
     * @return
     */
    @RequestMapping(value = "/refresh-token", method = RequestMethod.GET)
    public Result<Token> getRefreshToken(@RequestParam String refreshToken) {
        return SsoUtils.getHttpRefreshToken(refreshToken);
    }
}
