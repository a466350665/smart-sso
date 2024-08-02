package openjoe.smart.sso.demo.controller;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.client.util.SSOUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/auth")
public class AuthController {

    /**
     * 返回SSO登录地址
     *
     * @param redirectUri
     * @return
     */
    @RequestMapping("/login_url")
    public Result<String> getLoginUrl(@RequestParam String redirectUri) {
        return Result.success(SSOUtils.buildLoginUrl(redirectUri));
    }

    /**
     * 返回SSO退出地址
     *
     * @param redirectUri
     * @return
     */
    @RequestMapping("/logout_url")
    public Result<String> getLogoutUrl(@RequestParam String redirectUri) {
        return Result.success(SSOUtils.buildLogoutUrl(redirectUri));
    }

    /**
     * 获取SSO登录凭证
     *
     * @param code
     * @return
     */
    @RequestMapping(value = "/access_token", method = RequestMethod.GET)
    public Result<Token> getAccessToken(@RequestParam String code) {
        Result<Token> result = SSOUtils.getHttpAccessToken(code);
        if (!result.isSuccess()) {
            return result;
        }
        return Result.success(result.getData());
    }

    /**
     * 获取SSO刷新凭证
     *
     * @param refreshToken
     * @return
     */
    @RequestMapping(value = "/refresh_token", method = RequestMethod.GET)
    public Result<Token> getRefreshToken(String refreshToken) {
        Result<Token> result = SSOUtils.getHttpRefreshToken(refreshToken);
        if (!result.isSuccess()) {
            return result;
        }
        return Result.success(result.getData());
    }
}
