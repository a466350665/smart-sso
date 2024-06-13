package com.smart.sso.server.controller;

import com.smart.sso.base.constant.Oauth2Constant;
import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.Result;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.enums.GrantTypeEnum;
import com.smart.sso.server.entity.CodeContent;
import com.smart.sso.server.entity.TokenContent;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.token.CodeManager;
import com.smart.sso.server.token.TicketGrantingTicketManager;
import com.smart.sso.server.token.TokenManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Oauth2服务管理
 *
 * @author Joe
 */
@SuppressWarnings("rawtypes")
@RestController
@RequestMapping("/oauth2")
public class Oauth2Controller {

    @Autowired
    private AppService appService;
    @Autowired
    private UserService userService;

    @Autowired
    private CodeManager codeManager;
    @Autowired
    private TokenManager tokenManager;
    @Autowired
    private TicketGrantingTicketManager ticketGrantingTicketManager;

    /**
     * 获取accessToken
     *
     * @param appId
     * @param appSecret
     * @param code
     * @return
     */
    @RequestMapping(value = "/access_token", method = RequestMethod.GET)
    public Result getAccessToken(
            @RequestParam(value = Oauth2Constant.GRANT_TYPE) String grantType,
            @RequestParam(value = Oauth2Constant.APP_ID) String appId,
            @RequestParam(value = Oauth2Constant.APP_SECRET) String appSecret,
            @RequestParam(value = Oauth2Constant.AUTH_CODE) String code) {

        // 校验授权码方式
        if (!GrantTypeEnum.AUTHORIZATION_CODE.getValue().equals(grantType)) {
            return Result.createError("仅支持授权码方式");
        }

        // 校验应用
        Result<Void> appResult = appService.validate(appId, appSecret);
        if (!appResult.isSuccess()) {
            return appResult;
        }

        // 校验授权码
        CodeContent codeContent = codeManager.get(code);
        if (codeContent == null) {
            return Result.createError("code有误或已过期");
        }
        codeManager.remove(code);

        // 校验凭证
        Userinfo userinfo = ticketGrantingTicketManager.get(codeContent.getTgt());
        if (userinfo == null) {
            return Result.createError("服务端TGT已过期");
        }

        // 生成token
        TokenContent tc = tokenManager.generate(userinfo, appId, codeContent);

        // 刷新服务端凭证时效
        ticketGrantingTicketManager.refresh(tc.getTgt());

        // 返回token
        return Result.createSuccess(new AccessToken(tc.getAccessToken(), tokenManager.getExpiresIn(), tc.getRefreshToken(),
                tokenManager.getRefreshExpiresIn(), tc.getUserinfo()));
    }

    /**
     * 刷新accessToken，并延长TGT超时时间
     *
     * @param appId
     * @param refreshToken
     * @return
     */
    @RequestMapping(value = "/refresh_token", method = RequestMethod.GET)
    public Result getRefreshToken(
            @RequestParam(value = Oauth2Constant.APP_ID) String appId,
            @RequestParam(value = Oauth2Constant.REFRESH_TOKEN) String refreshToken) {
        if (!appService.exists(appId)) {
            return Result.createError("非法应用");
        }

        TokenContent atContent = tokenManager.get(refreshToken);
        if (atContent == null) {
            return Result.createError("refreshToken有误或已过期");
        }

        // 删除原有token
        tokenManager.remove(refreshToken);

        // 生成新token
        TokenContent tc = tokenManager.generate(atContent);

        // 刷新服务端凭证时效
        ticketGrantingTicketManager.refresh(tc.getTgt());

        // 返回新token
        return Result.createSuccess(new AccessToken(tc.getAccessToken(), tokenManager.getExpiresIn(), tc.getRefreshToken(),
                tokenManager.getRefreshExpiresIn(), tc.getUserinfo()));
    }
}