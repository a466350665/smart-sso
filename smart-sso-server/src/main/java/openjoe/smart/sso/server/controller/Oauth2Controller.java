package openjoe.smart.sso.server.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.constant.Oauth2Constant;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Userinfo;
import openjoe.smart.sso.base.enums.GrantTypeEnum;
import openjoe.smart.sso.server.entity.CodeContent;
import openjoe.smart.sso.server.entity.TokenContent;
import openjoe.smart.sso.server.manager.AppManager;
import openjoe.smart.sso.server.manager.UserinfoManager;
import openjoe.smart.sso.server.manager.AbstractCodeManager;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
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
@RequestMapping(BaseConstant.AUTH_PATH)
public class Oauth2Controller {

    @Autowired
    private AppManager appManager;
    @Autowired
    private UserinfoManager userinfoManager;

    @Autowired
    private AbstractCodeManager codeManager;
    @Autowired
    private AbstractTokenManager tokenManager;
    @Autowired
    private AbstractTicketGrantingTicketManager ticketGrantingTicketManager;

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
        Result<Void> appResult = appManager.validate(appId, appSecret);
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

        // 创建token
        TokenContent tc = tokenManager.create(userinfo, appId, codeContent);

        // 刷新服务端凭证时效
        ticketGrantingTicketManager.refresh(tc.getTgt());

        // 返回token
        return Result.createSuccess(new Token(tc.getAccessToken(), tokenManager.getAccessTokenTimeout(), tc.getRefreshToken(),
                tokenManager.getRefreshTokenTimeout(), tc.getUserinfo()));
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
        if (!appManager.exists(appId)) {
            return Result.createError("非法应用");
        }

        TokenContent atContent = tokenManager.get(refreshToken);
        if (atContent == null) {
            return Result.createError("refreshToken有误或已过期");
        }

        // 删除原有token
        tokenManager.remove(refreshToken);

        // 创建新token
        TokenContent tc = tokenManager.create(atContent);

        // 刷新服务端凭证时效
        ticketGrantingTicketManager.refresh(tc.getTgt());

        // 返回新token
        return Result.createSuccess(new Token(tc.getAccessToken(), tokenManager.getAccessTokenTimeout(), tc.getRefreshToken(),
                tokenManager.getRefreshTokenTimeout(), tc.getUserinfo()));
    }
}