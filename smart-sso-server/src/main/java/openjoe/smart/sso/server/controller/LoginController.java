package openjoe.smart.sso.server.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.server.manager.AbstractCodeManager;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AppManager;
import openjoe.smart.sso.server.manager.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

/**
 * 单点登录管理
 *
 * @author Joe
 */
@Controller
@RequestMapping(BaseConstant.LOGIN_PATH)
public class LoginController {

    @Autowired
    private AbstractCodeManager codeManager;
    @Autowired
    private AbstractTicketGrantingTicketManager tgtManager;
    @Autowired
    private UserManager userManager;
    @Autowired
    private AppManager appManager;

    /**
     * 登录页
     *
     * @param redirectUri
     * @param appKey
     * @param request
     * @return
     */
    @RequestMapping(method = RequestMethod.GET)
    public String login(
            @RequestParam(value = BaseConstant.REDIRECT_URI) String redirectUri,
            @RequestParam(value = BaseConstant.APP_KEY) String appKey,
            HttpServletRequest request) throws UnsupportedEncodingException {
        String tgt = tgtManager.get(request);
        if (!StringUtils.hasLength(tgt)) {
            return goLoginPath(redirectUri, appKey, request);
        }
        return generateCodeAndRedirect(redirectUri, tgt);
    }

    /**
     * 登录提交
     *
     * @param redirectUri
     * @param appKey
     * @param username
     * @param password
     * @param request
     * @param response
     * @return
     * @throws UnsupportedEncodingException
     */
    @RequestMapping(method = RequestMethod.POST)
    public String login(
            @RequestParam(value = BaseConstant.REDIRECT_URI) String redirectUri,
            @RequestParam(value = BaseConstant.APP_KEY) String appKey,
            @RequestParam String username,
            @RequestParam String password,
            HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {

        if (!appManager.exists(appKey)) {
            request.setAttribute("errorMessage", "非法应用");
            return goLoginPath(redirectUri, appKey, request);
        }

        Result<TokenUser> result = userManager.login(username, password);
        if (!result.isSuccess()) {
            request.setAttribute("errorMessage", result.getMessage());
            return goLoginPath(redirectUri, appKey, request);
        }

        String tgt = tgtManager.getOrCreate(result.getData(), request, response);
        return generateCodeAndRedirect(redirectUri, tgt);
    }

    /**
     * 设置request的redirectUri和appKey参数，跳转到登录页
     *
     * @param redirectUri
     * @param request
     * @return
     */
    private String goLoginPath(String redirectUri, String appKey, HttpServletRequest request) {
        request.setAttribute(BaseConstant.REDIRECT_URI, redirectUri);
        request.setAttribute(BaseConstant.APP_KEY, appKey);
        return BaseConstant.LOGIN_PATH;
    }

    /**
     * 创建授权码，跳转到redirectUri
     *
     * @param redirectUri
     * @param tgt
     * @return
     * @throws UnsupportedEncodingException
     */
    private String generateCodeAndRedirect(String redirectUri, String tgt) throws UnsupportedEncodingException {
        // 创建授权码
        String code = codeManager.create(tgt, redirectUri);
        return "redirect:" + authRedirectUri(redirectUri, code);
    }

    /**
     * 将授权码拼接到回调redirectUri中
     *
     * @param redirectUri
     * @param code
     * @return
     * @throws UnsupportedEncodingException
     */
    private String authRedirectUri(String redirectUri, String code) throws UnsupportedEncodingException {
        StringBuilder sbf = new StringBuilder(redirectUri);
        if (redirectUri.indexOf("?") > -1) {
            sbf.append("&");
        } else {
            sbf.append("?");
        }
        sbf.append(BaseConstant.AUTH_CODE).append("=").append(code);
        return URLDecoder.decode(sbf.toString(), "utf-8");
    }

}