package openjoe.smart.sso.server.controller;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
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
import org.springframework.web.bind.annotation.ResponseBody;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

/**
 * 单点登录管理
 *
 * @author Joe
 */
@Controller
@RequestMapping(BaseConstant.LOGIN_PATH)
public class SSOLoginController {

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
     * @param clientId
     * @param request
     * @return
     */
    @RequestMapping(method = RequestMethod.GET)
    public String login(
            @RequestParam(value = BaseConstant.REDIRECT_URI) String redirectUri,
            @RequestParam(value = BaseConstant.CLIENT_ID) String clientId,
            HttpServletRequest request) throws UnsupportedEncodingException {
        String tgt = tgtManager.get(request);
        if (!StringUtils.hasLength(tgt)) {
            String encodedRedirectUri = java.net.URLEncoder.encode(redirectUri, "utf-8");
            return "redirect:/login.html?" + BaseConstant.REDIRECT_URI + "=" + encodedRedirectUri
                    + "&" + BaseConstant.CLIENT_ID + "=" + clientId;
        }
        return generateCodeAndRedirect(tgt, clientId, redirectUri);
    }

    /**
     * 登录提交
     *
     * @param redirectUri
     * @param clientId
     * @param username
     * @param password
     * @param request
     * @param response
     * @return
     * @throws UnsupportedEncodingException
     */
    @RequestMapping(method = RequestMethod.POST)
    @ResponseBody
    public Result<String> login(
            @RequestParam(value = BaseConstant.REDIRECT_URI) String redirectUri,
            @RequestParam(value = BaseConstant.CLIENT_ID) String clientId,
            @RequestParam String username,
            @RequestParam String password,
            HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {

        Result<Long> appResult = appManager.validate(clientId);
        if (!appResult.isSuccess()) {
            return Result.error(appResult.getMessage());
        }

        Result<Long> result = userManager.validate(username, password);
        if (!result.isSuccess()) {
            return Result.error(result.getMessage());
        }

        String tgt = tgtManager.getOrCreate(result.getData(), request, response);
        return Result.success(authRedirectUri(redirectUri, codeManager.create(tgt, clientId)));
    }

    /**
     * 创建授权码，跳转到redirectUri
     *
     * @param tgt
     * @param clientId
     * @param redirectUri
     * @return
     * @throws UnsupportedEncodingException
     */
    private String generateCodeAndRedirect(String tgt, String clientId, String redirectUri) throws UnsupportedEncodingException {
        // 创建授权码
        String code = codeManager.create(tgt, clientId);
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
        String decodedRedirectUri = URLDecoder.decode(redirectUri, "utf-8");
        String[] parts = decodedRedirectUri.split("#", 2);
        StringBuilder sbf = new StringBuilder(parts[0]);
        if (parts[0].indexOf("?") > -1) {
            sbf.append("&");
        } else {
            sbf.append("?");
        }
        sbf.append(BaseConstant.AUTH_CODE).append("=").append(code);
        if (parts.length > 1) {
            sbf.append("#").append(parts[1]);
        }
        return sbf.toString();
    }

}
