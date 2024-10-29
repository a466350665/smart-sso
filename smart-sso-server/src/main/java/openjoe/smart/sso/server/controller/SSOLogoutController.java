package openjoe.smart.sso.server.controller;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * 单点退出
 *
 * @author Joe
 */
@Controller
@RequestMapping(BaseConstant.LOGOUT_PATH)
public class SSOLogoutController {

    @Autowired
    private AbstractTicketGrantingTicketManager tgtManager;

    /**
     * 退出
     *
     * @param redirectUri
     * @param request
     * @param response
     * @return
     */
    @RequestMapping(method = RequestMethod.GET)
    public String logout(
            @RequestParam(value = BaseConstant.REDIRECT_URI) String redirectUri,
            HttpServletRequest request, HttpServletResponse response) {
        tgtManager.invalidate(request, response);
        return "redirect:" + redirectUri;
    }
}