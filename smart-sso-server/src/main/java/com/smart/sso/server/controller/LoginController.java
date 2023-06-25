package com.smart.sso.server.controller;

import com.smart.core.entity.Result;
import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.RpcUserDto;
import com.smart.sso.server.common.TicketGrantingTicketManager;
import com.smart.sso.server.constant.AppConstant;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.util.CookieUtils;
import com.smart.sso.server.util.PasswordHelper;
import com.smart.sso.server.validator.ValidateParam;
import com.smart.sso.server.validator.Validator;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

/**
 * @author Joe
 */
@Api(tags = "单点登录管理")
@Controller
@RequestMapping("/login")
public class LoginController extends BaseController{
	
	@Autowired
    private TicketGrantingTicketManager ticketGrantingTicketManager;
	@Autowired
	private UserService userService;

	@ApiOperation("登录页")
	@RequestMapping(method = RequestMethod.GET)
	public String login(
			@ValidateParam(name = "返回链接", value = { Validator.NOT_BLANK }) String service,
			HttpServletRequest request) {
        String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
        if (StringUtils.isEmpty(tgt) || ticketGrantingTicketManager.validate(tgt) == null) {
            return goLoginPath(service, request);
        }
        return "redirect:" + authService(service, tgt);
	}

	@ApiOperation("登录提交")
	@RequestMapping(method = RequestMethod.POST)
	public String login(
			@ValidateParam(name = "返回链接", value = { Validator.NOT_BLANK }) String service,
			@ValidateParam(name = "登录名", value = { Validator.NOT_BLANK }) String account,
			@ValidateParam(name = "密码", value = { Validator.NOT_BLANK }) String password,
			HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {
	    Result<User> result = userService.login(account, PasswordHelper.encrypt(password));
		if (!result.isSuccess()) {
			request.setAttribute("errorMessage", result.getMessage());
			return goLoginPath(service, request);
		}
		else {
			String tgt = CookieUtils.getCookie(request, AppConstant.TGC);
			if (StringUtils.isEmpty(tgt) || ticketGrantingTicketManager.validate(tgt) == null) {
			    User user = result.getData();
			    tgt = ticketGrantingTicketManager.generate(new RpcUserDto(user.getId(), user.getAccount()));
			    
			    // TGT存cookie，和Cas登录保存cookie中名称一致为：TGC
			    CookieUtils.addCookie(AppConstant.TGC, tgt, "/", request, response);
	        }
			return "redirect:" + authService(service, tgt);
		}
	}
	
    /**
     * 设置request的service参数，跳转到登录页
     * 
     * @param service
     * @param request
     * @return
     */
    private String goLoginPath(String service, HttpServletRequest request) {
        request.setAttribute("service", service);
        return AppConstant.LOGIN_PATH;
    }

	/**
	 * 根据TGT生成ST，并拼接到回调service中
	 * @param service
	 * @param tgt
	 * @return
	 */
	private String authService(String service, String tgt) {
        StringBuilder sbf = new StringBuilder(service);
        if (service.indexOf("?") > 0) {
            sbf.append("&");
        } 
        else {
            sbf.append("?");
        }
        sbf.append(SsoConstant.TICKET_PARAMETER_NAME).append("=").append(ticketGrantingTicketManager.signSt(tgt, service));
        try {
            return URLDecoder.decode(sbf.toString(), "utf-8");
        } 
        catch (UnsupportedEncodingException e) {
            logger.error("", e);
            return sbf.toString();
        }
	}
}