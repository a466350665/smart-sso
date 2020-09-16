package com.smart.sso.server.controller;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.UUID;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.controller.BaseController;
import com.smart.mvc.model.Result;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.server.common.TokenManager;
import com.smart.sso.server.dto.LoginUserDto;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.util.CookieUtils;
import com.smart.sso.server.util.PasswordHelper;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

/**
 * @author Joe
 */
@Api(tags = "单点登录管理")
@Controller
@RequestMapping("/login")
@SuppressWarnings("rawtypes")
public class LoginController extends BaseController{
	
	// 登录页
	private static final String LOGIN_PATH = "/login";

	@Autowired
	private TokenManager tokenManager;
	@Autowired
	private UserService userService;

	@ApiOperation("登录页")
	@RequestMapping(method = RequestMethod.GET)
	public String login(
			@ValidateParam(name = "返回链接", value = { Validator.NOT_BLANK }) String backUrl,
			HttpServletRequest request) {
		String token = CookieUtils.getCookie(request, TokenManager.TOKEN);
		if (!StringUtils.isEmpty(token) && tokenManager.validate(token) != null) {
			return "redirect:" + authBackUrl(backUrl, token);
		}
		else {
			return goLoginPath(backUrl, request);
		}
	}

	@ApiOperation("登录提交")
	@RequestMapping(method = RequestMethod.POST)
	public String login(
			@ValidateParam(name = "返回链接", value = { Validator.NOT_BLANK }) String backUrl,
			@ValidateParam(name = "登录名", value = { Validator.NOT_BLANK }) String account,
			@ValidateParam(name = "密码", value = { Validator.NOT_BLANK }) String password,
			HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {
        Result result = userService.login(getIpAddr(request), account, PasswordHelper.encrypt(password));
		if (!result.isSuccess()) {
			request.setAttribute("errorMessage", result.getMessage());
			return goLoginPath(backUrl, request);
		}
		else {
			User user = (User) result.getData();
			LoginUserDto loginUser = new LoginUserDto(user.getId(), user.getAccount());
			String token = CookieUtils.getCookie(request, TokenManager.TOKEN);
			if (StringUtils.isEmpty(token) || tokenManager.validate(token) == null) {// 没有登录的情况
				token = createToken(loginUser);
				addTokenInCookie(token, request, response);
			}

			// 跳转到原请求
			backUrl = URLDecoder.decode(backUrl, "utf-8");
			return "redirect:" + authBackUrl(backUrl, token);
		}
	}
	
	/**
     * 获取IP地址
     * @param request
     * @return
     */
	private String getIpAddr(HttpServletRequest request) {
        String ip = request.getHeader("X-Real-IP");
        if (!StringUtils.isEmpty(ip) && !"unknown".equalsIgnoreCase(ip)) {
            return ip;
        }
        ip = request.getHeader("X-Forwarded-For");
        if (!StringUtils.isEmpty(ip) && !"unknown".equalsIgnoreCase(ip)) {
            // 多次反向代理后会有多个IP值，第一个为真实IP。
            int index = ip.indexOf(',');
            if (index != -1) {
                return ip.substring(0, index);
            }
            else {
                return ip;
            }
        }
        else {
            return request.getRemoteAddr();
        }
    }
	
	private String goLoginPath(String backUrl, HttpServletRequest request) {
		request.setAttribute("backUrl", backUrl);
		return LOGIN_PATH;
	}

	private String authBackUrl(String backUrl, String token) {
		StringBuilder sbf = new StringBuilder(backUrl);
		if (backUrl.indexOf("?") > 0) {
			sbf.append("&");
		}
		else {
			sbf.append("?");
		}
		sbf.append(SsoConstant.SSO_TOKEN_NAME).append("=").append(token);
		return sbf.toString();
	}

	private String createToken(LoginUserDto loginUser) {
		// 生成token
		String token = UUID.randomUUID().toString().replaceAll("-", "");

		// 缓存中添加token对应User
		tokenManager.addToken(token, loginUser);
		return token;
	}
	
	private void addTokenInCookie(String token, HttpServletRequest request, HttpServletResponse response) {
		// Cookie添加token
		Cookie cookie = new Cookie(TokenManager.TOKEN, token);
		cookie.setPath("/");
		if ("https".equals(request.getScheme())) {
			cookie.setSecure(true);
		}
		cookie.setHttpOnly(true);
		response.addCookie(cookie);
	}
}