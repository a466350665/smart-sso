package com.smart.sso.server.controller;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.smart.mvc.model.Result;
import com.smart.mvc.provider.IdProvider;
import com.smart.mvc.provider.PasswordProvider;
import com.smart.mvc.util.CookieUtils;
import com.smart.mvc.util.SpringUtils;
import com.smart.mvc.util.StringUtils;
import com.smart.mvc.validator.Validator;
import com.smart.mvc.validator.annotation.ValidateParam;
import com.smart.sso.client.ApplicationUtils;
import com.smart.sso.server.common.LoginUser;
import com.smart.sso.server.common.TokenManager;
import com.smart.sso.server.model.User;
import com.smart.sso.server.service.UserService;
import com.smart.sso.server.service.impl.PermissionSubject;

/**
 * 登录
 * 
 * @author Joe
 */
@Controller
@RequestMapping("/login")
public class LoginController {
	
	// 登录页
	private static final String LOGIN_PATH = "/login";

	@Resource
	private TokenManager tokenManager;
	@Resource
	private UserService userService;

	@RequestMapping(method = RequestMethod.GET)
	public String login(
			@ValidateParam(name = "返回链接", validators = { Validator.NOT_BLANK }) String backUrl,
			@ValidateParam(name = "应用编码", validators = { Validator.NOT_BLANK }) String appCode,
			HttpServletRequest request) {
		String token = CookieUtils.getCookie(request, "token");
		if (token == null) {
			request.setAttribute("backUrl", backUrl);
			request.setAttribute("appCode", appCode);
			return LOGIN_PATH;
		}
		else {
			LoginUser loginUser = tokenManager.validate(token);
			if (loginUser != null) {
				// 为应用添加权限主题观察者，以便应用权限修改通知到对应应用更新权限
				PermissionSubject permissionSubject = SpringUtils.getBean(PermissionSubject.class);
				if (permissionSubject != null)
					permissionSubject.attach(appCode);

				return "redirect:" + authBackUrl(backUrl, token);
			}
			else {
				request.setAttribute("backUrl", backUrl);
				request.setAttribute("appCode", appCode);
				return LOGIN_PATH;
			}
		}
	}

	@RequestMapping(method = RequestMethod.POST)
	public String login(
			@ValidateParam(name = "返回链接", validators = { Validator.NOT_BLANK }) String backUrl,
			@ValidateParam(name = "应用编码", validators = { Validator.NOT_BLANK }) String appCode,
			@ValidateParam(name = "登录名", validators = { Validator.NOT_BLANK }) String account,
			@ValidateParam(name = "密码", validators = { Validator.NOT_BLANK }) String password,
			HttpServletRequest request, HttpServletResponse response) throws UnsupportedEncodingException {
		Result result = userService.login(ApplicationUtils.getIpAddr(request), appCode, account,
				PasswordProvider.encrypt(password));
		if (!result.isSuccess()) {
			request.setAttribute("errorMessage", result.getMessage());
			request.setAttribute("backUrl", backUrl);
			request.setAttribute("appCode", appCode);
			return LOGIN_PATH;
		}
		else {
			User user = (User) result.getData();
			LoginUser loginUser = new LoginUser(user.getId(), user.getAccount(), user);
			
			String token = tokenManager.existsLoginUser(loginUser);
			if (StringUtils.isBlank(token)) {// 当前用户已登录
				token = createToken(loginUser);
			}
			addTokenInCookie(token, response);

			// 为应用添加权限主题观察者，以便应用权限修改通知到对应应用更新权限
			PermissionSubject permissionSubject = SpringUtils.getBean(PermissionSubject.class);
			if (permissionSubject != null)
				permissionSubject.attach(appCode);

			// 跳转到原请求
			backUrl = URLDecoder.decode(backUrl, "utf-8");
			return "redirect:" + authBackUrl(backUrl, token);
		}
	}

	private String authBackUrl(String backUrl, String token) {
		StringBuilder sbf = new StringBuilder(backUrl);
		if (backUrl.indexOf("?") > 0) {
			sbf.append("&");
		}
		else {
			sbf.append("?");
		}
		sbf.append(ApplicationUtils.SSO_TOKEN_NAME).append("=").append(token);
		return sbf.toString();
	}

	private String createToken(LoginUser loginUser) {
		// 生成token
		String token = IdProvider.createUUIDId();

		// 缓存中添加token对应User
		tokenManager.addToken(token, loginUser);
		return token;
	}
	
	private void addTokenInCookie(String token, HttpServletResponse response) {
		// Cookie添加token
		Cookie cookie = new Cookie("token", token);
		cookie.setPath("/");
		cookie.setHttpOnly(true);
		response.addCookie(cookie);
	}
}